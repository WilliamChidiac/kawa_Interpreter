open Kawa

type value =
  | VInt of int
  | VBool of bool
  | VObj of obj
  | Null

and obj = {
  cls : string;
  fields : (string, value) Hashtbl.t;
}

exception Error of string

exception Return of value

let exec_prog (p : program) : unit =
  let env = Hashtbl.create 16 in
  List.iter
    (fun (x, _, v) ->
      match v with
      | VAInt v -> Hashtbl.add env x (VInt v)
      | VABool b -> Hashtbl.add env x (VBool b)
      | VANull -> Hashtbl.add env x Null)
    p.globals ;

  let rec eval_call f this args =
    let local_env = Hashtbl.create 10 in
    let rec eval_meth cls_name f =
      match List.find_opt (fun cls -> cls.class_name = cls_name) p.classes with
      | Some cls -> begin
        match List.find_opt (fun m -> m.method_name = f) cls.methods with
        | Some m ->
          Hashtbl.add local_env "this" (VObj this) ;
          Hashtbl.add local_env "return" Null ;
          List.iter
            (fun (name, t, _) ->
              Hashtbl.add local_env name (Hashtbl.find env name))
            p.globals ;
          List.iter2
            (fun (name, t) value -> Hashtbl.add local_env name value)
            m.params args ;
          List.iter
            (fun (name, t, _) -> Hashtbl.add local_env name Null)
            m.locals ;
          (try exec_seq m.code local_env with Failure e -> ()) ;
          Hashtbl.find local_env "return"
        | None -> (
          match cls.parent with
          | Some x ->
            if f = "super" then
              eval_meth x "constructor"
            else
              eval_meth x f
          | None ->
            failwith
              (Printf.sprintf "the method you are trying to call is undefined.")
          )
      end
      | None -> failwith (Printf.sprintf "class undefined.") in
    eval_meth this.cls f
  and exec_seq s lenv =
    let rec evali e =
      match eval e with
      | VInt n -> n
      | _ -> assert false
    and evalb e =
      match eval e with
      | VBool b -> b
      | _ -> assert false
    and evalo e =
      match eval e with
      | VObj o -> o
      | _ -> assert false
    and memory (m : mem_access) env =
      match m with
      | Var s -> (s, env)
      | Field (e, s) ->
        let o = evalo e in
        (s, o.fields)
    and eval (e : expr) : value =
      match e with
      | Int n -> VInt n
      | Bool b -> VBool b
      | Binop (Add, e1, e2) -> VInt (evali e1 + evali e2)
      | Binop (Sub, e1, e2) -> VInt (evali e1 - evali e2)
      | Binop (Mul, e1, e2) -> VInt (evali e1 * evali e2)
      | Binop (Div, e1, e2) -> VInt (evali e1 / evali e2)
      | Binop (Rem, e1, e2) -> VInt (evali e1 mod evali e2)
      | Binop (Lt, e1, e2) -> VBool (evali e1 < evali e2)
      | Binop (Le, e1, e2) -> VBool (evali e1 <= evali e2)
      | Binop (Gt, e1, e2) -> VBool (evali e1 > evali e2)
      | Binop (Ge, e1, e2) -> VBool (evali e1 >= evali e2)
      | Binop (Eq, e1, e2) -> VBool (evali e1 = evali e2)
      | Binop (Neq, e1, e2) -> VBool (evali e1 <> evali e2)
      | Binop (Or, e1, e2) -> VBool (evalb e1 || evalb e2)
      | Binop (And, e1, e2) -> VBool (evalb e1 && evalb e2)
      | Unop (Opp, e) -> VInt (-evali e)
      | Unop (Not, e) -> VBool (not (evalb e))
      | Get id ->
        let s, nenv = memory id lenv in
        Hashtbl.find nenv s
      | This ->
        let res = Hashtbl.find lenv "this" in
        res
      | New s -> begin
        match List.find_opt (fun a -> a.class_name = s) p.classes with
        | Some cls ->
          let super =
            match cls.parent with
            | Some x -> (evalo (New x)).fields
            | None -> Hashtbl.create 1 in
          List.iter (fun (a, t) -> Hashtbl.add super a Null) cls.attributes ;
          VObj { cls = s; fields = super }
        | None ->
          failwith (Printf.sprintf "the class %s has not been implemented." s)
      end
      | NewCstr (cls_name, params) ->
        let obj = evalo (New cls_name) in
        let _ = eval_call "constructor" obj (get_params_value params) in
        VObj obj
      | MethCall (o, meth, param_expression) ->
        get_params_value param_expression |> eval_call meth (evalo o)
    and get_params_value params =
      List.rev
        (List.fold_left (fun p_eval p_exp -> eval p_exp :: p_eval) [] params)
    in
    let rec exec (i : instr) : unit =
      match i with
      | Print e -> begin
        match eval e with
        | VInt i -> Printf.printf "%d\n" i
        | VBool b -> Printf.printf "%b\n" b
        | _ -> failwith "case not implemented in exec"
      end
      | Set (m, e) ->
        let s, nenv = memory m lenv in
        Hashtbl.add nenv s (eval e)
      | If (e, i1, i2) ->
        if evalb e then
          exec_seq i1
        else
          exec_seq i2
      | While (e, i) ->
        while evalb e do
          exec_seq i
        done
      | Return e ->
        Hashtbl.add lenv "return" (eval e) ;
        raise (Failure "return statement reached")
      | Expr e ->
        let _ = eval e in
        ()
    and exec_seq s = List.iter exec s in

    exec_seq s in

  exec_seq p.main env
