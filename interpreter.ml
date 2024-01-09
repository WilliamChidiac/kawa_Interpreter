open Kawa

type value =
  | VInt of int
  | VBool of bool
  | VObj of obj
  | Null

and vars = {
  mutable v_value : value;
  v_final : bool;
  v_static : bool;
}

and obj = {
  cls : string;
  fields : (string, vars) Hashtbl.t;
}

exception Error of string

exception Return of vars

let create_var ?(s = false) ?(f = false) ?(v = Null) () =
  { v_value = v; v_final = f; v_static = s }

let change_var var value = var.v_value <- value

let exec_prog (p : program) : unit =
  let env = Hashtbl.create 16 in
  let static_env = Hashtbl.create (List.length p.classes) in
  List.iter
    (fun cls ->
      let class_env =
        Hashtbl.create
          (List.fold_left
             (fun i a ->
               if a.a_static then
                 i + 1
               else
                 i)
             0 cls.attributes) in
      List.iter
        (fun att ->
          if att.a_static then
            Hashtbl.add class_env att.a_name
              { v_value = Null; v_static = att.a_static; v_final = att.a_final })
        cls.attributes ;
      Hashtbl.add static_env cls.class_name class_env)
    p.classes ;
  let init_var =
    List.fold_left
      (fun l var ->
        Hashtbl.add env var.v_name (create_var ~f:var.v_final ()) ;
        match var.v_value with
        | Some v -> Set (Var var.v_name, v) :: l
        | None -> l)
      [] p.globals in

  let rec eval_call f this args env =
    let rec eval_meth cls_name f =
      match List.find_opt (fun cls -> cls.class_name = cls_name) p.classes with
      | Some cls -> begin
        match List.find_opt (fun m -> m.method_name = f) cls.methods with
        | Some m ->
          let local_env = Hashtbl.copy env in
          Hashtbl.add local_env "this" (create_var ~v:(VObj this) ()) ;
          List.iter2
            (fun (name, t) value ->
              Hashtbl.add local_env name (create_var ~v:value ()))
            m.params args ;
          let e_l =
            List.fold_left
              (fun l var ->
                Hashtbl.add local_env var.v_name (create_var ~f:var.v_final ()) ;
                match var.v_value with
                | Some e -> Set (Var var.v_name, e) :: l
                | None -> l)
              [] m.locals in
          exec_seq e_l local_env ;
          (try
             exec_seq m.code local_env ;
             create_var ()
           with Return e -> e)
            .v_value
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
    and structural_equality e1 e2 =
      match (eval e1, eval e2) with
      | VInt n1, VInt n2 -> n1 = n2
      | VBool b1, VBool b2 -> b1 = b2
      | VObj o1, VObj o2 ->
        if o1.cls = o2.cls then
          let res =
            Hashtbl.fold
              (fun a b acc ->
                let v = Hashtbl.find o2.fields a in
                v.v_value = b.v_value && acc)
              o1.fields true in
          res
        else
          false
      | _ -> failwith "non effectue"
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
      | Binop (Sequ, e1, e2) -> VBool (structural_equality e1 e2)
      | Binop (Sneq, e1, e2) -> VBool (not (structural_equality e1 e2))
      | Unop (Opp, e) -> VInt (-evali e)
      | Unop (Not, e) -> VBool (not (evalb e))
      | Get id ->
        let s, nenv = memory id lenv in
        (Hashtbl.find nenv s).v_value
      | This -> (Hashtbl.find lenv "this").v_value
      | New s -> begin
        match List.find_opt (fun a -> a.class_name = s) p.classes with
        | Some cls ->
          let super =
            match cls.parent with
            | Some x -> (evalo (New x)).fields
            | None -> Hashtbl.create 1 in
          List.iter
            (fun a ->
              Hashtbl.add super a.a_name
                (if a.a_static then (
                   let value =
                     Hashtbl.find
                       (Hashtbl.find static_env cls.class_name)
                       a.a_name in
                   if value.v_value = Null then
                     change_var value
                       (match a.a_value with
                       | None -> Null
                       | Some e -> eval e) ;
                   value
                 ) else
                   {
                     v_value =
                       (match a.a_value with
                       | Some value -> eval value
                       | None -> Null);
                     v_final = a.a_final;
                     v_static = a.a_static;
                   }))
            cls.attributes ;
          VObj { cls = s; fields = super }
        | None ->
          failwith (Printf.sprintf "the class %s has not been implemented." s)
      end
      | NewCstr (cls_name, params) ->
        let obj = evalo (New cls_name) in
        let _ = eval_call "constructor" obj (get_params_value params) lenv in
        VObj obj
      | MethCall (o, meth, param_expression) ->
        (get_params_value param_expression |> eval_call meth (evalo o)) lenv
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
      | Set (m, e) -> (
        let s, nenv = memory m lenv in
        match Hashtbl.find_opt nenv s with
        | None ->
          Hashtbl.add nenv s
            { v_value = eval e; v_final = false; v_static = false }
        | Some var ->
          if var.v_final && var.v_value != Null then
            failwith "the field you're trying to change is final."
          else
            change_var var (eval e))
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
        let ret = { v_value = eval e; v_final = false; v_static = false } in
        raise (Return ret)
      | Expr e ->
        let _ = eval e in
        ()
    and exec_seq s = List.iter exec s in

    exec_seq s in
  exec_seq init_var env ;
  exec_seq p.main env
