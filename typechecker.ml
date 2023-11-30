open Kawa

exception Error of string

let error s = raise (Error s)

let type_error ty_actual ty_expected =
  error
    (Printf.sprintf "expected %s, got %s"
       (typ_to_string ty_expected)
       (typ_to_string ty_actual))

module Env = Map.Make (String)

type tenv = typ Env.t

let add_env l tenv = List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in

  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ
  and type_expr e tenv =
    match e with
    | Int _ -> TInt
    | Bool _ -> TBool
    | Binop (exp, e1, e2)
      when exp = Add || exp = Sub || exp = Mul || exp = Rem || exp = Div ->
      check e1 TInt tenv ;
      check e2 TInt tenv ;
      TInt
    | Binop (exp, e1, e2) when exp = Lt || exp = Le || exp = Gt || exp = Ge ->
      check e1 TInt tenv ;
      check e2 TInt tenv ;
      TBool
    | Binop (exp, e1, e2) when exp = Eq || exp = Neq ->
      check e1 (type_expr e2 tenv) tenv ;
      TBool
    | Binop (exp, e1, e2) when exp = Or || exp = And ->
      check e1 TBool tenv ;
      check e2 TBool tenv ;
      TBool
    | Unop (Opp, e1) ->
      check e1 TInt tenv ;
      TInt
    | Unop (Not, e1) ->
      check e1 TBool tenv ;
      TBool
    | Get s -> type_mem_access s tenv
    | This -> type_mem_access (Var "this") tenv
    | New s -> TClass s
    | NewCstr (s, params) ->
      begin
        match List.find_opt (fun cls -> cls.class_name = s) p.classes with
        | Some cls -> begin
          match
            List.find_opt (fun m -> m.method_name = "constructor") cls.methods
          with
          | Some meth ->
            List.iter2 (fun (s, t) e -> check e t tenv) meth.params params
          | None -> error "no constructor defined for this class"
        end
        | None -> error "class is undefined"
      end ;
      TClass s
    | MethCall (obj, s, params) -> begin
      match type_expr obj tenv with
      | TClass cls_name -> begin
        match
          List.find_opt (fun cls -> cls.class_name = cls_name) p.classes
        with
        | Some cls -> begin
          match List.find_opt (fun m -> m.method_name = s) cls.methods with
          | Some meth ->
            List.iter2 (fun (s, t) e -> check e t tenv) meth.params params ;
            meth.return
          | None -> error "no constructor defined for this class"
        end
        | None -> error "class is undefined"
      end
      | _ -> error "syntaxe error"
    end
    | _ -> failwith "case not implemented in type_expr"
  and type_mem_access m tenv =
    match m with
    | Var s -> begin
      try Env.find s tenv
      with Not_found -> error (Printf.sprintf "Variable %s not found" s)
    end
    | Field (exp, attr) -> begin
      match type_expr exp tenv with
      | TClass s ->
        let rec check_att_typ cls_name =
          begin
            match
              List.find_opt (fun cls -> cls.class_name = cls_name) p.classes
            with
            | Some cls -> begin
              match
                List.find_opt
                  (fun (att_name, att_typ) -> att_name = attr)
                  cls.attributes
              with
              | Some (_, att_typ) -> att_typ
              | None -> (
                match cls.parent with
                | Some c_name -> check_att_typ c_name
                | None ->
                  failwith (Printf.sprintf "attribut %s is undefined." attr))
            end
            | None -> failwith (Printf.sprintf "object %s is undefined." s)
          end in
        check_att_typ s
      | _ -> failwith "erreur de syntaxe."
    end in
  let rec check_instr i ret tenv =
    match i with
    | Print e -> check_multi e [TInt; TBool] tenv
    | Set (m, e) -> check e (type_mem_access m tenv) tenv
    | If (e, i1, i2) ->
      check e TBool tenv ;
      check_seq i1 ret tenv ;
      check_seq i2 ret tenv
    | While (e, i) ->
      check e TBool tenv ;
      check_seq i ret tenv
    | Return e -> ()
    | Expr e -> ()
  and check_multi e type_list tenv =
    begin
      match type_list with
      | [] -> error "wrong type for print"
      | types :: type_list1 ->
        let typ_e = type_expr e tenv in
        if typ_e <> types then check_multi e type_list1 tenv
    end
  and check_seq s ret tenv = List.iter (fun i -> check_instr i ret tenv) s in
  let check_meth tenv =
    p.classes
    |> List.iter (fun cls ->
           let aux_env = Env.add "this" (TClass cls.class_name) tenv in
           cls.methods
           |> List.iter (fun meth ->
                  let ret = ref true in
                  meth.code
                  |> List.iter (fun instr ->
                         match instr with
                         | Return e ->
                           check e meth.return aux_env ;
                           ret := false
                         | _ -> ()) ;
                  if meth.return != TVoid && !ret then
                    type_error TVoid meth.return)) in
  check_meth tenv ;
  check_seq p.main TVoid tenv
