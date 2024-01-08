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

let typecheck_prog p =
  let rec add_env l tenv =
    List.fold_left
      (fun env var ->
        (match var.v_value with
        | Some e -> check e var.v_typ tenv
        | None -> ()) ;
        Env.add var.v_name var.v_typ env)
      tenv l
  and check e typ tenv =
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
        let s =
          if s = "super" then
            match
              (List.find (fun cls -> cls.class_name = cls_name) p.classes)
                .parent
            with
            | Some parent -> parent
            | None ->
              failwith "you can't use the super keyword if this has no parents."
          else
            s in
        let rec get_return_typ cls_name accessible =
          begin
            match
              List.find_opt (fun cls -> cls.class_name = cls_name) p.classes
            with
            | Some cls -> begin
              let s =
                if s = cls.class_name then
                  "constructor"
                else
                  s in
              match
                List.find_opt (fun meth -> meth.method_name = s) cls.methods
              with
              | Some meth ->
                if accessible || meth.visibility = Protected then
                  meth.return
                else
                  error
                    (Printf.sprintf
                       "the method %s is private and can only be accessed from \
                        inside the class %s"
                       meth.method_name cls.class_name)
              | None -> (
                match cls.parent with
                | Some c_name -> get_return_typ c_name false
                | None -> failwith (Printf.sprintf "method %s is undefined." s))
            end
            | None ->
              failwith (Printf.sprintf "class %s should not exist." cls_name)
          end in
        get_return_typ cls_name true
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
        let inscope = ref false in
        let main_class =
          match Env.find_opt "this" tenv with
          | None -> ""
          | Some (TClass something) -> something
          | Some _ -> error "this statment should never be reached." in
        let rec check_att_typ cls_name accessible =
          begin
            match
              List.find_opt (fun cls -> cls.class_name = cls_name) p.classes
            with
            | Some cls -> begin
              if cls.class_name = main_class then inscope := true ;
              match List.find_opt (fun a -> a.a_name = attr) cls.attributes with
              | Some a -> begin
                match (a.a_visibility, accessible) with
                | Public, _ -> a.a_type
                | Protected, _ | Private, true -> begin
                  if main_class = "" then
                    error
                      (Printf.sprintf
                         "the attribut %s is trying to be acccessed outside of \
                          it's defined scope."
                         a.a_name)
                  else if !inscope then
                    a.a_type
                  else
                    error
                      (Printf.sprintf
                         "the attribute %s is trying to be accessed outside of \
                          its defined scope. %s %s"
                         a.a_name main_class cls.class_name)
                end
                | Private, false ->
                  error
                    (Printf.sprintf
                       "the attribute %s is private and can only be accessed \
                        from inside the class %s"
                       a.a_name cls.class_name)
              end
              | None -> (
                match cls.parent with
                | Some c_name -> check_att_typ c_name false
                | None ->
                  failwith (Printf.sprintf "attribut %s is undefined." attr))
            end
            | None -> failwith (Printf.sprintf "object %s is undefined." s)
          end in
        check_att_typ s true
      | _ -> failwith "erreur de syntaxe."
    end
  and check_instr i ret tenv =
    match i with
    | Print e -> check_multi e [TInt; TBool] tenv
    | Set (m, e) -> begin
      match (type_mem_access m tenv, type_expr e tenv) with
      | TClass static_type, TClass abstract_type ->
        let rec check_heritage static =
          if static = abstract_type then
            ()
          else
            let cls = List.find (fun cls -> cls.class_name = static) p.classes in
            match cls.parent with
            | Some parent -> check_heritage parent
            | None ->
              error
                (Printf.sprintf "%s is not a subclass of %s" static_type
                   abstract_type) in
        check_heritage static_type
      | t1, t2 -> if t1 <> t2 then type_error t2 t1
    end
    | If (e, i1, i2) ->
      check e TBool tenv ;
      check_seq i1 ret tenv ;
      check_seq i2 ret tenv
    | While (e, i) ->
      check e TBool tenv ;
      check_seq i ret tenv
    | Return e -> ()
    | Expr e ->
      let _ = type_expr e tenv in
      ()
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
    List.iter
      (fun cls ->
        let aux_env = Env.add "this" (TClass cls.class_name) tenv in
        List.iter
          (fun meth ->
            let ret = ref true in
            let aux_env =
              List.fold_left
                (fun env (name, t) -> Env.add name t env)
                aux_env meth.params in
            let aux_env =
              List.fold_left
                (fun env var -> Env.add var.v_name var.v_typ env)
                aux_env meth.locals in
            List.iter
              (fun instr ->
                match instr with
                | Return e ->
                  check e meth.return aux_env ;
                  ret := false
                | instr -> check_instr instr ret aux_env)
              meth.code ;
            if meth.return != TVoid && !ret then type_error TVoid meth.return)
          cls.methods)
      p.classes in
  let tenv = add_env p.globals Env.empty in
  check_meth tenv ;
  check_seq p.main TVoid tenv
