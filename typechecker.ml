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

type class_utilities =
  | Method of method_def
  | Attribut of attribute

let typecheck_prog p =
  let rec check_accessiblity cls_name main_class m_a_name names_from_cls
      att_or_meth =
    let f_name = function
      | Method m -> m.method_name
      | Attribut a -> a.a_name in
    let f_type = function
      | Method m -> m.return
      | Attribut a -> a.a_type in
    let f_visibility = function
      | Method m -> m.visibility
      | Attribut a -> a.a_visibility in
    let inscope = ref false in
    let rec check_att_typ cls_name accessible =
      begin
        match
          List.find_opt (fun cls -> cls.class_name = cls_name) p.classes
        with
        | Some cls -> begin
          if cls.class_name = main_class then inscope := true ;
          match
            List.find_opt (fun a -> f_name a = m_a_name) (names_from_cls cls)
          with
          | Some a -> begin
            match (f_visibility a, accessible) with
            | Public, _ -> f_type a
            | Protected, _ | Private, true -> begin
              if main_class = "" then
                error
                  (Printf.sprintf
                     "the %s %s is trying to be acccessed outside of it's \
                      defined scope."
                     att_or_meth (f_name a))
              else if !inscope then
                f_type a
              else
                error
                  (Printf.sprintf
                     "the %s %s is trying to be accessed outside of its \
                      defined scope."
                     att_or_meth (f_name a))
            end
            | Private, false ->
              error
                (Printf.sprintf
                   "the %s %s is private and can only be accessed from inside \
                    the class %s"
                   att_or_meth (f_name a) cls.class_name)
          end
          | None -> (
            match cls.parent with
            | Some c_name -> check_att_typ c_name false
            | None ->
              failwith
                (Printf.sprintf "%s %s is undefined." att_or_meth m_a_name))
        end
        | None -> failwith (Printf.sprintf "object %s is undefined." cls_name)
      end in
    check_att_typ cls_name true
  and get_main_local_env exp tenv =
    let get_main_env key tenv =
      match Env.find_opt key tenv with
      | None -> ""
      | Some (TClass something) -> something
      | Some _ ->
        error
          (Printf.sprintf "%s cannot be defined as a variable or attribute." key)
    in
    ( (match exp with
      | Super -> get_main_env "super" tenv
      | _ -> get_main_env "this" tenv),
      match type_expr exp tenv with
      | TClass s -> s
      | _ -> failwith "object expected." )
  and add_env l tenv =
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
    | Binop (exp, e1, e2) when exp = Sequ || exp = Sneq ->
      let e1_type = type_expr e1 tenv in
      check e2 e1_type tenv ;
      TBool
    | Unop (Opp, e1) ->
      check e1 TInt tenv ;
      TInt
    | Unop (Not, e1) ->
      check e1 TBool tenv ;
      TBool
    | Get s -> type_mem_access s tenv
    | This -> type_mem_access (Var "this") tenv
    | Super -> type_mem_access (Var "super") tenv
    | New s -> TClass s
    | NewCstr (s, params) ->
      begin
        match List.find_opt (fun cls -> cls.class_name = s) p.classes with
        | Some cls -> begin
          match
            List.find_opt (fun m -> m.method_name = cls.class_name) cls.methods
          with
          | Some meth ->
            List.iter2 (fun (s, t) e -> check e t tenv) meth.params params
          | None -> error "no constructor defined for this class"
        end
        | None -> error "class is undefined"
      end ;
      TClass s
    | MethCall (obj, m_name, params) ->
      let main_class, local_class = get_main_local_env obj tenv in
      let m_name =
        if m_name = "super" then
          match
            (List.find (fun cls -> cls.class_name = local_class) p.classes)
              .parent
          with
          | Some parent -> parent
          | None ->
            failwith "you can't use the super keyword if this has no parents."
        else if m_name = local_class then
          "constructor"
        else
          m_name in
      check_accessiblity local_class main_class m_name
        (fun cls -> List.fold_left (fun ms m -> Method m :: ms) [] cls.methods
          : class_def -> 'b list)
        "method"
    | _ -> failwith "case not implemented in type_expr"
  and type_mem_access m tenv =
    match m with
    | Var s -> begin
      try Env.find s tenv
      with Not_found -> error (Printf.sprintf "Variable %s not found" s)
    end
    | Field (exp, attr) ->
      let main_class, local_class = get_main_local_env exp tenv in
      check_accessiblity local_class main_class attr
        (fun cls ->
           List.fold_left (fun atts a -> Attribut a :: atts) [] cls.attributes
          : class_def -> 'b list)
        "attribut"
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
        let aux = Env.add "this" (TClass cls.class_name) tenv in
        let parent =
          match cls.parent with
          | None -> TClass "Super not defined"
          | Some name_of_parent -> TClass name_of_parent in
        let aux_env = Env.add "super" parent aux in
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
