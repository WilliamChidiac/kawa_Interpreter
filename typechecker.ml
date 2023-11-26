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
    | Binop (exp, e1, e2)
      when exp = Lt || exp = Le || exp = Gt || exp = Ge || exp = Eq || exp = Neq
      ->
      check e1 TInt tenv ;
      check e2 TInt tenv ;
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
    | _ -> failwith "case not implemented in type_expr"
  and type_mem_access m tenv =
    match m with
    | Var s -> begin
      try Env.find s tenv
      with Not_found -> error (Printf.sprintf "Variable %s not found" s)
    end
    | _ -> failwith "case not implemented in type_mem_access" in

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
    | _ -> failwith "case not implemented in check_instr"
  and check_multi e type_list tenv =
    begin
      match type_list with
      | [] -> error "wrong type for print"
      | types :: type_list1 ->
        let typ_e = type_expr e tenv in
        if typ_e <> types then check_multi e type_list1 tenv
    end
  and check_seq s ret tenv = List.iter (fun i -> check_instr i ret tenv) s in

  check_seq p.main TVoid tenv
