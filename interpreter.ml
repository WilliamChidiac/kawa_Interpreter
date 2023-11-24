open Kawa

type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | Null
and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}

exception Error of string
exception Return of value

let exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in
  List.iter (fun (x, _) -> Hashtbl.add env x Null) p.globals;
  
  let rec eval_call f this args =
    failwith "eval_call not implemented"

  and exec_seq s lenv =
    let rec evali e = match eval e with
      | VInt n -> n
      | _ -> assert false
    and evalb e = match eval e with
      | VBool b -> b
      | _ -> assert false
    and evalo e = match eval e with
      | VObj o -> o
      | _ -> assert false

    and memory (m : mem_access) = match m with
      | Var(s) -> s
      | _ -> failwith "pas encore fait"
        
    and eval (e: expr): value = match e with
      | Int n  -> VInt n
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
      | Binop (Or, e1, e2) -> VBool ((evalb e1) || (evalb e2))
      | Binop (And, e1, e2) -> VBool ((evalb e1) && (evalb e2))
      | Unop(Opp, e) -> VInt (- evali e)
      | Unop(Not, e) -> VBool (not (evalb e))
      | Get id -> Hashtbl.find env (memory id)
      | _ -> failwith "case not implemented in eval"
    in
  
    let rec exec (i: instr): unit = match i with
      | Print e ->
        begin match eval e with 
        |VInt i ->  Printf.printf "%d\n" (i)
        |VBool b -> Printf.printf "%b\n" b
        | _ -> failwith "case not implemented in exec"
        end
      | Set(m, e) -> Hashtbl.add env (memory m) (eval e)
      | _ -> failwith "case not implemented in exec"
    and exec_seq s = 
      List.iter exec s
    in

    exec_seq s
  in
  exec_seq p.main (Hashtbl.create 1)
