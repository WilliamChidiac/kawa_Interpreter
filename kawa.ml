(**
   Kawa : un petit langage à objets inspiré de Java
 *)

(* Types déclarés pour les attributs, pour les variables, et pour les
   paramètres et résultats des méthodes. *)
type typ =
  | TVoid
  | TInt
  | TBool
  | TClass of string

let typ_to_string = function
  | TVoid -> "void"
  | TInt -> "int"
  | TBool -> "bool"
  | TClass c -> c

type unop =
  | Opp
  | Not

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Neq
  | And
  | Or
  | Sequ
  | Sneq

(* Expressions *)
type expr =
  (* Base arithmétique *)
  | Int of int
  | Bool of bool
  | Unop of unop * expr
  | Binop of binop * expr * expr
  (* Accès à une variable ou un attribut *)
  | Get of mem_access
  (* Objet courant *)
  | This
  | Super
  (* Création d'un nouvel objet *)
  | New of string
  | NewCstr of string * expr list
  (* Appel de méthode *)
  | MethCall of expr * string * expr list

(* Accès mémoire : variable ou attribut d'un objet *)
and mem_access =
  | Var of string
  | Field of expr (* objet *) * string (* nom d'un attribut *)

(* Instructions *)
type instr =
  (* Affichage d'un entier *)
  | Print of expr
  (* Écriture dans une variable ou un attribut *)
  | Set of mem_access * expr
  (* Structures de contrôle usuelles *)
  | If of expr * seq * seq
  | While of expr * seq
  (* Fin d'une fonction *)
  | Return of expr
  (* Expression utilisée comme instruction *)
  | Expr of expr

and seq = instr list

(* Définition de méthode

   Syntaxe : method <type de retour> <nom> (<params>) { ... }

   Le corps de la méthode est similaire au corps d'une fonction. *)

type visibility =
  | Private
  | Protected
  | Public

type variable = {
  v_name : string;
  v_typ : typ;
  v_value : expr option;
  v_final : bool;
}

type attribute = {
  a_name : string;
  a_type : typ;
  a_visibility : visibility;
  a_final : bool;
  a_static : bool;
  mutable a_value : expr option;
}

type method_def = {
  mutable method_name : string;
  code : seq;
  params : (string * typ) list;
  locals : variable list;
  return : typ;
  visibility : visibility;
}

(* Définition de classe

   Syntaxe : class <nom de la classe> { ... }
        ou : class <nom de la classe> extends <nom de la classe mère> { ... }

   On considère que toute classe C contient une définition de méthode de nom
   "constructor" et de type de retour void, qui initialise les champs du
   paramètre implicite this. *)

type class_def = {
  class_name : string;
  attributes : attribute list;
  methods : method_def list;
  parent : string option;
}

(* Programme complet : variables globales, classes, et une séquence
   d'instructions *)
type program = {
  classes : class_def list;
  globals : variable list;
  main : seq;
}

let f g a = g a

let _ = f (fun (a, b) -> a + b) (1, 2)

let _ = f (fun a -> 1 + a) 2
