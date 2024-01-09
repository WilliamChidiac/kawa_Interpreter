%{

  open Lexing
  open Kawa

%}

%token <bool> BOOL
%token <int> INT
%token VINT VBOOL VVOID
%token <string> IDENT
%token MAIN
%token VAR
%token LPAR RPAR BEGIN END SEMI
%token ADD SUB MUL DIV MOD U_SUB (*arithmetique numerique*)
%token LT LE GT GE EQ NEQ AND OR NOT (*arithmetique booleenne*)
%token SEQU SNEQ (*structural equality and inequality*)
%token IF ELSE WHILE
%token CLASS EXT DOT NEW
%token COMMA
%token METH THIS RETURN
%token PRINT SET

%token SUPER
%token PRIVATE PROTECTED PUBLIC
%token FINAL STATIC

%token EOF

(*declaration des priorite*)
%left OR
%left AND
%nonassoc LT LE GT GE EQ NEQ SEQU SNEQ
%right NOT 
%left ADD SUB
%left MUL DIV MOD 
%nonassoc U_SUB
%left DOT

%start program
%type <Kawa.program> program

%%

typ:
| VINT {TInt}
| VBOOL {TBool}
| i=IDENT {TClass(i)}

meth_typ:
| VVOID {TVoid}
| t=typ {t}

var:
| id=IDENT {(id, None)}
| id=IDENT SET v=expression {(id, Some v)}

final:
| FINAL {true}
| {false}

static:
| STATIC {true}
| {false}

decl:
|v_final=final v_typ=typ ids=separated_list(COMMA, var) 
{ List.map (fun (v_name, v_value) -> {v_name; v_typ; v_value; v_final}) ids }

var_decl_list:
| VAR v=decl SEMI l=var_decl_list { v @ l }
| { [] }
;

visibility:
| PRIVATE {Private}
| PROTECTED {Protected}
| PUBLIC {Public}

attr_decl:
| visibility=visibility s=static v=decl SEMI l=attr_decl 
  {List.fold_left 
    (fun li var -> match var.v_value, s, var.v_final with
    | None, true, true -> failwith "static final attribute should be initialized at declaration."
    | _, _, _ -> 
      {a_name=var.v_name; 
       a_type=var.v_typ; 
       a_visibility=visibility;
       a_final=var.v_final; 
       a_static=s; 
       a_value=var.v_value} :: li
    )l v}
| { [] }

params_decl:
| t=typ i=IDENT {(i, t)}

meth_def:
| METH visibility=visibility return=meth_typ method_name=IDENT LPAR params=separated_list(COMMA, params_decl) RPAR 
      BEGIN locals=var_decl_list code=instruction* END
  {{method_name; code; params; locals; return; visibility}}

extend_opt:
| EXT parent=IDENT {Some parent}
| { None }

class_def:
| CLASS class_name=IDENT parent=extend_opt BEGIN attributes=attr_decl methods=meth_def*  END 
    {{class_name; attributes; methods; parent}}

mem:
| id = IDENT { Var(id) }
| obj=expression DOT attr=IDENT { Field(obj, attr) }
;

program:
| globals=var_decl_list classes=class_def* MAIN BEGIN main=list(instruction) END EOF
    { {classes; globals; main} }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
| m=mem SET e=expression SEMI{Set(m, e)}
| IF LPAR e=expression RPAR BEGIN i1=instruction* END ELSE BEGIN i2=instruction* END { If(e, i1, i2) }
| WHILE LPAR e=expression RPAR BEGIN i=instruction* END { While(e, i) }
| RETURN e=expression SEMI {Return (e)}
| e=expression SEMI {Expr (e)}
;

expression:
| n = INT { Int(n) }
| b = BOOL { Bool(b) }
| m = mem {Get (m)}
| LPAR e=expression RPAR {e}
| e=expression b=bop e1=expression {Binop(b, e, e1)}
| SUB e=expression %prec U_SUB {Unop(Opp, e)}
| NOT e=expression {Unop(Not, e)}
| NEW obj=IDENT {New(obj)}
| NEW cls=IDENT LPAR params=separated_list(COMMA, expression) RPAR {NewCstr(cls, params)}
| THIS { This }
| obj=expression DOT meth=IDENT LPAR params=separated_list(COMMA, expression) RPAR {MethCall(obj, meth, params)}
| SUPER LPAR params=separated_list(COMMA, expression) RPAR {MethCall(This, "super", params)}
;

%inline bop : 
| SEQU {Sequ}
| SNEQ {Sneq}
| OR {Or}
| AND {And}
| ADD {Add}
| SUB {Sub}
| MUL {Mul}
| DIV {Div}
| MOD {Rem}
| LT {Lt}
| LE {Le}
| GT {Gt}
| GE {Ge}
| EQ {Eq}
| NEQ {Neq}
;