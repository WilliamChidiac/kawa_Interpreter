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
%token IF ELSE WHILE
%token CLASS EXT ATT DOT NEW
%token COMMA
%token METH THIS RETURN
%token PRINT SET
%token EOF

(*declaration des priorite*)
%left OR
%left AND
%nonassoc LT LE GT GE EQ NEQ
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

var_decl:
| VAR t=typ i=IDENT SEMI{(i, t)}
;

attr_decl:
| ATT t=typ i=IDENT SEMI {(i, t)}

params_decl:
| t=typ i=IDENT {(i, t)}

meth_def:
| METH return=meth_typ method_name=IDENT LPAR params=separated_list(COMMA, params_decl) RPAR 
      BEGIN locals=var_decl* code=instruction* END
  {{method_name; code; params; locals; return}}

extend_opt:
| EXT parent=IDENT {Some parent}
| { None }

class_def:
| CLASS class_name=IDENT parent=extend_opt BEGIN attributes=attr_decl* methods=meth_def*  END 
    {{class_name; attributes; methods; parent}}

mem:
| id = IDENT { Var(id) }
| obj=expression DOT attr=IDENT { Field(obj, attr) }
;

program:
| globals=var_decl* classes=class_def* MAIN BEGIN main=list(instruction) END EOF
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
;

%inline bop : 
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

expression:
| n = INT { Int(n) }
| b = BOOL { Bool(b) }
| m = mem {Get (m)}
| LPAR e=expression RPAR {e}
| e=expression b=bop e1=expression {Binop(b, e, e1)}
| SUB e=expression %prec U_SUB {Unop(Opp, e)}
| NOT e=expression {Unop(Not, e)}
;