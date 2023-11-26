%{

  open Lexing
  open Kawa

%}

%token <bool> BOOL
%token <int> INT
%token VINT VBOOL
%token <string> IDENT
%token MAIN
%token VAR
%token LPAR RPAR BEGIN END SEMI
%token ADD SUB MUL DIV MOD U_SUB (*arithmetique numerique*)
%token LT LE GT GE EQ NEQ AND OR NOT (*arithmetique booleenne*)
%token IF ELSE WHILE
%token CLASS ATT DOT NEW
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

var_decl:
| VAR t=typ i=IDENT SEMI{(i, t)}
;

attr_decl:
| ATT t=typ i=IDENT SEMI {(i, t)}

class_def:
| CLASS name=IDENT BEGIN attributs=attr_decl* END 
    {{class_name=name; attributes=attributs; methods=[]; parent=None}}

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

