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

%start program
%type <Kawa.program> program

%%

typ:
| VINT {TInt}
| VBOOL {TBool}

var_decl:
| VAR t=typ i=IDENT SEMI{(i, t)}
;

mem:
| id = IDENT { Var(id) }
;

program:
| globals=var_decl* MAIN BEGIN main=list(instruction) END EOF
    { {classes=[]; globals; main} }
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

