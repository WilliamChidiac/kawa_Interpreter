%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token <string> IDENT
%token MAIN
%token LPAR RPAR BEGIN END SEMI
%token ADD SUB MUL DIV MOD
%token PRINT
%token EOF
%left PLUS SUB
%left MUL DIV MOD 


%start program
%type <Kawa.program> program

%%

program:
| MAIN BEGIN main=list(instruction) END EOF
    { {classes=[]; globals=[]; main} }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
;

expression:
| n=INT { Int(n) }
| LPAR e=expression RPAR {e}
| e=expression b=bop e1=expression {Binop(b, e, e1)};

bop : 
| ADD {Add}
| SUB {Sub}
| MUL {Mul}
| DIV {Div}
| MOD {Rem}
      
