{

  open Lexing
  open Kawaparser

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "print",    PRINT;
      "main",     MAIN;
      "var" ,     VAR;
      "int" ,     VINT;
      "bool" ,    VBOOL;
      "true" ,    BOOL(true);
      "false",    BOOL(false);
      "if",       IF;
      "else",     ELSE;
      "while",    WHILE;
      "class",    CLASS;
      "new",      NEW;
      "method",   METH;
      "void",     VVOID;
      "this",     THIS;
      "return",   RETURN;
      "extends",  EXT; 
      (*extra features*)
      "super",    SUPER; 
      "private",  PRIVATE;
      "protected",PROTECTED;
      "public",   PUBLIC;
      "final",    FINAL;
      "static",   STATIC;
    ] ;
  fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
let number = ['-']? digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
  
rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "//" [^ '\n']* "\n"  { new_line lexbuf; token lexbuf }
  | "/*"                 { comment lexbuf; token lexbuf }

  | number as n  { INT(int_of_string n) }
  | ident as id  { keyword_or_ident id }
  | ","  { COMMA }
  | "."  { DOT }
  | ";"  { SEMI }
  | "="  {SET}
  | "===" {SEQU}
  | "=/=" {SNEQ}
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }
  | "+"  { ADD }
  | "-"  { SUB }
  | "*"  { MUL }
  | "/"  { DIV }
  | "%"  { MOD }
  | "<"  { LT }
  | "<="  { LE }
  | ">"  { GT }
  | ">="  { GE }
  | "=="  { EQ }
  | "!="  { NEQ }
  | "&&"  { AND }
  | "||"  { OR }
  | "!"  { NOT }
  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
