{
open Parser        (* The type token is defined in parser.mli *)
exception Eof

let string_of_set s = String.sub 1 ((String.length s)-1);;
let string_of_id s = String.sub 1 1;; 

}
(* add types *)
rule token = parse
      [' ' '\t' '\n']          {token lexbuf}    (* skip blanks *)
    | '{'                 {LCURLY}
    | '}'                 {RCURLY}
    | ','                 {COMMA}
    | ['a' - 'z']* as lt  {LETTER(lt)}
    | '*'['a' - 'z']+ as st {STARSET(string_of_set st)}
    | '£'['a'-'z'] as id  {IDENT(string_of_id id)}
    | ':'                 {EMPTY}
    | 'U'                 {UNION}
    | 'I'                 {INTER}
    | 'C'                 {CONCAT}
    | ['0' - '9']+ as num {INT(int_of_string num)}
    | "ASS2"              {ASSIGN}
    | "THISSTUFF"         {IN}
    | '!'                 { SEQ }
    | eof                 { EOF }
and firstrun = parse 
    | "READFROM"          {STDIN}
    | _                   {OTHERS(Lexing.lexeme lexbuf)}

(*
basic lexer skeleton features:
  take program and run
  list
  boolean
  new set

  parser
  declaration
  priorities
  functionality
  declare functions
*)
