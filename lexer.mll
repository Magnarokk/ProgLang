{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
      [' ' '\t']          {token lexbuf}    (* skip blanks *)
    | ['\n' ]             {EOL}
    | '{'                 {LCURLY}
    | '}'                 {RCURLY}
    | ','                 {COMMA}
    | ['a' - 'z']* as lt  {LETTER(lt)}
    | ':'                 {EMPTY}
    | 'U'                 {UNION}
    | 'I'                 {INTER}
    | 'C'                 {CONCAT}
    | ['0' - '9']+ as num {INT(int_of_string num)}
    | "WRITETO"           {STDOUT}
    | eof                 {raise Eof}
and firstrun = parse 
    | "READFROM"          {STDIN}
    | _                   {Lexing.lexeme lexbuf}
    
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
