{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
      [' ' '\t']     { token lexbuf }     (* skip blanks *)
    | ['\n' ]  { EOL }
    | '{'       {LCURLY}
    | '}'       {RCURLY}
    | ','       {COMMA}
    | ['a' - 'z']* as lt {LETTER(lt)}
    | ':'       {EMPTY}
    | 'U'       {UNION}
    | 'I'       {INTER}
    | 'C'       {CONCAT}
    | ['0' - '9']+ as num {INT(int_of_string num)}
    |

(*
basic lexer skeleton features:
  take program and run
  read
  take a file as input (stdin stderr)
  list
  boolean
  new set

  parser
  declaration
  priorities
  functionality
  declare functions
*)
