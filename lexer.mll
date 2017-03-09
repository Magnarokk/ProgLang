{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
      [' ' '\t']     { token lexbuf }     (* skip blanks *)
    | ['\n' ]  { EOL }
    | '{'       {L}

(*
basic lexer skeleton features:
  take program and run
  {    
  } 
  , 
  ['a' - 'z']* 
  : 
  union 
  inter 
  concat 
  add 
  ['0'-'9']+ 
  * 
  EOL
  read
  take a file as input (stdin stdout stderr)

  parser
  declaration
  priorities
  functionality
  declare functions
*)
