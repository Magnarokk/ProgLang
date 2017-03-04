{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
      [' ' '\t']     { token lexbuf }     (* skip blanks *)
    | ['\n' ]  { EOL }
    | '{'       {L}


(*basic lexer skeleton
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
THIS IS A TEST PUSH
take a file as input (stdin stdout stderr)