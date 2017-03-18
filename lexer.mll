{
open Parser        (* The type token is defined in parser.mli *)
exception Eof

let string_of_set s = (String.sub s 1 ((String.length s)-1));;
let string_of_id s = (String.sub s 1 1);; 

}
(* add types *)
rule lexer_token = parse
      [' ' '\t' '\n' '(' ')' '<' '>']      {lexer_token lexbuf}    (* skip blanks *)
    | '{'                 {LCURLY}
    | '}'                 {RCURLY}
    | ','                 {COMMA}
    | ['a' - 'z']+ as lt  {LETTER(lt)}

    | 'F'['a' - 'z'] as fixLt {FIXLETTER(string_of_id fixLt)}
    | '*'['a' - 'z']+ as st {STARSET(string_of_set st)}
    | '$'['a'-'z'] as id  {IDENT(string_of_id id)}
    | ':'                 {EMPTY}

    | 'U'                 {UNION}
    | 'I'                 {INTER}
    | 'C'                 {CONCAT}
    | 'R'                 {REDUCE}

    | '@'                 {LANGTYPE}
    | '#'                 {INTTYPE}

    | ['0' - '9']+ as num {INT(int_of_string num)}
    | "ASS2"              {ASSIGN}
    | "THISSTUFF"         {IN}
    | '!'                 { SEQ }
    | eof                 { EOF }
and lexer_firstrun = parse 
    | "READFROM"          {STDIN}
    | eof                 {EOF}
    | "\n"                {EOL}
    | _                   {OTHERS(Lexing.lexeme lexbuf)}
