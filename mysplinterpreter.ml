open Sdl
open Lexer
open Parser
open Arg
open Printf

let parseProgram c = 
    try let lexbuf = Lexing.from_channel c in
        let result1 = parser_firstrun lexer_firstrun lexbuf in
        let resultbuf = Lexing.from_string result1 in
        parser_main lexer_token resultbuf
    with Parsing.Parse_error -> failwith "Ew Poor Syntax" ;;

let arg = ref stdin in
let setProg p = arg := open_in p in
let usage = "./mysplinterpreter PROGRAM_FILE" in
parse [] setProg usage ;
let parsedProg = parseProgram !arg in
let _ = typeProg parsedProg in
eval parsedProg;
flush stdout