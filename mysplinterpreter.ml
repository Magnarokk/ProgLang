open Sdl
open Lexer
open Parser
open Arg
open Printf

let parseProgram c = 
    try let lexbuf = Lexing.from_channel c in
        (* let result1 = parser_firstrun lexer_firstrun lexbuf in
        let resultbuf = Lexing.from_string result1 in *)
        parser_main lexer_token lexbuf
    with Parsing.Parse_error -> failwith "Ew Poor Syntax" ;;

let arg = ref stdin in
let setProg p = arg := open_in p in
let usage = "./mysplinterpreter PROGRAM_FILE" in
parse [] setProg usage ;
let parsedProg = parseProgram !arg in
let () = print_string "Clap Clap Clap finally good syntax"; print_newline() in
let _ = typeProg parsedProg in
let () = print_string "Aww well done you got the type all right" in
let result = eval parsedProg in
let () = print_string "Finally got to the evaluation step, here is your reward =>"; print_term result; print_newline(); print_string "There is no reward, only laguages, you've been bamboozled" in 
flush stdout