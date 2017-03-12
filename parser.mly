%{
    module SS = Set.Make(String)
    open Sdl

    let newSet str =
        SS.singleton str
    
    let addStr str1 str2 =
        str1 ^ str2

    let addSetStr modStr set =
        SS.iter (addStr modStr) set
        
    let rec letterStar letter = 

    let print_set set =
        let list = SS.elements set in
        print_string "{";
        let rec aux l =
            match l with
                  [] -> ()
                | [x;y] -> print_string x; print_string ", "; print_string y; print_string "}"
                | h::t -> print_string h; print_string ", "; aux t; print_string "}" 
        in aux list;;

        
%}

%token EOF
%token LCURLY RCURLY COMMA EMPTY
%token <string> LETTER
%token UNION ITER CONCAT
%token <int> INT
%token STDIN STDOUT
%left UNION INTER CONCAT
%left COMMA
%nonassoc EOL
%left STDIN
%start main
%type <string> main
%start firstrun
%type <string> firstrun
%%
/* add types in own section */
firstrun:
      STDIN firstrun     {$1 ^ (input_line stdin) ^ $3}
    | OTHERS firstrun    { $1 }
    | EOF
main:
    expr EOF                      { $1 }
;
expr:
      IDENT ASSIGN expr2 IN expr     { sdlLet($1,$3,$5)}
    | setCreation                    { $1 }
    | IDENT                          { sdlVar($1) }
    | setCreation SEQ setCreation    { seq($1,$3)}
expr2:
      INT                            { sdlNum($1) }
    | setCreation                    { $1 }
setCreation:
      LCURLY setExpr RCURLY          { $2 }
    | STARSET                        { set(Sdl.newStarSet($1))}
    | LCURLY RCURLY                  { Set(Sdl.newEmptySet) }
    | setCreation UNION IDENT setCreation  { union($1, $4, $3) }
    | setCreation INTER IDENT setCreation  { iter($1, $4, $3)}
    | setCreation CONCAT IDENT setCreation { concat($1, $4, $3)}
;
setExpr:
    | LETTER                { set(Sdl.newSet($1)) }
    | EMPTY                 { set(Sdl.newSet("")) }
    | setExpr COMMA setExpr { union($1,$3) }
;
