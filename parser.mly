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
%token UNION ITER CONCAT PREFIX POSTFIX REDUCE
%token <int> INT
%token STDIN STDOUT
%left SEQ
%nonassoc REDUCE
%left CONCAT PREFIX POSTFIX
%left UNION INTER
%nonassoc ASSIGN IN
%left COMMA
%nonassoc EOL
%left STDIN
%start main
%type <Sdl.sdlTerm> main
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
    | STARSET IDENT                  { set(Sdl.newStarSet($1, $2)) }
    | LCURLY RCURLY                  { set(Sdl.newEmptySet) }
    | setCreation UNION setCreation  { sdlUnion($1, $3) }
    | setCreation INTER setCreation  { sdlInter($1, $3) }
    | setCreation CONCAT setCreation { sdlConcat($1, $3) }
    | setCreation PREFIX setCreation { sdlPrefix($1, $3) }
    | setCreation POSTFIX setCreation { sdlPostfix($1, $3) }
    | REDUCE setCreation              { sdlReduce($1) }
;
setExpr:
    | LETTER                { set(Sdl.newSet($1)) }
    | EMPTY                 { set(Sdl.newSet("")) }
    | setExpr COMMA setExpr { sdlUnion($1,$3) }
;
