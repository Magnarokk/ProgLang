%{

    open Sdl ;;

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
%token LANGTYPE INTTYPE
%token EOF
%token LCURLY RCURLY COMMA EMPTY
%token <string> LETTER FIXLETTER
%token UNION ITER CONCAT PREFIX POSTFIX REDUCE
%token <int> INT
%token STDIN STDOUT
%left SEQ
%nonassoc STARSET
%nonassoc REDUCE
%left CONCAT PREFIX POSTFIX
%left UNION INTER
%nonassoc ASSIGN IN
%left COMMA
%nonassoc EOL
%nonassoc OTHERS
%left STDIN
%start main
%type <Sdl.sdlTerm> main
%start firstrun
%type <string> firstrun
%type <Sdl.sdlType> type_spec
%type <Sdl.SS.t>  setCreation
%%
/* add types in own section */
parser_firstrun:
      STDIN firstrun     {$1 ^ (input_line stdin) ^ $3}
    | OTHERS firstrun    { $1 }
    | EOF
;

parser_main:
    expr EOF                      { $1 }
;

type_spec:
      INTTYPE                        { SdlInt }
    | LANGTYPE                       { SdlLang }
;;

expr:
      ASSIGN type_spec IDENT expr2 IN expr { SdlLet($3,$4,$6,$2)}
    | setCreation                    { $1 }
    | IDENT                          { SdlVar($1) }
    | setCreation SEQ setCreation    { Seq($1,$3)}

expr2:
      INT                            { SdlNum($1) }
    | setCreation                    { $1 }
;
setCreation:
      LCURLY setExpr RCURLY          { $2 }
    | STARSET IDENT                  { Set(Sdl.newStarSet($1, $2)) }
    | LCURLY RCURLY                  { Set(Sdl.newEmptySet) }
    | setCreation UNION setCreation  { SdlUnion($1, $3) }
    | setCreation INTER setCreation  { SdlInter($1, $3) }
    | setCreation CONCAT setCreation { SdlConcat($1, $3) }
    | setCreation PREFIX FIXLETTER { SdlPrefix($1, $3) }
    | setCreation POSTFIX FIXLETTER { SdlPostfix($1, $3) }
    | REDUCE setCreation              { SdlReduce($1) }
;
setExpr:
    | LETTER                { Set(Sdl.newSet($1)) }
    | EMPTY                 { Set(Sdl.newSet("")) }
    | setExpr COMMA setExpr { SdlUnion($1,$3) }
;
