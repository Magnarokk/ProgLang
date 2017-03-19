%{
    open Sdl
%}
%token LANGTYPE INTTYPE
%token EOF EOL
%token LCURLY RCURLY COMMA EMPTY
%token <string> LETTER FIXLETTER IDENT
%token UNION INTER CONCAT PREFIX REDUCE
%token <int> INT
%token STDIN 
%token <string> OTHERS
%token <string> STARSET
%token ASSIGN IN SEQ
%nonassoc EOF
%left SEQ
%nonassoc REDUCE REDUCTION
%left CONCAT
%left UNION INTER
%nonassoc STARSET
%nonassoc ASSIGN IN
%left COMMA
%left STDIN
%nonassoc OTHERS
%left EOL
%start parser_firstrun
%type <string> parser_firstrun
%start parser_main
%type <Sdl.sdlTerm> parser_main
%type <Sdl.sdlTerm> expr
%type <Sdl.sdlType> type_spec
%%
parser_main: expr EOF    { $1 }
;

/* add types in own section */

type_spec:
      INTTYPE                        { SdlInt }
    | LANGTYPE                       { SdlLang }
;

expr:
      ASSIGN type_spec IDENT setCreation IN expr { SdlLet($3,$4,$6,$2)}
    | setCreation SEQ expr    { Seq($1,$3)}
    | INT                     { SdlNum($1)}
;

setCreation:
      INT                            { SdlNum($1) }
    | IDENT                          { SdlVar($1) }
    | LCURLY setExpr RCURLY          { $2 }
    | STARSET setCreation            { SdlStarSet($1,$2) }
    | LCURLY RCURLY                  { Set(newEmptySet) }
    | setCreation UNION setCreation  { SdlUnion($1, $3) }
    | setCreation INTER setCreation  { SdlInter($1, $3) }
    | setCreation CONCAT setCreation { SdlConcat($1, $3) }
    | REDUCE setCreation setCreation %prec REDUCTION { SdlReduce($3, $2) }
;
setExpr:
      LETTER                { Set(newSet $1) }
    | EMPTY                 { Set(newSet "") }
    | setExpr COMMA setExpr { SdlUnion($1,$3) }
;

parser_firstrun:
    firstrun parser_firstrun {$1 ^ $2}
    | EOF                       { "" }
;

firstrun:
      STDIN firstrun {(input_line stdin) ^ $2}
    | OTHERS firstrun    { $1 ^ $2 }
    | EOL                { "\n"}
;
