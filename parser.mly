%{
    open Sdl
%}
%token LANGTYPE INTTYPE
%token EOF
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
%left CONCAT
%left UNION INTER
%nonassoc REDUCE REDUCTION
%nonassoc STARSET
%nonassoc ASSIGN IN
%left COMMA
%nonassoc OTHERS
%left STDIN
%start parser_firstrun
%type <string> parser_firstrun
%start parser_main
%type <Sdl.sdlTerm> parser_main
%type <Sdl.sdlTerm> expr
%type <Sdl.sdlType> type_spec
%type <Sdl.sdlTerm>  setCreation
%%
parser_main: expr EOF    { $1 }
;

/* add types in own section */

type_spec:
      INTTYPE                        { SdlInt }
    | LANGTYPE                       { SdlLang }
;

expr:
      ASSIGN type_spec IDENT expr2 IN expr { SdlLet($3,$4,$6,$2)}
    | setCreation                    { $1 }
    | IDENT                          { SdlVar($1) }
    | setCreation SEQ setCreation    { Seq($1,$3)}
;
expr2:
      INT                            { SdlNum($1) }
    | setCreation                    { $1 }
;
setCreation:
      LCURLY setExpr RCURLY          { $2 }
    | STARSET IDENT                  { SdlStarSet($1,SdlVar($2)) }
    | LCURLY RCURLY                  { Set(newEmptySet) }
    | setCreation UNION setCreation  { SdlUnion($1, $3) }
    | setCreation INTER setCreation  { SdlInter($1, $3) }
    | setCreation CONCAT setCreation { SdlConcat($1, $3) }
    | REDUCE IDENT setCreation %prec REDUCTION { SdlReduce($3, SdlVar($2)) }
;
setExpr:
      LETTER                { Set(newSet $1) }
    | EMPTY                 { Set(newSet "") }
    | setExpr COMMA setExpr { SdlUnion($1,$3) }
;

parser_firstrun:
      STDIN parser_firstrun     {(input_line stdin) ^ $2}
    | OTHERS parser_firstrun    { $1 ^ $2 }
    | EOF                { "" }
;
