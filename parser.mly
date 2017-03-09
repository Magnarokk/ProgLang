%{
    module SS = Set.Make(String);
%}

%token EOL
%token LCURLY RCURLY COMMA EMPTY
%token <string> LETTER
%token UNION ITER CONCAT
%token <int> INT
%token STDIN STDOUT
%nonassoc EOL
%left STDIN

%start main
%type <string> main
%start firstrun
%type <string> firstrun
%%
firstrun:
      firstrun STDIN firstrun     {$1 ^ (input_line stdin) ^ $3}
    | firstrun EOL                { $1 }
    | OTHERS                      { $1 }
main:
    expr EOL                      { $1 }
;
setCreation:
    LCURLY setCreation RCURLY     { SS.empty }
expr:
    | LETTER                {$1}
    | INT                   {$1}
    | EMPTY                 {$1}
    | expr COMMA expr       {$1, $3}
    | LCURLY expr RCURLY    {$2}
    | expr UNION expr       {union($1 $3)}
    | expr INTER expr       {inter($1 $3)}
    | expr CONCAT expr      {concat($1 $3)}
