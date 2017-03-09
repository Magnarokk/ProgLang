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
