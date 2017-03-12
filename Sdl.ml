(* for using sets *)
module SS = Set.Make(String);;

(* language terms *)
type sdlTerm =
     sdlLet of string * sdlTerm * sdlTerm
   | sdlVar of string
   | sdlNum of int 
   | union of sdlTerm * sdlTerm * string
   | iter of sdlTerm * sdlTerm * string
   | concat of sdlTerm * sdlTerm * string
   | set of SS.t
   | seq of sdlTerm * sdlTerm 

(* for storing variable values *)
type valContext = Env of (string * sdlTerm) list

(* create new set from string *)
let newSet str =
        SS.singleton str;;

(*create empty set *)
let newEmptySet =
    SS.empty;;

(* create Starset *)
(* args: string int returns: set (SS.t) with int elements*)

(* create print function for language *)
(* args: (sdlTerm, sdlTerm) (set pair) and prints it out in correct format to sdout *)
(* [] prints nothing, [] sdlterm print sdlterm (vice versa), sdlterm sdlterm print both sdlterm *)

(* create union function *)
(* args: two sets (SS.t) int returns: set (SS.t) with int number of elements *)

(* create iter function *)
(* args: two sets (SS.t) int returns: set (SS.t) with int number of elements *)

(* create concat function *)
(* args: two sets (SS.t) int returns: set (SS.t) with int number of elements *)
(* convert sets to lists for ease and interate through both concat each element *)
(* up to int. Then convert back to set *)

(*type checker *)

(* evaluator *)
let rec eval1 env e = match e with
    | (sdlVar s) -> (try ((lookup env s), env) with LookupError -> raise UnboundVariableError)
    | (sdlNum n) -> raise Terminated
    | (set s) -> 
