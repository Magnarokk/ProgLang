(* for using sets *)
module SS = Set.Make(String);;

(* language terms *)
type sdlTerm =
     sdlLet of string * sdlTerm * sdlTerm
   | sdlVar of string
   | sdlNum of int 
   | sdlUnion of sdlTerm * sdlTerm * string
   | sdlInter of sdlTerm * sdlTerm * string
   | sdlConcat of sdlTerm * sdlTerm * string
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
let newStarSet str count =
    let LetterStarSet = SS.empty in
    let element = "" in
    let rec makeSet element count =
        if int > 1 then
            List.Fold_Right SS.add element LetterStarSet
            makeSet element ^ "a" count-1
        else LetterStarSet
    in makeSet "" count;;
(* args: string int returns: set (SS.t) with int elements*)

(* create print function for language *)
(* args: (sdlTerm, sdlTerm) (set pair) and prints it out in correct format to sdout *)
(* [] prints nothing, [] sdlterm print sdlterm (vice versa), sdlterm sdlterm print both sdlterm *)

(* create union function *)
let union set1 set2 =
    SS.union set1 set2
(* args: two sets (SS.t) int returns: set (SS.t) with int number of elements *)

(* create inter function *)
let inter set1 set2 =
    SS.inter set1 set2
(* args: two sets (SS.t) int returns: set (SS.t) with int number of elements *)

(* create concat function *)
let concat set1 set2 =
    let list1 = SS.elements set1 in
    let list2 = SS.elements set2 in
    let pairOfList = {list1 list2}
    let rec zip pairOfList = match pairOfList with 
      [] , [] -> []
    | [] , _  -> _
    | _  , [] -> _
    | (l :: ls) , (r :: rs) -> ((l,r) :: zip (ls,rs))
    of_list 
(* args: two sets (SS.t) int returns: set (SS.t) with int number of elements *)
(* convert sets to lists for ease and interate through both concat each element *)
(* up to int. Then convert back to set *)

(*type checker *)

(* evaluator *)
let rec eval1 env e = match e with
    | (sdlVar s) -> (try ((lookup env s), env) with LookupError -> raise UnboundVariableError)
    | (sdlNum n) -> raise Terminated
    | (set s) -> raise Terminated 
    
    | (sdlLet(x,e1,e2)) when (isValue(e1)) -> (e2, addBinding emv x e1)
    | (sdlLet(x,e1,e2))                    -> let (e1', env') = (eval1 env e1) in (sdlLet(x,e1',e2), env')

    | (sdlUnion(set(x),set(y)))               -> (set(union(x,y)), env)
    | (sdlUnion(set(x),e2))                   -> let (e2',env') = (eval1 env e2) in (sdlUnion(set(x),e2'),env')