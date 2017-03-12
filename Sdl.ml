(* for using sets *)
module SS = Set.Make(String);;

(* language terms *)
type sdlTerm =
     sdlLet of string * sdlTerm * sdlTerm
   | sdlVar of string
   | sdlNum of int 
   | sdlUnion of sdlTerm * sdlTerm
   | sdlInter of sdlTerm * sdlTerm
   | sdlConcat of sdlTerm * sdlTerm
   | sdlPrefix of sdlTerm * sdlTerm
   | sdlPostfix of sdlTerm * sdlTerm
   | sdlReduce of SS.t
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
    let pairList = (list1,list2) in
    let rec zip pairOfList = match pairOfList with 
      [] , [] -> []
    | [] , ls  -> ls
    | ls  , [] -> ls
    | (l :: ls) , (r :: rs) -> (l ^ r) :: zip (ls,rs)) in
    SS.of_list (zip pairList);; 
(* args: two sets (SS.t) int returns: set (SS.t) with int number of elements *)
(* convert sets to lists for ease and interate through both concat each element *)
(* up to int. Then convert back to set *)

(* prefix adds a letter to the start of every element in set *)
(* postfix *)
(* reduce: reduces set to specified size*)

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
    | (sdlUnion(e1,e2))                       -> let (e1',env') = (eval1 env e1) in (sdlUnion(e1',e2),env')

    | (sdlInter(set(x),set(y)))               -> (set(inter(x,y)), env)
    | (sdlInter(set(x),e2))                   -> let (e2',env') = (eval1 env e2) in (sdlInter(set(x),e2'),env')
    | (sdlInter(e1,e2))                       -> let (e1',env') = (eval1 env e1) in (sdlInter(e1',e2),env')

    | (sdlConcat(set(x),set(y)))               -> (set(concat(x,y)), env)
    | (sdlConcat(set(x),e2))                   -> let (e2',env') = (eval1 env e2) in (sdlConcat(set(x),e2'),env')
    | (sdlConcat(e1,e2))                       -> let (e1',env') = (eval1 env e1) in (sdlConcat(e1',e2),env')

    | (sdlPrefix(set(x),set(y)))               -> (set(prefix(x,y)), env)
    | (sdlPrefix(set(x),e2))                   -> let (e2',env') = (eval1 env e2) in (sdlPrefix(set(x),e2'),env')
    | (sdlPrefix(e1,e2))                       -> let (e1',env') = (eval1 env e1) in (sdlPrefix(e1',e2),env')

    | (sdlPostfix(set(x),set(y)))               -> (set(postfix(x,y)), env)
    | (sdlPostfix(set(x),e2))                   -> let (e2',env') = (eval1 env e2) in (sdlPostfix(set(x),e2'),env')
    | (sdlPostfix(e1,e2))                       -> let (e1',env') = (eval1 env e1) in (sdlPostfix(e1',e2),env')

    | (sdlReduce(set(x))               -> (set(reduce(x)), env)
    | (sdlReduce(e1))                  -> let (e1',env') = (eval1 env e1) in (sdlReduce(e1'),env')

    


