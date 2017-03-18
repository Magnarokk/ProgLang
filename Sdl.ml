exception LookupError ;;
exception UnboundVariableError ;;
exception Terminated ;;
exception StuckTerm ;;
exception TypeError;;

open Printf;;
(* for using sets *)
module SS = Set.Make(String);;

type sdlType = SdlInt | SdlLang | Seq of sdlType * sdlType

(* language terms *)
type sdlTerm =
     SdlLet of string * sdlTerm * sdlTerm * sdlType
   | SdlVar of string
   | SdlNum of int 
   | SdlStarSet of string * sdlTerm
   | SdlUnion of sdlTerm * sdlTerm
   | SdlInter of sdlTerm * sdlTerm
   | SdlConcat of sdlTerm * sdlTerm
   | SdlReduce of sdlTerm * sdlTerm
   | Set of SS.t
   | Seq of sdlTerm * sdlTerm 

(* checks if returnable value *)
let rec isValue e = match e with
    | SdlNum (n) -> true
    | Set (s)    -> true
    | Seq (s1,s2)-> true
    | _ -> false
;;
    
(* for storing variable values *)
type 'a context = Env of (string * 'a) list
type valContext = sdlTerm context
type typeContext = sdlType context

(* lookup variable value *)
let rec lookup env str = match env with
      Env [] -> raise LookupError
    | Env ((name,thing) :: t) ->
            (match (name = str) with
                  true -> thing
                | false -> lookup (Env (t)) str
            )
;;

(* crate new variable *)
let addBinding env str thing = match env with
    Env(t) -> Env ((str, thing) :: t)
;;

(* create new set from string *)
let newSet str =
        SS.singleton str
;;

(*create empty set *)
let newEmptySet =
    SS.empty
;;

(* create Starset *)
let newStarSet str count =
    let letterStarSet = ref(SS.empty) in
    let rec makeSet element counter =
        if counter > 0 then begin
            letterStarSet := List.fold_right SS.add [element] !letterStarSet;
            makeSet (element ^ "a") (counter-1) end 
        else
            !letterStarSet;
    in makeSet "" count
;;
(* args: string int returns: set (SS.t) with int elements*)

(* union function *)
let union set1 set2 =
    SS.union set1 set2
;;

(* args: two sets (SS.t) int returns: set (SS.t) with int number of elements *)

(* inter function *)
let inter set1 set2 =
    SS.inter set1 set2
;;

let rec of_list l = match l with
      [] -> SS.empty
    | h :: t -> List.fold_right SS.add [h] (of_list t)
;;

(* concat function *)
 let concat set1 set2 =
    let list1 = SS.elements set1 in
    let acc = SS.elements set2 in

    (* adds elements from second list to head of first *)
    let rec accumulate str acc = match acc with
      [] -> []
    | ah :: at -> ((str ^ ah) :: accumulate str at) in

    (* creates concat list *)
    let rec zip listPost acc = match listPost with 
      [] -> []
    | h :: t -> List.append (accumulate h acc) (zip t acc) in
    
    of_list (zip list1 acc);;

(* reduce *)
let reduce set limit =
    let newSet = ref(SS.empty) in
    let list1 = SS.elements set in
    
    let rec reduceSet alist alimit = match alist with
          [] -> !newSet
        | h :: t -> if alimit > 0 then begin
            newSet := List.fold_right SS.add [h] !newSet;
            reduceSet t (alimit-1) end 
        else
            !newSet in
    reduceSet list1 limit;;

(* reduces set to specified size, also order elements alphabetically before reduction *)

(*type checker *)
let rec typeOf env e = match e with
      SdlNum (n) -> SdlInt
    | Set (s)    -> SdlLang
    | SdlVar (x) -> (try lookup env x with LookupError -> raise TypeError)

    | SdlLet (x, e1, e2, t) ->
        (let ty1 = typeOf env e1 in
         let ty2 = typeOf (addBinding env x t) e2 in
            (match (ty1 = t) with
                  true -> ty2
                | false -> raise TypeError))
    
    | SdlUnion (e1,e2) ->
        (match (typeOf env e1), (typeOf env e2) with
              SdlLang, SdlLang -> SdlLang
            | _ -> raise TypeError )
    | SdlInter (e1,e2) ->
        (match (typeOf env e1), (typeOf env e2) with
              SdlLang, SdlLang -> SdlLang
            | _ -> raise TypeError )
    | SdlConcat (e1,e2) ->
        (match (typeOf env e1), (typeOf env e2) with
              SdlLang, SdlLang -> SdlLang
            | _ -> raise TypeError )
    
    | SdlReduce (e1,e2) ->
        (match (typeOf env e1), (typeOf env e2) with
              SdlLang, SdlInt -> SdlLang
            | _ -> raise TypeError )

    | Seq (e1,e2) ->
        (match (typeOf env e1), (typeOf env e2) with
              SdlLang, SdlLang -> Seq(SdlLang,SdlLang)
            | _ -> raise TypeError )

    | SdlStarSet (e1,e2) ->
        (match (typeOf env e2) with
                SdlInt -> SdlLang
            | _ -> raise TypeError )
;;

let typeProg e = typeOf (Env []) e
;;
    
(* evaluator *)
let rec eval1 env e = match e with
    | (SdlVar (s)) -> (try ((lookup env s), env) with LookupError -> raise UnboundVariableError)
    | (Set(s))     -> raise Terminated
    
    | (SdlLet(x,e1,e2,t)) when (isValue(e1)) -> (e2, addBinding env x e1)
    | (SdlLet(x,e1,e2,t))                    -> let (e1', env') = (eval1 env e1) in (SdlLet(x,e1',e2,t), env')

    | (SdlUnion(Set(x),Set(y)))               -> (Set(union x y), env)
    | (SdlUnion(Set(x),e2))                   -> let (e2',env') = (eval1 env e2) in (SdlUnion(Set(x),e2'),env')
    | (SdlUnion(e1,e2))                       -> let (e1',env') = (eval1 env e1) in (SdlUnion(e1',e2),env')

    | (SdlInter(Set(x),Set(y)))               -> (Set(inter x y), env)
    | (SdlInter(Set(x),e2))                   -> let (e2',env') = (eval1 env e2) in (SdlInter(Set(x),e2'),env')
    | (SdlInter(e1,e2))                       -> let (e1',env') = (eval1 env e1) in (SdlInter(e1',e2),env')

    | (SdlConcat(Set(x),Set(y)))               -> (Set(concat x y), env)
    | (SdlConcat(Set(x),e2))                   -> let (e2',env') = (eval1 env e2) in (SdlConcat(Set(x),e2'),env')
    | (SdlConcat(e1,e2))                       -> let (e1',env') = (eval1 env e1) in (SdlConcat(e1',e2),env')

    | (SdlReduce(Set(x),SdlNum(y)))            -> (Set(reduce x y), env)
    | (SdlReduce(Set(x),e2))                   -> let (e2',env') = (eval1 env e2) in (SdlReduce(Set(x), e2'),env')
    | (SdlReduce(e1,e2))                       -> let (e1',env') = (eval1 env e1) in (SdlReduce(e1', e2),env')

    | (SdlStarSet(x, SdlNum(y)))          -> (Set(newStarSet x y), env)    
    | (SdlStarSet(x,e2))                  -> let (e2', env') = (eval1 env e2) in (SdlStarSet(x,e2'), env')
    
    | (Seq(Set(x),Set(y)))                      -> raise Terminated
    | (Seq(Set(x),e2))                          -> let (e2',env') = (eval1 env e2) in (Seq(Set(x),e2'),env')
    | (Seq(e1,e2))                              -> let (e1',env') = (eval1 env e1) in (Seq(e1',e2),env')

    | _ -> raise Terminated
;;

(* loop through program *)
let rec evalloop env e = 
    try 
        (let(e',env') = (eval1 env e) in 
        (evalloop env' e')) 
    with Terminated -> if (isValue e) then e else raise StuckTerm
;;

(* initiate eval *)
let eval e = evalloop (Env []) e
;; 

(* prints set in correct form *)
let print_set set =
        let list = SS.elements set in
        print_string "{";

        let checkforEmpty str =
            if (str = "") then
                ":"
            else
                str
        in

        let rec aux l =
            match l with
                  [] -> print_string "}"
                | [x] -> print_string (checkforEmpty x); print_string "}"
                | [x;y] -> print_string (checkforEmpty x); print_string ", "; print_string (checkforEmpty y); print_string "}"
                | h::t -> print_string (checkforEmpty h); print_string ", "; aux t; 
        in aux list
;;

let print_term value = match value with 
    | (Set s) -> print_set s
    | (Seq(Set(x),Set(y))) -> print_set x; print_newline(); print_set y 
;;

