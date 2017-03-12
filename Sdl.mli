module SS = Set.Make(String);;

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

   val newSet : SS.elt -> SS.t
   val newEmptySet : SS.t
   val newStarSet : SS.elt -> int -> SS.t
   val eval : sdlTerm -> sdlTerm
   val print_term : sdlTerm -> unit