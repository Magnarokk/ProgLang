module SS = Set.Make(String);;

type sdlTerm =
     sdlLet of string * sdlTerm * sdlTerm
   | sdlVar of string
   | sdlNum of int 
   | union of sdlTerm * sdlTerm * string
   | iter of sdlTerm * sdlTerm * string
   | concat of sdlTerm * sdlTerm * string
   | set of SS.t
   | seq of sdlTerm * sdlTerm 

   val newSet : SS.elt -> SS.t
   val newEmptySet : SS.t
   val newStarSet : SS.elt -> int -> SS.t
   val eval : sdlTerm -> sdlTerm
   val print_term : sdlTerm -> unit