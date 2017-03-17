module SS = Set.Make(String);;

type sdlType = SdlInt | SdlLang

type sdlTerm =
     SdlLet of string * sdlTerm * sdlTerm * sdlType
   | SdlVar of string
   | SdlNum of int 
   | SdlUnion of sdlTerm * sdlTerm
   | SdlInter of sdlTerm * sdlTerm
   | SdlConcat of sdlTerm * sdlTerm
   | SdlReduce of SS.t
   | Set of SS.t
   | Seq of sdlTerm * sdlTerm 

   val newSet : SS.elt -> SS.t
   val newEmptySet : SS.t
   val newStarSet : SS.elt -> int -> SS.t

   val typeProg : sdlTerm -> sdlType
   val eval : sdlTerm -> sdlTerm

   val print_term : sdlTerm -> unit
