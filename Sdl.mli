module SS :
  sig
    type elt = String.t
    type t = Set.Make(String).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val of_list : elt list -> t
  end

type sdlType = SdlInt | SdlLang | Seq of sdlType * sdlType

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

val newSet : SS.elt -> SS.t
val newEmptySet : SS.t
val newStarSet : 'a -> int -> SS.t
val typeProg : sdlTerm -> sdlType
val eval : sdlTerm -> sdlTerm
val print_set : SS.t -> unit
val print_term : sdlTerm -> unit
