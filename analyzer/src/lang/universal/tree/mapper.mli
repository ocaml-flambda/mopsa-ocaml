(** Implementation of a map that associates a (symbol : A.t) and a
   list of (state : B.t) to a (state : B.t) *)

module Make:
  functor (A: SIG.COMPARABLE) ->
  functor (B: SIG.COMPARABLE) ->
  sig
    type t
    val print: Format.formatter -> t -> unit
    val empty: t
    val filter_symbol: A.t -> t -> t
    val add_transition: (A.t * B.t list * B.t) -> t -> t
    val find_transition: A.t -> B.t list -> t -> B.t
    val fold: (A.t * B.t list * B.t -> 'a -> 'a) -> t -> 'a -> 'a
  end
