module Make:
  functor (Comp: SIG.COMPARABLE) ->
  sig
    type u = Comp.t * int
    type t
    val compare : t -> t -> int
    val print : Format.formatter -> t -> unit
    val mem_symbol : Comp.t -> t -> bool
    val max_arity : t -> int
    val fold : (u -> 'a -> 'a) -> t -> 'a -> 'a
    val iter : (u -> unit) -> t -> unit
    val union : t -> t -> t
    val equal : t -> t -> bool
    val add : u -> t -> t
    val empty : t
    val of_list : u list -> t
    val to_list : t -> u list
  end
