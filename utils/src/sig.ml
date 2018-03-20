(** Signatures needed throughout the project *)

(** Base type *)
module type VALUE = sig
  type t
  val compare : t -> t -> int
  val print : Format.formatter -> t -> unit
end
