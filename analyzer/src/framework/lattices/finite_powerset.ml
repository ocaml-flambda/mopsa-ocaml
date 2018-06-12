(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Powerset lattice over a finite set of elements. *)

module type VALUE =
sig
  type t
  val values : t list
  val print : Format.formatter -> t -> unit
end

module Make(Value: VALUE) =
struct

  module Set = Set.Make(struct type t = Value.t let compare = compare end)

  include Set

  let bottom = Set.empty

  let top = Set.of_list Value.values

  let is_bottom abs = Set.is_empty abs

  let is_top abs = Set.cardinal abs = List.length Value.values

  let leq = Set.subset

  let join  = Set.union

  let meet  = Set.inter

  let widening ctx = join

  let print fmt abs =
    let open Format in
    if is_bottom abs then fprintf fmt "⊥"
    else
      let size = Set.cardinal abs in
      if size = (List.length Value.values) && size > 1 then fprintf fmt "⊤"
    else
      let elts = Set.elements abs in
      fprintf fmt "@[<h>%a@]"
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt " | ")
           Value.print
        ) elts

end
