(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
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

  let subset = Set.subset

  let join annot = Set.union

  let meet annot = Set.inter

  let widen  = join

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
