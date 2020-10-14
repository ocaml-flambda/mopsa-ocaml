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

(** Map with printing builder *)

(** Builder *)
module Make
    (K : ValueSig.S)
    (E : ValueSig.S)
  :
  sig
    type +'a map
    include Map.S with type key := K.t and type 'a t := 'a map
    type t = E.t map
    val print : Format.formatter -> t -> unit
    val compare : t -> t -> int
  end
=
struct
  module M = Map.Make(K)
  type 'a map = 'a M.t
  module type S = Map.S with type key := K.t and type 'a t := 'a map
  include (M : S)
  type t = E.t map

  let print fmt (m : t) =
    Format.fprintf fmt "@[{%a}@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         (fun fmt (z,t) -> Format.fprintf fmt "%a ~> %a" K.print z E.print t)
      ) (bindings m)
  let compare (m : t) (m' : t) =
    try
      let r = fold (fun k v acc ->
          if mem k acc && E.compare (find k acc) v = 0 then
            remove k acc
          else
            raise Exit
        ) m m'
      in
      if cardinal r = 0 then
        0
      else -1
    with
    | Exit -> 1
end
