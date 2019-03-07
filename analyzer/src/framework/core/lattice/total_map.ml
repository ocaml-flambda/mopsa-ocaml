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

(** Map abstraction assuming ⊺ values for non-existing keys. *)

open Bot

let debug fmt = Debug.debug ~channel:"framework.lattices.total_map" fmt

module type KEY =
sig
  type t
  val compare: t -> t -> int
  val print : Format.formatter -> t -> unit
end

module Make
    (Key : KEY)
    (Value : Sig.LATTICE)
=
struct
  module Map = MapExt.Make(Key)

  type v = Value.t Map.t
  (** Maps from variable to non-⊥ non-relational values. *)

  type t = v with_bot
  (** Possibly ⊥ abstract elements. *)

  (** {2 Framework.Domain.DOMAIN functions} *)

  let bottom = BOT

  let top = Nb Map.empty

  let empty = bottom

  let is_bottom abs =
    bot_dfl1 true (fun m ->
        Map.exists (fun _ v -> Value.is_bottom v) m
      ) abs

  let subset (a1:t) (a2:t) : bool =
    bot_included
      (Map.for_all2zo
         (fun _ v1 -> true) (* non-⊤ ⊆ ⊤ *)
         (fun _ v2 -> Value.subset Value.top v2)  (* ⊤ ⊈ non-⊤ *)
         (fun _ v1 v2 -> Value.subset v1 v2)
      )
      a1 a2
  (** Inclusion testing. Missing variables in one map are assimilated to ⊤. *)

  let join (a1:t) (a2:t) : t =
    bot_neutral2
      (Map.map2zo
         (fun _ v1 -> Value.top) (* ⊤ *)
         (fun _ v2 -> Value.top) (* ⊤ *)
         (fun _ v1 v2 -> Value.join v1 v2)
      )
      a1 a2
  (** Join. Missing variables in one map are assimilated to ⊤. *)


  let widen annot (a1:t) (a2:t) : t =
    bot_neutral2
      (Map.map2zo
         (fun _ v1 -> v1)
         (fun _ v2 -> v2)
         (fun _ v1 v2 -> Value.widen annot v1 v2)
      )
      a1 a2
  (** Widening (naive). *)


  let meet (a1:t) (a2:t) : t =
    bot_absorb2
      (fun b1 b2 ->
        exn_to_bot
          (Map.map2zo
             (fun _ v1 -> v1)
             (fun _ v2 -> v2)
             (fun _ v1 v2 ->
                let v = Value.meet v1 v2 in
                if Value.is_bottom v then
                  raise Found_BOT
                else
                  v
             )
             b1) b2
      )
      a1 a2
  (** Meet. Missing variables in one map are assimilated to ⊤. *)

  let print fmt (a:t) =
    let open Format in
    bot_fprint (fun fmt m ->
        if Map.is_empty m then
          pp_print_string fmt "⊤"
        else
          fprintf fmt "@[<v>%a@]"
            (pp_print_list
               ~pp_sep:(fun fmt () -> fprintf fmt ",@,")
               (fun fmt (k, v) ->
                  fprintf fmt "%a  → @[<h2> %a@]" Key.print k Value.print v
               )
            ) (Map.bindings m)
      ) fmt a
  (** Printing. *)


  let find (k: Key.t) (a: t) =
    try begin
      let m = bot_to_exn a in
      try Map.find k m with Not_found -> Value.top
    end
    with
      Found_BOT -> Value.bottom

  let remove (k: Key.t) (a: t) =
    bot_lift1 (Map.remove k) a

  let add (k: Key.t) (v: Value.t) (a: t) =
    bot_lift1 (Map.add k v) a

  let fold f a x =
    bot_dfl1 x (fun m -> Map.fold f m x) a

  let mem x a =
    bot_dfl1 false (fun m -> Map.mem x m) a

  let map f a =
    bot_lift1 (Map.map f) a

  let bindings a =
    bot_dfl1 [] Map.bindings a

end
