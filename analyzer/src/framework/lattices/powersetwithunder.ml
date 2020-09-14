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

open Bot

module type ELT =
sig
  type t
  val compare: t -> t -> int
  val print : Format.formatter -> t -> unit
end

(** Powerset with lower and upper approximations *)
module Make(Elt: ELT) =
struct

  module Set = SetExt.Make(Elt)
  module USet = Powerset.Make(Elt)

  (* Lower approximation *)
  type l = Set.t

  (* Upper approximation *)
  type u = USet.t

  (* Powerset with lower and upper approximation *)
  type t = (l * u) with_bot

  let empty : t = Nb (Set.empty, USet.empty)

  let bottom : t = BOT

  let top : t = Nb (Set.empty, USet.top)

  let add_u (e: Elt.t) (su: t) : t =
    bot_lift1 (fun (l, u) -> Set.add e l, USet.add e u) su

  let add_o (e: Elt.t) (su: t) : t =
    bot_lift1 (fun (l, u) -> l, USet.add e u) su

  let mem_u (e: Elt.t) (su: t) : bool =
    bot_apply (fun _ (l, u) -> Set.mem e l) false su

  let mem_o (e: Elt.t) (su: t) : bool =
    bot_apply (fun _ (l, u) ->  USet.mem e u) false su

  let remove (e: Elt.t) (su: t) : t =
    bot_lift1 (fun (l,u) -> Set.remove e l, USet.remove e u) su

  let is_empty (su :t) : bool =
    bot_apply (fun _ (l, u) -> Set.is_empty l && USet.is_empty u) false su

  let is_bottom = function BOT -> true | _ -> false

  (* is that so? what if u is top but l is not empty ? *)
  let is_top (su: t) =
    bot_apply (fun _ (l, u) -> Set.is_empty l && USet.is_top u) false su

  let subset (su1: t) (su2: t) : bool =
    bot_apply2 false false (fun (l1, u1) (l2, u2) -> Set.subset l1 l2 && USet.subset u1 u2) su1 su2

  let equal (su1: t) (su2: t) : bool =
    bot_equal (fun (l1, u1) (l2, u2) -> Set.equal l1 l2 && USet.equal u1 u2) su1 su2

  let join (su1: t) (su2: t) : t =
    bot_neutral2 (fun (l1, u1) (l2, u2) -> Set.inter l1 l2, USet.join u1 u2) su1 su2

  let meet (su1: t) (su2: t) : t =
    bot_absorb2 (fun (l1, u1) (l2, u2) -> Nb (Set.union l1 l2, USet.meet u1 u2)) su1 su2

  let union = join

  let inter = meet

  let widen (su1: t) (su2: t) : t =
    bot_neutral2 (fun (l1, u1) (l2, u2) -> Set.inter l1 l2, USet.widen u1 u2) su1 su2

  let fold_u (f : Elt.t -> 'a -> 'a) (s : t) (init : 'a) : 'a =
    bot_to_exn s |> (fun (_, u) -> USet.fold f u init)

  let fold_o (f : Elt.t -> 'a -> 'a) (s : t) (init : 'a) : 'a =
    bot_to_exn s |> (fun (l, _) -> Set.fold f l init)

  (* let fold_uo f s init = fold_o f s (fold_u f s init) (\* FIXME: double iteration on underapproximated elements *\) *)

  open Format
  let print fmt (su:t) =
    bot_fprint (fun fmt (l, u) (* lower, upper *) ->
        let le = Set.elements l in
        fprintf fmt "@[<h>{U=";
        if le = [] then fprintf fmt "∅"
        else
          fprintf fmt "@[<h>{%a}@]"
            (pp_print_list
               ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
               Elt.print
            ) le
        ;
        let ud = USet.diff u (Nt l) in
        if USet.cardinal ud = 0 then
          fprintf fmt ", O=U}@]"
        else
          fprintf fmt ", O=U ∪ %a}@]"
            USet.print ud) fmt su

end
