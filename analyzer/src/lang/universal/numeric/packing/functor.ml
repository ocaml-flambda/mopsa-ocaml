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

(** Packing functor - bucketing of abstract elements to smaller parts with
   fewer dimensions.
*)

open Mopsa
open Sig.Functor.Simplified
open Ast
open Zone
open Strategy
open Format
open Top


module Make(S:STRATEGY) : FUNCTOR = functor(Domain:Sig.Domain.Simplified.DOMAIN) ->
struct

  module Strategy = S(Domain)
  module Map = Strategy.Map


  (** {2 Header of the functor} *)
  (** ************************* *)

  (** Packs are represented as maps from strategy keys to abstract elements of D *)
  type t = Domain.t Map.t with_top

  (** Name of the packing functor *)
  include GenDomainId(struct
      type nonrec t = t
      let name = "universal.numeric.packing." ^ Strategy.name
    end)

  (** Semantic zone of the domain *)
  let zones = [Z_u_num]

  (** Pretty printer *)
  let print fmt a =
    match a with
    | TOP -> pp_print_string fmt Top.top_string
    | Nt m when Map.is_empty m -> pp_print_string fmt Bot.bot_string
    | Nt m ->
      fprintf fmt "@[<v>%a@]@\n"
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt ",@,")
           (fun fmt (k, v) ->
              fprintf fmt "%a ⇥ @[%a@]" Strategy.print k Domain.print v
           )
        ) (Map.bindings m)


  (** {2 Lattice operators} *)
  (** ********************* *)

  let bottom = Nt Map.empty

  let top = TOP

  let is_bottom a =
    Top.top_apply Map.is_empty false a

  let subset a b =
    if a == b then true
    else match a, b with
      | _, TOP -> true
      | TOP, _ -> false
      | Nt m1, Nt m2 -> Strategy.subset m1 m2

  let join a b =
    if a == b then a
    else match a, b with
      | _, TOP | TOP, _ -> TOP
      | Nt m1, Nt m2 -> Nt (Strategy.join m1 m2)

  let meet a b =
    if a == b then a
    else match a, b with
      | x, TOP | TOP, x -> x
      | Nt m1, Nt m2 -> Nt (Strategy.meet m1 m2)

  let widen ctx a b =
    if a == b then a
    else match a, b with
      | _, TOP | TOP, _ -> TOP
      | Nt m1, Nt m2 -> Nt (Strategy.widen ctx m1 m2)

  let merge pre (a,log) (a',log') =
    match pre, a,a' with
    | TOP, _, _ | _, TOP, _ | _, _, TOP -> TOP
    | Nt m, Nt m1, Nt m2 -> Nt (Strategy.merge m (m1,log) (m2,log))


  (** {2 Transfer functions} *)
  (** ********************** *)

  let init prog = Nt (Strategy.init prog)

  let exec stmt a =
    match a with
    | TOP -> Some TOP
    | Nt m -> Strategy.exec stmt m |>
              Option.lift (fun mm -> Nt mm)

  let ask q a =
    match a with
    | TOP -> None
    | Nt m ->
      let rep = Map.map (Domain.ask q) m |>
                Map.bindings |>
                List.map snd
      in
      let rec loop = function
        | [] -> None
        | r :: tl ->
          Option.neutral2 (meet_query q) r (loop tl)
      in
      loop rep

  let refine _ _ = assert false

end

let register_strategy name s =
  let module S = (val s : STRATEGY) in
  let module F = Make(S) in
  let name = "universal.numeric.packing." ^ name in
  register_functor name (module F)
