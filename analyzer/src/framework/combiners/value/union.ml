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

(** Union of two value abstractions *)

open Core.All
open Sig.Abstraction.Value
open Common


module Make(V1:VALUE)(V2:VALUE) : VALUE with type t = V1.t * V2.t =
struct

  (** {2 Header of the abstraction} *)
  (** ***************************** *)

  include Lattices.Pair.Make(V1)(V2)

  let name = "framework.combiners.value.union"

  let id = V_pair(V1.id,V2.id)

  let display = V1.display ^ " ∪ " ^ V2.display

  let accept_type t = V1.accept_type t || V2.accept_type t

  let print printer (v1,v2) =
    match V1.is_bottom v1, V2.is_bottom v2 with
    | true, true -> pp_string printer Bot.bot_string
    | false, true -> V1.print printer v1
    | true, false -> V2.print printer v2
    | false, false ->
      pp_obj_list printer
        [ pbox V1.print v1;
          pbox V2.print v2 ]
        ~lopen:"" ~lsep:"∨" ~lclose:""


  (** {2 Lattice operators} *)
  (** ********************* *)

  let is_bottom (v1,v2) = V1.is_bottom v1 && V2.is_bottom v2


  (** {2 Value managers} *)
  (** ****************** *)

  let v1_man (man:('v,t) value_man) : ('v,V1.t) value_man = {
    man with
    get  = (fun a -> man.get a |> fst);
    set  = (fun x a ->
        let (v1,v2) = man.get a in
        if x == v1 then a else man.set (x,v2) a);
  }

  let v2_man (man:('v,t) value_man) : ('v,V2.t) value_man = {
    man with
    get  = (fun a -> man.get a |> snd);
    set  = (fun x a ->
        let (v1,v2) = man.get a in
        if x == v2 then a else man.set (v1,x) a);
  }


  (** {2 Local semantics} *)
  (** ******************* *)


  let eval man e : t =
    let man1 = v1_man man in
    let man2 = v2_man man in
    let accept1 = V1.accept_type e.etyp in
    let accept2 = V2.accept_type e.etyp in
    if accept1 && accept2 then
      let r1 = V1.eval man1 e in
      let r2 = V2.eval man2 e in
      (r1, r2)
    else
    if accept1 then
      let r1 = V1.eval man1 e in
      let r2 = V2.eval_ext man2 e in
      match r2 with
      | None   -> (r1,V2.bottom)
      | Some v -> (V1.meet r1 (man1.get v), V2.bottom)
    else
    if accept2 then
      let r1 = V1.eval_ext man1 e in
      let r2 = V2.eval man2 e in
      match r1 with
      | None   -> (V1.bottom,r2)
      | Some v -> (V1.bottom,V2.meet r2 (man2.get v))
    else
      assert false

  let filter man b e =
    let r1 = V1.filter (v1_man man) b e in
    let r2 = V2.filter (v2_man man) b e in
    match r1,r2 with
    | None, None       -> None
    | Some v1, Some v2 -> Some (v1,v2)
    | Some v1, None    -> Some (v1,V2.bottom)
    | None, Some v2    -> Some (V1.bottom,v2)


  let backward man e ve r : t vexpr =
    let man1 = v1_man man in
    let man2 = v2_man man in
    let accept1 = for_all_child_expr (fun ee -> V1.accept_type ee.etyp) (fun s -> false) e in
    let accept2 = for_all_child_expr (fun ee -> V2.accept_type ee.etyp) (fun s -> false) e in
    if accept1 && accept2 then
      let r1 = V1.backward man1 e (map_vexpr fst ve) r in
      let r2 = V2.backward man2 e (map_vexpr snd ve) r in
      map2_vexpr
        (fun v1 -> assert false)
        (fun v2 -> assert false)
        (fun v1 v2 -> (v1, v2))
        r1 r2
    else
    if accept1 then
      let r1 = V1.backward man1 e (map_vexpr fst ve) r in
      let r2 = V2.backward_ext man2 e (map_vexpr (fun v -> man.set v man.top) ve) r in
      match r2 with
      | None   -> map_vexpr (fun v1 -> (v1,V2.bottom)) r1
      | Some rr2 ->
        map2_vexpr
          (fun v1 -> assert false)
          (fun v2 -> assert false)
          (fun v1 v2 -> (V1.meet v1 (man1.get v2), V2.bottom))
          r1 rr2
    else
    if accept2 then
      let r1 = V1.backward_ext man1 e (map_vexpr (fun v -> man.set v man.top) ve) r in
      let r2 = V2.backward man2 e (map_vexpr snd ve) r in
      match r1 with
      | None   -> map_vexpr (fun v2 -> (V1.bottom,v2)) r2
      | Some rr1 ->
        map2_vexpr
          (fun v1 -> assert false)
          (fun v2 -> assert false)
          (fun v1 v2 -> (V1.bottom,V2.meet v2 (man2.get v1)))
          rr1 r2
    else
      assert false

  let compare man op b e1 v1 e2 v2 : t * t =
    let man1 = v1_man man in
    let man2 = v2_man man in
    let accept1 = V1.accept_type e1.etyp && V1.accept_type e2.etyp in
    let accept2 = V2.accept_type e1.etyp && V2.accept_type e2.etyp in
    if accept1 && accept2 then
      let (r11,r12) = V1.compare man1 op b e1 (fst v1) e2 (fst v2) in
      let (r21,r22) = V2.compare man2 op b e1 (snd v1) e2 (snd v2) in
      (r11,r21), (r12,r22)
    else
    if accept1 then
      let (r11,r12) = V1.compare man1 op b e1 (fst v1) e2 (fst v2) in
      let r2 = V2.compare_ext man2 op b e1 (man.set v1 man.top) e2 (man.set v2 man.top) in
      match r2 with
      | None   -> (r11,snd v1), (r12,snd v2)
      | Some (r21,r22) ->
        (V1.meet r11 (man1.get r21), man2.get r21),
        (V1.meet r12 (man1.get r22), man2.get r22)
    else
    if accept2 then
      let r1 = V1.compare_ext man1 op b e1 (man.set v1 man.top) e2 (man.set v2 man.top) in
      let (r21,r22) = V2.compare man2 op b e1 (snd v1) e2 (snd v2) in
      match r1 with
      | None   -> (fst v1,r21), (fst v2,r22)
      | Some (r11,r12) ->
        (man1.get r11, V2.meet r21 (man2.get r11)),
        (man1.get r12, V2.meet r22 (man2.get r12))
    else
      assert false


  (** {2 Extended semantics} *)
  (** ********************** *)

  let eval_ext man e =
    let r1 = V1.eval_ext (v1_man man) e in
    let r2 = V2.eval_ext (v2_man man) e in
    OptionExt.neutral2 man.meet r1 r2

  let backward_ext man e ev r =
    let r1 = V1.backward_ext (v1_man man) e ev r in
    let r2 = V2.backward_ext (v2_man man) e ev r in
    OptionExt.neutral2 (merge_vexpr man.meet) r1 r2

  let compare_ext man op b e1 v1 e2 v2 =
    let r1 = V1.compare_ext (v1_man man) op b e1 v1 e2 v2 in
    let r2 = V2.compare_ext (v2_man man) op b e1 v1 e2 v2 in
    OptionExt.neutral2 (fun (r11,r12) (r21,r22) -> (man.meet r11 r21),(man.meet r12 r22)) r1 r2

  (** {2 Communication handlers} *)
  (** ************************** *)

  let avalue aval (v1,v2) =
    OptionExt.neutral2
      (join_avalue aval)
      (V1.avalue aval v1)
      (V2.avalue aval v2)

  let ask man query =
    OptionExt.neutral2
      (join_query query)
      (V1.ask (v1_man man) query)
      (V2.ask (v2_man man) query)
end

let rec make (values:(module VALUE) list) : (module VALUE) =
  match values with
  | [] -> assert false
  | [v] -> v
  | hd::tl -> (module Make(val hd : VALUE)(val (make tl) : VALUE) : VALUE)
