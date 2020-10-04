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


  (** {2 Forward semantics} *)
  (** ********************* *)

  let apply_on_acceptable_expr f man accept e =
    if exists_expr (fun ee -> accept e.etyp) (fun s -> false) e then
      f man e
    else
      None

  let combine man f1 f2 e =
    combine_pair_eval man (v1_man man) (v2_man man) V1.bottom V2.bottom
      (fun man1 -> apply_on_acceptable_expr f1 man1 V1.accept_type e)
      (fun man2 -> apply_on_acceptable_expr f2 man2 V2.accept_type e)

  let eval man e =
    combine man V1.eval V2.eval e

  let filter man b e =
    combine man
      (fun man e -> V1.filter man b e)
      (fun man e -> V2.filter man b e)
      e
    


  (** {2 Backward semantics} *)
  (** ********************** *)    

  let backward man e ve r =
    let doit f man other_man accept =
      if exists_expr (fun ee -> accept e.etyp) (fun s -> false) e then
        f man e ve r
      else
        None
    in
    let man1 = v1_man man in
    let man2 = v2_man man in
    let r1 = doit V1.backward man1 man2 V1.accept_type in
    let r2 = doit V2.backward man2 man1 V2.accept_type in
    OptionExt.neutral2 (merge_vexpr man.meet) r1 r2

  let compare man op b e1 v1 e2 v2 =
    let doit vman compare accept =
      if accept e1.etyp || accept e2.etyp then
        compare vman op b e1 v1 e2 v2
      else
        None
    in
    let r1 = doit (v1_man man) V1.compare V1.accept_type in
    let r2 = doit (v2_man man) V2.compare V2.accept_type in
    OptionExt.neutral2 (fun (v1,v2) (w1,w2) -> (man.meet v1 w1,man.meet v1 w1)) r1 r2


  (** {2 Communication handlers} *)
  (** ************************** *)

  let avalue man aval v =
    OptionExt.neutral2
      (join_avalue aval)
      (V1.avalue (v1_man man) aval v)
      (V2.avalue (v2_man man) aval v)

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
