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
open Sig.Combiner.Value
open Common


module Make(V1:VALUE_COMBINER)(V2:VALUE_COMBINER) : VALUE_COMBINER with type t = V1.t * V2.t =
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

  let v1_man (man:('a,'v,t) value_man) : ('a,'v,V1.t) value_man = {
    man with
    get  = (fun a -> man.get a |> fst);
    set  = (fun x a ->
        let (v1,v2) = man.get a in
        if x == v1 then a else man.set (x,v2) a);
  }

  let v2_man (man:('a,'v,t) value_man) : ('a,'v,V2.t) value_man = {
    man with
    get  = (fun a -> man.get a |> snd);
    set  = (fun x a ->
        let (v1,v2) = man.get a in
        if x == v2 then a else man.set (v1,x) a);
  }


  (** {2 Forward semantics} *)
  (** ********************* *)

  let constant t c =
    let v1 = if V1.accept_type t then Some (V1.constant t c) else None in
    let v2 = if V2.accept_type t then Some (V2.constant t c) else None in
    match v1, v2 with
    | None, None -> assert false
    | Some v1, Some v2 -> (v1,v2)
    | Some v1, None -> (v1,V2.bottom)
    | None, Some v2 -> (V1.bottom,v2)

  let unop man t op (a,e) =
    apply
      (fun v1 -> if V1.is_bottom v1 then v1 else V1.unop (v1_man man) t op (a,e))
      (fun v2 -> if V2.is_bottom v2 then v2 else V2.unop (v2_man man) t op (a,e))
      (man.get a)

  let binop man t op (a1,e1) (a2,e2) =
    apply2
      (fun v1 w1 -> if V1.(is_bottom v1 || is_bottom w1) then V1.bottom else V1.binop (v1_man man) t op (a1,e1) (a2,e2))
      (fun v2 w2 -> if V2.(is_bottom v2 || is_bottom w2) then V2.bottom else V2.binop (v2_man man) t op (a1,e1) (a2,e2))
      (man.get a1) (man.get a2)

  let filter t b =
    apply
      (fun v1 -> if V1.is_bottom v1 then v1 else V1.filter t b v1)
      (fun v2 -> if V2.is_bottom v2 then v2 else V2.filter t b v2)

  (** {2 Backward semantics} *)
  (** ********************** *)

  let bwd_unop man t op (a,e) (r1,r2) =
    let aa = if V1.is_bottom r1 then a else V1.bwd_unop (v1_man man) t op (a,e) r1 in
    if V2.is_bottom r2 then aa else V2.bwd_unop (v2_man man) t op (aa,e) r2

  let bwd_binop man t op (a1,e1) (a2,e2) (r1,r2) =
    let aa1,aa2 = if V1.is_bottom r1 then a1,a2 else V1.bwd_binop (v1_man man) t op (a1,e1) (a2,e2) r1 in
    if V2.is_bottom r2 then aa1,aa2 else V2.bwd_binop (v2_man man) t op (aa1,e1) (aa2,e2) r2

  let predicate t op b =
    apply
      (fun v1 -> if V1.is_bottom v1 then v1 else V1.predicate t op b v1)
      (fun v2 -> if V2.is_bottom v2 then v2 else V2.predicate t op b v2)

  let compare t op b (v1,v2) (w1,w2) =
    let x1,y1 = if V1.(is_bottom v1 || is_bottom w1) then V1.bottom,V1.bottom else V1.compare t op b v1 w1 in
    let x2,y2 = if V2.(is_bottom v2 || is_bottom w2) then V2.bottom,V2.bottom else V2.compare t op b v2 w2 in
    ((x1,x2),(y1,y2))


  (** {2 Communication handlers} *)
  (** ************************** *)

  let ask man q =
    OptionExt.neutral2
      (join_query q
         ~join:(fun _ _ -> Exceptions.panic "abstract queries called from value abstraction"))
      (V1.ask (v1_man man) q)
      (V2.ask (v2_man man) q)

  let refine msg ((v1,v2) as v) =
    let r1 = V1.refine msg v1 in
    let r2 = V2.refine msg v2 in
    match r1,r2 with
    | None,None -> None
    | Some r1,Some r2 ->
      if r1 == v1 && r2 == v2 then Some v else Some (r1,r2)
    | Some r1, None ->
      if r1 == v1 then Some v else Some (r1,v2)
    | None, Some r2 ->
      if r2 == v2 then Some v else Some (v1,r2)




end

let rec make (values:(module VALUE_COMBINER) list) : (module VALUE_COMBINER) =
  match values with
  | [] -> assert false
  | [v] -> v
  | hd::tl -> (module Make(val hd : VALUE_COMBINER)(val (make tl) : VALUE_COMBINER) : VALUE_COMBINER)
