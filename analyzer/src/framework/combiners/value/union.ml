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


module Make(V1:VALUE)(V2:VALUE) : VALUE =
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

  let v1_man (man:('a,t) value_man) : ('a,V1.t) value_man = {
    man with
    eval = (fun e -> man.eval e |> fst);
  }

  let v2_man (man:('a,t) value_man) : ('a,V2.t) value_man = {
    man with
    eval = (fun e -> man.eval e |> snd);
  }


  (** {2 Forward semantics} *)
  (** ********************* *)

  let constant t c =
    let v1 = if V1.accept_type t then V1.constant t c else None in
    let v2 = if V2.accept_type t then V2.constant t c else None in
    match v1, v2 with
    | None, None -> None
    | Some v1, Some v2 -> Some (v1,v2)
    | Some v1, None -> Some (v1,V2.bottom)
    | None, Some v2 -> Some (V1.bottom,v2)

  let cast man t e =
    let v1 = if V1.accept_type t then V1.cast (v1_man man) t e else None in
    let v2 = if V2.accept_type t then V2.cast (v2_man man) t e else None in
    match v1, v2 with
    | None, None       -> None
    | Some v1, Some v2 -> Some (v1,v2)
    | Some v1, None    -> Some (v1,V2.bottom)
    | None, Some v2    -> Some (V1.bottom,v2)

  let unop op t a =
    apply
      (fun v1 -> if V1.is_bottom v1 then v1 else V1.unop op t v1)
      (fun v2 -> if V2.is_bottom v2 then v2 else V2.unop op t v2)
      a

  let binop op t =
    apply2
      (fun v1 w1 -> if V1.(is_bottom v1 || is_bottom w1) then V1.bottom else V1.binop op t v1 w1)
      (fun v2 w2 -> if V2.(is_bottom v2 || is_bottom w2) then V2.bottom else V2.binop op t v2 w2)

  let filter b t =
    apply
      (fun v1 -> if V1.is_bottom v1 then v1 else V1.filter b t v1)
      (fun v2 -> if V2.is_bottom v2 then v2 else V2.filter b t v2)

  (** {2 Backward semantics} *)
  (** ********************** *)

  let bwd_unop op t =
    apply2
      (fun v1 r1 -> if V1.is_bottom v1 then v1 else V1.bwd_unop op t v1 r1)
      (fun v2 r2 -> if V2.is_bottom v2 then v2 else V2.bwd_unop op t v2 r2)

  let bwd_binop op t (v1,v2) (w1,w2) (r1,r2) =
    let x1,y1 = if V1.(is_bottom v1 || is_bottom w1) then V1.bottom,V1.bottom else V1.bwd_binop op t v1 w1 r1 in
    let x2,y2 = if V2.(is_bottom v2 || is_bottom w2) then V2.bottom,V2.bottom else V2.bwd_binop op t v2 w2 r2 in
    ((x1,x2),(y1,y2))

  let bwd_cast man t e =
    apply
      (fun v1 -> if V1.is_bottom v1 then v1 else V1.bwd_cast (v1_man man) t e v1)
      (fun v2 -> if V2.is_bottom v2 then v2 else V2.bwd_cast (v2_man man) t e v2)


  let predicate op b t =
    apply
      (fun v1 -> if V1.is_bottom v1 then v1 else V1.predicate op b t v1)
      (fun v2 -> if V2.is_bottom v2 then v2 else V2.predicate op b t v2)

  let compare op b t (v1,v2) (w1,w2) =
    let x1,y1 = if V1.(is_bottom v1 || is_bottom w1) then V1.bottom,V1.bottom else V1.compare op b t v1 w1 in
    let x2,y2 = if V2.(is_bottom v2 || is_bottom w2) then V2.bottom,V2.bottom else V2.compare op b t v2 w2 in
    ((x1,x2),(y1,y2))


  (** {2 Query handler} *)
  (** ***************** *)

  let ask man q =
    OptionExt.neutral2
      (join_query q
         ~join:(fun _ _ -> Exceptions.panic "abstract queries called from value abstraction"))
      (V1.ask (v1_man man) q)
      (V2.ask (v2_man man) q)




end

let rec make (values:(module VALUE) list) : (module VALUE) =
  match values with
  | [] -> (module EmptyValue)
  | [v] -> v
  | hd::tl -> (module Make(val hd : VALUE)(val (make tl) : VALUE) : VALUE)
