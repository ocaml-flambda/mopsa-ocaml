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

(** Simplified signature of a value abstraction. *)

open Core.All
open Value


(*==========================================================================*)
(**                          {2 Value domain}                               *)
(*==========================================================================*)


module type SIMPLIFIED_VALUE =
sig

  (** {2 Header of the abstraction} *)
  (** ***************************** *)

  type t
  (** Type of the abstract value. *)

  val id : t id
  (** Identifier of the value domain *)

  val accept_type : typ -> bool
  (** Predicate of types abstracted by the value domain *)

  val name : string
  (** Name of the value domain *)

  val display : string
  (** Display name used in debug messages *)

  val bottom: t
  (** Least abstract element of the lattice. *)

  val top: t
  (** Greatest abstract element of the lattice. *)


  (** {2 Lattice operators} *)
  (** ********************* *)

  val is_bottom: t -> bool
  (** [is_bottom a] tests whether [a] is bottom or not. *)

  val subset: t -> t -> bool
  (** Partial order relation. [subset a1 a2] tests whether [a1] is
      related to (or included in) [a2]. *)

  val join: t -> t -> t
  (** [join a1 a2] computes an upper bound of [a1] and [a2]. *)

  val meet: t -> t -> t
  (** [meet a1 a2] computes a lower bound of [a1] and [a2]. *)

  val widen: 'a ctx -> t -> t -> t
  (** [widen ctx a1 a2] computes an upper bound of [a1] and [a2] that
      ensures stabilization of ascending chains. *)


  (** {2 Forward semantics} *)
  (** ********************* *)

  val constant : constant -> typ -> t
  val unop : operator -> typ -> t -> typ -> t
  val binop : operator -> typ -> t -> typ -> t -> typ -> t
  val filter : bool -> typ -> t -> t
  val avalue : 'r avalue_kind -> t -> 'r option


  (** {2 Backward semantics} *)
  (** ********************** *)

  val backward_unop  : operator -> typ -> t -> typ -> t -> t
  val backward_binop : operator -> typ -> t -> typ -> t -> typ -> t -> t * t
  val compare : operator -> bool -> typ -> t -> typ -> t -> (t * t)


  (** {2 Pretty printer} *)
  (** ****************** *)

  val print: printer -> t -> unit
  (** Printer of an abstract element. *)

end


let default_filter b t a = a
let default_backward_unop op t a rt r = a
let default_backward_binop op t1 a1 t2 a2 rt r = (a1,a2)
let default_compare op b t1 a1 t2 a2 = (a1,a2)
let default_avalue avalue_kind a = None
  
module DefaultValueFunctions =
struct
  let filter = default_filter
  let backward_unop = default_backward_unop
  let backward_binop = default_backward_binop
  let compare = default_compare
  let avalue = default_avalue
end


(*==========================================================================*)
(**                    {2 Lifter to VALUE Signature}                        *)
(*==========================================================================*)

module MakeValue(V:SIMPLIFIED_VALUE) : VALUE with type t = V.t =
struct

  include V

  let eval man e =
    if not (accept_type e.etyp) then
      None
    else
    match ekind e with
    | E_constant c ->
      let r = constant c e.etyp in
      man.set r man.top |>
      OptionExt.return

    | E_unop(op,ee) when accept_type ee.etyp->
      let r = unop op ee.etyp (man.eval ee |> man.get) e.etyp in
      man.set r man.top |>
      OptionExt.return

    | E_binop(op,e1,e2) when accept_type e1.etyp && accept_type e2.etyp ->
      let r = binop op e1.etyp (man.eval e1 |> man.get) e2.etyp (man.eval e2 |> man.get) e.etyp in
      man.set r man.top |>
      OptionExt.return

    | _ -> None

  let filter man b e =
    if not (accept_type e.etyp) then
      None
    else
      let v = V.filter b e.etyp (man.eval e |> man.get) in
      man.set v man.top |>
      OptionExt.return

  let avalue man aval v =
    V.avalue aval (man.get v)

  let backward man e ve r =
    if not (accept_type e.etyp) then
      None
    else
    match ekind e with
    | E_unop(op,ee) when accept_type ee.etyp ->
      let vv,_ = find_vexpr ee ve in
      let vv' = backward_unop op ee.etyp (man.get vv) e.etyp (man.get r) in
      refine_vexpr ee (man.set vv' vv) ve |>
      OptionExt.return

    | E_binop(op,e1,e2) when accept_type e1.etyp && accept_type e2.etyp ->
      let v1,_ = find_vexpr e1 ve in
      let v2,_ = find_vexpr e2 ve in
      let v1',v2' = backward_binop op e1.etyp (man.get v1) e2.etyp (man.get v2) e.etyp (man.get r) in
      refine_vexpr e1 (man.set v1' v1) ve |>
      refine_vexpr e2 (man.set v2' v2) |>
      OptionExt.return

    | _ -> None

  let compare man op b e1 v1 e2 v2 =
    if accept_type e1.etyp && accept_type e2.etyp then
      let v1',v2' = compare op b e1.etyp (man.get v1) e2.etyp (man.get v2) in
      Some (man.set v1' v1, man.set v2' v2)
    else
      None

  let ask man q = None

end


(*==========================================================================*)
(**                          {2 Registration}                               *)
(*==========================================================================*)

let register_simplified_value_abstraction dom =
  register_value_abstraction (module MakeValue(val dom : SIMPLIFIED_VALUE))
