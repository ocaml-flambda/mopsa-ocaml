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

(** Finite powerset of integer constants *)

open Mopsa
open Sig.Abstraction.Simplified_value
open Ast
open Top
open Common


module Value =
struct


  (** {2 Types} *)
  (** ********* *)

  module Powerset = Framework.Lattices.Powerset.Make(struct
      type t = Z.t
      let compare = Z.compare
      let print = unformat Z.pp_print
    end)

  include Powerset


  include GenValueId(
    struct
        type nonrec t = t
        let name = "universal.numeric.values.powersets.standard"
        let display = "powerset"
    end
    )

  (** {2 Options} *)
  (** *********** *)


  let opt_max_intset = ref 10

  let () =
    register_domain_option name {
      key = "-max-set-size";
      category = "Numeric";
      doc = " maximum size of integer sets";
      spec = ArgExt.Set_int opt_max_intset;
      default = string_of_int !opt_max_intset;
    }


  (** {2 Utilities} *)
  (** ************* *)


  (* ensures that x is not too large *)
  let bound (x:t) : t =
    match x with
    | Nt s when Set.cardinal s <= !opt_max_intset -> x
    | _ -> TOP

  (* add 1 if t is true, 0 if f is true *)
  let of_bool t f = match t,f with
    | false, false -> bottom
    | true, false -> singleton Z.one |> bound
    | false, true -> singleton Z.zero |> bound
    | true, true -> of_list [Z.zero;Z.one] |> bound

  (* [l;h] *)
  let of_bounds (l:Z.t) (h:Z.t) : t =
    let rec doit acc i =
      if i > h then Nt acc
      else doit (Set.add i acc) (Z.succ i)
    in
    if Z.sub h l >= Z.of_int !opt_max_intset then top else doit Set.empty l

  let zero =
    singleton Z.zero

  let is_zero x =
    equal x (singleton Z.zero)

  let contains_zero a =
    mem Z.zero a

  let contains_nonzero a =
    not (is_empty (remove Z.zero a))

  let to_itv (a:t) : int_itv =
    if is_bottom a then BOT else
    match a with
    | Nt s -> Nb (I.of_z (Set.min_elt s) (Set.max_elt s))
    | TOP  -> Nb I.minf_inf

  (** {2 Forward operators} *)
  (** ********************* *)

  include DefaultValueFunctions

  let accept_type = function
    | T_int | T_bool -> true
    | _ -> false

  let constant c t =
    match c with
    | C_bool true -> singleton Z.one |> bound
    | C_bool false -> singleton Z.zero |> bound
    | C_top T_bool -> of_bounds Z.zero Z.one |> bound
    | C_int n -> singleton n |> bound
    | C_int_interval (ItvUtils.IntBound.Finite i1, ItvUtils.IntBound.Finite i2) -> of_bounds i1 i2
    | _ -> TOP

  let unop op t a tr =
    match op with
    | O_plus       -> a
    | O_minus      -> map Z.neg a
    | O_log_not    -> of_bool (contains_zero a) (contains_nonzero a)
    | O_bit_invert -> map Z.lognot a
    | _ -> top

  (* utility for binary operators *)
  let map2 f a1 a2 =
    if is_top a1 || is_top a2 then TOP
    else
      fold
        (fun n1 acc ->
           fold
             (fun n2 acc ->
                add (f n1 n2) acc
             ) a2 acc
        ) a1 empty
      |> bound

  let binop op t1 a1 t2 a2 tr =
    if is_bottom a1 || is_bottom a2 then bottom else
    if is_top a1 || is_top a2 then top else
      let with_int f a b = f a (Z.to_int b) in
      try
        match op with
        | O_plus -> map2 Z.add a1 a2
        | O_minus -> map2 Z.sub a1 a2
        | O_mult ->
          if is_zero a1 || is_zero a2 then zero
          else map2 Z.mul a1 a2
        | O_div ->
          if is_zero a1 then zero
          else map2 Z.div a1 (remove Z.zero a2)
        | O_mod ->
          if is_zero a1 then zero
          else map2 Z.rem a1 (remove Z.zero a2)
        | O_pow -> map2 (with_int Z.pow) a1 a2
        | O_bit_and -> map2 Z.logand a1 a2
        | O_bit_or -> map2 Z.logor a1 a2
        | O_bit_xor -> map2 Z.logxor a1 a2
        | O_bit_lshift ->
          if is_zero a1 then zero
          else map2 (with_int Z.shift_left )a1 a2
        | O_bit_rshift ->
          if is_zero a1 then zero
          else map2 (with_int Z.shift_right) a1 a2
        | O_log_or ->
          of_bool
            (contains_nonzero a1 || contains_nonzero a2)
            (contains_zero a1 && contains_zero a2)
        | O_log_and    ->
          of_bool
            (contains_nonzero a1 && contains_nonzero a2)
            (contains_zero a1 || contains_zero a2)
        | O_log_xor ->
          of_bool
            ((contains_nonzero a1 && contains_zero a2) ||
             (contains_zero a1 && contains_nonzero a2))
            ((contains_zero a1 && contains_zero a2) ||
             (contains_nonzero a1 && contains_nonzero a2))
        | _     -> top
      with Z.Overflow -> TOP

  let join (a1: t) (a2: t) : t =
    Powerset.join a1 a2 |> bound

  let widen ctx (a1:t) (a2:t) : t =
    (*if subset a2 a1 then a1 else TOP*)
    join a1 a2



  (** {2 Backward operators} *)
  (** ********************** *)


  let filter b t a =
    if b then remove Z.zero a
    else meet a (singleton Z.zero)

  let avalue : type r. r avalue_kind -> t -> r option =
    fun aval a ->
    match aval with
    | Common.V_int_interval       -> Some (to_itv a)
    | Common.V_int_interval_fast  -> Some (to_itv a)
    | Common.V_int_congr_interval -> Some (to_itv a, Bot.Nb Common.C.minf_inf)
    | _ -> None

  let backward_unop op t a tr r =
    match op with
    | O_plus       -> meet a r
    | O_minus      -> meet a (map Z.neg r)
    | O_bit_invert -> meet a (map Z.lognot r)
    | _ -> default_backward_unop op t a tr r

  let backward_binop op t1 a1 t2 a2 tr r =
    let b1, b2 =  match op with
      | O_plus  -> meet a1 (map2 Z.sub r a2), meet a2 (map2 Z.sub r a1)
      | O_minus -> meet a1 (map2 Z.add a2 r), meet a2 (map2 Z.sub a1 r)
      | O_mult  ->
        if contains_zero a1 || contains_zero a2 then a1,a2
        else meet a1 (map2 Z.div r a2), meet a2 (map2 Z.div r a1)
      | _ ->
        (* TODO: support precisely other operators *)
        default_backward_binop op t1 a1 t2 a2 tr r
    in
    if is_empty b1 || is_empty b2 then bottom, bottom
    else b1,b2



  (** {2 Comparisons} *)
  (** *************** *)


  (* utility for compare *)
  let filt a1 cmp minmax a2 =
    match a2 with
    | TOP -> a1
    | Nt b2 ->
      let m = minmax b2 in
      Powerset.filter (fun n -> cmp n m) a1

  let compare op b t1 a1 t2 a2 =
    let op = if b then op else negate_comparison_op op in
    let b1,b2 =
      match op with
      | O_eq -> let a = meet a1 a2 in a,a
      | O_ne ->
        let a1 = if is_singleton a2 then diff a1 a2 else a1 in
        let a2 = if is_singleton a1 then diff a2 a1 else a2 in
        a1,a2
      | O_le ->
        filt a1 Z.leq Set.max_elt a2, filt a2 Z.geq Set.min_elt a1
      | O_ge ->
        filt a1 Z.geq Set.min_elt a2, filt a2 Z.leq Set.max_elt a1
      | O_lt ->
        filt a1 Z.lt Set.max_elt a2, filt a2 Z.gt Set.min_elt a1
      | O_gt ->
        filt a1 Z.gt Set.min_elt a2, filt a2 Z.lt Set.max_elt a1
      | _ -> default_compare op b t1 a1 t2 a2
    in
    if is_empty b1 || is_empty b2 then bottom, bottom
    else b1,b2

end

let () =
  register_simplified_value_abstraction (module Value)
