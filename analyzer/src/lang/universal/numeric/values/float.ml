(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Interval abstraction of floating-point values. *)

open Framework.Query
open Framework.Ast
open Ast
open Bot

let debug fmt = Debug.debug ~channel:"universal.numeric.floats.value" fmt

module I = Intervals.FloatItv

type v = I.t
type t = v with_bot

let bottom = BOT

let top = Nb (I.minf_inf)

let is_bottom abs =
  bot_dfl1 true (fun itv -> not (I.is_valid itv)) abs

let is_top abs =
  bot_dfl1 false (fun itv -> not (I.is_bounded itv)) abs

let leq (a1:t) (a2:t) : bool = I.included_bot a1 a2

let join (a1:t) (a2:t) : t = I.join_bot a1 a2

let widening ctx (a1:t) (a2:t) : t = I.widen_bot a1 a2

let meet (a1:t) (a2:t) : t = I.meet_bot a1 a2

let print fmt (a:t) = I.fprint_bot I.F.dfl_fmt fmt a

let debug_bin name op printres a1 a2 =
  let r = op a1 a2 in
  debug "@[%a@] %s @[%a@] -> @[%a@]" print a1 name print a2 printres r;
  r

let leq = debug_bin "⊆" leq Format.pp_print_bool
let join = debug_bin "∪" join print
let meet = debug_bin "∩" meet print
let widening ctx = debug_bin "∇" (widening ctx) print


let of_constant = function
  | C_float i ->
    Nb (I.cst i)
  | C_float_interval (i1,i2) ->
    Nb (I.of_float i1 i2)
  | C_true ->
    Nb (I.of_float 1. 1.)
  | C_false ->
    Nb (I.of_float 0. 0.)
  | _ -> top

let to_int a =
  bot_absorb1 (fun f ->
      let {I.lo; up} = I.round f in
      Int.of_float lo up
    ) a


let fwd_unop op a =
  match op with
  | O_log_not -> bot_lift1 I.log_not a
  | O_minus -> bot_lift1 I.neg a
  | O_sqrt -> bot_absorb1 I.sqrt a
  | O_plus -> a
  | _ -> top

let fwd_binop op a1 a2 =
  match op with
  | O_plus  -> bot_lift2 I.add a1 a2
  | O_minus -> bot_lift2 I.sub a1 a2
  | O_mult  -> bot_lift2 I.mul a1 a2
  | O_div   -> bot_absorb2 I.div a1 a2
  | O_pow   -> bot_lift2 I.pow a1 a2
  | O_log_or   -> bot_lift2 I.log_or a1 a2
  | O_log_and  -> bot_lift2 I.log_and a1 a2
  | O_mod   -> bot_absorb2 I.fmod a1 a2
  | _     -> top

let fwd_filter op a1 a2 =
  match op with
  | O_eq    -> bot_apply2 false false I.is_log_eq a1 a2
  | O_ne    -> bot_apply2 false true I.is_log_neq a1 a2
  | O_lt    -> bot_apply2 false false I.is_log_lt a1 a2
  | O_le    -> bot_apply2 false false I.is_log_leq a1 a2
  | O_gt    -> bot_apply2 false false I.is_log_gt a1 a2
  | O_ge    -> bot_apply2 false false I.is_log_geq a1 a2
  | _ -> assert false

let assume_true a =
  bot_absorb1 I.meet_nonzero a

let assume_false a =
  bot_absorb1 I.meet_zero a

let bwd_unop op abs rabs =
  try
    let a, r = bot_to_exn abs, bot_to_exn rabs in
    let aa = match op with
      | O_log_not -> assert false
      | O_minus -> bot_to_exn (I.bwd_neg a r)
      | _ -> assert false
    in
    Nb aa
  with Found_BOT ->
    bottom


let bwd_binop op a1 a2 r =
  try
    let a1, a2, r = bot_to_exn a1, bot_to_exn a2, bot_to_exn r in
    let aa1, aa2 =
      match op with
      | O_plus  -> bot_to_exn (I.bwd_add a1 a2 r)
      | O_minus -> bot_to_exn (I.bwd_sub a1 a2 r)
      | O_mult  -> bot_to_exn (I.bwd_mul a1 a2 r)
      | O_div   -> bot_to_exn (I.bwd_div a1 a2 r)
      | O_mod   -> bot_to_exn (I.bwd_rem a1 a2 r)
      | _ -> assert false
    in
    Nb aa1, Nb aa2
  with Found_BOT ->
    bottom, bottom

let bwd_filter op a1 a2 =
  try
    let a1, a2 = bot_to_exn a1, bot_to_exn a2 in
    let aa1, aa2 =
      match op with
        | O_eq -> bot_to_exn (I.filter_eq a1 a2)
        | O_ne -> bot_to_exn (I.filter_neq a1 a2)
        | O_lt -> bot_to_exn (I.filter_lt a1 a2)
        | O_gt -> bot_to_exn (I.filter_gt a1 a2)
        | O_le -> bot_to_exn (I.filter_leq a1 a2)
        | O_ge -> bot_to_exn (I.filter_geq a1 a2)
        | _ -> assert false
    in
    Nb aa1, Nb aa2
  with Found_BOT ->
    bottom, bottom


let from_apron (apitv: Apron.Interval.t) : t =
  if Apron.Interval.is_bottom apitv then
    bottom
  else
    let inf = apitv.Apron.Interval.inf
    and sup = apitv.Apron.Interval.sup
    in

    let scalar_to_float s =
      let styp = Apron.Scalar.is_infty s in
      if styp = -1 then neg_infinity
      else if styp = 1 then infinity
      else
        let x = Apron.Scalar.to_string s in
        float_of_string x
    in

    Nb (I.of_float (scalar_to_float inf) (scalar_to_float sup))

let is_bounded a = bot_dfl1 true I.is_bounded a

let contains c abs =
  match c with
  | C_float i -> bot_dfl1 false (I.contains i) abs
  | C_float_interval (i1,i2) -> assert false
  | _ -> false

let can_be_true abs =
  bot_dfl1 false I.contains_nonzero abs

let can_be_false abs =
  bot_dfl1 false I.contains_zero abs

let of_int_interval (iitv: Int.t) : t =
  let bound_to_float = function
    | Intervals.IntBound.MINF -> neg_infinity
    | Intervals.IntBound.PINF -> infinity
    | Intervals.IntBound.Finite z -> Z.to_float z
  in
  bot_absorb1 (fun (a, b) ->
      Nb (I.of_float (bound_to_float a) (bound_to_float b))
    ) iitv
