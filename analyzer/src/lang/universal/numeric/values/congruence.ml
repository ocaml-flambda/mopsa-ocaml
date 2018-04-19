(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Congruences abstraction of integer values. *)

open Framework.Query
open Framework.Ast
open Ast
open Bot

let name = "universal.numeric.congruence.value"
let debug fmt = Debug.debug ~channel:name fmt

module C = Congruences.IntCong

type v = C.t
type t = v with_bot

let bottom = BOT

let top = Nb (C.minf_inf)

let is_bottom abs =
  bot_dfl1 true (fun itv -> not (C.is_valid itv)) abs

let is_top abs =
  bot_dfl1 false (fun itv -> not (C.is_bounded itv)) abs

let leq (a1:t) (a2:t) : bool = C.included_bot a1 a2

let join (a1:t) (a2:t) : t = C.join_bot a1 a2

let widening ctx (a1:t) (a2:t) : t = join a1 a2

let meet (a1:t) (a2:t) : t = C.meet_bot a1 a2

let print fmt (a:t) = C.fprint_bot fmt a

let debug_bin name op printres a1 a2 =
  let r = op a1 a2 in
  debug "@[%a@] %s @[%a@] -> @[%a@]" print a1 name print a2 printres r;
  r

let leq = debug_bin "⊆" leq Format.pp_print_bool
let join = debug_bin "∪" join print
let meet = debug_bin "∩" meet print
let widening ctx = debug_bin "∇" (widening ctx) print


let of_constant = function
  | C_int i ->
    Nb (C.cst i)

  | C_int_interval (i1,i2) ->
    Nb (C.of_range i1 i2)

  | C_true ->
    Nb (C.cst_int 1)

  | C_false ->
    Nb (C.cst_int 0)

  | _ -> top

let fwd_unop op a =
  match op with
  | O_log_not -> bot_lift1 C.log_not a
  | O_minus -> bot_lift1 C.neg a
  | O_plus -> a
  | _ -> top

let fwd_binop op a1 a2 =
  match op with
  | O_plus  -> bot_lift2 C.add a1 a2
  | O_minus -> bot_lift2 C.sub a1 a2
  | O_mult  -> bot_lift2 C.mul a1 a2
  | O_div   -> bot_absorb2 C.div a1 a2
  | O_log_or   -> bot_lift2 C.log_or a1 a2
  | O_log_and  -> bot_lift2 C.log_and a1 a2
  | O_mod   -> bot_absorb2 C.rem a1 a2
  | O_bit_rshift -> bot_absorb2 C.shift_right a1 a2
  | O_bit_lshift -> bot_absorb2 C.shift_left a1 a2
  | _     -> top

let fwd_filter op a1 a2 =
  match op with
  | O_eq    -> bot_apply2 false false C.is_log_eq a1 a2
  | O_ne    -> bot_apply2 false true C.is_log_neq a1 a2
  | O_lt    -> bot_apply2 false false C.is_log_lt a1 a2
  | O_le    -> bot_apply2 false false C.is_log_leq a1 a2
  | O_gt    -> bot_apply2 false false C.is_log_gt a1 a2
  | O_ge    -> bot_apply2 false false C.is_log_geq a1 a2
  | _ -> assert false

let assume_true a =
  bot_absorb1 C.meet_nonzero a

let assume_false a =
  bot_absorb1 C.meet_zero a

let bwd_unop op abs rabs =
  try
    let a, r = bot_to_exn abs, bot_to_exn rabs in
    let aa = match op with
      | O_log_not -> assert false
      | O_minus -> bot_to_exn (C.bwd_neg a r)
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
      | O_plus  -> bot_to_exn (C.bwd_add a1 a2 r)
      | O_minus -> bot_to_exn (C.bwd_sub a1 a2 r)
      | O_mult  -> bot_to_exn (C.bwd_mul a1 a2 r)
      | O_div   -> bot_to_exn (C.bwd_div a1 a2 r)
      | O_mod   -> bot_to_exn (C.bwd_rem a1 a2 r)
      | O_bit_rshift -> bot_to_exn (C.bwd_shift_right a1 a2 r)
      | O_bit_lshift -> bot_to_exn (C.bwd_shift_left a1 a2 r)
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
      | O_eq -> bot_to_exn (C.filter_eq a1 a2)
      | O_ne -> bot_to_exn (C.filter_neq a1 a2)
      | O_lt -> bot_to_exn (C.filter_lt a1 a2)
      | O_gt -> bot_to_exn (C.filter_gt a1 a2)
      | O_le -> bot_to_exn (C.filter_leq a1 a2)
      | O_ge -> bot_to_exn (C.filter_geq a1 a2)
      | _ -> assert false
    in
    Nb aa1, Nb aa2
  with Found_BOT ->
    bottom, bottom


let is_bounded a = bot_dfl1 true C.is_bounded a

let contains c abs =
  match c with
  | C_int i -> bot_dfl1 false (C.contains i) abs
  | _ -> true

let can_be_true abs =
  bot_dfl1 false C.contains_nonzero abs

let can_be_false abs =
  bot_dfl1 false C.contains_zero abs

let is_minf_inf abs =
  bot_dfl1 false C.is_minf_inf abs

let as_tuple (abs: t) =
  let a, b = bot_to_exn abs in
  a, b
