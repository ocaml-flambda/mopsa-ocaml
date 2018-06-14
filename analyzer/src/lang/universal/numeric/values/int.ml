(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Interval abstraction of integer values. *)

open Framework.Essentials
open Ast
open Bot

let name = "universal.numeric.values.int"
let debug fmt = Debug.debug ~channel:name fmt

module Value =
struct

  module I = Intervals.IntItv

  type v = I.t
  type t = v with_bot

  let zone = Zone.Z_num_int

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

  let print fmt (a:t) = I.fprint_bot fmt a

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
      Nb (I.of_z i i)

    | C_int_interval (i1,i2) ->
      Nb (I.of_z i1 i2)

    | _ -> top

  let of_bool = function
    | Some true -> Nb (I.cst Z.one)
    | Some false -> Nb (I.cst Z.zero)
    | None -> Nb (I.of_range Z.zero Z.one)


  let of_float lo up = Nb (I.of_float lo up)

  let fwd_unop op a =
    match op with
    | O_log_not -> bot_lift1 I.log_not a
    | O_minus  -> bot_lift1 I.neg a
    | O_plus  -> a
    | O_wrap(l, u) ->
      let rep =  bot_lift1 (fun itv -> I.wrap itv l u) a in
      let () = debug "O_wrap done : %a [%a-%a] : %a" print a Z.pp_print l Z.pp_print u print rep in
      rep
    | _ -> top

  let fwd_binop op a1 a2 =
    match op with
    | O_plus   -> bot_lift2 I.add a1 a2
    | O_minus  -> bot_lift2 I.sub a1 a2
    | O_mult   -> bot_lift2 I.mul a1 a2
    | O_div    -> bot_absorb2 I.div a1 a2
    | O_pow   -> bot_lift2 I.pow a1 a2
    | O_log_or   -> bot_lift2 I.log_or a1 a2
    | O_log_and  -> bot_lift2 I.log_and a1 a2
    | O_mod    -> bot_absorb2 I.rem a1 a2
    | O_bit_and -> bot_lift2 I.bit_and a1 a2
    | O_bit_or -> bot_lift2 I.bit_or a1 a2
    | O_bit_xor -> bot_lift2 I.bit_xor a1 a2
    | O_bit_rshift -> bot_absorb2 I.shift_right a1 a2
    | O_bit_lshift -> bot_absorb2 I.shift_left a1 a2
    | _     -> top

  let fwd_filter op a1 a2 =
    match op with
    | O_eq    -> bot_apply2 false false I.is_log_eq a1 a2
    | O_ne    -> bot_apply2 false true I.is_log_neq a1 a2
    | O_lt    -> bot_apply2 false false I.is_log_lt a1 a2
    | O_le    -> bot_apply2 false false I.is_log_leq a1 a2
    | O_gt    -> bot_apply2 false false I.is_log_gt a1 a2
    | O_ge    -> bot_apply2 false false I.is_log_geq a1 a2
    | _ -> Framework.Utils.Exceptions.fail "fwd_filter: unknown operator %a" Framework.Ast.pp_operator op

  let assume_true a =
    bot_absorb1 I.meet_nonzero a

  let assume_false a =
    bot_absorb1 I.meet_zero a

  let bwd_unop op abs rabs =
    try
      let a, r = bot_to_exn abs, bot_to_exn rabs in
      let aa = match op with
        | O_minus  -> bot_to_exn (I.bwd_neg a r)
        | O_wrap(l,u) -> bot_to_exn (I.bwd_wrap a (l,u) r)
        | _ ->
          let () = Debug.fail "following backward %a unary operator is not yet implemented"
              Framework.Ast.pp_operator op in
          assert false
      in
      Nb aa
    with Found_BOT ->
      bottom


  let bwd_binop op a1 a2 r =
    try
      let a1, a2, r = bot_to_exn a1, bot_to_exn a2, bot_to_exn r in
      let aa1, aa2 =
        match op with
        | O_plus   -> bot_to_exn (I.bwd_add a1 a2 r)
        | O_minus  -> bot_to_exn (I.bwd_sub a1 a2 r)
        | O_mult   -> bot_to_exn (I.bwd_mul a1 a2 r)
        | O_div    -> bot_to_exn (I.bwd_div a1 a2 r)
        | O_mod    -> bot_to_exn (I.bwd_rem a1 a2 r)
        | O_pow   -> bot_to_exn (I.bwd_pow a1 a2 r)
        | O_bit_and -> bot_to_exn (I.bwd_bit_and a1 a2 r)
        | O_bit_or  -> bot_to_exn (I.bwd_bit_or a1 a2 r)
        | O_bit_xor -> bot_to_exn (I.bwd_bit_xor a1 a2 r)
        | O_bit_rshift -> bot_to_exn (I.bwd_shift_right a1 a2 r)
        | O_bit_lshift -> bot_to_exn (I.bwd_shift_left a1 a2 r)
        | _ -> Exceptions.fail "bwd_binop: unknown operator %a" Framework.Ast.pp_operator op
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
        | _ -> Exceptions.fail "bwd_filter: unknown operator %a" pp_operator op
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

      let scalar_to_bound s =
        let styp = Apron.Scalar.is_infty s in
        if styp = -1 then Intervals.IntBound.MINF
        else if styp = 1 then Intervals.IntBound.PINF
        else
          let x = Apron.Scalar.to_string s in
          let f = float_of_string x in
          Intervals.IntBound.of_float f
      in

      Nb (I.of_bound (scalar_to_bound inf) (scalar_to_bound sup))

  let is_constant a = bot_dfl1 false I.is_singleton a

  let is_bounded a = bot_dfl1 true I.is_bounded a

  exception Unbounded
  let fold f x a =
    try
      bot_dfl1 x (fun itv ->
          debug "fold";
          let elements = I.to_list itv in
          debug "elements %d" (List.length elements);
          List.fold_left (fun acc i -> f acc i) x elements
        ) a
    with Invalid_argument _ ->
      raise Unbounded

  let get_bounds (a:t) =
    let itv = bot_to_exn a in
    let min, max = itv in
    match min, max with
    | Intervals.IntBound.Finite min, Intervals.IntBound.Finite max -> (min, max)
    | _ -> raise Unbounded


  let contains c abs =
    match c with
    | C_int i -> bot_dfl1 false (I.contains i) abs
    | _ -> true

  let can_be_true abs =
    bot_dfl1 false I.contains_nonzero abs

  let can_be_false abs =
    bot_dfl1 false I.contains_zero abs

  let z_of_z2 z z' round =
    let open Z in
    let d, r = div_rem z z' in
    if equal r zero then
      d
    else
      begin
        if round then
          d + one
        else
          d
      end

  let z_of_mpzf mp =
    Z.of_string (Mpzf.to_string mp)

  let z_of_mpqf mp round =
    let open Mpqf in
    let l, r = to_mpzf2 mp in
    let lz, rz = z_of_mpzf l, z_of_mpzf r in
    z_of_z2 lz rz round

  let z_of_apron_scalar a r =
    let open Apron.Scalar in
    match a, r with
    | Float f, true  -> Z.of_float (ceil f)
    | Float f, false -> Z.of_float (floor f)
    | Mpqf q, _ ->  z_of_mpqf q r
    | Mpfrf mpf, _ -> z_of_mpqf (Mpfr.to_mpq mpf) r

  let of_apron (itv : Apron.Interval.t) =
    let open Apron.Interval in
    if is_bottom itv then
      BOT
    else
      let open Apron.Scalar in
      let mi = itv.inf in
      let ma = itv.sup in
      let to_b m r =
        let x = is_infty m in
        if x = 0 then I.B.Finite (z_of_apron_scalar m r)
        else if x > 0 then I.B.PINF
        else I.B.MINF
      in
      Nb (to_b mi false, to_b ma true)
end

let () =
  Framework.Value.register_value name (module Value)
