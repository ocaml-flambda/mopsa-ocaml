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

(** Interval abstraction of integer values. *)

open Mopsa
open Ast
open Bot
open Sig.Abstraction.Value
open Common


module Value =
struct

  type t = Common.int_itv

  module I = ItvUtils.IntItv
  module FI = ItvUtils.FloatItv


  include GenValueId(struct
      type nonrec t = t
      let name = "universal.numeric.values.intervals.integer"
      let display = "int-itv"
    end)

  let accept_type = function
    | T_int | T_bool -> true
    | _ -> false

  let bottom = BOT

  let top = Nb (I.minf_inf)

  let is_bottom abs =
    bot_dfl1 true (fun itv -> not (I.is_valid itv)) abs

  let subset (a1:t) (a2:t) : bool = I.included_bot a1 a2

  let join (a1:t) (a2:t) : t = I.join_bot a1 a2

  let meet (a1:t) (a2:t) : t = I.meet_bot a1 a2

  let widen ctx (a1:t) (a2:t) : t =
    (* Retrieve thresholds if any *)
    match find_ctx_opt Common.widening_thresholds_ctx_key ctx with
    | None -> I.widen_bot a1 a2
    | Some thresholds ->
      bot_neutral2
        (fun (a,b) (c,d) ->
           (* Translate constants to bounds *)
           let thresholds = SetExt.ZSet.elements thresholds |>
                            List.map (fun n -> I.B.Finite n) in
           (* Apply the definition of the widening with thresholds *)
           (if I.B.leq a c then a
            else List.filter (I.B.geq c) thresholds |>
                 List.fold_left I.B.max I.B.MINF),
           (if I.B.geq b d then b
            else List.filter (I.B.leq d) thresholds |>
                 List.fold_left I.B.min I.B.PINF)
        )
        a1 a2

  let print printer (a:t) = unformat I.fprint_bot printer a

  let apply_int t default f =
    match t with
    | T_int | T_bool -> f ()
    | _              -> default

  let constant t c =
    apply_int t None @@ fun () ->
    match c with
    | C_bool true ->
      Nb (I.cst_int 1) |>
      OptionExt.return

    | C_bool false ->
      Nb (I.cst_int 0) |>
      OptionExt.return

    | C_top T_bool ->
      Nb (I.of_int 0 1) |>
      OptionExt.return

    | C_int i ->
      Nb (I.of_z i i) |>
      OptionExt.return

    | C_int_interval (i1,i2) ->
      Nb (I.of_z i1 i2) |>
      OptionExt.return

    | _ -> Some top

  let cast man t e =
    match t, e.etyp with
    | (T_int | T_bool), T_float _ ->
      let float_itv = man.ask (Common.Q_float_interval e) in
      let itv = ItvUtils.FloatItvNan.to_int_itv float_itv in
      Some itv

    | (T_int | T_bool), _ -> Some top

    | _ -> None


  let zero = Nb (I.zero)
  let one = Nb (I.one)
  let of_z z1 z2 : t = Nb (I.of_z z1 z2)
  let of_int n1 n2 : t = Nb (I.of_int n1 n2)

  let unop op t a =
    apply_int t bottom @@ fun () ->
    match op with
    | O_log_not -> bot_lift1 I.log_not a
    | O_minus  -> bot_lift1 I.neg a
    | O_plus  -> a
    | O_wrap(l, u) -> bot_lift1 (fun itv -> I.wrap itv l u) a
    | O_bit_invert -> bot_lift1 I.bit_not a
    | _ -> top

  let binop op t a1 a2 =
    apply_int t bottom @@ fun () ->
    match op with
    | O_plus   -> bot_lift2 I.add a1 a2
    | O_minus  -> bot_lift2 I.sub a1 a2
    | O_mult   -> bot_lift2 I.mul a1 a2
    | O_div    -> bot_absorb2 I.div a1 a2
    | O_pow    -> bot_lift2 I.pow a1 a2
    | O_eq     -> bot_lift2 I.log_eq a1 a2
    | O_ne     -> bot_lift2 I.log_neq a1 a2
    | O_lt     -> bot_lift2 I.log_lt a1 a2
    | O_le     -> bot_lift2 I.log_leq a1 a2
    | O_gt     -> bot_lift2 I.log_gt a1 a2
    | O_ge     -> bot_lift2 I.log_geq a1 a2
    | O_log_or   -> bot_lift2 I.log_or a1 a2
    | O_log_and  -> bot_lift2 I.log_and a1 a2
    | O_mod    -> bot_absorb2 I.rem a1 a2
    | O_bit_and -> bot_lift2 I.bit_and a1 a2
    | O_bit_or -> bot_lift2 I.bit_or a1 a2
    | O_bit_xor -> bot_lift2 I.bit_xor a1 a2
    | O_bit_rshift -> bot_absorb2 I.shift_right a1 a2
    | O_bit_lshift -> bot_absorb2 I.shift_left a1 a2
    | _     -> top

  let filter b t a =
    if b then bot_absorb1 I.meet_nonzero a
    else bot_absorb1 I.meet_zero a

  let bwd_unop op t a r =
    apply_int t a @@ fun () ->
    try
      let a, r = bot_to_exn a, bot_to_exn r in
      let aa = match op with
        | O_minus  -> bot_to_exn (I.bwd_neg a r)
        | O_wrap(l,u) -> bot_to_exn (I.bwd_wrap a (l,u) r)
        | O_bit_invert -> bot_to_exn (I.bwd_bit_not a r)
        | _ -> bot_to_exn (I.bwd_default_unary a r)
      in
      Nb aa
    with Found_BOT ->
      bottom

  let bwd_binop op t a1 a2 r =
    apply_int t (a1,a2) @@ fun () ->
    try
      let a1, a2, r = bot_to_exn a1, bot_to_exn a2, bot_to_exn r in
      let aa1, aa2 =
        match op with
        | O_plus   -> bot_to_exn (I.bwd_add a1 a2 r)
        | O_minus  -> bot_to_exn (I.bwd_sub a1 a2 r)
        | O_mult   -> bot_to_exn (I.bwd_mul a1 a2 r)
        | O_div    -> bot_to_exn (I.bwd_div a1 a2 r)
        | O_mod    -> bot_to_exn (I.bwd_rem a1 a2 r)
        | O_pow    -> bot_to_exn (I.bwd_pow a1 a2 r)
        | O_eq     -> bot_to_exn (I.bwd_log_eq a1 a2 r)
        | O_ne     -> bot_to_exn (I.bwd_log_neq a1 a2 r)
        | O_lt     -> bot_to_exn (I.bwd_log_lt a1 a2 r)
        | O_le     -> bot_to_exn (I.bwd_log_leq a1 a2 r)
        | O_gt     -> bot_to_exn (I.bwd_log_gt a1 a2 r)
        | O_ge     -> bot_to_exn (I.bwd_log_geq a1 a2 r)
        | O_bit_and -> bot_to_exn (I.bwd_bit_and a1 a2 r)
        | O_bit_or  -> bot_to_exn (I.bwd_bit_or a1 a2 r)
        | O_bit_xor -> bot_to_exn (I.bwd_bit_xor a1 a2 r)
        | O_bit_rshift -> bot_to_exn (I.bwd_shift_right a1 a2 r)
        | O_bit_lshift -> bot_to_exn (I.bwd_shift_left a1 a2 r)
        | _ -> Exceptions.panic "bwd_binop: unknown operator %a" pp_operator op
      in
      Nb aa1, Nb aa2
    with Found_BOT ->
      bottom, bottom

  let cast_range = tag_range (mk_fresh_range ()) "cast-of-int"

  let bwd_cast man t e a =
    match t with
    | T_float p ->
      (* Compute the interval of the float expression cast(t,e) *)
      let cast = mk_unop O_cast e ~etyp:t cast_range in
      let fitv = man.ask (Common.Q_float_interval cast) in
      begin match a with
        | BOT    -> bottom
        | Nb itv -> ItvUtils.FloatItvNan.bwd_of_int_itv (prec p) (round ()) itv fitv
      end
     
    | _ -> default_bwd_cast man t e a

  let predicate = default_predicate

  let compare op b t a1 a2 =
    apply_int t (a1,a2) @@ fun () ->
    try
      let a1, a2 = bot_to_exn a1, bot_to_exn a2 in
      let op = if b then op else negate_comparison_op op in
      let aa1, aa2 =
        match op with
        | O_eq -> bot_to_exn (I.filter_eq a1 a2)
        | O_ne -> bot_to_exn (I.filter_neq a1 a2)
        | O_lt -> bot_to_exn (I.filter_lt a1 a2)
        | O_gt -> bot_to_exn (I.filter_gt a1 a2)
        | O_le -> bot_to_exn (I.filter_leq a1 a2)
        | O_ge -> bot_to_exn (I.filter_geq a1 a2)
        | _ -> Exceptions.panic "compare: unknown operator %a" pp_operator op
      in
      Nb aa1, Nb aa2
    with Found_BOT ->
      bottom, bottom


  (** {2 Query handlers} *)

  let ask : type r. ('a,t) value_man -> ('a,r) query -> r option =
    fun man q ->
    match q with
    | Common.Q_int_interval e ->
      man.eval e |>
      OptionExt.return

    | Common.Q_fast_int_interval e ->
      man.eval e |>
      OptionExt.return

    | Common.Q_int_congr_interval e ->
      (man.eval e, Common.C.minf_inf) |> OptionExt.return

    | _ -> None



  (** {2 Utility functions} *)


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

  let of_apron (itv: Apron.Interval.t) : t =
    if Apron.Interval.is_bottom itv then
      bottom
    else
      let mi = itv.Apron.Interval.inf in
      let ma = itv.Apron.Interval.sup in
      let to_b m r =
        let x = Apron.Scalar.is_infty m in
        if x = 0 then I.B.Finite (z_of_apron_scalar m r)
        else if x > 0 then I.B.PINF
        else I.B.MINF
      in
      Nb (to_b mi false, to_b ma true)

  let to_apron (itv:t) : Apron.Interval.t =
    match itv with
    | BOT -> Apron.Interval.bottom
    | Nb(a,b) ->
      let bound_to_scalar b =
        match b with
        | I.B.MINF -> Apron.Scalar.of_infty (-1)
        | I.B.PINF -> Apron.Scalar.of_infty 1
        | I.B.Finite z -> Apron.Scalar.of_float (Z.to_float z)
      in
      Apron.Interval.of_infsup (bound_to_scalar a) (bound_to_scalar b)

  let is_bounded (itv:t) : bool =
    bot_dfl1 true I.is_bounded itv

  let bounds (itv:t) : Z.t * Z.t =
    bot_dfl1 (Z.one, Z.zero) (function
        | I.B.Finite a, I.B.Finite b -> (a, b)
        | _ -> panic "bounds called on a unbounded interval %a" (format print) itv
      ) itv

  let bounds_opt (itv:t) : Z.t option * Z.t option =
    bot_dfl1 (None, None) (function
        | I.B.Finite a, I.B.Finite b -> (Some a, Some b)
        | I.B.Finite a, _ -> (Some a, None)
        | _, I.B.Finite b -> (None, Some b)
        | _ -> (None, None)
      ) itv

  let mem (i: Z.t) (itv:t) : bool =
    bot_dfl1 true (fun (a, b) ->
        let open I.B in
        let i = Finite i in
        geq i a && leq i b
      ) itv

  let compare_interval itv1 itv2 =
    bot_compare (I.compare) itv1 itv2

  let map (f: Z.t -> 'a) (itv:t) : 'a list =
    if not (is_bounded itv) then panic ~loc:__LOC__ "map: unbounded interval %a" (format print) itv
    else if is_bottom itv then []
    else
      let a, b = bounds itv in
      let rec iter i =
        if Z.equal i b then [f i]
        else f i :: iter (Z.succ i)
      in
      iter a




end


let () =
  register_value_abstraction (module Value)
