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
open Sig.Abstraction.Simplified_value
open Common

(** Use the simplified signature for handling homogenous operators *)
module SimplifiedValue =
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

  let top_of_typ = function
    | T_int  -> top
    | T_bool -> Nb (I.of_int 0 1)
    | _      -> assert false

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

  let constant c t =
    match c with
    | C_bool true ->
      Nb (I.cst_int 1)

    | C_bool false ->
      Nb (I.cst_int 0)

    | C_top T_bool ->
      Nb (I.of_int 0 1)

    | C_int i ->
      Nb (I.of_z i i)

    | C_int_interval (i1,i2) ->
      Nb (I.of_bound i1 i2)

    | C_avalue(V_int_interval, itv) -> itv
    | C_avalue(V_int_interval_fast, itv) -> itv

    | _ -> top_of_typ t

  let unop op t a tr =
    match op with
    | O_log_not -> bot_lift1 I.log_not a
    | O_minus  -> bot_lift1 I.neg a
    | O_plus  -> a
    | O_abs -> bot_lift1 I.abs a
    | O_wrap(l, u) -> bot_lift1 (fun itv -> I.wrap itv l u) a
    | O_bit_invert -> bot_lift1 I.bit_not a
    | _ -> top_of_typ tr

  let binop op t1 a1 t2 a2 tr =
    match op with
    | O_plus   -> bot_lift2 I.add a1 a2
    | O_minus  -> bot_lift2 I.sub a1 a2
    | O_mult   -> bot_lift2 I.mul a1 a2
    | O_div    -> bot_absorb2 I.div a1 a2
    | O_ediv   -> bot_absorb2 I.ediv a1 a2
    | O_pow    -> bot_lift2 I.pow a1 a2
    | O_eq     -> bot_lift2 I.log_eq a1 a2
    | O_ne     -> bot_lift2 I.log_neq a1 a2
    | O_lt     -> bot_lift2 I.log_lt a1 a2
    | O_le     -> bot_lift2 I.log_leq a1 a2
    | O_gt     -> bot_lift2 I.log_gt a1 a2
    | O_ge     -> bot_lift2 I.log_geq a1 a2
    | O_log_or   -> bot_lift2 I.log_or a1 a2
    | O_log_and  -> bot_lift2 I.log_and a1 a2
    | O_log_xor -> bot_lift2 I.log_xor a1 a2
    | O_mod    -> bot_absorb2 I.rem a1 a2
    | O_erem   -> bot_absorb2 I.erem a1 a2
    | O_bit_and -> bot_lift2 I.bit_and a1 a2
    | O_bit_or -> bot_lift2 I.bit_or a1 a2
    | O_bit_xor -> bot_lift2 I.bit_xor a1 a2
    | O_bit_rshift -> bot_absorb2 I.shift_right a1 a2
    | O_bit_lshift -> bot_absorb2 I.shift_left a1 a2
    | O_convex_join -> bot_lift2 I.join a1 a2
    | _     -> top_of_typ tr


  let filter b t a =
    if b then bot_absorb1 I.meet_nonzero a
    else bot_absorb1 I.meet_zero a

  let backward_unop op t a t r =
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

  let backward_binop op t1 a1 t2 a2 tr r =
    try
      let a1, a2, r = bot_to_exn a1, bot_to_exn a2, bot_to_exn r in
      let aa1, aa2 =
        match op with
        | O_plus   -> bot_to_exn (I.bwd_add a1 a2 r)
        | O_minus  -> bot_to_exn (I.bwd_sub a1 a2 r)
        | O_mult   -> bot_to_exn (I.bwd_mul a1 a2 r)
        | O_div    -> bot_to_exn (I.bwd_div a1 a2 r)
        | O_ediv   -> bot_to_exn (I.bwd_ediv a1 a2 r)
        | O_mod    -> bot_to_exn (I.bwd_rem a1 a2 r)
        | O_erem   -> bot_to_exn (I.bwd_erem a1 a2 r)
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
        | O_convex_join -> bot_to_exn (I.bwd_convex_join a1 a2 r)
        | _ -> Exceptions.panic "bwd_binop: unknown operator %a" pp_operator op
      in
      Nb aa1, Nb aa2
    with Found_BOT ->
      bottom, bottom


  let compare op b t1 a1 t2 a2 =
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

  let avalue : type r. r avalue_kind -> t -> r option =
    fun aval a ->
    match aval with
    | Common.V_int_interval -> Some a
    | Common.V_int_interval_fast -> Some a
    | Common.V_int_congr_interval -> Some (a, Bot.Nb Common.C.minf_inf)
    | _ -> None


end


(** We lift now to the advanced signature to handle casts and queries *)
open Sig.Abstraction.Value
module Value =
struct

  include SimplifiedValue

  module V = MakeValue(SimplifiedValue)

  include V

  (** Cast a non-integer value to an integer *)
  let cast man e =
    match e.etyp with
    | T_float p ->
      (* Get the value of the float *)
      let v = man.eval e in
      (* Convert it to a float interval *)
      let float_itv = man.avalue (Common.V_float_interval p) v in
      (* Perform the cast to an integer interval *)
      ItvUtils.FloatItvNan.to_int_itv float_itv

    | _ -> top

  (* Evaluation of integer expressions *)
  let eval man e =
    match ekind e with
    (* Casts *)
    | E_unop(O_cast,ee) -> cast man ee
    | _ ->
      (* Other expressions are handled by the simplified domain *)
      let r = V.eval man e in
      (* Ensure that boolean values are in [0,1] *)
      match e.etyp with
      | T_bool -> meet r (top_of_typ T_bool)
      | _ -> r

  (* Extended backward refinement of casts to integers. *)
  let backward_ext_cast man e ve r =
    match e.etyp with
    | T_float p ->
      begin match r with
        | BOT -> None
        | Nb iitv ->
          (* Get the float value *)
          let v,_ = find_vexpr e ve in
          (* Convert it to a float interval *)
          let fitv = man.avalue (Common.V_float_interval p) v in
          (* Refine it with the integer result *)
          let fitv' = ItvUtils.FloatItvNan.bwd_to_int_itv fitv iitv in
          (* Evaluate the float interval to a float value *)
          let v' = man.eval (mk_avalue_expr (Common.V_float_interval p) fitv' e.erange) in
          (* Refine the expression [e] with the new value [v'] *)
          refine_vexpr e (man.meet v v') ve |>
          OptionExt.return
      end
    | _ -> None

  (* Extended backward evaluations *)
  let backward_ext man e ve r =
    match ekind e with
    | E_unop(O_cast,ee) ->
      (* We use the extended transfer function because we need to refine
         a non-integer value *)
      backward_ext_cast man ee ve (man.get r)
    | _ -> V.backward_ext man e ve r


  (** {2 Utility functions} *)

  let zero = Nb (I.zero)
  let one = Nb (I.one)
  let of_z z1 z2 : t = Nb (I.of_z z1 z2)
  let of_int n1 n2 : t = Nb (I.of_int n1 n2)

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
    if not (is_bounded itv) then
      panic ~loc:__LOC__ "map: unbounded interval %a" (format print) itv
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
