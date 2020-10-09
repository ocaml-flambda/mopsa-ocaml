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

(** Interval abstraction of float values. *)

open Mopsa
open Sig.Abstraction.Value
open Rounding
open Ast
open Bot
open Common


module Value =
struct

  (** Types *)

  module I = ItvUtils.FloatItvNan
  module FI = ItvUtils.FloatItv
  module II = ItvUtils.IntItv

  type t = I.t

  include GenValueId(
    struct
      type nonrec t = t
      let name = "universal.numeric.values.intervals.float"
      let display = "float-itv"
    end
    )


  let () =
    import_standalone_option Rounding.name ~into:name

  let accept_type = function
    | T_float _ -> true
    | _ -> false

  (** Lattice operations *)

  let bottom = I.bot

  let top_of_prec = function
    | F_SINGLE -> I.single_special
    | F_DOUBLE -> I.double_special
    | F_LONG_DOUBLE -> I.long_double_special
    | F_REAL -> I.real

  let top = top_of_prec F_LONG_DOUBLE

  let is_bottom = I.is_bot

  let subset (a1:t) (a2:t) : bool = I.included a1 a2

  let join (a1:t) (a2:t) : t = I.join a1 a2

  let meet (a1:t) (a2:t) : t = I.meet a1 a2

  let widen ctx (a1:t) (a2:t) : t = I.widen a1 a2

  let print printer (a:t) = unformat (I.fprint I.dfl_fmt) printer a


  (** Arithmetic operators *)

  let constant t c =
    match t, c with
    | T_float p, C_float i ->
      I.of_float_prec (prec p) (round ()) i i |>
      OptionExt.return

    | T_float p, C_float_interval (lo,up) ->
      I.of_float_prec (prec p) (round ()) lo up |>
      OptionExt.return

    | T_float p, C_int_interval (lo,up) ->
      I.of_z (prec p) (round ()) lo up |>
      OptionExt.return

    | T_float p, C_int i ->
      I.of_z (prec p) (round ()) i i |>
      OptionExt.return

    | T_float _, C_bool false ->
      I.zero |>
      OptionExt.return

    | T_float _, C_bool true ->
      I.one |>
      OptionExt.return

    | T_float p, C_top (T_float pp) when p = pp ->
      top_of_prec p |>
      OptionExt.return

    | T_float _, _ -> Some top

    | _ -> None

  let cast man t e =
    match t, e.etyp with
    | T_float p, (T_int | T_bool) ->
      let int_itv = man.ask (Common.Q_int_interval e) in
      I.of_int_itv_bot (prec p) (round ()) int_itv |>
      OptionExt.return

    | T_float p, _ -> Some (top_of_prec p)

    | _ -> None

  let apply_float t default f =
    match t with
    | T_float p -> f p
    | _ -> default

  let unop op t a =
    apply_float t bottom @@ fun p ->
    match op with
    | O_minus -> I.neg a
    | O_plus  -> a
    | O_sqrt  -> I.sqrt (prec p) (round ()) a
    | _ -> top_of_prec p


  let binop op t a1 a2 =
    apply_float t bottom @@ fun p ->
    match op with
    | O_plus  -> I.add (prec p) (round ()) a1 a2
    | O_minus -> I.sub (prec p) (round ()) a1 a2
    | O_mult  -> I.mul (prec p) (round ()) a1 a2
    | O_div   -> I.div (prec p) (round ()) a1 a2
    | O_mod   -> I.fmod (prec p) (round ()) a1 a2
    | _       -> top_of_prec p

  let filter b t a = a

  let bwd_unop op t a r =
    apply_float t a @@ fun p ->
    match op with
    | O_minus -> I.bwd_neg a r
    | O_plus  -> I.meet a r
    | O_sqrt  -> I.bwd_sqrt (prec p) (round ()) a r
    | _       -> a

  let bwd_binop op t a1 a2 r =
    apply_float t (a1,a2) @@ fun p ->
    match op with
    | O_plus  -> I.bwd_add (prec p) (round ()) a1 a2 r
    | O_minus -> I.bwd_sub (prec p) (round ()) a1 a2 r
    | O_mult  -> I.bwd_mul (prec p) (round ()) a1 a2 r
    | O_div   -> I.bwd_div (prec p) (round ()) a1 a2 r
    | O_mod   -> I.bwd_fmod (prec p) (round ()) a1 a2 r
    | _       -> a1,a2

  let cast_range = tag_range (mk_fresh_range ()) "cast-of-float"

  let bwd_cast man t e a =
    match t with
    | T_int ->
      (* Compute the interval of the integer expression cast(t,e) *)
      let cast = mk_unop O_cast e ~etyp:t cast_range in
      let iitv = man.ask (Common.Q_int_interval cast) in
      begin match iitv with
        | BOT    -> bottom
        | Nb itv -> ItvUtils.FloatItvNan.bwd_to_int_itv a itv
      end

    | _ -> default_bwd_cast man t e a

  let predicate op b t a =
    match op with
    | O_float_class c ->
      let c = if b then c else inv_float_class c in
      { I.itv  = if c.float_valid then a.I.itv  else BOT;
        I.pinf = if c.float_inf   then a.I.pinf else false;
        I.minf = if c.float_inf   then a.I.minf else false;
        I.nan  = if c.float_nan   then a.I.nan  else false;
      }
    | _ -> a

  let compare op b t a1 a2 =
    apply_float t (a1,a2) @@ fun p ->
    match b, op with
    | true, O_eq | false, O_ne -> I.filter_eq  (prec p) a1 a2
    | true, O_ne | false, O_eq -> I.filter_neq (prec p) a1 a2
    | true, O_lt -> I.filter_lt  (prec p) a1 a2
    | true, O_le -> I.filter_leq (prec p) a1 a2
    | true, O_gt -> I.filter_gt  (prec p) a1 a2
    | true, O_ge -> I.filter_geq (prec p) a1 a2
    | false, O_le -> I.filter_leq_false (prec p) a1 a2
    | false, O_lt -> I.filter_lt_false  (prec p) a1 a2
    | false, O_ge -> I.filter_geq_false (prec p) a1 a2
    | false, O_gt -> I.filter_gt_false  (prec p) a1 a2
    | _ -> a1,a2

  let ask : type r. ('a,t) value_man -> ('a,r) query -> r option = fun man q ->
    match q with
    | Common.Q_float_interval e ->
      man.eval e |>
      OptionExt.return

    | _ -> None


end


let () =
  register_value_abstraction (module Value)
