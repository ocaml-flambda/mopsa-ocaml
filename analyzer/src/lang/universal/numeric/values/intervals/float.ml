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
open Core.Sig.Value.Lowlevel
open Rounding
open Ast
open Bot


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


  let zones = [Zone.Z_u_num; Zone.Z_u_float]

  let mem_type = function T_float _ -> true | _ -> false


  let () =
    import_standalone_option Rounding.name ~into:name


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

  let print fmt (a:t) = I.fprint I.dfl_fmt fmt a


  (** Arithmetic operators *)

  let prec : float_prec -> I.prec = function
    | F_SINGLE -> `SINGLE
    | F_DOUBLE -> `DOUBLE
    | F_LONG_DOUBLE -> `LONG_DOUBLE
    | F_REAL -> `REAL

  let round () : I.round =
    match !opt_float_rounding with
    | Apron.Texpr1.Near -> `NEAR
    | Apron.Texpr1.Zero -> `ZERO
    | Apron.Texpr1.Up -> `UP
    | Apron.Texpr1.Down -> `DOWN
    | Apron.Texpr1.Rnd -> `ANY

  let constant t c =
    match t, c with
    | T_float p, C_float i ->
       I.of_float_prec (prec p) (round ()) i i

    | T_float p, C_float_interval (lo,up) ->
       I.of_float_prec (prec p) (round ()) lo up

    | T_float p, C_int_interval (lo,up) ->
       I.of_z (prec p) (round ()) lo up

    | T_float p, C_int i ->
       I.of_z (prec p) (round ()) i i

    | T_float _, C_bool false ->
       I.zero

    | T_float _, C_bool true ->
      I.one

    | T_float p, C_top (T_float pp) when p = pp ->
      top_of_prec p

    | _ -> top

  let unop man t op v = lift_simplified_unop (fun op a ->
      match t with
      | T_float p ->
        (match op with
         | O_minus -> I.neg a
         | O_plus  -> a
         | O_sqrt  -> I.sqrt (prec p) (round ()) a
         | O_cast (T_int, T_float p)  ->
           let int_itv = man.ask (ValueQuery (v,Common.VQ_to_int_interval)) in
           I.of_int_itv_bot (prec p) (round ()) int_itv
         (* this seems to return top every time. Why don't we use
            I.round_int (prec p) (round ()) a ? *)
         | _ -> top_of_prec p)
      | _ -> top
    ) man t op v

  let binop man t op a1 a2 = lift_simplified_binop (fun op a1 a2 ->
      match t with
      | T_float p ->
        (match op with
         | O_plus  -> I.add (prec p) (round ()) a1 a2
         | O_minus -> I.sub (prec p) (round ()) a1 a2
         | O_mult  -> I.mul (prec p) (round ()) a1 a2
         | O_div   -> I.div (prec p) (round ()) a1 a2
         | O_mod   -> I.fmod (prec p) (round ()) a1 a2
         | _ -> top_of_prec p)
      | _ -> top
    ) man t op a1 a2

  let filter man a b = lift_simplified_filter (fun a b -> a) man a b

  let bwd_unop man t op a r = lift_simplified_bwd_unop (fun op a r ->
        match t with
        | T_float p ->
          (match op with
           | O_minus -> I.bwd_neg a r
           | O_plus  -> I.meet a r
           | O_sqrt  -> I.bwd_sqrt (prec p) (round ()) a r
           | _ -> a)
        |_ -> a
      ) man t op a r

  let bwd_binop man t op a1 a2 r = lift_simplified_bwd_binop (fun op a1 a2 r ->
      match t with
      | T_float p ->
        (match op with
         | O_plus  -> I.bwd_add (prec p) (round ()) a1 a2 r
         | O_minus -> I.bwd_sub (prec p) (round ()) a1 a2 r
         | O_mult  -> I.bwd_mul (prec p) (round ()) a1 a2 r
         | O_div   -> I.bwd_div (prec p) (round ()) a1 a2 r
         | O_mod   -> I.bwd_fmod (prec p) (round ()) a1 a2 r
         | _ -> a1,a2)
      | _ -> a1,a2
    ) man t op a1 a2 r

  let compare man t op a1 a2 r = lift_simplified_compare (fun op a1 a2 r ->
      match t with
      | T_float p ->
         (match r, op with
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
         | _ -> a1,a2)
      | _ -> a1,a2
    ) man t op a1 a2 r


  let ask : type r. ('a,t) man -> ('a,r) vquery -> r option =
    fun man q ->
    match q with
    | NormalQuery(Common.Q_float_interval e) ->
      man.eval e |>
      man.get |>
      Option.return

    | ValueQuery(a,Common.VQ_to_float_interval) ->
      man.get a |>
      Option.return

    | _ -> None


end


let () =
  Sig.Value.Lowlevel.register_value (module Value)
