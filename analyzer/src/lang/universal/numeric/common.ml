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

(** Common constructs for numeric abstractions. *)

open Mopsa
open Ast
open Zone

module I = ItvUtils.IntItv
module C = CongUtils.IntCong


(** {2 Integer intervals} *)
(** ********************* *)

(** Integer intervals *)
type int_itv = I.t_with_bot

type _ query +=
  | Q_int_interval : expr -> int_itv query (** Query to evaluate the integer interval of an expression *)
  | Q_fast_int_interval : expr -> int_itv query (** Query handled by non-relational domains only *)
  | VQ_to_int_interval : int_itv query (** Value query to cast an abstract value to integers *)

let mk_int_interval_query ?(fast=true) e =
  if fast then Q_fast_int_interval e else Q_int_interval e

let pp_int_interval fmt itv = I.fprint_bot fmt itv

let compare_int_interval itv1 itv2 = I.compare_bot itv1 itv2

let () =
  register_query {
    join = (
      let f : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_int_interval _ -> I.join_bot a b
          | Q_fast_int_interval _ -> I.join_bot a b
          | VQ_to_int_interval -> I.join_bot a b
          | _ -> next.join_query query a b
      in
      f
    );
    meet = (
      let f : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_int_interval e -> I.meet_bot a b
          | Q_fast_int_interval e -> I.meet_bot a b
          | VQ_to_int_interval -> I.meet_bot a b
          | _ -> next.meet_query query a b
      in
      f
    );
  }



(** {2 Integer intervals with congruence} *)
(** ************************************* *)

(** Integer step intervals *)
type int_congr_itv = int_itv * C.t


(** Query to evaluate the integer interval of an expression *)
type _ query += Q_int_congr_interval : expr -> int_congr_itv query


let () =
  register_query {
    join = (
      let f : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with

          | Q_int_congr_interval e ->
            let (i1,c1), (i2,c2) = a, b in
            (I.join_bot i1 i2, C.join c1 c2)

          | _ -> next.join_query query a b
      in
      f
    );
    meet = (
      let f : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with

          | Q_int_congr_interval e ->
            let (i1,c1), (i2,c2) = a, b in
            let i = I.meet_bot i1 i2 in
            let c = C.meet c1 c2 in
            Bot.bot_absorb2 C.meet_inter c i |>
            Bot.bot_dfl1
              (Bot.BOT, C.minf_inf)
              (fun (c,i) -> (Bot.Nb i, c))

          | _ -> next.meet_query query a b
      in
      f
    );
  }



(** {2 Float intervals} *)
(** ******************* *)


module F = ItvUtils.FloatItvNan


(** Float intervals *)
type float_itv = F.t

type _ query +=
  | Q_float_interval : expr -> float_itv query (** Query to evaluate the float interval of an expression, with infinities and NaN *)
  | VQ_to_float_interval : float_itv query (** Value query to cast an abstract value to a float interval *)

let mk_float_interval_query e =
  Q_float_interval e

let pp_float_interval fmt itv = F.fprint F.dfl_fmt fmt itv

let compare_float_interval itv1 itv2 = F.compare itv1 itv2

let () =
  register_query {
    join = (
      let f : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_float_interval _ -> F.join a b
          | VQ_to_float_interval -> F.join a b
          | _ -> next.join_query query a b
      in
      f
    );
    meet = (
      let f : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_float_interval e -> F.meet a b
          | VQ_to_float_interval -> F.meet a b
          | _ -> next.meet_query query a b
      in
      f
    );
  }



let prec : Ast.float_prec -> ItvUtils.FloatItvNan.prec = function
    | F_SINGLE -> `SINGLE
    | F_DOUBLE -> `DOUBLE
    | F_LONG_DOUBLE -> `LONG_DOUBLE
    | F_REAL -> `REAL

let round () : ItvUtils.FloatItvNan.round =
  match !Rounding.opt_float_rounding with
  | Apron.Texpr1.Near -> `NEAR
  | Apron.Texpr1.Zero -> `ZERO
  | Apron.Texpr1.Up -> `UP
  | Apron.Texpr1.Down -> `DOWN
  | Apron.Texpr1.Rnd -> `ANY



(** {2 Fast assume on numeric conditions} *)
(** ************************************* *)

(** Get the intervals of a numeric expression *)
let interval_of_num_expr e man flow : int_itv =
  match expr_to_z e with
  | Some n -> I.of_range_bot n n
  | None -> man.ask (mk_int_interval_query ~fast:true e) flow

(** Evaluate a numeric condition *)
let eval_num_cond cond man flow : bool option =
  match interval_of_num_expr cond man flow with
  | Bot.Nb(I.B.Finite a, I.B.Finite b) when Z.equal a b -> Some Z.(equal a one)
  | _ -> None
  

(** Optimized assume function that uses intervals to check a
    condition or falls back to classic assume *)
let assume_num cond ~fthen ~felse ?(zone=Z_u_num) man flow =
  begin
    if zone = Z_u_num then Eval.singleton cond flow
    else man.eval cond ~zone:(zone,Z_u_num) flow
  end >>$ fun cond flow ->
  match eval_num_cond cond man flow with
  | Some true  -> fthen flow
  | Some false -> felse flow
  | None       -> assume cond ~fthen ~felse ~zone:Z_u_num man flow
