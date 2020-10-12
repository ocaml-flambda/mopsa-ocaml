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

module I = ItvUtils.IntItv
module C = CongUtils.IntCong


(** {2 Integer intervals} *)
(** ********************* *)

(** Integer intervals *)
type int_itv = I.t_with_bot

type _ avalue_kind += V_int_interval : bool -> int_itv avalue_kind
(** Query to evaluate the integer interval of an expression *)

let mk_int_interval_query ?(fast=true) e = Q_avalue (e,V_int_interval fast)

let pp_int_interval fmt itv = I.fprint_bot fmt itv

let compare_int_interval itv1 itv2 = I.compare_bot itv1 itv2


(** {2 Integer intervals with congruence} *)
(** ************************************* *)

(** Integer step intervals *)
type int_congr_itv = int_itv * C.t_with_bot


(** Query to evaluate the integer interval of an expression *)
type _ avalue_kind += V_int_congr_interval : int_congr_itv avalue_kind

let mk_int_congr_interval_query e = Q_avalue (e,V_int_congr_interval)


(** {2 Float intervals} *)
(** ******************* *)


module F = ItvUtils.FloatItvNan


(** Float intervals *)
type float_itv = F.t

type _ avalue_kind += V_float_interval : float_prec -> float_itv avalue_kind (** Query to evaluate the float interval of an expression, with infinities and NaN *)

let mk_float_interval_query ?(prec=F_DOUBLE) e = Q_avalue (e,V_float_interval prec)

let pp_float_interval fmt itv = F.fprint F.dfl_fmt fmt itv

let compare_float_interval itv1 itv2 = F.compare itv1 itv2

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
let assume_num cond ~fthen ~felse ?(route=toplevel) man flow =
  man.eval cond flow >>$ fun cond flow ->
  match eval_num_cond cond man flow with
  | Some true  -> fthen flow
  | Some false -> felse flow
  | None       -> assume cond ~fthen ~felse man ~route flow


(** {2 Widening thresholds} *)
(** *********************** *)

open Framework.Combiners.Value.Nonrel

module K = GenContextKey
    (struct
      type 'a t = SetExt.ZSet.t
      let print pp fmt s =
        Format.fprintf fmt "widening thresholds: @[%a@]"
          (SetExt.ZSet.fprint
             SetExtSig.{
               print_empty = "∅";
               print_begin = "{";
               print_sep = ", ";
               print_end = "}";
             }
             Z.pp_print
          ) s
    end)

(** Key for accessing widening thresholds *)
let widening_thresholds_ctx_key = K.key

(** Add a constant to the widening thresholds of a variable *)
let add_widening_threshold var n ctx =
  let thresholds =
    try find_var_ctx var widening_thresholds_ctx_key ctx
    with Not_found -> SetExt.ZSet.empty
  in
  add_var_ctx var widening_thresholds_ctx_key
    (SetExt.ZSet.add n thresholds) ctx

(** Remove all widening thresholds of a variable *)
let remove_widening_thresholds var ctx =
  remove_var_ctx var widening_thresholds_ctx_key ctx


let () =
  register_avalue {
    typ = (fun (type a) next (avk:a avalue_kind) ->
        match avk with
        | V_int_interval _ -> T_int
        | V_int_congr_interval -> T_int
        | V_float_interval p -> T_float p
        | _ -> next.pool_typ avk
      );
    bottom = (
      let f : type a. avalue_pool -> a avalue_kind -> a =
        fun next avk ->
          match avk with
          | V_int_interval _ -> (Bot.BOT:int_itv)
          | V_int_congr_interval -> (Bot.BOT,Bot.BOT)
          | V_float_interval p -> F.bot
          | _ -> next.pool_bottom avk
      in f
    );
    top = (
      let f : type a. avalue_pool -> a avalue_kind -> a =
        fun next avk ->
          match avk with
          | V_int_interval _ -> Bot.Nb I.minf_inf
          | V_int_congr_interval -> (Bot.Nb I.minf_inf,Bot.Nb C.minf_inf)
          | V_float_interval p ->
            begin match p with
                | F_SINGLE -> F.single_special
                | F_DOUBLE -> F.double_special
                | F_LONG_DOUBLE -> F.long_double_special
                | F_REAL -> F.real
            end
          | _ -> next.pool_top avk
      in f
    );
    join = (
      let f : type a. avalue_pool -> a avalue_kind -> a -> a -> a =
        fun next avk av1 av2 ->
          match avk with
          | V_int_interval _ -> I.join_bot av1 av2
          | V_int_congr_interval -> I.join_bot (fst av1) (fst av2), C.join_bot (snd av1) (snd av2)
          | V_float_interval p -> F.join av1 av2
          | _ -> next.pool_join avk av1 av2
      in f
    );
    meet = (
      let f : type a. avalue_pool -> a avalue_kind -> a -> a -> a =
        fun next avk av1 av2 ->
          match avk with
          | V_int_interval _ -> I.meet_bot av1 av2
          | V_int_congr_interval -> I.meet_bot (fst av1) (fst av2), C.meet_bot (snd av1) (snd av2)
          | V_float_interval p -> F.meet av1 av2
          | _ -> next.pool_meet avk av1 av2
      in f
    );
    print = (
      let f : type a. avalue_pool -> a avalue_kind -> Format.formatter -> a -> unit =
        fun next avk fmt av ->
          match avk with
          | V_int_interval _ -> I.fprint_bot fmt av
          | V_int_congr_interval -> Format.fprintf fmt "%a:%a" I.fprint_bot (fst av) C.fprint_bot (snd av)
          | V_float_interval p -> F.fprint F.dfl_fmt fmt av
          | _ -> next.pool_print avk fmt av
      in f
    );
    compare = (
      let f : type a b. avalue_pool -> a avalue_kind -> a -> b avalue_kind -> b -> int =
        fun next avk1 av1 avk2 av2 ->
          match avk1,avk2 with
          | V_int_interval _, V_int_interval _ -> I.compare_bot av1 av2
          | V_int_congr_interval, V_int_congr_interval -> Compare.pair I.compare_bot C.compare_bot av1 av2
          | V_float_interval p1, V_float_interval p2 -> Compare.pair compare F.compare (p1,av1) (p2,av2)
          | _ -> next.pool_compare avk1 av1 avk2 av2
      in f
    )
  }
