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

type _ avalue_kind += V_int_interval : int_itv avalue_kind
(** Query to evaluate the integer interval of an expression *)

type _ avalue_kind += V_int_interval_fast : int_itv avalue_kind
(** Same as [V_int_interval] but should be handled by optimized domains, such Boxes *)

let mk_int_interval_query ?(fast=true) e =
  if fast then Q_avalue (e,V_int_interval_fast)
  else Q_avalue (e,V_int_interval)

let pp_int_interval fmt itv = I.fprint_bot fmt itv

let compare_int_interval itv1 itv2 = I.compare_bot itv1 itv2

(** Creates var \in itv constraint *)
let constraints_of_itv var (itv : int_itv) range : expr =
  match itv with
  | Nb (ItvUtils.IntBound.Finite lo, ItvUtils.IntBound.Finite hi) ->
    mk_in var (mk_z lo range) (mk_z hi range) range
  | Nb (ItvUtils.IntBound.MINF, ItvUtils.IntBound.Finite hi) -> mk_le var (mk_z hi range) range
  | Nb (ItvUtils.IntBound.Finite lo, ItvUtils.IntBound.PINF) -> mk_ge var (mk_z lo range) range
  | Nb _ -> mk_true range
  | BOT -> assert false


(** {2 Integer intervals with congruence} *)
(** ************************************* *)

(** Integer step intervals *)
type int_congr_itv = int_itv * C.t_with_bot


(** Query to evaluate the integer interval of an expression *)
type _ avalue_kind += V_int_congr_interval : int_congr_itv avalue_kind

let mk_int_congr_interval_query e = Q_avalue (e,V_int_congr_interval)



(** {2 Rounding mode of floats} *)
(** *************************** *)

let opt_float_rounding = ref Apron.Texpr1.Near

let rounding_option_name = "universal.numeric.rounding"

let () =
  register_shared_option rounding_option_name {
    key = "-float-rounding-mode";
    category = "Numeric";
    spec = Symbol (
        ["near"; "zero"; "up"; "down"; "rnd"],
        (function
          | "near" -> opt_float_rounding := Apron.Texpr1.Near
          | "zero" -> opt_float_rounding := Apron.Texpr1.Zero
          | "up"   -> opt_float_rounding := Apron.Texpr1.Up
          | "down" -> opt_float_rounding := Apron.Texpr1.Down
          | "rnd"  -> opt_float_rounding := Apron.Texpr1.Rnd
          | x -> Exceptions.panic "unknown rounding mode %s" x
        )
      );
    doc = "rounding mode of floating-point computations.";
    default = "near";
  }


(** {2 Float intervals} *)
(** ******************* *)

module F = ItvUtils.FloatItvNan


(** Float intervals *)
type float_itv = F.t

type _ avalue_kind +=
  | V_float_interval : float_prec -> float_itv avalue_kind (** Query to evaluate the float interval of an expression, with infinities and NaN *)
  | V_float_maybenan : float_prec -> bool avalue_kind

let mk_float_interval_query ?(prec=F_DOUBLE) e = Q_avalue (e,V_float_interval prec)
let mk_float_maybenan_query ?(prec=F_DOUBLE) e = Q_avalue (e,V_float_maybenan prec)

let pp_float_interval fmt itv = F.fprint F.dfl_fmt fmt itv

let compare_float_interval itv1 itv2 = F.compare itv1 itv2

let prec : Ast.float_prec -> ItvUtils.FloatItvNan.prec = function
    | F_SINGLE -> `SINGLE
    | F_DOUBLE -> `DOUBLE
    | F_LONG_DOUBLE -> `EXTRA
    | F_FLOAT128 -> `EXTRA
    | F_REAL -> `REAL

let round () : ItvUtils.FloatItvNan.round =
  match !opt_float_rounding with
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
  | None -> ask_and_reduce man.ask (mk_int_interval_query ~fast:true e) flow

(** Evaluate a numeric condition using intervals *)
let eval_num_cond cond man flow : bool option =
  (* Skip expressions that contain non-universal sub-expressions or statements *)
  if exists_expr (fun e -> not (is_universal_type e.etyp)) (fun s -> true) cond then
    None
  else
    (* Evaluate the interval of the condition *)
    match interval_of_num_expr cond man flow with
    | Bot.Nb itv ->
      begin match I.contains_zero itv, I.contains_nonzero itv with
        | true, false -> Some false
        | false, true -> Some true
        | _           -> None
      end
    | _ -> None


(** Optimized assume function that uses intervals to check a
    condition or falls back to classic assume *)
let assume_num cond ~fthen ~felse ?(route=toplevel) man flow =
  let r1 =
    (* Use [eval_num_cond] directly without evaluation if the expression
       contains only pure universal sub-expressions (no statement) *)
    if for_all_expr (fun e -> is_universal_type e.etyp) (fun s -> false) cond then
      match eval_num_cond cond man flow with
      | Some true  -> Some (fthen flow)
      | Some false -> Some (felse flow)
      | None       -> None
    else
      None
  in
  match r1 with
  | Some r -> r
  | None ->
    (* Evaluate the expression if it is not a pure universal expression, or when
       [eval_num_cond] failed *)
    man.eval cond flow ~translate:"Universal" >>$ fun ncond flow ->
    match eval_num_cond ncond man flow with
    | Some true  -> fthen flow
    | Some false -> felse flow
    | None       -> assume ncond ~fthen ~felse ~eval:false man flow


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
        | V_int_interval -> T_int
        | V_int_interval_fast -> T_int
        | V_int_congr_interval -> T_int
        | V_float_interval p -> T_float p
        | _ -> next.pool_typ avk
      );
    bottom = (
      let f : type a. avalue_pool -> a avalue_kind -> a =
        fun next avk ->
          match avk with
          | V_int_interval -> (Bot.BOT:int_itv)
          | V_int_interval_fast -> (Bot.BOT:int_itv)
          | V_int_congr_interval -> (Bot.BOT,Bot.BOT)
          | V_float_interval p -> F.bot
          | _ -> next.pool_bottom avk
      in f
    );
    top = (
      let f : type a. avalue_pool -> a avalue_kind -> a =
        fun next avk ->
          match avk with
          | V_int_interval -> Bot.Nb I.minf_inf
          | V_int_interval_fast -> Bot.Nb I.minf_inf
          | V_int_congr_interval -> (Bot.Nb I.minf_inf,Bot.Nb C.minf_inf)
          | V_float_interval p ->
            begin match p with
                | F_SINGLE -> F.single_special
                | F_DOUBLE -> F.double_special
                | F_LONG_DOUBLE -> F.extra
                | F_FLOAT128 -> F.extra
                | F_REAL -> F.real
            end
          | _ -> next.pool_top avk
      in f
    );
    join = (
      let f : type a. avalue_pool -> a avalue_kind -> a -> a -> a =
        fun next avk av1 av2 ->
          match avk with
          | V_int_interval -> I.join_bot av1 av2
          | V_int_interval_fast -> I.join_bot av1 av2
          | V_int_congr_interval -> I.join_bot (fst av1) (fst av2), C.join_bot (snd av1) (snd av2)
          | V_float_interval p -> F.join av1 av2
          | _ -> next.pool_join avk av1 av2
      in f
    );
    meet = (
      let f : type a. avalue_pool -> a avalue_kind -> a -> a -> a =
        fun next avk av1 av2 ->
          match avk with
          | V_int_interval -> I.meet_bot av1 av2
          | V_int_interval_fast -> I.meet_bot av1 av2
          | V_int_congr_interval -> I.meet_bot (fst av1) (fst av2), C.meet_bot (snd av1) (snd av2)
          | V_float_interval p -> F.meet av1 av2
          | _ -> next.pool_meet avk av1 av2
      in f
    );
    print = (
      let f : type a. avalue_pool -> a avalue_kind -> Format.formatter -> a -> unit =
        fun next avk fmt av ->
          match avk with
          | V_int_interval -> I.fprint_bot fmt av
          | V_int_interval_fast -> I.fprint_bot fmt av
          | V_int_congr_interval -> Format.fprintf fmt "%a:%a" I.fprint_bot (fst av) C.fprint_bot (snd av)
          | V_float_interval p -> F.fprint F.dfl_fmt fmt av
          | _ -> next.pool_print avk fmt av
      in f
    );
    compare = (
      let f : type a b. avalue_pool -> a avalue_kind -> a -> b avalue_kind -> b -> int =
        fun next avk1 av1 avk2 av2 ->
          match avk1,avk2 with
          | V_int_interval, V_int_interval -> I.compare_bot av1 av2
          | V_int_interval_fast, V_int_interval_fast -> I.compare_bot av1 av2
          | V_int_congr_interval, V_int_congr_interval -> Compare.pair I.compare_bot C.compare_bot av1 av2
          | V_float_interval p1, V_float_interval p2 -> Compare.pair compare F.compare (p1,av1) (p2,av2)
          | _ -> next.pool_compare avk1 av1 avk2 av2
      in f
    )
  }
