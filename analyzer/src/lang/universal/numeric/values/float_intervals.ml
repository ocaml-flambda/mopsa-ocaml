(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Interval abstraction of float values. *)

open Mopsa
open Framework.Value
open Rounding
open Ast
open Bot

let name = "universal.numeric.values.float_intervals"

let () =
  import_standalone_option Rounding.name ~into:name
    

module Value =
struct

  (** Types *)

  module I = ItvUtils.FloatItvNan
  module FI = ItvUtils.FloatItv
  module II = ItvUtils.IntItv

  type t = I.t

  type _ Framework.Query.query +=
  | Q_float_interval : Framework.Ast.expr -> t Framework.Query.query

  type _ value += V_float_interval : t value

  let id = V_float_interval
  let name = name, "float_intervals"

  let identify : type a. a value -> (t, a) eq option =
    function
    | V_float_interval -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:(fst @@ name) fmt

  let zone = Zone.Z_u_num

           
  (** Lattice operations *)

           
  let bottom = I.bot

  let top = I.double_special

  let is_bottom = I.is_bot

  let subset (a1:t) (a2:t) : bool = I.included a1 a2

  let join annot (a1:t) (a2:t) : t = I.join a1 a2

  let meet annot (a1:t) (a2:t) : t = I.meet a1 a2

  let widen annot (a1:t) (a2:t) : t = I.widen a1 a2

  let print fmt (a:t) = I.fprint I.dfl_fmt fmt a


  (** Arithmetic operators *)
                      
  let prec : float_prec -> I.prec = function
    | F_SINGLE -> `SINGLE
    | F_DOUBLE -> `DOUBLE
    | F_LONG_DOUBLE -> panic "unhandled `DOUBLE intervals"
    | F_REAL -> panic "unhandled `REAL intervals"

  let round () : I.round =
    match !opt_float_rounding with
    | Apron.Texpr1.Near -> `NEAR
    | Apron.Texpr1.Zero -> `ZERO
    | Apron.Texpr1.Up -> `UP
    | Apron.Texpr1.Down -> `DOWN
    | Apron.Texpr1.Rnd -> `ANY
                      
  let of_constant t c =
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

    | _ -> top

  let unop t op a =
    return (
        match t with
        | T_float p ->
           (match op with
            | O_minus -> I.neg a
            | O_plus  -> a
            | O_sqrt  -> I.sqrt (prec p) (round ()) a
            | O_cast  -> I.round (prec p) (round ()) a
            | _ -> top)
        | _ -> top)
    
  let binop t op a1 a2 =
    return (
        match t with
        | T_float p ->
           (match op with
            | O_plus  -> I.add (prec p) (round ()) a1 a2
            | O_minus -> I.sub (prec p) (round ()) a1 a2
            | O_mult  -> I.mul (prec p) (round ()) a1 a2
            | O_div   -> I.div (prec p) (round ()) a1 a2
            | O_mod   -> I.fmod (prec p) (round ()) a1 a2
            | _ -> top)
        | _ -> top)
    
  let filter _ a b =
    return a

  let bwd_unop t op a r =
    return (
         match t with
        | T_float p ->
           (match op with
            | O_minus -> I.bwd_neg a r
            | O_plus  -> I.meet a r
            | O_sqrt  -> I.bwd_sqrt (prec p) (round ()) a r
            | _ -> a)
        |_ -> a)

  let bwd_binop t op a1 a2 r =
    return (
        match t with
        | T_float p ->
           (match op with
            | O_plus  -> I.bwd_add (prec p) (round ()) a1 a2 r
            | O_minus -> I.bwd_sub (prec p) (round ()) a1 a2 r
            | O_mult  -> I.bwd_mul (prec p) (round ()) a1 a2 r
            | O_div   -> I.bwd_div (prec p) (round ()) a1 a2 r
            | O_mod   -> I.bwd_fmod (prec p) (round ()) a1 a2 r
            | _ -> a1,a2)
        | _ -> a1,a2)

  let compare t op a1 a2 r =
    return (       
        match t with
        | T_float p ->
           (match r, op with
            | true, O_eq | false, O_ne -> I.filter_eq  (prec p) a1 a2
            | true, O_ne | false, O_eq -> I.filter_neq (prec p) a1 a2
            | true, O_lt -> I.filter_lt  (prec p) a1 a2
            | true, O_le -> I.filter_leq (prec p) a1 a2
            | true, O_gt -> I.filter_gt  (prec p) a1 a2
            | true, O_ge -> I.filter_geq (prec p) a1 a2
            | false, O_lt -> I.filter_lt_false  (prec p) a1 a2
            | false, O_le -> I.filter_leq_false (prec p) a1 a2
            | false, O_gt -> I.filter_gt_false  (prec p) a1 a2
            | false, O_ge -> I.filter_geq_false (prec p) a1 a2
            | _ -> a1,a2)
        | _ -> a1,a2)
    
  let ask : type r. r Framework.Query.query -> (expr -> t) -> r option =
    fun query eval ->
      match query with
      | Q_float_interval e -> Some (eval e)
      | _ -> None


  (** Casts *)

  type float_t = t
  type int_t = Intervals.Value.t
           
  let int_of_float (i:float_t) : int_t with_channel =
    return (I.to_int_itv i)

  let float_of_int (p:float_prec) (i:int_t) : float_t with_channel =
    return (I.of_int_itv_bot (prec p) (round ()) i)
                                      
end


let () =
  register_value (module Value)
