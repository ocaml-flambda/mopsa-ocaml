(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Interval abstraction of float values. *)

open Framework.Essentials
open Framework.Value
open Ast
open Bot


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
  let name = "universal.numeric.values.float_intervals", "float_intervals"

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

  let round () : I.round = `ANY (* TODO *)
                      
  let of_constant = function
    | C_float i ->
       I.cst i

    | C_float_interval (lo,up) ->
       I.of_float lo up

    | _ -> top

  let unop op a =
    return (
      match op with
      | O_float_minus _ -> I.neg a
      | O_float_plus _ -> a
      | O_float_sqrt p -> I.sqrt (prec p) (round ()) a
      | _ -> top
    )    

  let binop op a1 a2 =
    return (
      match op with
      | O_float_plus p -> I.add (prec p) (round ()) a1 a2
      | O_float_minus p -> I.sub (prec p) (round ()) a1 a2
      | O_float_mult p -> I.mul (prec p) (round ()) a1 a2
      | O_float_div p -> I.div (prec p) (round ()) a1 a2
      | O_float_mod p -> I.fmod (prec p) (round ()) a1 a2
      | _ -> top
    )

  let filter a b =
    return a

  let bwd_unop op a r =
    return (
      match op with
      | O_float_minus _ -> I.bwd_neg a r
      | O_float_plus _ -> I.meet a r
      | O_float_sqrt p -> I.bwd_sqrt (prec p) (round ()) a r
      | _ -> a
    )    

  let bwd_binop op a1 a2 r =
    return (
      match op with
      | O_float_plus p -> I.bwd_add (prec p) (round ()) a1 a2 r
      | O_float_minus p -> I.bwd_sub (prec p) (round ()) a1 a2 r
      | O_float_mult p -> I.bwd_mul (prec p) (round ()) a1 a2 r
      | O_float_div p -> I.bwd_div (prec p) (round ()) a1 a2 r
      | O_float_mod p -> I.bwd_fmod (prec p) (round ()) a1 a2 r
      | _ -> a1,a2
    )

  let compare op a1 a2 =
    return (       
      match op with
      | O_float_eq p -> I.filter_eq  (prec p) a1 a2
      | O_float_ne p -> I.filter_neq (prec p) a1 a2
      | O_float_lt p -> I.filter_lt  (prec p) a1 a2
      | O_float_le p -> I.filter_leq (prec p) a1 a2
      | O_float_gt p -> I.filter_gt  (prec p) a1 a2
      | O_float_ge p -> I.filter_geq (prec p) a1 a2
      | O_float_neg_lt p -> I.filter_lt_false  (prec p) a1 a2
      | O_float_neg_le p -> I.filter_leq_false (prec p) a1 a2
      | O_float_neg_gt p -> I.filter_gt_false  (prec p) a1 a2
      | O_float_neg_ge p -> I.filter_geq_false (prec p) a1 a2
      | _ -> a1,a2
    )

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
