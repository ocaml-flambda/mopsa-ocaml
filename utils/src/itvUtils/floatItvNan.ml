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

(**
  FloatItvNan - Floating-point intervals with special IEEE numbers.

  Adds special IEEE number managenement (NaN, infinities) to FloatItv.
 *)

open Bot
module F = Float
module FI = FloatItv
module B = IntBound
module II = IntItv


(** {2 Types} *)

       
type t =
  { itv:  FI.t with_bot; (** Interval of non-special values. Bounds cannot be NaN nor infinities. *)
    nan:  bool; (** Whether to include NaN. *)
    pinf: bool; (** Whether to include +∞. *)
    minf: bool; (** Whether to include -∞. *)
  }
(** A set of IEEE floating-point values.
    Represented as a set of non-special values, and boolean flags to
    indicate the presence of each special IEEE value.
    The value 0 in the interval represents both IEEE +0 and -0.
    Note: the type can naturally represent the empty set.
 *)

let is_valid (a:t) : bool =
  match a.itv with
  | BOT -> true
  | Nb i -> F.is_finite i.FI.lo && F.is_finite i.FI.up && i.FI.lo <= i.FI.up
(** All elements of type t whould satisfy this predicate. *)  

          
type prec =
  [ `SINGLE (** 32-bit single precision *)
  | `DOUBLE (** 64-bit double precision *)
  ]
(** Precision. *)
                   
type round =
  [ `NEAR  (** To nearest *)
  | `UP    (** Upwards *)
  | `DOWN  (** Downwards *)
  | `ZERO  (** Towards 0 *)
  | `ANY   (** Any rounding mode *)
  ]
(** Rounding direction. *)


  
(** {2 Constructors} *)


let bot = { itv = BOT; nan = false; pinf = false; minf = false; }
(** Empty float set. *)

let pinf : t = { bot with pinf = true; }
let minf : t = { bot with minf = true; }
let nan : t = { bot with nan = true; }
let infinities : t = { itv = BOT; nan = false; pinf = true; minf = true; }
let specials : t = { itv = BOT; nan = true; pinf = true; minf = true; }
(** Special values. *)

let of_float (lo:float) (up:float) : t =
  if lo > up then bot
  else if not (F.is_finite lo) || not (F.is_finite up)
  then invalid_arg (Printf.sprintf "FloatItvNan.of_float: invalid bound [%g,%g]" lo up)
  else  { bot with itv = Nb { FI.lo; FI.up; }; }
(** Float set reduced to an interval of non-special values.
    lo and up should not be infinity nor NaN.
 *)

let of_interval (a:FI.t) : t =
  of_float a.FI.lo a.FI.up

let of_interval_bot (a:FI.t_with_bot) : t =
  match a with BOT -> bot | Nb aa -> of_interval aa
  
let hull (a:float) (b:float) : t =
  of_float (min a b) (max a b)
(** Constructs the smallest interval containing a and b. *)

let cst (x:float) : t =
  match classify_float x with
  | FP_nan -> nan
  | FP_infinite -> if x > 0. then pinf else minf
  | _ -> { bot with itv = Nb { FI.lo = x; FI.up = x; }; }
(** Singleton (possibly infinity or NaN). *)

let zero : t = cst 0.
let one : t = cst 1.
let two : t = cst 2.
let mone : t = cst (-1.)
let zero_one : t  = of_float 0. 1.
let mone_zero : t = of_float (-1.) 0.
let mone_one : t  = of_float (-1.) 1.
let mhalf_half : t = of_float (-0.5) 0.5
(** Useful intervals. *)

                 
let add_special (x:t) : t =
  { x with nan = true; pinf = true; minf = true; }
(** Adds NaN and infinities to a set. *)  

let remove_special (x:t) : t =
  { x with nan = false; pinf = false; minf = false; }
(** Removes NaN and infinities from a set. *)  
  
let single : t =
  of_float (-. F.Single.max_normal) F.Single.max_normal
(** Non-special single precision floats. *)  

let double : t =
  of_float (-. F.Double.max_normal) F.Double.max_normal
(** Non-special double precision floats. *)  

let single_special : t =
  add_special single
(** Single precision floats with specials. *)

let double_special : t =
  add_special double
(** Double precision floats with specials. *)



(** {2 Set-theoretic} *)


let equal (a:t) (b:t) : bool =
  a = b
(** Set equality. = also works. *)
  
let included (a:t) (b:t) : bool =
  (match a.itv, b.itv with
   | BOT, _ -> true
   | _, BOT -> false
   | Nb aa, Nb bb -> aa.FI.lo >= bb.FI.lo && aa.FI.up <= bb.FI.up
  ) &&
  (b.nan  || not a.nan)  &&
  (b.minf || not a.minf) &&
  (b.pinf || not a.pinf)
(** Set inclusion. *)

let intersect_finite (a:t) (b:t) : bool =
  (match a.itv, b.itv with
   | BOT, _ | _, BOT -> false
   | Nb aa, Nb bb -> aa.FI.lo <= bb.FI.up && aa.FI.up >= bb.FI.lo
  )
(** Whether the finite parts of the sets have a non-empty intersection. *)  

let intersect (a:t) (b:t) : bool =
  (intersect_finite a b) ||
  (a.nan  && b.nan)  ||
  (a.minf && b.minf) ||
  (a.pinf && b.pinf)
(** Whether the sets have an non-empty intersection. *)  

let contains (x:float) (a:t) =
  match classify_float x with
  | FP_nan -> a.nan
  | FP_infinite -> (x > 0. && a.pinf) || (x < 0. && a.minf)
  | _ ->
     match a.itv with
     | BOT -> false
     | Nb aa -> aa.lo <= x && aa.up >= x
(** Whether the set contains a certain (finite, infinite or NaN) value. *)
                                              
let compare (a:t) (b:t) : int =
  Compare.compose
    [(fun () -> FI.compare_bot a.itv b.itv);
     (fun () -> Pervasives.compare a.nan  b.nan);
     (fun () -> Pervasives.compare a.minf b.minf);
     (fun () -> Pervasives.compare a.pinf b.pinf);
    ]
(** A total ordering returning -1, 0, or 1. *)
  
let is_bot (a:t) : bool =
  (a.itv = BOT) && (not a.nan) && (not a.pinf) && (not a.minf)
(** Whether the argument is the empty set. *)
  
let is_finite (a:t) : bool =
  (not a.nan) && (not a.pinf) && (not a.minf)
(** Whether the argument contains only finite values (or is empty). *)

let is_infinity (a:t) : bool =
  (not a.nan) && (a.itv = BOT)
(** Whether the argument contains only infinities (or is empty). *)
  
let is_special (a:t) : bool =
  a.itv = BOT
(** Whether the argument contains only special values (or is empty). *)

let is_zero (a:t) : bool =
  a = zero
(** Whether the argument is the singleton 0. *)

let is_null (a:t) : bool =
  a = zero || a = bot
(** Whether the argument contains only 0 (or is empty). *)

let is_positive (a:t) : bool =
  (not a.nan) && (not a.minf) &&
  (match a.itv with BOT -> true | Nb x -> x.FI.lo >= 0.)
(** Whether the argument contains only (possibly infinite) positive non-NaN values (or is empty). *)

let is_negative (a:t) : bool =
  (not a.nan) && (not a.pinf) &&
  (match a.itv with BOT -> true | Nb x -> x.FI.up <= 0.)
(** Whether the argument contains only (possibly infinite) negative non-NaN values (or is empty). *)

let is_positive_strict (a:t) : bool =
  (not a.nan) && (not a.minf) &&
  (match a.itv with BOT -> true | Nb x -> x.FI.lo > 0.)
(** Whether the argument contains only (possibly infinite) strictly positive non-NaN values (or is empty). *)

let is_negative_strict (a:t) : bool =
  (not a.nan) && (not a.pinf) &&
  (match a.itv with BOT -> true | Nb x -> x.FI.up < 0.)
(** Whether the argument contains only (possibly infinite) strictly negative non-NaN values (or is empty). *)

let is_nonzero (a:t) : bool =
  (match a.itv with BOT -> true | Nb x -> x.FI.lo > 0. || x.FI.up < 0.)
(** Whether the argument contains only (possibly infinite or NaN) non-zero values (or is empty). *)

let approx_size (a:t) : int =
  (if a.nan then 1 else 0)
  + (if a.pinf then 1 else 0)
  + (if a.minf then 1 else 0)
  + (match a.itv with
     | BOT -> 0
     | Nb x -> if x.FI.lo = x.FI.up then 1 else 2
    )
(* approximate size: 0 is empty, 1 is singleton, > 1 is non-singleton. *)
  
let is_singleton (a:t) : bool =
  approx_size a == 1
(** Whether the argument contains only a single element. *)
  
let contains_finite (a:t) : bool =
  a.itv <> BOT
(** Whether the argument contains at least one finite value. *)

let contains_infinity (a:t) : bool =
  a.pinf || a.minf
(** Whether the argument contains an infinity. *)
  
let contains_special (a:t) : bool =
  a.nan || a.pinf || a.minf
(** Whether the argument contains an infinity or NaN. *)  

let contains_zero (a:t) : bool =
  match a.itv with Nb x -> x.FI.lo <= 0. && x.FI.up >= 0. | BOT -> false
(** Whether the argument contains 0. *)
                                                                 
let contains_nonzero (a:t) : bool =
  a.nan || a.pinf || a.minf ||
  (match a.itv with BOT -> false | Nb x -> x.FI.lo <> 0. || x.FI.up <> 0.)
(** Whether the argument contains a (possibly NaN or infinite) non-0 value. *)

let contains_positive (a:t) : bool =
  a.pinf || (match a.itv with BOT -> false | Nb x -> x.FI.up >= 0.)
(** Whether the argument contains a (possibly infinite) positive value. *)

let contains_negative (a:t) : bool =
  a.minf || (match a.itv with BOT -> false | Nb x -> x.FI.lo <= 0.)
(** Whether the argument contains a (possibly infinite) negative value. *)

let contains_positive_strict (a:t) : bool =
  a.pinf || (match a.itv with BOT -> false | Nb x -> x.FI.up > 0.)
(** Whether the argument contains a (possibly infinite) strictly positive value. *)

let contains_negative_strict (a:t) : bool =
  a.minf || (match a.itv with BOT -> false | Nb x -> x.FI.lo < 0.)
(** Whether the argument contains a (possibly infinite) strictly negative value. *)

let contains_non_nan (a:t) : bool =
  a.minf || a.pinf || a.itv <> BOT
(** Whether the argument contains a non-NaN value. *)  
  
let is_in_range (a:t) (lo:float) (up:float) =
  (not a.nan) && (not a.pinf) && (not a.minf) &&
  (match a.itv with BOT -> true | Nb x -> x.FI.lo >= lo || x.FI.up <= up)
(** Whether the argument contains only finite values, and they are included in the range [lo,up]. *)

let join (a:t) (b:t) =
  { itv = bot_neutral2 FI.join a.itv b.itv;
    nan = a.nan || b.nan;
    minf = a.minf || b.minf;
    pinf = a.pinf || b.pinf;
  }
  
let join_list : t list -> t  =
  List.fold_left join bot

let meet (a:t) (b:t) =
  { itv = bot_absorb2 FI.meet a.itv b.itv;
    nan = a.nan && b.nan;
    minf = a.minf && b.minf;
    pinf = a.pinf && b.pinf;
  }

let widen (a:t) (b:t) =
  { itv = bot_neutral2 FI.widen a.itv b.itv;
    nan = a.nan || b.nan;
    minf = a.minf || b.minf;
    pinf = a.pinf || b.pinf;
  }
  
let positive (x:t) : t =
  { itv = bot_absorb1 FI.positive x.itv;
    nan = false;
    pinf = x.pinf;
    minf = false;
  }
(** Positive part of the argument, excluding NaN. *)

let negative (x:t) : t =
  { itv = bot_absorb1 FI.negative x.itv;
    nan = false;
    minf = x.minf;
    pinf = false;
  }
(** Negative part of the argument, excluding NaN. *)

let meet_zero (a:t) : t =
  meet a zero
(** Intersects with {0} (excluding infinities and NaN). *)


(** {2 Printing} *)

       
type print_format = FI.print_format
let dfl_fmt = FI.dfl_fmt
  
let to_string (fmt:print_format) (x:t) : string =
  let app x y = if x = "" then y else x ^ "∨" ^ y in
  let r = (match x.itv with Nb i -> FI.to_string fmt i | BOT -> "") in
  let r = if x.pinf then app r "+∞"  else r in
  let r = if x.minf then app r "-∞"  else r in
  let r = if x.nan  then app r "NaN" else r in
  if r = "" then bot_string else r
                       
let print fmt ch (x:t) = output_string ch (to_string fmt x)
let fprint fmt ch (x:t) = Format.pp_print_string ch (to_string fmt x)
let bprint fmt ch (x:t) = Buffer.add_string ch (to_string fmt x)


                   
(** {2 C predicates} *)
  
  
let is_log_eq (a:t) (b:t) : bool =
  (intersect_finite a b) || (a.pinf && b.pinf) || (a.minf && b.minf)

let is_log_leq (a:t) (b:t) : bool =
  (match a.itv, b.itv with Nb x, Nb y -> x.FI.lo <= y.FI.up | _ -> false) ||
  (a.minf && contains_non_nan b) ||
  (b.pinf && contains_non_nan a)
  
let is_log_lt (a:t) (b:t) : bool =
  (match a.itv, b.itv with Nb x, Nb y -> x.FI.lo < y.FI.up | _ -> false) ||
  (a.minf && (b.pinf || b.itv <> BOT)) ||
  (b.pinf && (a.minf || a.itv <> BOT))
  
let is_log_geq (a:t) (b:t) : bool =
  is_log_leq b a

let is_log_gt (a:t) (b:t) : bool =
  is_log_lt b a

let is_log_neq (a:t) (b:t) : bool =
  match approx_size a, approx_size b with
  | 0,_ | _,0 -> false
  | 1,1 -> a.nan || b.nan || not (equal a b)
  | _ -> true

(** C comparison tests. 
    Returns true if the test may succeed, false if it cannot.
    Note that NaN always compares different to all values (including NaN).
 *)


let is_log_leq_false (a:t) (b:t) : bool =
  is_log_gt a b || (a.nan && not (is_bot b)) || (b.nan && not (is_bot a))

let is_log_lt_false (a:t) (b:t) : bool =
  is_log_geq a b || (a.nan && not (is_bot b)) || (b.nan && not (is_bot a))

let is_log_geq_false (a:t) (b:t) : bool =
  is_log_leq_false b a

let is_log_gt_false (a:t) (b:t) : bool =
  is_log_lt_false b a

let is_log_eq_false  = is_log_neq
let is_log_neq_false = is_log_eq
   
(** Returns true if the test may fail, false if it cannot.
    Due to NaN, which compare always different, <= (resp. >) do not
    return the boolean negation of > (resp. <).
    However, == is the negation of != even for NaN.
 *)


       
(** {2 Forward arithmetic} *)


let fix_itv (prec:prec) (x:t) : t =
  let m = F.max_normal prec in
  match x.itv with
  | BOT -> x
  | Nb i ->
     let lo,minf,nan1 =
       if F.is_nan i.FI.lo then -. m, false, true
       else if i.FI.lo < -. m then -. m, true, false
       else i.FI.lo, false, false
     and up,pinf,nan2 =
       if F.is_nan i.FI.up then m, false, true
       else if i.FI.up > m then m, true, false
       else i.FI.up, false, false
     in
     { itv = FI.of_float_bot lo up;
       nan = x.nan || nan1 || nan2;
       minf = x.minf || minf;
       pinf = x.pinf || pinf;
     }
(* Utility to fix interval bounds after an operation.
   NaN, infinities and overflowing bounds are reset to maximal
   bounds according to the precision, and the nan, minf, and pinf fields
   are updated.
 *)


let neg (x:t) : t =
  { itv = bot_lift1 FI.neg x.itv;
    nan = x.nan;
    pinf = x.minf;
    minf = x.pinf;
  }
(** Negation. *)

  
let abs (x:t) : t =
  { itv = bot_lift1 FI.abs x.itv;
    nan = x.nan;
    pinf = x.pinf || x.minf;
    minf = false;
  }
(** Absolute value. *)
  
     
let add (prec:prec) (round:round) (x:t) (y:t) =
  fix_itv
    prec
    { itv = bot_lift2 (FI.add (prec:>FI.prec) round) x.itv y.itv;
      nan = x.nan || y.nan || (x.pinf && y.minf) || (x.minf && y.pinf);
      pinf = (x.pinf && y.pinf) || (x.pinf && contains_finite y) || (y.pinf && contains_finite x);
      minf = (x.minf && y.minf) || (x.minf && contains_finite y) || (y.minf && contains_finite x);
    }
(** Addition. *)  
     
let sub (prec:prec) (round:round) (x:t) (y:t) =
  fix_itv
    prec
    { itv = bot_lift2 (FI.sub (prec:>FI.prec) round) x.itv y.itv;
      nan = x.nan || y.nan || (x.pinf && y.pinf) || (x.minf && y.minf);
      pinf = (x.pinf && y.minf) || (x.pinf && contains_finite y) || (y.minf && contains_finite x);
      minf = (x.minf && y.pinf) || (x.minf && contains_finite y) || (y.pinf && contains_finite x);
    }
(** Subtraction. *)  

let mul (prec:prec) (round:round) (x:t) (y:t) =
  (* signs of x and y *)
  let xm, xz, xp, xi = contains_negative_strict x, contains_zero x,
                       contains_positive_strict x, contains_infinity x
  and ym, yz, yp, yi = contains_negative_strict y, contains_zero y,
                       contains_positive_strict y, contains_infinity y
  in
  fix_itv
    prec
    { itv = bot_lift2 (FI.mul (prec:>FI.prec) round) x.itv y.itv;
      nan = x.nan || y.nan || (xz && yi) || (xi && yz);
      pinf = (x.pinf && yp) || (x.minf && ym) || (y.pinf && xp) || (y.minf && xm);
      minf = (x.pinf && ym) || (x.minf && yp) || (y.pinf && xm) || (y.minf && xp);
    }
(** Multiplication. *)  
  
let div (prec:prec) (round:round) (x:t) (y:t) =
  (* signs of x and y *)
  let xm, xz, xp, xi = contains_negative_strict x, contains_zero x,
                       contains_positive_strict x, contains_infinity x
  and ym, yz, yp, yi = contains_negative y, contains_zero y,
                       contains_positive y, contains_infinity y
  in
  let r = 
    fix_itv
      prec
      { itv = bot_absorb2 (FI.div (prec:>FI.prec) round) x.itv y.itv;
        nan = x.nan || y.nan || (xi && yi) || (xz && yz);
        pinf = yz || (x.pinf && yp) || (x.minf && ym);
        minf = yz || (x.pinf && ym) || (x.minf && yp);
      }
  in
  (* add zero if dividing by infinity *)
  if yi then join zero r else r
(** Division. *)  
  

let fmod (prec:prec) (round:round) (x:t) (y:t) : t =
  fix_itv
    prec
    { itv = bot_absorb2 FI.fmod x.itv y.itv;
      nan = (contains_special x) || y.nan || (contains_zero y);
      minf = false;
      pinf = false;
    }
(** Remainder (modulo). *)
  

let square (prec:prec) (round:round) (x:t) : t =
  fix_itv
    prec
    { itv = bot_lift1 (FI.square (prec:>FI.prec) round) x.itv;
      nan = x.nan;
      pinf = x.pinf || x.minf;
      minf = false;
    }
(** Square. *)

  
let sqrt (prec:prec) (round:round) (x:t) : t =
  fix_itv
    prec
    { itv = bot_absorb1 (FI.sqrt (prec:>FI.prec) round) x.itv;
      nan = x.nan || (contains_negative_strict x);
      pinf = x.pinf;
      minf = false;
    }
(** Square root. *)

  
let round_int (prec:prec) (round:round) (x:t) : t =
  fix_itv prec { x with itv = bot_lift1 (FI.round_int (prec:>FI.prec) round) x.itv; }
(** Round to integer. *)
  
let round (prec:prec) (round:round) (x:t) : t =
  fix_itv prec { x with itv = bot_lift1 (FI.round (prec:>FI.prec) round) x.itv; }
(** Round to float. *)

let of_int (prec:prec) (round:round) (x:int) (y:int) : t =
  fix_itv prec { bot with itv = Nb (FI.of_int (prec:>FI.prec) round x y); }
(** Conversion from integer range. *)

let of_int64 (prec:prec) (round:round) (x:int64) (y:int64) : t =
  fix_itv prec { bot with itv = Nb (FI.of_int64 (prec:>FI.prec) round x y); }
(** Conversion from int64 range. *)

let of_z (prec:prec) (round:round) (x:Z.t) (y:Z.t) : t =
  fix_itv prec { bot with itv = Nb (FI.of_z (prec:>FI.prec) round x y); }
(** Conversion from integer range. *)

let to_z (x:t) : (Z.t * Z.t) with_bot =
  bot_lift1 FI.to_z x.itv
(** Conversion to integer range with truncation. NaN and infinities are discarded. *)

let of_float_prec (prec:prec) (round:round) (lo:float) (up:float) : t =
  let r = FI.of_float_bot lo up in  
  fix_itv prec { bot with itv = bot_lift1 (FI.round (prec:>FI.prec) round) r; }
(** From bounds, with rounding, precision and handling of specials. *)  
     
let of_int_itv (prec:prec) (round:round) ((lo,up):II.t) : t =
  let lo = match lo, round with
    | B.Finite l, `NEAR -> F.of_z prec `NEAR l
    | B.Finite l, (`DOWN | `ANY) -> F.of_z prec `DOWN l
    | B.Finite l, `UP   -> F.of_z prec `UP l
    | B.Finite l, `ZERO -> F.of_z prec `ZERO l
    | _ -> neg_infinity
  and up = match up, round with
    | B.Finite l, `NEAR -> F.of_z prec `NEAR l
    | B.Finite l, `DOWN -> F.of_z prec `DOWN l
    | B.Finite l, (`ANY | `UP)  -> F.of_z prec `UP l
    | B.Finite l, `ZERO -> F.of_z prec `ZERO l
    | _ -> infinity
  in
  if lo > up then bot
  else fix_itv prec { bot with itv = Nb { lo; up; }; }
(** Conversion from integer intervals (handling overflows to infinities). *)   

let of_int_itv_bot (prec:prec) (round:round) (i:II.t with_bot) : t =
  match i with
  | BOT -> bot
  | Nb (lo,up) -> of_int_itv prec round (lo,up)
(** Conversion from integer intervals (handling overflows to infinities). *)   

let to_int_itv (r:t) : II.t with_bot =
  match r.itv with
  | BOT -> if is_bot r then BOT else Nb II.minf_inf
  | Nb i ->
     II.of_bound_bot
       (if r.minf then B.MINF else B.Finite (Z.of_float i.lo))
       (if r.pinf then B.PINF else B.Finite (Z.of_float i.up))
(** Conversion to integer interval with truncation. Handles infinities. *)

  

       
(** {2 Filters} *)

  
let filter_eq (prec:prec) (x:t) (y:t) : t * t =
  let r = meet x y in
  let r = { r with nan = false; } in
  r, r


let lift_filter_itv f x y =
  match bot_absorb2 f x.itv y.itv with
  | BOT -> BOT, BOT
  | Nb (xx,yy) -> Nb xx, Nb yy

let filter_leq (prec:prec) (x:t) (y:t) : t * t =
  (* compare finite with finite *)
  let ix, iy = lift_filter_itv (FI.filter_leq (prec:>FI.prec)) x y in
  (* compare finite with infinity *)
  let ix = if y.pinf then x.itv else ix
  and iy = if x.minf then y.itv else iy
  in
  { itv = ix; nan = false; minf = x.minf; pinf = x.pinf && y.pinf; },
  { itv = iy; nan = false; pinf = y.pinf; minf = x.minf && y.minf; }

let filter_lt (prec:prec) (x:t) (y:t) : t * t =
  (* compare finite with finite *)
  let ix, iy = lift_filter_itv (FI.filter_lt (prec:>FI.prec)) x y in
  (* compare finite with infinity *)
  let ix = if y.pinf then x.itv else ix
  and iy = if x.minf then y.itv else iy
  in
  { itv = ix; nan = false; minf = x.minf; pinf = false; },
  { itv = iy; nan = false; pinf = y.pinf; minf = false; }

(** C comparison filters.
    Keep the parts of the arguments that can satisfy the condition.
    NaN is assumed to be different from any value (including NaN).
 *)

let filter_geq (prec:prec) (x:t) (y:t) : t * t =
  let yy, xx = filter_leq prec y x in xx, yy

let filter_gt (prec:prec) (x:t) (y:t) : t * t =
  let yy, xx = filter_lt prec y x in xx, yy

let rec filter_neq (prec:prec) (x:t) (y:t) : t * t =
  if x.nan || y.nan then x, y (* NaN -> no refinement *)
  else if is_singleton x then
    (* case: remove infinity *)
    if x.pinf then x, { y with pinf = false; }
    else if x.minf then x, { y with minf = false; }
    else
      (* case: remove finite value *)
      let ix,iy = match bot_absorb2 (FI.filter_neq (prec:>FI.prec)) x.itv y.itv with
        | BOT -> BOT, BOT
        | Nb (xx,yy) -> Nb xx, Nb yy
      in
      { x with itv = ix; }, { y with itv = iy; }
  else if is_singleton y then
    let yy,xx = filter_neq prec y x in xx, yy (* symmetric case *)
  else x, y (* no singleton -> no refinement *)

(** Refine both arguments assuming that the test is true. *)

  
let filter_leq_false (prec:prec) (x:t) (y:t) : t * t =
  if x.nan || y.nan then x, y else filter_gt prec x y
                               
let filter_lt_false (prec:prec) (x:t) (y:t) : t * t =
  if x.nan || y.nan then x, y else filter_geq prec x y

let filter_geq_false (prec:prec) (x:t) (y:t) : t * t =
  let yy, xx = filter_leq_false prec y x in xx, yy
                                    
let filter_gt_false (prec:prec) (x:t) (y:t) : t * t =
  let yy, xx = filter_lt_false prec y x in xx, yy

let filter_eq_false  = filter_neq
let filter_neq_false = filter_eq
                                         
(** Refine both arguments assuming that the test is false. *)


(** {2 Backward arithmetic} *)


let bwd_neg (a:t) (r:t) : t =
  meet a (neg r)
(** Backward negation. *)

let bwd_abs (a:t) (r:t) : t =
  join (meet a r) (meet a (neg r))  
(** Backward absolute value. *)
    

let bwd_generic2  (prec:prec) (round:round) f (x:t) (y:t) (r:t) : t * t =
  if contains_special r then
    (* no refinement if specials in the result *)
    x, y 
  else
    (* no special in the result -> no special in the arguments *)
    match x.itv, y.itv, r.itv with
    | _, _, BOT -> bot, bot
    | BOT,_,_ | _,BOT,_ -> x, y
    | Nb xx, Nb yy, Nb rr ->
       match f (prec:>FI.prec) round xx yy rr with
       | BOT -> x, y
       | Nb (ix,iy) ->
          meet x (fix_itv prec { bot with itv = Nb ix; }),
          meet y (fix_itv prec { bot with itv = Nb iy; })
(* utility function for all binary operations *)
         
let bwd_add (prec:prec) (round:round) (x:t) (y:t) (r:t) : t * t =
  bwd_generic2 prec round FI.bwd_add x y r
(** Backward addition. *)
  
let bwd_sub (prec:prec) (round:round) (x:t) (y:t) (r:t) : t * t =
  bwd_generic2 prec round FI.bwd_sub x y r
(** Backward subtraction. *)

let bwd_mul (prec:prec) (round:round) (x:t) (y:t) (r:t) : t * t =
  bwd_generic2 prec round FI.bwd_add x y r 
(** Backward multiplication. *)

let bwd_div (prec:prec) (round:round) (x:t) (y:t) (r:t) : t * t =
  let xx, yy = bwd_generic2 prec round FI.bwd_div x y r in
  (* add back infinities to y if the result can be 0 *)
  let yy =
    if contains_zero r then { yy with pinf = y.pinf; minf = y.minf; }
    else yy
  in
  xx, yy
(** Backward division. *)

let bwd_fmod (prec:prec) (round:round) (x:t) (y:t) (r:t) : t * t =
  bwd_generic2 prec round (fun _ _ -> FI.bwd_fmod) x y r 
(** Backward modulo. *)

let bwd_generic1 (prec:prec) (round:round) f (x:t) (r:t) : t =
  if contains_special r then
    (* no refinement if specials in the result *)
    x
  else 
    (* no special in the result -> no special in the arguments *)
    match x.itv, r.itv with
    | _, BOT -> bot
    | BOT, _ -> x
    | Nb ix, Nb ir ->
       let itv = f (prec:>FI.prec) round ix ir in
       meet x (fix_itv prec { bot with itv; })
              
let bwd_round_int (prec:prec) (round:round) (x:t) (r:t) : t =
  bwd_generic1 prec round FI.bwd_round_int x r
(** Backward rounding to int. *)

let bwd_round (prec:prec) (round:round) (x:t) (r:t) : t =
  bwd_generic1 prec round FI.bwd_round x r
(** Backward rounding to float. *)

let bwd_square (prec:prec) (round:round) (x:t) (r:t) : t =
  bwd_generic1 prec round FI.bwd_square x r
(** Backward square. *)
  
let bwd_sqrt (prec:prec) (round:round) (x:t) (r:t) : t =
  bwd_generic1 prec round FI.bwd_sqrt x r
(** Backward square root. *)

let bwd_of_int_itv (prec:prec) (round:round) ((lo,up):II.t) (r:t)
    : II.t_with_bot =
  match r.itv with
  | Nb i ->
     let i = FI.unround_int (prec:>FI.prec) round i in
     let l =
       if F.is_finite i.lo && not r.minf
       then B.Finite (Z.of_float i.lo)
       else B.MINF
     and u =
       if F.is_finite i.up && not r.pinf
       then B.Finite (Z.of_float i.up)
       else B.PINF
     in
     II.meet (lo,up) (l,u)
  | BOT ->
     if is_bot r then BOT else Nb II.minf_inf
(** Backward conversion from integer interval. *)
    
let bwd_to_int_itv (a:t) ((lo,up):II.t) : t =
  let l = match lo with
    | B.Finite x -> F.of_z `DOUBLE `DOWN x
    | _ -> neg_infinity
  and u = match up with
    | B.Finite x -> F.of_z `DOUBLE `UP x
    | _ -> infinity
  in
  let itv =
    bot_absorb1
      (fun i -> FI.of_float_bot (max l i.FI.lo) (min u i.FI.up)) a.itv
  in
  { itv;
    nan = a.nan;
    minf = a.pinf && (l == neg_infinity);
    pinf = a.minf && (u == infinity);
  }
(** Backward conversion to integer interval (with truncation). *)
  
