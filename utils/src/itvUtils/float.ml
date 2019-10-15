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
  Float - Floating-point arihmetics with rounding.
  
  We rely on C code to provide functions with correct rounding
  (rounding direction and rounding precision).
 *)

(** {2 Types} *)


type t = float

type bit_float = {
    sign: bool; (** sign bit (true means negative) *)
    fraction: Z.t; (** fraction bits *)
    exponent: int; (** exponent (positive, with bias) *)
  }
(** Bit-representation of a float value. *)


          
(** {2 Global rounding direction} *)

                   
external set_round_near: unit -> unit = "ml_round_near" [@@noalloc]
external set_round_up:   unit -> unit = "ml_round_up"   [@@noalloc]
external set_round_down: unit -> unit = "ml_round_down" [@@noalloc]
external set_round_zero: unit -> unit = "ml_round_zero" [@@noalloc]
(** 
  Set the rounding mode globally.
  This affects the behaviors of all floating-point operations, including
  OCaml's native float operations, but excluding the operations in this
  module (and the float interval module) that specify a rounding direction.

  Note that the operations with specified rounding directions may change
  the rounding direction globally in some unspecified way, and not reset it 
  to its former value (this is done for efficiency).
 *)



(** {2 Operations without rounding} *)

let neg (x:t) : t =
  -. x

let abs (x:t) : t =
  abs_float x

let fmod (x:t) (y:t) : t =
  mod_float x y
(** Remainder (fmod, not equivalent to IEEE 754's float remainder). *)

let infinite (sign:int) : t =
  if sign > 0 then infinity
  else if sign < 0 then neg_infinity
  else 0.
(** Constructs an infinity with the given sign. Zero maps to zero. *)         


(** {2 Predicates} *)

let is_nan (x:t) : bool =
  classify_float x = FP_nan

let is_finite (x:t) : bool =
  match classify_float x with
  | FP_infinite | FP_nan -> false
  | _ -> true
(** Whether x is finite or not (infinite or NaN). *)

let is_infinite (x:t) : bool =
  classify_float x = FP_infinite

let is_normal (x:t) : bool =
  classify_float x = FP_normal

let is_denormal (x:t) : bool =
  classify_float x = FP_subnormal

let sign (x:t) : int =
  if x > 0. then 1
  else if x < 0. then -1
  else 0
(** Sign of x: -1 (negative), 0 (zero or NaN), or 1 (positive). *)

let sign_zero (x:t) : int =
  if x > 0. then 1
  else if x < 0. then -1
  else if 1. /. x > 0. then 1
  else if 1. /. x < 0. then -1
  else 0
(** As sign, but zero is signed.
    Returns -1 (negative or -0), 0 (NaN), or 1 (positive of +0)
 *)
  
let equal (x:t) (y:t) : bool =
  x = y
(** Equality comparison. Identical to =, i.e., NaN ≠ NaN *)

let equal_nan (x:t) (y:t) : bool =
  if is_nan x then is_nan y else x=y
(** As equal, but NaN equals NaN (and NaN ≠ non-NaN). *)
  
let leq (x:t) (y:t) : bool = x <= y
let geq (x:t) (y:t) : bool = x >= y
let lt  (x:t) (y:t) : bool = x < y
let gt  (x:t) (y:t) : bool = x > y
let eq  (x:t) (y:t) : bool = x = y
let neq (x:t) (y:t) : bool = x <> y
(** Comparison predicates. Returns false if one argument is NaN. *)
                  
let min (x:t) (y:t) : t = if x <= y then x else y
let max (x:t) (y:t) : t = if x <= y then y else x
(** Minimum and maximum. *)

let is_zero (x:t) : bool = sign x = 0
let is_nonzero (x:t) : bool = sign x <> 0
let is_positive (x:t) : bool = sign x >= 0
let is_negative (x:t) : bool = sign x <= 0
let is_positive_strict (x:t) : bool = sign x > 0
let is_negative_strict (x:t) : bool = sign x < 0
(** Sign predicates. *)

                                                    
(** {2 Printing} *)

type print_format = unit
(** Control the printing of a float (precision, rounding, etc.). *)
(* TODO *)

let dfl_fmt : print_format = ()
(** Default format. *)

let to_string (fmt:print_format) (x:t) : string =
  if is_nan x then "NaN"
  else if x = infinity then "+∞"
  else if x = neg_infinity then "-∞"
  else string_of_float (x+.0.)
(* Note: don't remove the "+.0."; it is here to ensure we never print "-0." 
   TODO: printing a guaranteed under/over-approximation.
 *)

let print fmt ch (x:t) = output_string ch (to_string fmt x)
let fprint fmt ch (x:t) = Format.pp_print_string ch (to_string fmt x)
let bprint fmt ch (x:t) = Buffer.add_string ch (to_string fmt x)


                        

(** {2 Operations with specific rounding direction and precision} *)

(**
  We provide the classic operations (and more) for single and double precision
  and all four rounding directions.
 *)                                                    
                                                    
module Single = struct

  (** Single precision numbers are stored inside OCaml's floats, that are
      actually double precion, but rounding is done to single precision. *)

             
  (** {2 Operations} *)

  external add_near: t -> t -> t = "ml_add_sgl_near" "ml_add_sgl_near_opt" [@@unboxed] [@@noalloc]
  external add_up:   t -> t -> t = "ml_add_sgl_up"   "ml_add_sgl_up_opt"   [@@unboxed] [@@noalloc]
  external add_down: t -> t -> t = "ml_add_sgl_down" "ml_add_sgl_down_opt" [@@unboxed] [@@noalloc]
  external add_zero: t -> t -> t = "ml_add_sgl_zero" "ml_add_sgl_zero_opt" [@@unboxed] [@@noalloc]
  (** Addition *)
                                                     
  external sub_near: t -> t -> t = "ml_sub_sgl_near" "ml_sub_sgl_near_opt" [@@unboxed] [@@noalloc]
  external sub_up:   t -> t -> t = "ml_sub_sgl_up"   "ml_sub_sgl_up_opt"   [@@unboxed] [@@noalloc]
  external sub_down: t -> t -> t = "ml_sub_sgl_down" "ml_sub_sgl_down_opt" [@@unboxed] [@@noalloc]
  external sub_zero: t -> t -> t = "ml_sub_sgl_zero" "ml_sub_sgl_zero_opt" [@@unboxed] [@@noalloc]
  (** Subtraction *)

  external mul_near: t -> t -> t = "ml_mul_sgl_near" "ml_mul_sgl_near_opt" [@@unboxed] [@@noalloc]
  external mul_up:   t -> t -> t = "ml_mul_sgl_up"   "ml_mul_sgl_up_opt"   [@@unboxed] [@@noalloc]
  external mul_down: t -> t -> t = "ml_mul_sgl_down" "ml_mul_sgl_down_opt" [@@unboxed] [@@noalloc]
  external mul_zero: t -> t -> t = "ml_mul_sgl_zero" "ml_mul_sgl_zero_opt" [@@unboxed] [@@noalloc]
  (** Multiplication *)

  external mulz_near: t -> t -> t = "ml_mulz_sgl_near" "ml_mulz_sgl_near_opt" [@@unboxed] [@@noalloc]
  external mulz_up:   t -> t -> t = "ml_mulz_sgl_up"   "ml_mulz_sgl_up_opt"   [@@unboxed] [@@noalloc]
  external mulz_down: t -> t -> t = "ml_mulz_sgl_down" "ml_mulz_sgl_down_opt" [@@unboxed] [@@noalloc]
  external mulz_zero: t -> t -> t = "ml_mulz_sgl_zero" "ml_mulz_sgl_zero_opt" [@@unboxed] [@@noalloc]
  (** Special multiplication where 0 times an infinity is 0, not NaN.
      This is particularly useful for interal bounds.
   *)

  external div_near: t -> t -> t = "ml_div_sgl_near" "ml_div_sgl_near_opt" [@@unboxed] [@@noalloc]
  external div_up:   t -> t -> t = "ml_div_sgl_up"   "ml_div_sgl_up_opt"   [@@unboxed] [@@noalloc]
  external div_down: t -> t -> t = "ml_div_sgl_down" "ml_div_sgl_down_opt" [@@unboxed] [@@noalloc]
  external div_zero: t -> t -> t = "ml_div_sgl_zero" "ml_div_sgl_zero_opt" [@@unboxed] [@@noalloc]
  (** Division *)

  external divz_near: t -> t -> t = "ml_divz_sgl_near" "ml_divz_sgl_near_opt" [@@unboxed] [@@noalloc]
  external divz_up:   t -> t -> t = "ml_divz_sgl_up"   "ml_divz_sgl_up_opt"   [@@unboxed] [@@noalloc]
  external divz_down: t -> t -> t = "ml_divz_sgl_down" "ml_divz_sgl_down_opt" [@@unboxed] [@@noalloc]
  external divz_zero: t -> t -> t = "ml_divz_sgl_zero" "ml_divz_sgl_zero_opt" [@@unboxed] [@@noalloc]
  (** Special division where 0 / 0 is 0, not NaN.
      This is particularly useful for interal bounds.
   *)
                                                     
  external mod_near: t -> t -> t = "ml_mod_sgl_near" "ml_mod_sgl_near_opt" [@@unboxed] [@@noalloc]
  external mod_up:   t -> t -> t = "ml_mod_sgl_up"   "ml_mod_sgl_up_opt"   [@@unboxed] [@@noalloc]
  external mod_down: t -> t -> t = "ml_mod_sgl_down" "ml_mod_sgl_down_opt" [@@unboxed] [@@noalloc]
  external mod_zero: t -> t -> t = "ml_mod_sgl_zero" "ml_mod_sgl_zero_opt" [@@unboxed] [@@noalloc]
  (** Remainder. *)

  let square_near x = mul_near x x
  let square_up   x = mul_up   x x
  let square_down x = mul_down x x
  let square_zero x = mul_zero x x
  (** Square. *)                               

  external sqrt_near: t -> t = "ml_sqrt_sgl_near" "ml_sqrt_sgl_near_opt" [@@unboxed] [@@noalloc]
  external sqrt_up:   t -> t = "ml_sqrt_sgl_up"   "ml_sqrt_sgl_up_opt"   [@@unboxed] [@@noalloc]
  external sqrt_down: t -> t = "ml_sqrt_sgl_down" "ml_sqrt_sgl_down_opt" [@@unboxed] [@@noalloc]
  external sqrt_zero: t -> t = "ml_sqrt_sgl_zero" "ml_sqrt_sgl_zero_opt" [@@unboxed] [@@noalloc]
  (** Square root. *)
                                                  
  external round_int_near: t -> t = "ml_round_int_sgl_near" "ml_round_int_sgl_near_opt" [@@unboxed] [@@noalloc]
  external round_int_up:   t -> t = "ml_round_int_sgl_up"   "ml_round_int_sgl_up_opt"   [@@unboxed] [@@noalloc]
  external round_int_down: t -> t = "ml_round_int_sgl_down" "ml_round_int_sgl_down_opt" [@@unboxed] [@@noalloc]
  external round_int_zero: t -> t = "ml_round_int_sgl_zero" "ml_round_int_sgl_zero_opt" [@@unboxed] [@@noalloc]
  (** Rounding to an integer. *)
                                                    
  external of_double_near: t -> t = "ml_to_sgl_near" "ml_to_sgl_near_opt" [@@unboxed] [@@noalloc]
  external of_double_up:   t -> t = "ml_to_sgl_up"   "ml_to_sgl_up_opt"   [@@unboxed] [@@noalloc]
  external of_double_down: t -> t = "ml_to_sgl_down" "ml_to_sgl_down_opt" [@@unboxed] [@@noalloc]
  external of_double_zero: t -> t = "ml_to_sgl_zero" "ml_to_sgl_zero_opt" [@@unboxed] [@@noalloc]
  (** Rounding from double to single precision. *)                                                      

  external of_int_near: int -> t = "ml_of_int_sgl_near" "ml_of_int_sgl_near"
  external of_int_up:   int -> t = "ml_of_int_sgl_up"   "ml_of_int_sgl_up"
  external of_int_down: int -> t = "ml_of_int_sgl_down" "ml_of_int_sgl_down"
  external of_int_zero: int -> t = "ml_of_int_sgl_zero" "ml_of_int_sgl_zero"
  external of_int_cur:  int -> t = "ml_of_int_sgl_cur"  "ml_of_int_sgl_cur"
  (** Conversion from int with rounding. *)

  external of_int64_near: int64 -> t = "ml_of_int64_sgl_near" "ml_of_int64_sgl_near_opt" [@@unboxed] [@@noalloc]
  external of_int64_up:   int64 -> t = "ml_of_int64_sgl_up"   "ml_of_int64_sgl_up_opt"   [@@unboxed] [@@noalloc]
  external of_int64_down: int64 -> t = "ml_of_int64_sgl_down" "ml_of_int64_sgl_down_opt" [@@unboxed] [@@noalloc]
  external of_int64_zero: int64 -> t = "ml_of_int64_sgl_zero" "ml_of_int64_sgl_zero_opt" [@@unboxed] [@@noalloc]
  external of_int64_cur:  int64 -> t = "ml_of_int64_sgl_cur"  "ml_of_int64_sgl_cur_opt"  [@@unboxed] [@@noalloc]
  (** Conversion from int64 with rounding. *)


  (**
     Code from Zarith to convert from Z.t to float using the current rounding mode.
     We add a version for single precision rounding.
   *)
                                     
  let round_z_to_single x exact =
    let m = Z.to_int x in
    (* Unless the fractional part is exactly 0, round m to an odd integer *)
    let m = if exact then m else m lor 1 in
    (* Then convert m to float, with the current rounding mode. *)
    of_int_cur m
    
  let z_to_single x =
    if Obj.is_int (Obj.repr x) then
      (* Fast path *)
      of_int_cur (Obj.magic x : int)
    else begin
        let n = Z.numbits x in
        if n <= 30 then
          of_int_cur (Z.to_int x)
        else begin
            let n = n - 26 in
            (* Extract top 26 bits of x *)
            let top = Z.shift_right x n in
            (* Check if the other bits are all zero *)
            let exact = Z.equal x (Z.shift_left top n) in
            (* Round to float and apply exponent *)
            ldexp (round_z_to_single top exact) n
          end
      end
                      
                                     
  let of_z_near x = set_round_near (); z_to_single x
  let of_z_up   x = set_round_up   (); z_to_single x
  let of_z_down x = set_round_down (); z_to_single x
  let of_z_zero x = set_round_zero (); z_to_single x
  (** Conversion from Zarith with rounding. *)
                    
                                                              
  (** {2 Floating-point format characteristics} *)
                                                              
  let mantissa_bits : int = 23 (* excluding hidden bit *)
  let exponent_bits : int = 8
  let exponent_bias : int = 127
                          
  let min_exponent : int = -126 (* for normal values *)
  let max_exponent : int = 127
  let nan_infinity_exponent : int = 128

  let min_denormal : t = ldexp 1. (min_exponent-mantissa_bits)
  let min_normal : t = ldexp 1. min_exponent
  let max_normal : t = ldexp (2. -. ldexp 1. (-mantissa_bits)) max_exponent
  let max_exact : t = ldexp 1. mantissa_bits

  let ulp : t = ldexp 1. (-mantissa_bits)
  (** Units in the last place (relative precision). *)

  let rep_of_bits (i:int32) : bit_float =
    { sign = i < 0l;
      exponent = (Int32.to_int (Int32.shift_right i mantissa_bits) land 0xff);
      fraction = Z.of_int32 (Int32.logand i 0x7fffffl);
    }

  let bits_of_rep (r:bit_float) : int32 =
    let sign = if r.sign then 0x80000000l else 0l in
    let exp = Int32.shift_left (Int32.of_int r.exponent) mantissa_bits in
    Int32.logor sign (Int32.logor exp (Z.to_int32 r.fraction))
    
  external to_bits: t -> int32 = "ml_bits_of_float"
  external of_bits: int32 -> t = "ml_float_of_bits"


  let succ (a:t) : t = if a = neg_infinity then -.max_normal else add_up a min_denormal
  let pred (a:t) : t = if a = infinity then max_normal else sub_down a min_denormal
  (** Returns the float immediately follownig or preceeding the argument. *)
                        
  let succ_zero (a:t) : t = if a == 0. then a else succ a
  let pred_zero (a:t) : t = if a == 0. then a else pred a
  (** As succ and pred, but does not cross zero. *)

  external of_string_up:   string -> t = "ml_of_string_sgl_up"
  external of_string_down: string -> t = "ml_of_string_sgl_down"
  (** Conversion from string. *)
                                       
end
(** Single precision operations. *)
                  


module Double = struct
                    
  (** {2 Operations} *)

  external add_near: t -> t -> t = "ml_add_dbl_near" "ml_add_dbl_near_opt" [@@unboxed] [@@noalloc]
  external add_up:   t -> t -> t = "ml_add_dbl_up"   "ml_add_dbl_up_opt"   [@@unboxed] [@@noalloc]
  external add_down: t -> t -> t = "ml_add_dbl_down" "ml_add_dbl_down_opt" [@@unboxed] [@@noalloc]
  external add_zero: t -> t -> t = "ml_add_dbl_zero" "ml_add_dbl_zero_opt" [@@unboxed] [@@noalloc]
  (** Addition *)
                                                     
  external sub_near: t -> t -> t = "ml_sub_dbl_near" "ml_sub_dbl_near_opt" [@@unboxed] [@@noalloc]
  external sub_up:   t -> t -> t = "ml_sub_dbl_up"   "ml_sub_dbl_up_opt"   [@@unboxed] [@@noalloc]
  external sub_down: t -> t -> t = "ml_sub_dbl_down" "ml_sub_dbl_down_opt" [@@unboxed] [@@noalloc]
  external sub_zero: t -> t -> t = "ml_sub_dbl_zero" "ml_sub_dbl_zero_opt" [@@unboxed] [@@noalloc]
  (** Subtraction *)

  external mul_near: t -> t -> t = "ml_mul_dbl_near" "ml_mul_dbl_near_opt" [@@unboxed] [@@noalloc]
  external mul_up:   t -> t -> t = "ml_mul_dbl_up"   "ml_mul_dbl_up_opt"   [@@unboxed] [@@noalloc]
  external mul_down: t -> t -> t = "ml_mul_dbl_down" "ml_mul_dbl_down_opt" [@@unboxed] [@@noalloc]
  external mul_zero: t -> t -> t = "ml_mul_dbl_zero" "ml_mul_dbl_zero_opt" [@@unboxed] [@@noalloc]
  (** Multiplication *)

  external mulz_near: t -> t -> t = "ml_mulz_dbl_near" "ml_mulz_dbl_near_opt" [@@unboxed] [@@noalloc]
  external mulz_up:   t -> t -> t = "ml_mulz_dbl_up"   "ml_mulz_dbl_up_opt"   [@@unboxed] [@@noalloc]
  external mulz_down: t -> t -> t = "ml_mulz_dbl_down" "ml_mulz_dbl_down_opt" [@@unboxed] [@@noalloc]
  external mulz_zero: t -> t -> t = "ml_mulz_dbl_zero" "ml_mulz_dbl_zero_opt" [@@unboxed] [@@noalloc]
  (** Special multiplication where 0 * ∞ is 0, not NaN.
      This is particularly useful for interal bounds.
   *)

  external div_near: t -> t -> t = "ml_div_dbl_near" "ml_div_dbl_near_opt" [@@unboxed] [@@noalloc]
  external div_up:   t -> t -> t = "ml_div_dbl_up"   "ml_div_dbl_up_opt"   [@@unboxed] [@@noalloc]
  external div_down: t -> t -> t = "ml_div_dbl_down" "ml_div_dbl_down_opt" [@@unboxed] [@@noalloc]
  external div_zero: t -> t -> t = "ml_div_dbl_zero" "ml_div_dbl_zero_opt" [@@unboxed] [@@noalloc]
  (** Division *)

  external divz_near: t -> t -> t = "ml_divz_dbl_near" "ml_divz_dbl_near_opt" [@@unboxed] [@@noalloc]
  external divz_up:   t -> t -> t = "ml_divz_dbl_up"   "ml_divz_dbl_up_opt"   [@@unboxed] [@@noalloc]
  external divz_down: t -> t -> t = "ml_divz_dbl_down" "ml_divz_dbl_down_opt" [@@unboxed] [@@noalloc]
  external divz_zero: t -> t -> t = "ml_divz_dbl_zero" "ml_divz_dbl_zero_opt" [@@unboxed] [@@noalloc]
  (** Special division where 0 / 0 and ∞ / ∞ are 0, not NaN.
      This is particularly useful for interal bounds.
   *)
                                                     
  external mod_near: t -> t -> t = "ml_mod_dbl_near" "ml_mod_dbl_near_opt" [@@unboxed] [@@noalloc]
  external mod_up:   t -> t -> t = "ml_mod_dbl_up"   "ml_mod_dbl_up_opt"   [@@unboxed] [@@noalloc]
  external mod_down: t -> t -> t = "ml_mod_dbl_down" "ml_mod_dbl_down_opt" [@@unboxed] [@@noalloc]
  external mod_zero: t -> t -> t = "ml_mod_dbl_zero" "ml_mod_dbl_zero_opt" [@@unboxed] [@@noalloc]
  (** Remainder. *)

  let square_near x = mul_near x x
  let square_up   x = mul_up   x x
  let square_down x = mul_down x x
  let square_zero x = mul_zero x x
  (** Square. *)
                                 
  external sqrt_near: t -> t = "ml_sqrt_dbl_near" "ml_sqrt_dbl_near_opt" [@@unboxed] [@@noalloc]
  external sqrt_up:   t -> t = "ml_sqrt_dbl_up"   "ml_sqrt_dbl_up_opt"   [@@unboxed] [@@noalloc]
  external sqrt_down: t -> t = "ml_sqrt_dbl_down" "ml_sqrt_dbl_down_opt" [@@unboxed] [@@noalloc]
  external sqrt_zero: t -> t = "ml_sqrt_dbl_zero" "ml_sqrt_dbl_zero_opt" [@@unboxed] [@@noalloc]
  (** Square root. *)
                                                  
  external round_int_near: t -> t = "ml_round_int_dbl_near" "ml_round_int_dbl_near_opt" [@@unboxed] [@@noalloc]
  external round_int_up:   t -> t = "ml_round_int_dbl_up"   "ml_round_int_dbl_up_opt"   [@@unboxed] [@@noalloc]
  external round_int_down: t -> t = "ml_round_int_dbl_down" "ml_round_int_dbl_down_opt" [@@unboxed] [@@noalloc]
  external round_int_zero: t -> t = "ml_round_int_dbl_zero" "ml_round_int_dbl_zero_opt" [@@unboxed] [@@noalloc]
  (** Rounding to an integer. *)
                                                    
  external of_int_near: int -> t = "ml_of_int_dbl_near" "ml_of_int_dbl_near"
  external of_int_up:   int -> t = "ml_of_int_dbl_up"   "ml_of_int_dbl_up"
  external of_int_down: int -> t = "ml_of_int_dbl_down" "ml_of_int_dbl_down"
  external of_int_zero: int -> t = "ml_of_int_dbl_zero" "ml_of_int_dbl_zero"
  (** Conversion from int with rounding. *)

  external of_int64_near: int64 -> t = "ml_of_int64_dbl_near" "ml_of_int64_dbl_near_opt" [@@unboxed] [@@noalloc]
  external of_int64_up:   int64 -> t = "ml_of_int64_dbl_up"   "ml_of_int64_dbl_up_opt"   [@@unboxed] [@@noalloc]
  external of_int64_down: int64 -> t = "ml_of_int64_dbl_down" "ml_of_int64_dbl_down_opt" [@@unboxed] [@@noalloc]
  external of_int64_zero: int64 -> t = "ml_of_int64_dbl_zero" "ml_of_int64_dbl_zero_opt" [@@unboxed] [@@noalloc]
  (** Conversion from int64 with rounding. *)

  let of_z_near x = set_round_near (); Z.to_float x
  let of_z_up   x = set_round_up   (); Z.to_float x
  let of_z_down x = set_round_down (); Z.to_float x
  let of_z_zero x = set_round_zero (); Z.to_float x
  (** Conversion from Zarith with rounding. *)
                    

                                                              
  (** {2 Floating-point format characteristics} *)

  let mantissa_bits : int = 52 (* excluding hidden bit *)
  let exponent_bits : int = 11
  let exponent_bias : int = 1023

  let min_exponent : int = -1022 (* for normal values *)
  let max_exponent : int = 1023
  let nan_infinity_exponent : int = 1024
                                  
  let min_denormal : t = ldexp 1. (min_exponent-mantissa_bits)
  let min_normal : t = ldexp 1. min_exponent
  let max_normal : t = ldexp (2. -. ldexp 1. (-mantissa_bits)) max_exponent
  let max_exact : t = ldexp 1. mantissa_bits

  let ulp : t = ldexp 1. (-mantissa_bits)
  (** Units in the last place (relative precision). *)

  let rep_of_bits (i:int64) : bit_float =
    { sign = i < 0L;
      exponent = (Int64.to_int (Int64.shift_right i mantissa_bits) land 0x7ff);
      fraction = Z.of_int64 (Int64.logand i 0xfffffffffffffL);
    }

  let bits_of_rep (r:bit_float) : int64 =
    let sign = if r.sign then 0x8000000000000000L else 0L in
    let exp = Int64.shift_left (Int64.of_int r.exponent) mantissa_bits in
    Int64.logor sign (Int64.logor exp (Z.to_int64 r.fraction))
    
  external to_bits: t -> int64 = "ml_bits_of_double"
  external of_bits: int64 -> t = "ml_double_of_bits"

                                   
  let succ (a:t) : t = if a = neg_infinity then -.max_normal else add_up a min_denormal
  let pred (a:t) : t = if a = infinity then max_normal else sub_down a min_denormal
  (** Returns the float immediately following or preceeding the argument. *)
                        
  let succ_zero (a:t) : t = if a == 0. then a else succ a
  let pred_zero (a:t) : t = if a == 0. then a else pred a
  (** As succ and pred, but does not cross zero. *)

  external of_string_up:   string -> t = "ml_of_string_dbl_up"
  external of_string_down: string -> t = "ml_of_string_dbl_down"
  (** Conversion from string. *)

end
(** Double precision operations. *)
                  


(** {2 Operations with rounding mode as argument} *)


type prec =
  [ `SINGLE (** 32-bit single precision *)
  | `DOUBLE (** 64-bit double precision *)
  ]
(** Precision. *)
                   
type round =
  [ `NEAR (** To nearest *)
  | `UP   (** Upwards *)
  | `DOWN (** Downwards *)
  | `ZERO (** Towards 0 *)
  ]
(** Rounding direction. *)

  
let add (prec:prec) (round:round) x y =
  match prec,round with
  | `SINGLE, `NEAR -> Single.add_near x y
  | `SINGLE, `UP   -> Single.add_up   x y
  | `SINGLE, `DOWN -> Single.add_down x y
  | `SINGLE, `ZERO -> Single.add_zero x y
  | `DOUBLE, `NEAR -> Double.add_near x y
  | `DOUBLE, `UP   -> Double.add_up   x y
  | `DOUBLE, `DOWN -> Double.add_down x y
  | `DOUBLE, `ZERO -> Double.add_zero x y
(** Addition. *)
                  
let sub (prec:prec) (round:round) x y =
  match prec,round with
  | `SINGLE, `NEAR -> Single.sub_near x y
  | `SINGLE, `UP   -> Single.sub_up   x y
  | `SINGLE, `DOWN -> Single.sub_down x y
  | `SINGLE, `ZERO -> Single.sub_zero x y
  | `DOUBLE, `NEAR -> Double.sub_near x y
  | `DOUBLE, `UP   -> Double.sub_up   x y
  | `DOUBLE, `DOWN -> Double.sub_down x y
  | `DOUBLE, `ZERO -> Double.sub_zero x y
(** Subtraction. *)
                  
let mul (prec:prec) (round:round) x y =
  match prec,round with
  | `SINGLE, `NEAR -> Single.mul_near x y
  | `SINGLE, `UP   -> Single.mul_up   x y
  | `SINGLE, `DOWN -> Single.mul_down x y
  | `SINGLE, `ZERO -> Single.mul_zero x y
  | `DOUBLE, `NEAR -> Double.mul_near x y
  | `DOUBLE, `UP   -> Double.mul_up   x y
  | `DOUBLE, `DOWN -> Double.mul_down x y
  | `DOUBLE, `ZERO -> Double.mul_zero x y
(** Multiplication. *)
                  
let mulz (prec:prec) (round:round) x y =
  match prec,round with
  | `SINGLE, `NEAR -> Single.mulz_near x y
  | `SINGLE, `UP   -> Single.mulz_up   x y
  | `SINGLE, `DOWN -> Single.mulz_down x y
  | `SINGLE, `ZERO -> Single.mulz_zero x y
  | `DOUBLE, `NEAR -> Double.mulz_near x y
  | `DOUBLE, `UP   -> Double.mulz_up   x y
  | `DOUBLE, `DOWN -> Double.mulz_down x y
  | `DOUBLE, `ZERO -> Double.mulz_zero x y
(** Multiplication, where 0 * infinity is 0, not Nan. *)

let div (prec:prec) (round:round) x y =
  match prec,round with
  | `SINGLE, `NEAR -> Single.div_near x y
  | `SINGLE, `UP   -> Single.div_up   x y
  | `SINGLE, `DOWN -> Single.div_down x y
  | `SINGLE, `ZERO -> Single.div_zero x y
  | `DOUBLE, `NEAR -> Double.div_near x y
  | `DOUBLE, `UP   -> Double.div_up   x y
  | `DOUBLE, `DOWN -> Double.div_down x y
  | `DOUBLE, `ZERO -> Double.div_zero x y
(** Division. *)

let divz (prec:prec) (round:round) x y =
  match prec,round with
  | `SINGLE, `NEAR -> Single.divz_near x y
  | `SINGLE, `UP   -> Single.divz_up   x y
  | `SINGLE, `DOWN -> Single.divz_down x y
  | `SINGLE, `ZERO -> Single.divz_zero x y
  | `DOUBLE, `NEAR -> Double.divz_near x y
  | `DOUBLE, `UP   -> Double.divz_up   x y
  | `DOUBLE, `DOWN -> Double.divz_down x y
  | `DOUBLE, `ZERO -> Double.divz_zero x y
(** Division, where 0 / 0 is 0, not Nan. *)

let rem (prec:prec) (round:round) x y =
  match prec,round with
  | `SINGLE, `NEAR -> Single.mod_near x y
  | `SINGLE, `UP   -> Single.mod_up   x y
  | `SINGLE, `DOWN -> Single.mod_down x y
  | `SINGLE, `ZERO -> Single.mod_zero x y
  | `DOUBLE, `NEAR -> Double.mod_near x y
  | `DOUBLE, `UP   -> Double.mod_up   x y
  | `DOUBLE, `DOWN -> Double.mod_down x y
  | `DOUBLE, `ZERO -> Double.mod_zero x y
(** Modulo. *)
                  
let square (prec:prec) (round:round) x =
  match prec,round with
  | `SINGLE, `NEAR -> Single.square_near x
  | `SINGLE, `UP   -> Single.square_up   x
  | `SINGLE, `DOWN -> Single.square_down x
  | `SINGLE, `ZERO -> Single.square_zero x
  | `DOUBLE, `NEAR -> Double.square_near x
  | `DOUBLE, `UP   -> Double.square_up   x
  | `DOUBLE, `DOWN -> Double.square_down x
  | `DOUBLE, `ZERO -> Double.square_zero x
(** Square. *)

let sqrt (prec:prec) (round:round) x =
  match prec,round with
  | `SINGLE, `NEAR -> Single.sqrt_near x
  | `SINGLE, `UP   -> Single.sqrt_up   x
  | `SINGLE, `DOWN -> Single.sqrt_down x
  | `SINGLE, `ZERO -> Single.sqrt_zero x
  | `DOUBLE, `NEAR -> Double.sqrt_near x
  | `DOUBLE, `UP   -> Double.sqrt_up   x
  | `DOUBLE, `DOWN -> Double.sqrt_down x
  | `DOUBLE, `ZERO -> Double.sqrt_zero x
(** Square root. *)
                  
let round_int (prec:prec) (round:round) x =
  match prec,round with
  | `SINGLE, `NEAR -> Single.round_int_near x
  | `SINGLE, `UP   -> Single.round_int_up   x
  | `SINGLE, `DOWN -> Single.round_int_down x
  | `SINGLE, `ZERO -> Single.round_int_zero x
  | `DOUBLE, `NEAR -> Double.round_int_near x
  | `DOUBLE, `UP   -> Double.round_int_up   x
  | `DOUBLE, `DOWN -> Double.round_int_down x
  | `DOUBLE, `ZERO -> Double.round_int_zero x
(** Rounds to integer (the result remains a float). *)
                  
let of_int (prec:prec) (round:round) x =
  match prec,round with
  | `SINGLE, `NEAR -> Single.of_int_near x
  | `SINGLE, `UP   -> Single.of_int_up   x
  | `SINGLE, `DOWN -> Single.of_int_down x
  | `SINGLE, `ZERO -> Single.of_int_zero x
  | `DOUBLE, `NEAR -> Double.of_int_near x
  | `DOUBLE, `UP   -> Double.of_int_up   x
  | `DOUBLE, `DOWN -> Double.of_int_down x
  | `DOUBLE, `ZERO -> Double.of_int_zero x
(** Conversion from int. *)

let of_int64 (prec:prec) (round:round) x =
  match prec,round with
  | `SINGLE, `NEAR -> Single.of_int64_near x
  | `SINGLE, `UP   -> Single.of_int64_up   x
  | `SINGLE, `DOWN -> Single.of_int64_down x
  | `SINGLE, `ZERO -> Single.of_int64_zero x
  | `DOUBLE, `NEAR -> Double.of_int64_near x
  | `DOUBLE, `UP   -> Double.of_int64_up   x
  | `DOUBLE, `DOWN -> Double.of_int64_down x
  | `DOUBLE, `ZERO -> Double.of_int64_zero x
(** Conversion from int64. *)

let of_z (prec:prec) (round:round) x =
  match prec,round with
  | `SINGLE, `NEAR -> Single.of_z_near x
  | `SINGLE, `UP   -> Single.of_z_up   x
  | `SINGLE, `DOWN -> Single.of_z_down x
  | `SINGLE, `ZERO -> Single.of_z_zero x
  | `DOUBLE, `NEAR -> Double.of_z_near x
  | `DOUBLE, `UP   -> Double.of_z_up   x
  | `DOUBLE, `DOWN -> Double.of_z_down x
  | `DOUBLE, `ZERO -> Double.of_z_zero x
(** Conversion from Z.t *)

let of_string (prec:prec) (round:[`UP | `DOWN]) x =
    match prec,round with
  | `SINGLE, `UP   -> Single.of_string_up   x
  | `SINGLE, `DOWN -> Single.of_string_down x
  | `DOUBLE, `UP   -> Double.of_string_up   x
  | `DOUBLE, `DOWN -> Double.of_string_down x
(** Conversion from string, with safe rounding. *)

let succ (prec:prec) x =
  match prec with
  | `SINGLE -> Single.succ x
  | `DOUBLE -> Double.succ x
(** Number immediately after. *)

let pred (prec:prec) x =
  match prec with
  | `SINGLE -> Single.pred x
  | `DOUBLE -> Double.pred x
(** Number immediately before. *)            

let succ_zero (prec:prec) x =
  match prec with
  | `SINGLE -> Single.succ_zero x
  | `DOUBLE -> Double.succ_zero x
(** Number immediately after. Does not cross zero. *)

let pred_zero (prec:prec) x =
  match prec with
  | `SINGLE -> Single.pred_zero x
  | `DOUBLE -> Double.pred_zero x
(** Number immediately before. Does not cross zero. *)            

            
let mantissa_bits (prec:prec) =
  match prec with
  | `SINGLE -> Single.mantissa_bits
  | `DOUBLE -> Double.mantissa_bits

let exponent_bits (prec:prec) =
  match prec with
  | `SINGLE -> Single.exponent_bits
  | `DOUBLE -> Double.exponent_bits

let exponent_bias (prec:prec) =
  match prec with
  | `SINGLE -> Single.exponent_bias
  | `DOUBLE -> Double.exponent_bias

let min_exponent (prec:prec) =
  match prec with
  | `SINGLE -> Single.min_exponent
  | `DOUBLE -> Double.min_exponent

let max_exponent (prec:prec) =
  match prec with
  | `SINGLE -> Single.max_exponent
  | `DOUBLE -> Double.max_exponent

let nan_infinity_exponent (prec:prec) =
  match prec with
  | `SINGLE -> Single.nan_infinity_exponent
  | `DOUBLE -> Double.nan_infinity_exponent

let min_denormal (prec:prec) =
  match prec with
  | `SINGLE -> Single.min_denormal
  | `DOUBLE -> Double.min_denormal

let min_normal (prec:prec) =
  match prec with
  | `SINGLE -> Single.min_normal
  | `DOUBLE -> Double.min_normal

let max_normal (prec:prec) =
  match prec with
  | `SINGLE -> Single.max_normal
  | `DOUBLE -> Double.max_normal

let max_exact (prec:prec) =
  match prec with
  | `SINGLE -> Single.max_exact
  | `DOUBLE -> Double.max_exact

let ulp (prec:prec) =
  match prec with
  | `SINGLE -> Single.ulp
  | `DOUBLE -> Double.ulp

(** Useful constants. *)

            
let to_bits (prec:prec) (x:float) : bit_float =
  match prec with
  | `SINGLE -> x |> Single.to_bits |> Single.rep_of_bits
  | `DOUBLE -> x |> Double.to_bits |> Double.rep_of_bits

let of_bits (prec:prec) (x:bit_float) : float =
  match prec with
  | `SINGLE -> x |> Single.bits_of_rep |> Single.of_bits
  | `DOUBLE -> x |> Double.bits_of_rep |> Double.of_bits

(** Bit-level extraction. *)
            

