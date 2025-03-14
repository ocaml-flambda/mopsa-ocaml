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
  IntBound - Enriches arbitrary precision integers with +∞ and -∞.

  Useful as interval bounds.
 *)


(** {2 Types} *)


type t = Finite of Z.t | MINF (** -∞ *) | PINF (** +∞ *)


(** {2 Internal utilities} *)



(** {2 Constructors} *)

let zero : t = Finite Z.zero
let one : t = Finite Z.one
let minus_one : t = Finite Z.minus_one
(** Useful constants *)

let of_int (x:int) : t = Finite (Z.of_int x)
let of_int64 (x:int64) : t = Finite (Z.of_int64 x)
(** Exact conversion from machine integers. *)

let of_float (x:float) : t =
  match classify_float x with
  | FP_infinite -> if x >= 0. then PINF else MINF
  | FP_nan -> invalid_arg "IntBound.of_float"
  | _ -> Finite (Z.of_float x)
(** Conversion with truncation from floats. Handles infinites. Raises an exception on NaNs. *)

let infinite (sign:int) : t =
  if sign > 0 then PINF
  else if sign < 0 then MINF
  else zero
(** Constructs an infinity with the given sign. Zero maps to zero. *)

let pow2 (nb:int) : t =
  Finite (Z.shift_left Z.one nb)
(** A power of two. The argument must be positive. *)



(** {2 Predicates} *)

let sign (x:t) : int =
  match x with MINF -> -1 | PINF -> 1 | Finite x -> Z.sign x
(** Sign of x: -1, 0, or 1. *)

let is_finite (x:t) : bool =
  match x with
  | Finite _ -> true
  | _ -> false
(** Whether x is finite or not. *)

let equal (x:t) (y:t) : bool =
  match x,y with
  | Finite a, Finite b -> Z.equal a b
  | MINF,MINF | PINF,PINF -> true
  | _ -> false
(** Equality comparison. = also works. *)

let compare (x:t) (y:t) : int =
  match x,y with
  | PINF,PINF | MINF,MINF -> 0
  | MINF,_ | _,PINF -> -1
  | PINF,_ | _,MINF -> 1
  | Finite x, Finite y -> Z.compare x y
(** Total order. Returns -1 (strictly smaller), 0 (equal), or 1 (strictly greater). *)

let leq (x:t) (y:t) : bool = compare x y <= 0
let geq (x:t) (y:t) : bool = compare x y >= 0
let lt  (x:t) (y:t) : bool = compare x y < 0
let gt  (x:t) (y:t) : bool = compare x y > 0
let eq  (x:t) (y:t) : bool = equal x y
let neq (x:t) (y:t) : bool = not (equal x y)
(** Comparison predicates. *)

let min (x:t) (y:t) : t = if leq x y then x else y
let max (x:t) (y:t) : t = if leq x y then y else x
(** Minimum and maximum. *)

let is_zero (x:t) : bool = sign x = 0
let is_nonzero (x:t) : bool = sign x <> 0
let is_positive (x:t) : bool = sign x >= 0
let is_negative (x:t) : bool = sign x <= 0
let is_positive_strict (x:t) : bool = sign x > 0
let is_negative_strict (x:t) : bool = sign x < 0
(** Sign predicates. *)

let hash (a:t) : int =
  match a with PINF -> 1 | MINF -> -1 | Finite x -> Z.hash x
(** Hashing function. *)


(** {2 Printing} *)

let to_string (x:t) : string =
  match x with
  | PINF -> "+∞"
  | MINF -> "-∞"
  | Finite x -> Z.to_string x

let print ch (x:t) = output_string ch (to_string x)
let fprint ch (x:t) = Format.pp_print_string ch (to_string x)
let bprint ch (x:t) = Buffer.add_string ch (to_string x)


(** {2 Operators} *)

let succ (a:t) : t =
  match a with Finite x -> Finite (Z.succ x) | _ -> a
(** +1. Infinities are left unchanged. *)

let pred (a:t) :t =
  match a with Finite x -> Finite (Z.pred x) | _ -> a
(** -1. Infinities are left unchanged. *)

let neg (a:t) : t =
  match a with MINF -> PINF | PINF -> MINF | Finite x -> Finite (Z.neg x)
(** Negation. *)

let abs (a:t) : t =
  match a with MINF | PINF -> PINF | Finite x -> Finite (Z.abs x)
(** Absolute value. *)

let add (a:t) (b:t) : t =
  match a, b with
  | PINF,MINF | MINF,PINF-> invalid_arg "IntBound.add"
  | PINF,_ | _,PINF -> PINF
  | MINF,_ | _,MINF -> MINF
  | Finite x, Finite y -> Finite (Z.add x y)
(** Addition. +∞ + -∞ is undefined (invalid argument exception). *)

let sub (a:t) (b:t) : t =
  match a, b with
  | PINF,PINF | MINF,MINF-> invalid_arg "IntBound.sub"
  | PINF,_ | _,MINF -> PINF
  | MINF,_ | _,PINF -> MINF
  | Finite x, Finite y -> Finite (Z.sub x y)
(** Subtraction. +∞ - +∞ is undefined (invalid argument exception). *)

let mul (a:t) (b:t) =
  match a, b with
  | Finite x, Finite y -> Finite (Z.mul x y)
  | _ -> infinite (sign a * sign b)
(** Multiplication. Always defined: +∞ * 0 = 0 *)

let div (a:t) (b:t) :t =
  match b with
  | PINF | MINF -> zero
  | Finite y ->
     match a with
     | PINF -> infinite (Z.sign y)
     | MINF -> infinite (-(Z.sign y))
     | Finite x ->
        if y = Z.zero then infinite (Z.sign x)
        else Finite (Z.div x y)
(** Division. Always defined: 0 / 0 = 0, x / +∞ = 0, x / 0 = sign(x) * ∞ *)

let ediv (a:t) (b:t) :t =
  match b with
  | PINF | MINF -> zero
  | Finite y ->
     match a with
     | PINF -> infinite (Z.sign y)
     | MINF -> infinite (-(Z.sign y))
     | Finite x ->
        if y = Z.zero then infinite (Z.sign x)
        else Finite (Z.ediv x y)
(** Euclidian division. Always defined: 0 / 0 = 0, x / +∞ = 0, x / 0 = sign(x) * ∞ *)

let rem (a:t) (b:t) : t =
  match a with
  | PINF | MINF -> invalid_arg "IntBound.rem INF"
  | Finite x ->
     match b with
     | PINF | MINF -> a
     | Finite y ->
        if y = Z.zero then invalid_arg "IntBound.rem ZERO"
        else Finite (Z.rem x y)
(** Remainder. rem x y has the sign of x, rem x (-y) = rem x y, and rem x +∞ = x.
    rem +∞ y and rem x 0 are undefined (invalid argument exception).
*)

let erem (a:t) (b:t) : t =
  match a with
  | PINF | MINF -> invalid_arg "IntBound.rem"
  | Finite x ->
     match b with
     | PINF | MINF -> a
     | Finite y ->
        if y = Z.zero then invalid_arg "IntBound.rem"
        else Finite (Z.erem x y)
(** Euclidian remainder. erem x y >= 0, rem x y < |b| and a = b * ediv a b + erem a b.
    rem +∞ y and rem x 0 are undefined (invalid argument exception).
*)

let shift_left (a:t) (b:t) : t =
  match a with
  | PINF -> PINF | MINF -> MINF
  | Finite x ->
     match b with
     | PINF -> infinite (sign a)
     | Finite y when Z.geq y Z.zero ->
       (try
          if Z.geq y (Z.of_int 2048) then
            (* let's avoid allocating xx GB memory of Z.t *)
            raise Z.Overflow
          else 
          let r = (Z.shift_left x (Z.to_int y)) in
          Finite r
        with Z.Overflow -> infinite (sign a)
       )
     | _ -> invalid_arg "IntBound.shift_left"
(** Left bitshift.
    Undefined if the second argument is negative (invalid argument exception).
    Returns an infinity if the second argument is too large.
 *)

let shift_right (a:t) (b:t) :t =
  match a with
  | PINF -> PINF | MINF -> MINF
  | Finite x ->
     match b with
     | PINF -> zero
     | Finite y when Z.geq y Z.zero ->
        (try Finite (Z.shift_right x (Z.to_int y))
         with Z.Overflow -> zero)
     | _ -> invalid_arg "IntBound.shift_right"
(** Right bitshift, rounding towards -∞.
    Undefined if the second argument is negative (invalid argument exception).
    Returns zero if the second argument is too large.
 *)

let shift_right_trunc (a:t) (b:t) : t =
  match a with
  | PINF -> PINF | MINF -> MINF
  | Finite x ->
     match b with
     | PINF -> zero
     | Finite y when Z.geq y Z.zero ->
        (try Finite (Z.shift_right_trunc x (Z.to_int y))
         with Z.Overflow -> zero)
     | _ -> invalid_arg "IntBound.shift_right_trunc"
(** Right bitshift, rounding towards 0 (truncation).
    Undefined if the second argument is negative (invalid argument exception).
    Returns zero if the second argument is too large.
*)

let only_finite msg op a b =
  match a,b with
  | Finite x, Finite y -> Finite (op x y)
  | _ -> invalid_arg msg

let bit_or : t -> t -> t = only_finite "IntBound.bit_or"  Z.logor
let bit_xor : t -> t -> t = only_finite "IntBound.bit_xor" Z.logxor
let bit_and : t -> t -> t = only_finite "IntBound.bit_and" Z.logand
(** Bitwise operations. Undefined for infinites (invalid argument exception). *)

let pow (a:t) (b:t) : t =
  match a, b with
  | Finite x, Finite y when Z.geq y Z.zero ->
    (try
       if Z.geq y (Z.of_int 2048) then
         (* let's avoid allocating xx GB memory of Z.t *)
         raise Z.Overflow
       else 
         Finite (Z.pow x (Z.to_int y))
     with Z.Overflow -> invalid_arg "IntBound.pow")
  | _ -> invalid_arg "IntBound.pow"
(** Power. Undefined if the second argument is negative or too large. *)
