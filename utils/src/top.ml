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
  Top - Top element
 *)

(** {2 Types} *)

type 'a with_top = TOP | Nt of 'a
(** Add a top element to a type. *)


(** {2 Operator lifting} *)


let top_lift1 (f:'a -> 'b) (a:'a with_top) : 'b with_top =
  match a with TOP -> TOP | Nt x -> Nt (f x)

let top_lift2 (f:'a  -> 'b -> 'c) (a:'a with_top) (b:'b with_top) : 'c with_top =
  match a,b with TOP,_ | _,TOP -> TOP | Nt x, Nt y -> Nt (f x y)

let top_absorb1 (f:'a -> 'b with_top) (a:'a with_top) : 'b with_top =
  match a with TOP -> TOP | Nt x -> f x

let top_absorb2 (f:'a  -> 'b -> 'c with_top) (a:'a with_top) (b:'b with_top) : 'c with_top =
  match a,b with TOP,_ | _,TOP -> TOP | Nt x, Nt y -> f x y

let top_neutral2 (f:'a -> 'a -> 'a) (a:'a with_top) (b:'a with_top) : 'a with_top =
  match a,b with TOP,_ -> b | _,TOP -> a | Nt x, Nt y -> Nt (f x y)

let top_apply (f:'b->'a) (a:'a) (b:'b with_top) : 'a =
  match b with TOP -> a | Nt x -> f x

let top_apply2 (a1:'a) (a2:'a) (f:'b ->'b -> 'a) (b1:'b with_top) (b2:'b with_top) : 'a =
  match b1, b2 with
  | TOP, TOP -> a1
  | TOP, _ | _, TOP -> a2
  | Nt x1, Nt x2 -> f x1 x2


let top_equal (f:'a->'b->bool) (a:'a with_top) (b:'b with_top) : bool =
  match a,b with TOP, TOP -> true | Nt x, Nt y -> f x y | _ -> false

let top_included (f:'a->'b->bool) (a:'a with_top) (b:'b with_top) : bool =
  match a,b with _, TOP -> true | Nt x, Nt y -> f x y | _ -> false

let top_compare (cmp:'a -> 'a -> int) (a:'a with_top) (b:'a with_top) : int =
  match a,b with TOP,TOP -> 0 | TOP,_ -> 1 | _,TOP -> -1 | Nt x, Nt y -> cmp x y

let top_dfl1 (dfl:'b) (f:'a->'c) (a:'a with_top) : 'c =
  match a with TOP -> dfl | Nt x -> f x


(** {2 Exceptions} *)


exception Found_TOP

let raise_top () = raise Found_TOP
let catch_top (f:'a -> 'b) (a:'a) (dfl:'b) : 'b = try f a with Found_TOP -> dfl
let detop (a:'a with_top) : 'a = match a with TOP -> raise Found_TOP | Nt x -> x
let retop (f:'a ->'b)  (x:'a) : 'b with_top = try Nt (f x) with Found_TOP -> TOP

let exn_to_top  = retop
let top_to_exn = detop

(** {2 Printing} *)

let top_string = "⊤"

let top_to_string f x = match x with TOP -> top_string | Nt x -> f x
let top_print f ch x = match x with TOP -> output_string ch top_string | Nt x -> f ch x
let top_fprint f ch x = match x with TOP -> Format.pp_print_string ch top_string | Nt x -> f ch x
let top_bprint f ch x = match x with TOP -> Buffer.add_string ch top_string | Nt x -> f ch x
