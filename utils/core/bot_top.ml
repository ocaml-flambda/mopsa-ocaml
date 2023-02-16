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
  Bot_top - Lift operations to a top/bottom element.
 *)


(** {2 Types} *)

type 'a with_bot_top = BOT | TOP | Nbt of 'a
(** Adds bottom/top element to a type. *)


(** {2 Operator lifting} *)


let bot_top_lift1 (f:'a -> 'b) (a:'a with_bot_top) : 'b with_bot_top =
  match a with BOT -> BOT | TOP -> TOP | Nbt x -> Nbt (f x)

let bot_top_lift2 (f:'a  -> 'b -> 'c) (a:'a with_bot_top) (b:'b with_bot_top) : 'c with_bot_top =
  match a,b with TOP,_ | _,TOP -> TOP | BOT,_ | _,BOT -> BOT | Nbt x, Nbt y -> Nbt (f x y)

let bot_top_absorb1 (f:'a -> 'b with_bot_top) (a:'a with_bot_top) : 'b with_bot_top =
  match a with BOT -> BOT | TOP -> TOP | Nbt x -> f x

let bot_top_absorb2 (f:'a  -> 'b -> 'c with_bot_top) (a:'a with_bot_top) (b:'b with_bot_top) : 'c with_bot_top =
  match a,b with TOP,_ | _,TOP -> TOP | BOT,_ | _,BOT -> BOT | Nbt x, Nbt y -> f x y

let bot_top_neutral2 (f:'a -> 'a -> 'a) (a:'a with_bot_top) (b:'a with_bot_top) : 'a with_bot_top =
  match a,b with TOP,_ -> TOP | _,TOP -> TOP | BOT,_ -> b | _,BOT -> a | Nbt x, Nbt y -> Nbt (f x y)

let bot_top_neutral_bind2 (f:'a -> 'a -> 'a with_bot_top) (a:'a with_bot_top) (b:'a with_bot_top) : 'a with_bot_top =
  match a,b with TOP,_ -> TOP | _,TOP -> TOP | BOT,_ -> b | _,BOT -> a | Nbt x, Nbt y -> f x y

let bot_top_equal (f:'a->'b->bool) (a:'a with_bot_top) (b:'b with_bot_top) : bool =
  match a,b with BOT, BOT -> true | TOP, TOP -> true | Nbt x, Nbt y -> f x y | _ -> false

let bot_top_included (f:'a->'b->bool) (a:'a with_bot_top) (b:'b with_bot_top) : bool =
  match a,b with BOT, _ -> true | _, TOP -> true | Nbt x, Nbt y -> f x y | _ -> false

let bot_top_compare (cmp:'a -> 'a -> int) (a:'a with_bot_top) (b:'a with_bot_top) : int =
  match a,b with BOT,BOT -> 0 | BOT,_ -> -1 | _,BOT -> 1 | TOP,TOP -> 0 | TOP,_ -> -1 | _,TOP -> 1 | Nbt x, Nbt y -> cmp x y

let bot_top_fprint f ch x =
  match x with
  | TOP -> Format.pp_print_string ch Top.top_string
  | BOT -> Format.pp_print_string ch Bot.bot_string
  | Nbt x -> f ch x
