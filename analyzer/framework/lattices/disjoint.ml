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


(** Lattice of disjoint elements *)

open Core.All



module Make(Left:LATTICE)(Right:LATTICE)  =
struct
  open Utils_core

  type ('a, 'b) disjoint = L of 'a | R of 'b

  type t = ((Left.t, Right.t) disjoint) Bot_top.with_bot_top

  let bottom : t = BOT

  let top : t = TOP
 
  let embed_left (x: Left.t) : t = Nbt (L x)
  let embed_right (x: Right.t) : t = Nbt (R x)

  let is_bottom (x:t) : bool = 
    match x with
    | BOT -> true 
    | Nbt (L x) -> Left.is_bottom x
    | Nbt (R x) -> Right.is_bottom x
    | TOP -> false 

  let subset (x:t) (y:t) : bool = 
    match x, y with 
    | BOT, _ -> true 
    | _, TOP -> true 
    | Nbt (L _), Nbt (R _) -> false
    | Nbt (R _), Nbt (L _) -> false 
    | Nbt (L x), Nbt (L y) -> Left.subset x y 
    | Nbt (R x), Nbt (R y) -> Right.subset x y 
    | _ -> false 

  let join (x: t) (y: t) : t =
    match x, y with 
    | TOP, _ -> TOP 
    | _, TOP -> TOP
    | BOT, y -> y
    | x, BOT -> x
    | Nbt (L _), Nbt (R _) -> TOP
    | Nbt (R _), Nbt (L _) -> TOP
    | Nbt (L x), Nbt (L y) -> Nbt (L (Left.join x y))
    | Nbt (R x), Nbt (R y) -> Nbt (R (Right.join x y))
      
  let meet (x: t) (y: t) : t =
    match x, y with 
    | TOP, y -> y 
    | x, TOP -> x
    | BOT, _ -> BOT
    | _, BOT -> BOT
    | Nbt (L _), Nbt (R _) -> BOT
    | Nbt (R _), Nbt (L _) -> BOT
    | Nbt (L x), Nbt (L y) -> Nbt (L (Left.meet x y))
    | Nbt (R x), Nbt (R y) -> Nbt (R (Right.meet x y))
      
  let widen ctx (x: t) (y: t) : t = 
    match x, y with 
    | TOP, _ -> TOP 
    | _, TOP -> TOP
    | BOT, y -> y
    | x, BOT -> x
    | Nbt (L _), Nbt (R _) -> TOP
    | Nbt (R _), Nbt (L _) -> TOP
    | Nbt (L x), Nbt (L y) -> Nbt (L (Left.widen ctx x y))
    | Nbt (R x), Nbt (R y) -> Nbt (R (Right.widen ctx x y))
    
  let print (printer: Print.printer) (x: t) : unit =
    match x with 
    | TOP -> Print.pp_string printer Utils_core.Top.top_string
    | BOT -> Print.pp_string printer Utils_core.Bot.bot_string
    | Nbt (L x) -> Left.print printer x
    | Nbt (R x) -> Right.print printer x

end
