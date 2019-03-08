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

(* Slot for a specific file descriptor *)

open Mopsa
open Universal.Ast
module Itv = Universal.Numeric.Values.Intervals.Value

type slot =
  | Bot
  | Free
  | NotFree of AddrSet.t
  | MaybeFree of AddrSet.t
  | Top

let bottom = Bot

let is_bottom s =
  s = Bot

let subset s1 s2 =
  match s1, s2 with
  | Bot, _ -> true
  | _, Bot -> false

  | _, Top -> true
  | Top, _ -> false

  | Free, Free
  | Free, MaybeFree _ -> true

  | NotFree a1, NotFree a2
  | NotFree a1, MaybeFree a2 -> AddrSet.subset a1 a2

  | MaybeFree a1, MaybeFree a2 -> AddrSet.subset a1 a2

  | _ -> false

let join s1 s2 =
  match s1, s2 with
  | Bot, s
  | s, Bot -> s

  | Top, _
  | _, Top -> Top

  | Free, Free -> Free

  | Free, MaybeFree a
  | MaybeFree a, Free -> MaybeFree a

  | NotFree a1, NotFree a2 -> NotFree (AddrSet.union a1 a2)

  | NotFree a, Free
  | Free, NotFree a -> MaybeFree a

  | NotFree a1, MaybeFree a2
  | MaybeFree a1, NotFree a2 -> MaybeFree (AddrSet.union a1 a2)

  | MaybeFree a1, MaybeFree a2 -> MaybeFree (AddrSet.union a1 a2)

let canonize s =
  match s with
  | MaybeFree a ->
    if AddrSet.is_empty a then Free
    else s

  | NotFree a ->
    if AddrSet.is_empty a then Bot
    else s

  | _ -> s

let meet s1 s2 =
  match s1, s2 with
  | Bot, _
  | _, Bot -> Bot

  | Top, s
  | s, Top -> s

  | Free, Free -> Free

  | Free, MaybeFree _
  | MaybeFree _, Free -> Free

  | NotFree a1, NotFree a2 -> NotFree (AddrSet.inter a1 a2) |>
                              canonize

  | NotFree _, Free
  | Free, NotFree _ -> Bot

  | NotFree a1, MaybeFree a2
  | MaybeFree a1, NotFree a2 -> NotFree (AddrSet.inter a1 a2) |>
                                canonize

  | MaybeFree a1, MaybeFree a2 -> MaybeFree (AddrSet.inter a1 a2) |>
                                  canonize

let print fmt s =
  match s with
  | Bot -> Format.fprintf fmt "⊥"
  | Top -> Format.fprintf fmt "⊤"
  | Free -> Format.fprintf fmt "🔓"
  | NotFree a -> Format.fprintf fmt "🔒 : %a" AddrSet.print a
  | MaybeFree a -> Format.fprintf fmt "❓ : %a" AddrSet.print a


(** Insert an address in a slot. Returns the new state of the
    slot after insertion, or its state when the insertion is
    not possible.
*)
let insert addr (s:slot) : slot * slot =
  match s with
  | Bot         -> Bot, Bot
  | Top         -> Top, Top
  | Free        -> NotFree (AddrSet.singleton addr), Bot
  | NotFree a   -> Bot, s
  | MaybeFree a -> NotFree (AddrSet.add addr a), NotFree a


(** Check whether [addr] is in the slot. Returns an abstract state when
    [addr] is in the slot, and another abstract state when [addr] is not
    in the slot. *)
let mem addr s : slot * slot =
  match s with
  | Bot -> Bot, Bot
  | Top -> Top, Top
  | Free -> Bot, Free
  | NotFree a ->
    if AddrSet.mem addr a then NotFree a, MaybeFree (AddrSet.remove addr a) |> canonize
    else Bot, NotFree a
  | MaybeFree a ->
    if AddrSet.mem addr a then NotFree a, MaybeFree (AddrSet.remove addr a) |> canonize
    else Bot, MaybeFree a

let get s : addr list =
  match s with
  | Bot -> []
  | Top -> raise Top.Found_TOP
  | Free -> []
  | NotFree a -> AddrSet.elements a
  | MaybeFree a -> AddrSet.elements a

(** Remove an address from the slot *)
let remove addr s =
  match s with
  | Bot | Free -> s
  | Top -> Top
  | NotFree a | MaybeFree a ->
    if not (AddrSet.mem addr a) then s
    else
      let a' = AddrSet.remove addr a in
      if AddrSet.is_empty a' then Free
      else MaybeFree a'
