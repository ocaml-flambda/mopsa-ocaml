(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(* Availability info of a slot in the file table *)

open Mopsa
open Universal.Ast
module Itv = Universal.Numeric.Values.Intervals.Value

type slot =
  | Bot
  | Free
  | NotFree of AddrSet.t
  | MaybeFree of AddrSet.t

let subset s1 s2 =
  match s1, s2 with
  | Bot, _ -> true
  | _, Bot -> false

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
  | MaybeFree a
  | NotFree a ->
    if AddrSet.is_empty a then Bot
    else s
  | _ -> s

let meet s1 s2 =
  (
    match s1, s2 with
    | Bot, _
    | _, Bot -> Bot

    | Free, Free -> Free

    | Free, MaybeFree _
    | MaybeFree _, Free -> Free

    | NotFree a1, NotFree a2 -> NotFree (AddrSet.inter a1 a2)

    | NotFree _, Free
    | Free, NotFree _ -> Bot

    | NotFree a1, MaybeFree a2
    | MaybeFree a1, NotFree a2 -> NotFree (AddrSet.inter a1 a2)

    | MaybeFree a1, MaybeFree a2 -> MaybeFree (AddrSet.inter a1 a2)
  )
  |> canonize

let print fmt s =
  match s with
  | Bot -> Format.fprintf fmt "⊥"
  | Free -> Format.fprintf fmt "⚪"
  | NotFree a -> Format.fprintf fmt "⚫ : %a" AddrSet.print a
  | MaybeFree a -> Format.fprintf fmt "◐ : %a" AddrSet.print a


(** Insert an address in a slot. Returns the new state of the 
    slot after insertion, or its state when the insertion is 
    not possible. 
*)
let insert addr (s:slot) : slot option * slot option =
  match s with
  | Bot         -> None, None
  | Free        -> Some (NotFree (AddrSet.singleton addr)), None
  | NotFree a   -> None, Some s
  | MaybeFree a ->
    Some (NotFree (AddrSet.add addr a)), Some (NotFree a)

let mem addr s =
  match s with
  | Bot | Free -> false
  | NotFree a | MaybeFree a ->
    AddrSet.mem addr a

let addr_opt s =
  match s with
  | Bot | Free -> None
  | NotFree a | MaybeFree a -> Some a

let remove_addr addr s =
  match s with
  | Bot | Free -> s
  | NotFree a | MaybeFree a ->
    let a' = AddrSet.remove addr a in
    if AddrSet.is_empty a' then
      Free
    else
      MaybeFree a'
