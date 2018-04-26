(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Pool of allocated heap addresses.

   Maintains the set of allocated addresses by distinguishing between
   recent and old allocations.
*)

open Framework.Ast
open Ast

module AttrSet = Framework.Lattices.Top_set.Make(struct type t = string let compare = compare let print = Format.pp_print_string end)

module AddrSet = Framework.Lattices.Top_set.Make
    (struct type t = addr let compare = compare_addr let print = Pp.pp_addr end)

type t = {
  recent : AddrSet.t;
  old : AddrSet.t;
}

(** For a given range and a given address kind, we distinguish the old and recent
      addresses with a fixed encoding.
*)
let old_uid = 0
let recent_uid = 1

let empty = {
  recent = AddrSet.empty;
  old = AddrSet.empty;
}

let bottom = {
  recent = AddrSet.bottom;
  old = AddrSet.bottom;
}

let is_bottom _ = false

let top = {
  recent = AddrSet.top;
  old = AddrSet.top;
}

let is_top abs = AddrSet.is_top abs.recent && AddrSet.is_top abs.old

let leq abs1 abs2 =
  AddrSet.leq abs1.recent abs2.recent &&
  AddrSet.leq abs1.old abs2.old

let mem_old addr abs = AddrSet.mem addr abs.old
let mem_recent addr abs = AddrSet.mem addr abs.recent

let add_old (addr: addr) (abs: t) : t = {abs with old = AddrSet.add addr abs.old}
let add_recent (addr: addr) (abs: t) : t = {abs with recent =AddrSet.add addr abs.recent}


let join abs1 abs2 = {
  recent = AddrSet.join abs1.recent abs2.recent;
  old = AddrSet.join abs1.old abs2.old;
}

let meet abs1 abs2 = {
  recent = AddrSet.meet abs1.recent abs2.recent;
  old = AddrSet.meet abs1.old abs2.old;
}

let widening ctx = join

let print fmt abs =
  Format.fprintf fmt "recent:@,@[<v2>  %a@]@\nold:@,@[<v2>  %a@]"
    AddrSet.print abs.recent
    AddrSet.print abs.old
