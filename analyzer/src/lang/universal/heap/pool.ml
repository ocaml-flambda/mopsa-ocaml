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
   recent and old allocations. It is implemented as a partial map to attach
   a set of attributes to allocated addresses.
*)

open Framework.Ast
open Ast

module AttrSet = Framework.Lattices.Top_set.Make(struct type t = string let compare = compare let print = Format.pp_print_string end)

module AddrMap = Framework.Lattices.Partial_map.Make
    (struct type t = addr let compare = compare_addr let print = Pp.pp_addr end)
    (struct include AttrSet let init = empty end)

type t = {
  recent : AddrMap.t;
  old : AddrMap.t;
}

(** For a given range and a given address kind, we distinguish the old and recent
      addresses with a fixed encoding.
*)
let old_uid = 0
let recent_uid = 1

let is_weak addr =
  addr.addr_uid = old_uid


let empty = {
  recent = AddrMap.empty;
  old = AddrMap.empty;
}

let bottom = {
  recent = AddrMap.bottom;
  old = AddrMap.bottom;
}

let top = {
  recent = AddrMap.top;
  old = AddrMap.top;
}

let leq abs1 abs2 =
  AddrMap.leq abs1.recent abs2.recent &&
  AddrMap.leq abs1.old abs2.old

let mem_old addr abs = AddrMap.mem addr abs.old
let mem_recent addr abs = AddrMap.mem addr abs.recent

let find_old addr abs = AddrMap.find addr abs.old
let find_recent addr abs = AddrMap.find addr abs.recent

let add_old (addr: addr) (attrs: AttrSet.t) (abs: t) : t = {abs with old = AddrMap.add addr attrs abs.old}
let add_recent (addr: addr) (attrs: AttrSet.t) (abs: t) : t = {abs with recent =AddrMap.add addr attrs abs.recent}

let add_attribute addr attr abs =
  if is_weak addr then
    let attrs = find_old addr abs in
    add_old addr (AttrSet.add attr attrs) abs
  else
    let attrs = find_recent addr abs in
    add_recent addr (AttrSet.add attr attrs) abs

let join abs1 abs2 = {
  recent = AddrMap.join abs1.recent abs2.recent;
  old = AddrMap.join abs1.old abs2.old;
}

let meet abs1 abs2 = {
  recent = AddrMap.meet abs1.recent abs2.recent;
  old = AddrMap.meet abs1.old abs2.old;
}

let widening = join

let print fmt abs =
  Format.fprintf fmt "recent:@,@[<v2>  %a@]@\nold:@,@[<v2>  %a@]"
    AddrMap.print abs.recent
    AddrMap.print abs.old
