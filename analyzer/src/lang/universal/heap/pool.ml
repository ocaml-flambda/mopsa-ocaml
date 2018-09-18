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

open Framework.Essentials
open Framework.Ast
open Ast

let uid_ref = ref 0
let get_fresh () =
  let rep = !uid_ref in
  let () = incr uid_ref in
  rep

module AddrInfo = struct
  open Iterators.Interproc.Inlining
  type t = call_stack * range * int
  (** The following compare function can be modified so as to modify
      the granularity of the recency abstraction. *)
  let compare (cs, r, i) (cs', r', i') = Compare.compose
      [
        (fun () -> compare_call_stack cs cs');
        (fun () -> compare_range r r');
        (fun () -> (-) i i')
      ]
  let print fmt (cs, r, i) =
    Format.fprintf fmt "(%a, %a, %a)"
      pp_call_stack cs
      pp_range r
      Format.pp_print_int i
end

module AddrUid =
struct
  type t = int
  let compare = compare
  let print = Format.pp_print_int
end

module Equiv = Equiv.Make(AddrInfo)(AddrUid)

let get_id_equiv (info: AddrInfo.t) (e: Equiv.t) =
  try
    Equiv.find_l info e, e
  with
  | Not_found ->
    let x = get_fresh () in
    x, Equiv.add (info, x) e

type ('a, _) Annotation.key +=
  | KAddr : ('a, Equiv.t) Annotation.key

let get_id_flow (info: AddrInfo.t) (f: 'a flow) : (int * 'a flow) =
  let e = Flow.get_annot KAddr f in
  let x, e = get_id_equiv info e in
  (x, Flow.set_annot KAddr e f)


module AddrSet = Framework.Lattices.Powerset.Make(
  struct
    type t = addr
    let compare = compare_addr
    let print = pp_addr
  end
  )


type t = {
  recent : AddrSet.t;
  old : AddrSet.t;
}


(** For a given range and a given address kind, we distinguish the old
    and recent addresses with a fixed encoding.  *)

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

let subset abs1 abs2 =
  AddrSet.subset abs1.recent abs2.recent &&
   AddrSet.subset abs1.old abs2.old


let mem_old addr abs = AddrSet.mem addr abs.old
let mem_recent addr abs = AddrSet.mem addr abs.recent

let add_old (addr: addr) (abs: t) : t = {abs with old = AddrSet.add addr abs.old}
let add_recent (addr: addr) (abs: t) : t = {abs with recent =AddrSet.add addr abs.recent}


let join (annot: 'a annot) abs1 abs2 = {
  recent = AddrSet.join annot abs1.recent abs2.recent;
  old = AddrSet.join annot abs1.old abs2.old;
}

let meet (annot: 'a annot) abs1 abs2 = {
  recent = AddrSet.meet annot abs1.recent abs2.recent;
  old = AddrSet.meet annot abs1.old abs2.old;
}

let widen = join

let print fmt abs =
  Format.fprintf fmt "recent: @[%a@]@\nold: @[%a@]@\n"
    AddrSet.print abs.recent
    AddrSet.print abs.old
