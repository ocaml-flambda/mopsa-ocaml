(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Recency abstraction of heap allocations. *)

open Framework.Essentials
open Ast

let name = "universal.heap.recency"
let debug fmt = Debug.debug ~channel:name fmt


let strong_addr_uid = 0
let weak_addr_uid = 1

let is_weak addr =
  if addr.addr_uid = strong_addr_uid then false else true

let assign_mode_of_addr addr =
  if is_weak addr then WEAK else STRONG

module Make(Sub: Framework.Domain.DOMAIN) =
struct


  (*==========================================================================*)
  (**                        {2 Lattice definition}                           *)
  (*==========================================================================*)

  module AttrSet = Framework.Lattices.Powerset.Make(struct type t = string let compare = compare let print = Format.pp_print_string end)

  module AddrSet = Framework.Lattices.Powerset.Make
      (struct type t = addr let compare = compare_addr let print = Pp.pp_addr end)

  type t = {
    recent : AddrSet.t;
    old : AddrSet.t;
  }


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

  let mem_old addr abs = AddrSet.mem addr abs.old
  let mem_recent addr abs = AddrSet.mem addr abs.recent

  let add_old (addr: addr) (abs: t) : t = {abs with old = AddrSet.add addr abs.old}
  let add_recent (addr: addr) (abs: t) : t = {abs with recent =AddrSet.add addr abs.recent}


  let leq (abs1, sub1) (abs2, sub2) =
    AddrSet.leq abs1.recent abs2.recent
    && AddrSet.leq abs1.old abs2.old,
    sub1, sub2

  let join (abs1, sub1) (abs2, sub2) =
    {
      recent = AddrSet.join abs1.recent abs2.recent;
      old = AddrSet.join abs1.old abs2.old;
    },
    sub1, sub2

  let meet (abs1, sub1) (abs2, sub2) =
    {
      recent = AddrSet.meet abs1.recent abs2.recent;
      old = AddrSet.meet abs1.old abs2.old;
    },
    sub1, sub2

  let widening ctx = join

  let print fmt abs =
    Format.fprintf fmt "recent: @[%a@]@\nold: @[%a@]@\n"
      AddrSet.print abs.recent
      AddrSet.print abs.old



  (*==========================================================================*)
  (**                        {2 Transfer functions}                           *)
  (*==========================================================================*)

  let init prog man ctx flow =
    Some (ctx, set_domain_cur empty man flow)

  let import_exec = []
  let export_exec = []

  let exec zone man subman ctx stmt flow = None

  let import_eval = []
  let export_eval = [Zone.Z_heap, Zone.Z_heap]

  let eval zpath exp man subman ctx flow =
    let range = erange exp in
    match ekind exp with
    (* Allocation of a head address *)
    | E_alloc_addr(addr_kind, addr_range) ->
      let pool = get_domain_cur man flow in

      let recent_addr = {
        addr_kind;
        addr_range;
        addr_uid = strong_addr_uid;
      }

      and old_addr = {
        addr_kind;
        addr_range;
        addr_uid = weak_addr_uid;
      }

      in

      let flow1 = match mem_recent recent_addr pool, mem_old old_addr pool with
        | false, false ->
          (* First time we allocate at this site, so no change to the sub-domain. *)
          flow

        | true, false ->
          (* Only a previous strong address exists =>
             Rebase the previous strong address with strong updates. *)
          man.exec (mk_rebase_addr old_addr recent_addr STRONG range) ctx flow

        | true, true ->
          (* Both strong and weak addresses exist =>
             Rebase the previous strong address with weak updates. *)
          man.exec (mk_rebase_addr old_addr recent_addr WEAK range) ctx flow


        | false, true ->
          debug "? case";
          assert false
      in

      let flow2 = set_domain_cur (add_recent recent_addr pool) man flow1 in

      Eval.singleton (Some (mk_addr recent_addr range)) flow2 |>
      return


    | _ -> None

  let ask : type r. r Framework.Query.query -> _ -> _ -> _ -> 'a Flow.flow -> r option =
    fun query man subman ctx flow ->
      match query with
      | Query.QAllocatedAddresses ->
        let pool = get_domain_cur man flow in
        let addrs = AddrSet.elements pool.recent @ AddrSet.elements pool.old in
        Some addrs

      | _ -> None


end


let setup () =
  Framework.Domains.Stacked.register_domain name (module Make)
