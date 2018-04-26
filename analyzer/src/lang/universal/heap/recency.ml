(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Recency abstraction of heap allocations. *)

open Framework.Ast
open Framework.Domains
open Framework.Domains.Unify.Domain
open Framework.Flow
open Framework.Manager
open Framework.Eval
open Ast

let name = "universal.heap.recency"
let debug fmt = Debug.debug ~channel:name fmt

let assign_mode_of_addr addr =
  if addr.addr_uid = Pool.recent_uid then STRONG else WEAK


module Make(Sub: Framework.Domains.Stateful.DOMAIN) =
struct


  include Pool

  (*==========================================================================*)
  (**                        {2 Lattice definition}                           *)
  (*==========================================================================*)



  let unify ctx (pool1, sub1) (pool2, sub2) =
    (** FIXME: this unification is imprecise. It may destroy information about weak addresses
        present in only one environments. It needs to take into consideration optional
        dimensions.
    *)
    (pool1, sub1), (pool2, sub2)


  (*==========================================================================*)
  (**                        {2 Transfer functions}                           *)
  (*==========================================================================*)

  let init man ctx prog flow =
    ctx, set_domain_cur empty man flow

  let exec man subman ctx stmt flow = None

  let eval (man: ('a, t) manager) subman ctx  exp (flow: 'a flow) =
    let range = erange exp in
    match ekind exp with
    (* Allocation of a head address *)
    | E_alloc_addr(addr_kind, addr_range) ->
      let pool = get_domain_cur man flow in

      let recent_addr = {
        addr_kind;
        addr_range;
        addr_uid = Pool.recent_uid;
      }

      and old_addr = {
        addr_kind;
        addr_range;
        addr_uid = Pool.old_uid;
      }

      in

      let flow1 = match Pool.mem_recent recent_addr pool, Pool.mem_old old_addr pool with
        | false, false ->
          (* First time we allocate at this site, so no change to the sub-domain. *)
          flow

        | true, false ->
          (* Only a previous strong address exists =>
             Rebase the previous strong address with strong updates. *)
          man.exec ctx (mk_rebase_addr old_addr recent_addr STRONG range) flow

        | true, true ->
          (* Both strong and weak addresses exist =>
             Rebase the previous strong address with weak updates. *)
          man.exec ctx (mk_rebase_addr old_addr recent_addr WEAK range) flow


        | false, true ->
          debug "? case";
          assert false
      in

      let flow2 = set_domain_cur (add_recent recent_addr pool) man flow1 in

      oeval_singleton (Some (mk_addr recent_addr range), flow2, [])


    | _ -> None

  let ask man subman ctx query flow = None


end


let setup () =
  register_domain name (module Make)
