(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Root node of a unification domain. *)

open Flow
open Manager
open Domain

let debug fmt = Debug.debug ~channel:"framework.domains.unify.root" fmt

module Make(Domain: Domain.DOMAIN)(SubDomain: Stateful.DOMAIN) =
struct

  type t = Domain.t * SubDomain.t

  let head_man man = {
    man with ax = {
      get = (fun gabs -> fst @@ man.ax.get gabs);
      set = (fun hd gabs -> man.ax.set (hd, snd @@ man.ax.get gabs) gabs);
    }
  }

  let tail_man man = {
    man with ax = {
      get = (fun gabs -> snd @@ man.ax.get gabs);
      set = (fun tl gabs -> man.ax.set (fst @@ man.ax.get gabs, tl) gabs);
    }
  }


  let local_man = mk_local_manager (module SubDomain : Stateful.DOMAIN with type t = SubDomain.t)

  let bottom = Domain.bottom, SubDomain.bottom

  let is_bottom (hd, tl) = Domain.is_bottom hd || SubDomain.is_bottom tl

  let top = Domain.top, SubDomain.top

  let is_top (hd, tl) = Domain.is_top hd && SubDomain.is_top tl


  let unify ctx (d1, s1) (d2, s2) =
    let flow1 = set_domain_cur s1 local_man local_man.flow.bottom in
    let flow2 = set_domain_cur s2 local_man local_man.flow.bottom in
    let (d1, flow1), (d2, flow2) = Domain.unify local_man ctx (d1, flow1) (d2, flow2) in
    let s1 = get_domain_cur local_man flow1 in
    let s2 = get_domain_cur local_man flow2 in
    (d1, s1), (d2, s2)

  let leq a1 a2 =
    let (d1', s1'), (d2', s2') = unify Context.empty a1 a2 in
    Domain.leq d1' d2' && SubDomain.leq s1' s2'

  let join a1 a2 =
    let (d1', s1'), (d2', s2') = unify Context.empty a1 a2 in
    Domain.join d1' d2', SubDomain.join s1' s2'

  let meet a1 a2 =
    let (d1', s1'), (d2', s2') = unify Context.empty a1 a2 in
    Domain.meet d1' d2', SubDomain.join s1' s2'

  let widening ctx a1 a2 =
    let (d1', s1'), (d2', s2') = unify ctx a1 a2 in
    Domain.widening ctx d1' d2', SubDomain.widening ctx s1' s2'


  let print fmt (hd, tl) =
    Format.fprintf fmt "%a%a" Domain.print hd SubDomain.print tl


  and exec man ctx stmt gabs =
    let gabs' = Domain.exec (head_man man) (tail_man man) (SubDomain.exec (tail_man man) ctx) ctx stmt gabs in
    match gabs' with
    | None -> SubDomain.exec (tail_man man) ctx stmt gabs
    | _ -> gabs'


  and eval man ctx exp gabs =
    let head_ev = Domain.eval (head_man man) (tail_man man) ctx exp gabs in
    match head_ev with
    | None -> SubDomain.eval (tail_man man) ctx exp gabs
    | _ -> head_ev


  and ask man ctx query gabs =
    let head_reply = Domain.ask (head_man man) (tail_man man) ctx query gabs in
    let tail_reply = SubDomain.ask (tail_man man) ctx query gabs in
    Query.join query head_reply tail_reply

  let init man ctx prog fa =
    let ctx, fa = Domain.init (head_man man) ctx prog fa in
    SubDomain.init (tail_man man) ctx prog fa


end
