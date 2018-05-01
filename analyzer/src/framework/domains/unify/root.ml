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

module Make(UniDomain: Domain.DOMAIN)(SubDomain: Stateful.DOMAIN) =
struct

  module UniDomain = UniDomain(SubDomain)

  type t = UniDomain.t * SubDomain.t

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


  let bottom = UniDomain.bottom, SubDomain.bottom

  let is_bottom (hd, tl) = UniDomain.is_bottom hd || SubDomain.is_bottom tl

  let top = UniDomain.top, SubDomain.top

  let is_top (hd, tl) = UniDomain.is_top hd && SubDomain.is_top tl

  let leq a1 a2 =
    let (d1', s1'), (d2', s2') = UniDomain.unify Context.empty a1 a2 in
    UniDomain.leq d1' d2' && SubDomain.leq s1' s2'

  let join a1 a2 =
    let (d1', s1'), (d2', s2') = UniDomain.unify Context.empty a1 a2 in
    UniDomain.join d1' d2', SubDomain.join s1' s2'

  let meet a1 a2 =
    let (d1', s1'), (d2', s2') = UniDomain.unify Context.empty a1 a2 in
    UniDomain.meet d1' d2', SubDomain.join s1' s2'

  let widening ctx a1 a2 =
    let (d1', s1'), (d2', s2') = UniDomain.unify ctx a1 a2 in
    UniDomain.widening ctx d1' d2', SubDomain.widening ctx s1' s2'


  let print fmt (hd, tl) =
    Format.fprintf fmt "%a%a" UniDomain.print hd SubDomain.print tl


  and exec man ctx stmt gabs =
    let gabs' = UniDomain.exec (head_man man) (tail_man man) ctx stmt gabs in
    match gabs' with
    | None -> SubDomain.exec (tail_man man) ctx stmt gabs
    | _ -> gabs'


  and eval man ctx exp gabs =
    let head_ev = UniDomain.eval (head_man man) (tail_man man) ctx exp gabs in
    match head_ev with
    | None -> SubDomain.eval (tail_man man) ctx exp gabs
    | _ -> head_ev


  and ask man ctx query gabs =
    let head_reply = UniDomain.ask (head_man man) (tail_man man) ctx query gabs in
    let tail_reply = SubDomain.ask (tail_man man) ctx query gabs in
    Query.join query head_reply tail_reply

  let init man ctx prog fa =
    let ctx, fa = SubDomain.init (tail_man man) ctx prog fa in
    UniDomain.init (head_man man) ctx prog fa


end
