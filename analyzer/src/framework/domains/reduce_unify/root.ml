(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Root domain of a reduced product with unification. *)

open Flow
open Manager
open Domain

let debug fmt = Debug.debug ~channel:"framework.domains.reduce_unify.root" fmt

module MakeReduce(RedUniDomain: Domain.DOMAIN)(SubDomain: Stateful.DOMAIN) : Reduce.Domain.DOMAIN =
struct

  module RedUniDomain = RedUniDomain(SubDomain)

  type t = RedUniDomain.t * SubDomain.t

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


  let bottom = RedUniDomain.bottom, SubDomain.bottom

  let is_bottom (hd, tl) = RedUniDomain.is_bottom hd || SubDomain.is_bottom tl

  let top = RedUniDomain.top, SubDomain.top

  let is_top (hd, tl) = RedUniDomain.is_top hd && SubDomain.is_top tl

  let leq a1 a2 =
    let (d1', s1'), (d2', s2') = RedUniDomain.unify Context.empty a1 a2 in
    RedUniDomain.leq d1' d2' && SubDomain.leq s1' s2'

  let join a1 a2 =
    let (d1', s1'), (d2', s2') = RedUniDomain.unify Context.empty a1 a2 in
    RedUniDomain.join d1' d2', SubDomain.join s1' s2'

  let meet a1 a2 =
    let (d1', s1'), (d2', s2') = RedUniDomain.unify Context.empty a1 a2 in
    RedUniDomain.meet d1' d2', SubDomain.join s1' s2'

  let widening ctx a1 a2 =
    let (d1', s1'), (d2', s2') = RedUniDomain.unify ctx a1 a2 in
    RedUniDomain.widening ctx d1' d2', SubDomain.widening ctx s1' s2'


  let print fmt (hd, tl) =
    Format.fprintf fmt "%a%a" RedUniDomain.print hd SubDomain.print tl


  let exec man ctx stmt flow =
    match RedUniDomain.exec (head_man man) (tail_man man) ctx stmt flow with
    | Some rflow -> Some rflow
    | None ->
      match SubDomain.exec (tail_man man) ctx stmt flow with
      | None -> None
      | Some flow -> return_flow flow

  let refine man ctx channel gabs = RedUniDomain.refine (head_man man) (tail_man man) ctx channel gabs

  let eval man ctx exp flow =
    match RedUniDomain.eval (head_man man) (tail_man man) ctx exp flow with
    | Some revals -> Some revals
    | None ->
      match SubDomain.eval (tail_man man) ctx exp flow with
      | None -> None
      | Some evals ->
        return_evals evals

  let ask man ctx query flow =
    match RedUniDomain.ask (head_man man) (tail_man man) ctx query flow with
    | Some rpl -> Some rpl
    | None -> SubDomain.ask (tail_man man) ctx query flow

  let init man ctx prog flow =
    let ctx', flow' = RedUniDomain.init (head_man man) ctx prog flow in
    SubDomain.init (tail_man man) ctx' prog flow'

end


module Make(RedUniDomain: Domain.DOMAIN)(SubDomain: Stateful.DOMAIN) : Stateful.DOMAIN =
struct

  module D = MakeReduce(RedUniDomain)(SubDomain)
  module R = Reduce.Root.Make(D)
  include R

end
