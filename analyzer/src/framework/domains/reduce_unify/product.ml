(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Unification-Reduction composer.*)

open Flow
open Manager
open Domain

let debug fmt = Debug.debug ~channel:"framework.domains.reduce_unify.product" fmt

module Make(Head: DOMAIN)(Tail: DOMAIN)(Reduction: Reduce.Reduction.REDUCTION) : DOMAIN =
  functor(SubDomain: Stateful.DOMAIN) ->
  struct

    module Head = Head(SubDomain)
    module Tail = Tail(SubDomain)

    type t = Head.t * Tail.t

    let bottom = Head.bottom, Tail.bottom

    let is_bottom (hd, tl) = Head.is_bottom hd || Tail.is_bottom tl

    let top = Head.top, Tail.top

    let is_top (hd, tl) = Head.is_top hd && Tail.is_top tl

    let unify ctx ((hd1, tl1), sub1) ((hd2, tl2), sub2) =
      let (hd1', sub1'), (hd2', sub2') = Head.unify ctx (hd1, sub1) (hd2, sub2) in
      let (tl1', sub1''), (tl2', sub2'') = Tail.unify ctx (tl1, sub1') (tl2, sub2') in
      ((hd1', tl1'), sub1''), ((hd2', tl2'), sub2'')

    let leq (hd1, tl1) (hd2, tl2) = Head.leq hd1 hd2 && Tail.leq tl1 tl2

    let join (hd1, tl1) (hd2, tl2) = (Head.join hd1 hd2), (Tail.join tl1 tl2)

    let meet (hd1, tl1) (hd2, tl2) = (Head.meet hd1 hd2), (Tail.meet tl1 tl2)

    let widening ctx (hd1, tl1) (hd2, tl2) =
      (Head.widening ctx hd1 hd2), (Tail.widening ctx tl1 tl2)

    let print fmt (hd, tl) =
      Format.fprintf fmt "%a%a" Head.print hd Tail.print tl

    let head_man man = {
      man with
      ax = {
        get = (fun env -> fst @@ man.ax.get env);
        set = (fun hd env -> man.ax.set (hd, snd @@ man.ax.get env) env);
      }
    }

  let tail_man man = {
    man with
    ax = {
      get = (fun env -> snd @@ man.ax.get env);
      set = (fun tl env -> man.ax.set (fst @@ man.ax.get env, tl) env);
    }
  }

  let init man subman ctx prog flow =
    let ctx, flow = Head.init (head_man man) subman ctx prog flow in
    Tail.init (tail_man man) subman ctx prog flow


  (** Execute statement on both domains and merge their post conditions *)
  let exec man subman ctx stmt flow =
    let hman = head_man man in
    let tman = tail_man man in
    debug "exec %a on head" Pp.pp_stmt stmt;
    let hout = Head.exec hman subman ctx stmt flow in
    debug "exec %a on tail" Pp.pp_stmt stmt;
    let tout = Tail.exec tman subman ctx stmt flow in
    debug "merging rflows";
    let res = Reduce.Product.merge_rflow man hman tman is_bottom bottom ctx hout tout in
    debug "done";
    res

  (** Refine a post condition with a reduction channel *)
  let refine man subman ctx channel flow =
    let hman = head_man man in
    let tman = tail_man man in
    debug "refine head";
    let hout = Head.refine hman subman ctx channel flow in
    debug "refine tail";
    let tout = Tail.refine tman subman ctx channel flow in
    debug "merging rflows";
    let rflow = Reduce.Product.merge_rflow man hman tman is_bottom bottom ctx hout tout in
    debug "done";
    rflow

  let eval man subman ctx exp flow =
    let hman = head_man man in
    let tman = tail_man man in
    let hevl = Head.eval hman subman ctx exp flow
    and tevl = Tail.eval tman subman ctx exp flow in
    Reduce.Product.merge_revals man hman tman ctx (Reduction.refine_eval man ctx exp flow) hevl tevl


  let ask man subman ctx query flow =
    let head_reply = Head.ask (head_man man) subman ctx query flow in
    let tail_reply = Tail.ask (tail_man man) subman ctx query flow in
    Query.meet query head_reply tail_reply

end
