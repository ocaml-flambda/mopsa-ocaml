(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** The [Apply ∈ 𝒮 × 𝒟 → 𝒟] operator implements the classic function
    application of a stack domain on a domain *)

open Ast.All
open Core.All
open Log

module Stacked = Sig.Stacked.Lowlevel
open Stacked

module Domain = Sig.Domain.Lowlevel
open Domain

module Make
    (S:STACK)
    (D:DOMAIN)
  : DOMAIN with type t = S.t * D.t
=
struct


  (**************************************************************************)
  (**                         {2 Domain header}                             *)
  (**************************************************************************)

  type t = S.t * D.t

  include GenDomainId(
    struct
      type nonrec t = t
      let name = "framework.transformers.domain.lowlevel.apply"
    end
    )

  let interface = Core.Interface.concat S.interface D.interface

  let bottom = S.bottom, D.bottom

  let top = S.top, D.top

  let is_bottom (s,d) =
    S.is_bottom s ||
    D.is_bottom d

  let print fmt (s,d) =
    Format.fprintf fmt "%a%a"
      S.print s
      D.print d



  (**************************************************************************)
  (**                           {2 Managers}                                *)
  (**************************************************************************)

  (** Global manager of [D] *)
  let d_man (man:('a, t) Domain.man) : ('a, D.t) Domain.man = {
    man with
    get = (fun flow -> man.get flow |> snd);
    set = (fun b flow -> man.set (man.get flow |> fst, b) flow);
    get_log = (fun glog -> man.get_log glog |> Log.second);
    set_log = (fun log glog -> man.set_log (
        Log.tuple (man.get_log glog |> Log.first, log)
      ) glog);
  }

  (** Stack manager of [S] *)
  let s_man (man:('a, t) Domain.man) : ('a, S.t,D.t) Stacked.man =
    let dman = d_man man in
    {
      lattice = man.lattice;
      exec = man.exec;
      post = dman.post;
      eval = man.eval;
      ask = man.ask;

      get = (fun a -> man.get a |> fst);
      set = (fun a1 a -> man.set (a1, man.get a |> snd) a);
      get_sub = dman.get;
      set_sub = dman.set;
      get_log = (fun log -> man.get_log log |> Log.first);
      set_log = (fun l log ->
          man.set_log (
            Log.tuple (l, man.get_log log |> Log.second)
          ) log
        );
      get_sub_log = dman.get_log;
      set_sub_log = dman.set_log;
      merge_sub = D.merge;
  }

  (**************************************************************************)
  (**                      {2 Lattice operators}                            *)
  (**************************************************************************)

  let subset man ctx a a' =
    let b1, a, a' = S.subset (s_man man) ctx a a' in
    b1 && D.subset (d_man man) ctx a a'

  let join man ctx a a' =
    let a1, a, a' = S.join (s_man man) ctx a a' in
    let a2 = D.join (d_man man) ctx a a' in
    (a1,a2)

  let meet man ctx a a' =
    let a1, a, a' = S.meet (s_man man) ctx a a' in
    let a2 = D.meet (d_man man) ctx a a' in
    (a1,a2)

  let widen man ctx a a' =
    let a1, a, a', stable = S.widen (s_man man) ctx a a' in
    if not stable then
      let a2 = D.join (d_man man) ctx a a' in
      (a1,a2)
    else
      let a2 = D.widen (d_man man) ctx a a' in
      (a1,a2)


  let merge (pre1,pre2) ((a1,a2), log) ((a1',a2'), log') =
    S.merge pre1 (a1, Log.first log) (a1', Log.first log'),
    D.merge pre2 (a2, Log.second log) (a2', Log.second log')


  (**************************************************************************)
  (**                      {2 Transfer functions}                           *)
  (**************************************************************************)

  (** Initialization function *)
  let init prog man flow =
    S.init prog (s_man man) flow |>
    D.init prog (d_man man)

  (** Execution of statements *)
  let exec zone =
    match Core.Interface.sat_exec zone S.interface,
          Core.Interface.sat_exec zone D.interface
    with
    | false, false ->
      (* Both domains do not provide an [exec] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [S] provides an [exec] for such zone *)
      let f = S.exec zone in
      (fun stmt man flow ->
         f stmt (s_man man) flow
      )

    | false, true ->
      (* Only [D] provides an [exec] for such zone *)
      let f = D.exec zone in
      (fun stmt man flow ->
         f stmt (d_man man) flow
      )

    | true, true ->
      (* Both [S] and [D] provide an [exec] for such zone *)
      let f1 = S.exec zone in
      let f2 = D.exec zone in
      (fun stmt man flow ->
         match f1 stmt (s_man man) flow with
         | Some post ->
           Some post

         | None ->
           f2 stmt (d_man man) flow
      )


  (** Evaluation of expressions *)
  let eval zone =
    match Core.Interface.sat_eval zone S.interface,
          Core.Interface.sat_eval zone D.interface
    with
    | false, false ->
      (* Both domains do not provide an [eval] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [S] provides an [eval] for such zone *)
      let f = S.eval zone in
      (fun exp man flow ->
         f exp (s_man man) flow
      )

    | false, true ->
      (* Only [D] provides an [eval] for such zone *)
      let f = D.eval zone in
      (fun exp man flow ->
         f exp (d_man man) flow
      )

    | true, true ->
      (* Both [S] and [D] provide an [eval] for such zone *)
      let f1 = S.eval zone in
      let f2 = D.eval zone in
      (fun exp man flow ->
         match f1 exp (s_man man) flow with
         | Some evl -> Some evl

         | None -> f2 exp (d_man man) flow
      )


  (** Query handler *)
  let ask query man flow =
    let reply1 = S.ask query (s_man man) flow in
    let reply2 = D.ask query (d_man man) flow in
    Option.neutral2 (join_query query) reply1 reply2

  (** Reduction refinement *)
  let refine channel man flow =
    S.refine channel (s_man man) flow |>
    Core.Channel.bind @@ D.refine channel (d_man man)



end
