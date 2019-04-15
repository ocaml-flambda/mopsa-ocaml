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


module Make
    (S:Sig.Stacked.Lowlevel.STACK)
    (D:Sig.Domain.Lowlevel.DOMAIN)
  : Sig.Domain.Lowlevel.DOMAIN with type t = S.t * D.t
=
struct


  (**************************************************************************)
  (**                         {2 Domain header}                             *)
  (**************************************************************************)

  type t = S.t * D.t

  include GenDomainId(
    struct
      type typ = t
      let name = "framework.operators.apply"
    end
    )

  let interface = Core.Interface.concat S.interface D.interface

  let bottom = S.bottom, D.bottom

  let top = S.top, D.top


  (**************************************************************************)
  (**                           {2 Managers}                                *)
  (**************************************************************************)

  (** Global manager of [S] *)
  let s_man (man:('a, t) man) : ('a, S.t) man = {
    man with
    get = (fun flow -> man.get flow |> fst);
    set = (fun a flow -> man.set (a, man.get flow |> snd) flow);
    get_log = (fun glog -> man.get_log glog |> Log.first);
    set_log = (fun log glog -> man.set_log (
        Log.tuple (log, man.get_log glog |> Log.second)
      ) glog);
  }

  (** Global manager of [D] *)
  let d_man (man:('a, t) man) : ('a, D.t) man = {
    man with
    get = (fun flow -> man.get flow |> snd);
    set = (fun b flow -> man.set (man.get flow |> fst, b) flow);
    get_log = (fun glog -> man.get_log glog |> Log.second);
    set_log = (fun log glog -> man.set_log (
        Log.tuple (man.get_log glog |> Log.first, log)
      ) glog);
  }

  (** Sub-tree manager of [S] *)
  let s_sman (man:('a,t) man) : ('a, D.t) man =
    let dman = d_man man in
    {
      dman with
      post = (fun ?(zone=any_zone) stmt flow ->
          dman.post ~zone stmt flow |>
          log_post_stmt stmt dman
        );
    }

  (**************************************************************************)
  (**                      {2 Lattice operators}                            *)
  (**************************************************************************)

  let is_bottom (man:('a,t) man) a =
    S.is_bottom (s_man man) (d_man man) a ||
    D.is_bottom (d_man man) a

  let print man fmt a =
    Format.fprintf fmt "%a%a"
      (S.print @@ s_man man) a
      (D.print @@ d_man man) a

  let subset man a a' =
    let b1, a, a' = S.subset (s_man man) (s_sman man) a a' in
    b1 && D.subset (d_man man) a a'

  let join man a a' =
    let a1, a, a' = S.join (s_man man) (s_sman man) a a' in
    let a2 = D.join (d_man man) a a' in
    (a1,a2)

  let meet man a a' =
    let a1, a, a' = S.meet (s_man man) (s_sman man) a a' in
    let a2 = D.meet (d_man man) a a' in
    (a1,a2)

  let widen man ctx a a' =
    let a1, a, a', stable = S.widen (s_man man) (s_sman man) ctx a a' in
    if not stable then
      let a2 = D.join (d_man man) a a' in
      (a1,a2)
    else
      let a2 = D.widen (d_man man) ctx a a' in
      (a1,a2)


  let merge man pre (post, log) (post', log') =
    S.merge (s_man man) pre (post, log) (post', log'),
    D.merge (d_man man) pre (post, log) (post', log')


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
         f stmt (s_man man) (s_sman man) flow |>
         Option.lift @@ log_post_stmt stmt (s_man man)
      )

    | false, true ->
      (* Only [D] provides an [exec] for such zone *)
      let f = D.exec zone in
      (fun stmt man flow ->
         f stmt (d_man man) flow |>
         Option.lift @@ log_post_stmt stmt (d_man man)
      )

    | true, true ->
      (* Both [S] and [D] provide an [exec] for such zone *)
      let f1 = S.exec zone in
      let f2 = D.exec zone in
      (fun stmt man flow ->
         match f1 stmt (s_man man) (s_sman man) flow with
         | Some post ->
           Some (log_post_stmt stmt (s_man man) post)

         | None ->
           f2 stmt (d_man man) flow |>
           Option.lift (log_post_stmt stmt (d_man man))
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
    Option.neutral2 (Query.join query) reply1 reply2

  (** Reduction refinement *)
  let refine channel man flow =
    S.refine channel (s_man man) (s_sman man) flow |>
    Core.Channel.bind @@ D.refine channel (d_man man)



end
