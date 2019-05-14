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


(** The operator [Sequence ∈ 𝒟 × 𝒟 → 𝒟] operator combines two domains by
    "concatenating" their transfer functions (i.e. return the result of the
    first answering domain).
*)


open Ast.All
open Core.All
open Sig.Domain.Intermediate
open Log


module Make
    (D1:DOMAIN)
    (D2:DOMAIN)
  : DOMAIN with type t = D1.t * D2.t
=
struct

  (**************************************************************************)
  (**                         {2 Domain header}                             *)
  (**************************************************************************)

  type t = D1.t * D2.t

  include GenDomainId(
    struct
      type typ = t
      let name = "framework.operators.sequence.domain"
    end
    )

  let interface = Core.Interface.concat D1.interface D2.interface

  let bottom = D1.bottom, D2.bottom

  let top = D1.top, D2.top


  (**************************************************************************)
  (**                           {2 Managers}                                *)
  (**************************************************************************)

  (** Global manager of [D1] *)
  let d1_man (man:('a, t) man) : ('a, D1.t) man = {
    man with
    get = (fun flow -> man.get flow |> fst);
    set = (fun a flow -> man.set (a, man.get flow |> snd) flow);
    get_log = (fun glog -> man.get_log glog |> Log.first);
    set_log = (fun log glog -> man.set_log (
        Log.tuple (log, man.get_log glog |> Log.second)
      ) glog);
  }

  (** Global manager of [D] *)
  let d2_man (man:('a, t) man) : ('a, D2.t) man = {
    man with
    get = (fun flow -> man.get flow |> snd);
    set = (fun b flow -> man.set (man.get flow |> fst, b) flow);
    get_log = (fun glog -> man.get_log glog |> Log.second);
    set_log = (fun log glog -> man.set_log (
        Log.tuple (man.get_log glog |> Log.first, log)
      ) glog);
  }



  (**************************************************************************)
  (**                      {2 Lattice operators}                            *)
  (**************************************************************************)

  let is_bottom (a1,a2) =
    D1.is_bottom a1 ||
    D2.is_bottom a2

  let print fmt (a1,a2) =
    Format.fprintf fmt "%a%a"
      D1.print a1
      D2.print a2

  let subset (a1,a2) (a1',a2') =
    D1.subset a1 a1' &&
    D2.subset a2 a2'

  let join (a1,a2) (a1',a2') =
    D1.join a1 a1',
    D2.join a2 a2'

  let meet (a1,a2) (a1',a2') =
    D1.meet a1 a1',
    D2.meet a2 a2'

  let widen ctx (a1,a2) (a1',a2') =
    D1.widen ctx a1 a1',
    D2.widen ctx a2 a2'

  let merge (pre1,pre2) ((a1,a2), log) ((a1',a2'), log') =
    D1.merge pre1 (a1, Log.first log) (a1', Log.first log'),
    D2.merge pre2 (a2, Log.second log) (a2', Log.second log')


  (**************************************************************************)
  (**                      {2 Transfer functions}                           *)
  (**************************************************************************)

  (** Initialization procedure *)
  let init prog man flow =
    D1.init prog (d1_man man) flow |>
    D2.init prog (d2_man man)

  (** Execution of statements *)
  let exec zone =
    match Core.Interface.sat_exec zone D1.interface,
          Core.Interface.sat_exec zone D2.interface
    with
    | false, false ->
      (* Both domains do not provide an [exec] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [D1] provides an [exec] for such zone *)
      let f = D1.exec zone in
      (fun stmt man flow ->
         f stmt (d1_man man) flow
      )

    | false, true ->
      (* Only [D2] provides an [exec] for such zone *)
      let f = D2.exec zone in
      (fun stmt man flow ->
         f stmt (d2_man man) flow
      )

    | true, true ->
      (* Both [D1] and [D2] provide an [exec] for such zone *)
      let f1 = D1.exec zone in
      let f2 = D2.exec zone in
      (fun stmt man flow ->
         match f1 stmt (d1_man man) flow with
         | Some post ->
           Option.return post

         | None ->
           f2 stmt (d2_man man) flow
      )


  (** Evaluation of expressions *)
  let eval zone =
    match Core.Interface.sat_eval zone D1.interface,
          Core.Interface.sat_eval zone D2.interface
    with
    | false, false ->
      (* Both domains do not provide an [eval] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [D1] provides an [eval] for such zone *)
      let f = D1.eval zone in
      (fun exp man flow ->
         f exp (d1_man man) flow
      )

    | false, true ->
      (* Only [D2] provides an [eval] for such zone *)
      let f = D2.eval zone in
      (fun exp man flow ->
         f exp (d2_man man) flow
      )

    | true, true ->
      (* Both [D1] and [D2] provide an [eval] for such zone *)
      let f1 = D1.eval zone in
      let f2 = D2.eval zone in
      (fun exp man flow ->
         match f1 exp (d1_man man) flow with
         | Some evl -> Some evl

         | None -> f2 exp (d2_man man) flow
      )


  (** Query handler *)
  let ask query man flow =
    let reply1 = D1.ask query (d1_man man) flow in
    let reply2 = D2.ask query (d2_man man) flow in
    Option.neutral2 (join_query query) reply1 reply2


  (** Reduction refinement *)
  let refine channel man flow =
    D1.refine channel (d1_man man) flow |>
    Core.Channel.bind @@ D2.refine channel (d2_man man)


end
