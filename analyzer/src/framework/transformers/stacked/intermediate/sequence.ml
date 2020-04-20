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


open Ast.All
open Core.All
open Log
open Sig.Stacked.Intermediate

(** The [Sequence.Stack ∈ 𝒮 × 𝒮 → 𝒮] operator combines two stacks over the
    same sub-abstraction, by "concatenating" their transfer functions (i.e.
    return the result of the first answering domain).
*)
module Make
    (S1:STACK)
    (S2:STACK)
  : STACK with type t = S1.t * S2.t
=
struct

  (**************************************************************************)
  (**                         {2 Domain header}                             *)
  (**************************************************************************)

  type t = S1.t * S2.t

  include GenDomainId(
    struct
      type nonrec t = t
      let name = "framework.transformers.stacked.intermediate.sequence"
    end
    )

  let interface = Core.Interface.concat S1.interface S2.interface

  let alarms = S1.alarms @ S2.alarms |> List.sort_uniq compare

  let bottom = S1.bottom, S2.bottom

  let top = S1.top, S2.top


  (**************************************************************************)
  (**                           {2 Managers}                                *)
  (**************************************************************************)

  (** Global manager of [S1] *)
  let s1_man (man:('a, t, 's) man) : ('a, S1.t, 's) man = {
    man with
    get = (fun flow -> man.get flow |> fst);
    set = (fun a flow -> man.set (a, man.get flow |> snd) flow);
    get_log = (fun glog -> man.get_log glog |> Log.get_left_log);
    set_log = (fun log glog ->
        man.set_log (
          Log.mk_log [] log (man.get_log glog |> Log.get_right_log)
        ) glog
      );
  }

  (** Global manager of [D] *)
  let s2_man (man:('a, t, 's) man) : ('a, S2.t, 's) man = {
    man with
    get = (fun flow -> man.get flow |> snd);
    set = (fun b flow -> man.set (man.get flow |> fst, b) flow);
    get_log = (fun glog -> man.get_log glog |> Log.get_right_log);
    set_log = (fun log glog ->
        man.set_log (
          Log.mk_log [] (man.get_log glog |> Log.get_left_log) log
        ) glog
      );
  }


  (**************************************************************************)
  (**                      {2 Lattice operators}                            *)
  (**************************************************************************)

  let is_bottom (a1,a2) =
    S1.is_bottom a1 ||
    S2.is_bottom a2

  let print fmt (a1,a2) =
    Format.fprintf fmt "%a%a"
      S1.print a1
      S2.print a2

  let subset man ctx ((a1,a2),s) ((a1',a2'),s') =
    let b1, s, s' = S1.subset (s1_man man) ctx (a1,s) (a1',s') in
    let b2, s, s' = S2.subset (s2_man man) ctx (a2,s) (a2',s') in
    b1 && b2, s, s'

  let join man ctx ((a1,a2),s) ((a1',a2'),s') =
    let a1, s, s' = S1.join (s1_man man) ctx (a1,s) (a1',s') in
    let a2, s, s' = S2.join (s2_man man) ctx (a2,s) (a2',s') in
    (a1,a2), s, s'

  let meet man ctx ((a1,a2),s) ((a1',a2'),s') =
    let a1, s, s' = S1.meet (s1_man man) ctx (a1,s) (a1',s') in
    let a2, s, s' = S2.meet (s2_man man) ctx (a2,s) (a2',s') in
    (a1,a2), s, s'

  let widen man ctx ((a1,a2),s) ((a1',a2'),s') =
    let a1, s, s', stable1 = S1.widen (s1_man man) ctx (a1,s) (a1',s') in
    let a2, s, s', stable2 = S2.widen (s2_man man) ctx (a2,s) (a2',s') in
    (a1,a2), s, s', stable1 && stable2

  let merge (pr1,pr2) ((a1,a2), log) ((a1',a2'), log') =
    S1.merge pr1 (a1, Log.get_left_log log) (a1', Log.get_left_log log'),
    S2.merge pr2 (a2, Log.get_right_log log) (a2', Log.get_right_log log')



  (**************************************************************************)
  (**                      {2 Transfer functions}                           *)
  (**************************************************************************)

  (** Initialization procedure *)
  let init prog man flow =
    S1.init prog (s1_man man) flow |>
    S2.init prog (s2_man man)

  (** Execution of statements *)
  let exec zone =
    match Core.Interface.sat_exec zone S1.interface,
          Core.Interface.sat_exec zone S2.interface
    with
    | false, false ->
      (* Both domains do not provide an [exec] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [S1] provides an [exec] for such zone *)
      let f = S1.exec zone in
      (fun stmt man flow ->
         f stmt (s1_man man) flow
      )

    | false, true ->
      (* Only [S2] provides an [exec] for such zone *)
      let f = S2.exec zone in
      (fun stmt man flow ->
         f stmt (s2_man man) flow
      )

    | true, true ->
      (* Both [S1] and [S2] provide an [exec] for such zone *)
      let f1 = S1.exec zone in
      let f2 = S2.exec zone in
      (fun stmt man flow ->
         match f1 stmt (s1_man man) flow with
         | Some post ->
           OptionExt.return post

         | None ->
           f2 stmt (s2_man man) flow
      )


  (** Evaluation of expressions *)
  let eval zone =
    match Core.Interface.sat_eval zone S1.interface,
          Core.Interface.sat_eval zone S2.interface
    with
    | false, false ->
      (* Both domains do not provide an [eval] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [S1] provides an [eval] for such zone *)
      let f = S1.eval zone in
      (fun exp man flow ->
         f exp (s1_man man) flow
      )

    | false, true ->
      (* Only [S2] provides an [eval] for such zone *)
      let f = S2.eval zone in
      (fun exp man flow ->
         f exp (s2_man man) flow
      )

    | true, true ->
      (* Both [S1] and [S2] provide an [eval] for such zone *)
      let f1 = S1.eval zone in
      let f2 = S2.eval zone in
      (fun exp man flow ->
         match f1 exp (s1_man man) flow with
         | Some evl -> Some evl

         | None -> f2 exp (s2_man man) flow
      )


  (** Query handler *)
  let ask query man flow =
    let reply1 = S1.ask query (s1_man man) flow in
    let reply2 = S2.ask query (s2_man man) flow in
    OptionExt.neutral2 (join_query query) reply1 reply2


  (** Reduction refinement *)
  let refine channel man flow =
    S1.refine channel (s1_man man) flow |>
    Core.Channel.bind @@ S2.refine channel (s2_man man)


end
