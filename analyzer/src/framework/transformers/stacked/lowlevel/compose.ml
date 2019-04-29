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

(** The [Compose ∈ 𝒮 × 𝒮 → 𝒮] operator implements the classic function
    composition between two stack domains
*)

open Ast.All
open Core.All
open Log
open Sig.Stacked.Lowlevel

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
      type typ = t
      let name = "framework.transformers.stacked.lowlevel.compose"
    end
    )

  let interface = Core.Interface.concat S1.interface S2.interface

  let bottom = S1.bottom, S2.bottom

  let top = S1.top, S2.top

  let is_bottom (a1,a2) =
    S1.is_bottom a1 ||
    S2.is_bottom a2

  let print fmt (a1,a2) =
    Format.fprintf fmt "%a%a"
      S1.print a1
      S2.print a2

  (**************************************************************************)
  (**                           {2 Managers}                                *)
  (**************************************************************************)

  (** Global manager of [D] *)
  let s2_man (man:('a, t, 's) man) : ('a, S2.t, 's) man = {
    man with
    get = (fun a -> man.get a |> snd);
    set = (fun a2 a -> man.set (man.get a |> fst, a2) a);
    get_log = (fun log -> man.get_log log |> Log.second);
    set_log = (fun l log ->
        man.set_log (
          Log.tuple (man.get_log log |> Log.first, l)
        ) log
      );
  }

  (** Global manager of [S1] *)
  let s1_man (man:('a, t, 's) man) : ('a, S1.t, S2.t * 's) man =
    let man2 = s2_man man in
    {
      man with
      get = (fun a -> man.get a |> fst);
      set = (fun a1 a -> man.set (a1, man.get a |> snd) a);
      get_sub = (fun a -> man2.get a, man.get_sub a);
      set_sub = (fun (a2,s) a -> man2.set a2 a |> man.set_sub s);
      get_log = (fun log -> man.get_log log |> Log.first);
      set_log = (fun l log ->
          man.set_log (
            Log.tuple (l, man.get_log log |> Log.second)
          ) log
        );
      get_sub_log = (fun log -> Log.tuple (man2.get_log log, man.get_sub_log log));
      set_sub_log = (fun l log ->
          man2.set_log (Log.first l) log |>
          man.set_sub_log (Log.second l)
        );
      exec_sub = (fun ?(zone=any_zone) stmt flow ->
          man2.post ~zone stmt flow |>
          log_post_stmt stmt man2
        );

    }

  (**************************************************************************)
  (**                      {2 Lattice operators}                            *)
  (**************************************************************************)

  let subset man a a' =
    let b1, a, a' = S1.subset (s1_man man) a a' in
    let b2, a, a' = S2.subset (s2_man man) a a' in
    b1 && b2, a, a'

  let join man a a' =
    let a1, a, a' = S1.join (s1_man man) a a' in
    let a2, a, a' = S2.join (s2_man man) a a' in
    (a1,a2), a, a'

  let meet man a a' =
    let a1, a, a' = S1.meet (s1_man man) a a' in
    let a2, a, a' = S2.meet (s2_man man) a a' in
    (a1,a2), a, a'

  let widen man ctx a a' =
    let a1, a, a', stable1 = S1.widen (s1_man man) ctx a a' in
    let a2, a, a', stable2 = S2.widen (s2_man man) ctx a a' in
    (a1,a2), a, a', stable1 && stable2

  let merge man pre (post, log) (post', log') =
    S1.merge (s1_man man) pre (post, log) (post', log'),
    S2.merge (s2_man man) pre (post, log) (post', log')



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
         f stmt (s1_man man) flow |>
         Option.lift @@ log_post_stmt stmt (s1_man man)
      )

    | false, true ->
      (* Only [S2] provides an [exec] for such zone *)
      let f = S2.exec zone in
      (fun stmt man flow ->
         f stmt (s2_man man) flow |>
         Option.lift @@ log_post_stmt stmt (s2_man man)
      )

    | true, true ->
      (* Both [S1] and [S2] provide an [exec] for such zone *)
      let f1 = S1.exec zone in
      let f2 = S2.exec zone in
      (fun stmt man flow ->
         match f1 stmt (s1_man man) flow with
         | Some post ->
           Option.return @@ log_post_stmt stmt (s1_man man) post

         | None ->
           f2 stmt (s2_man man) flow |>
           Option.lift @@ log_post_stmt stmt (s2_man man)
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
    Option.neutral2 (join_query query) reply1 reply2


  (** Reduction refinement *)
  let refine channel man flow =
    S1.refine channel (s1_man man) flow |>
    Core.Channel.bind @@ S2.refine channel (s2_man man)

end
