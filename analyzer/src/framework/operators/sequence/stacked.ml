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

(** The [Sequence.Stack] operator combines two stacks over the same
    sub-abstraction, by "concatenating" their transfer functions (i.e. return
    the result of the first answering domain). *)

open Ast.All
open Core.All
open Log


module Make (S1:STACK) (S2:STACK) : STACK =
struct

  (**************************************************************************)
  (**                       {2 Type declaration}                            *)
  (**************************************************************************)

  type t = S1.t * S2.t

  include GenDomainId(
    struct
      type typ = t
      let name = "framework.combiners.sequence"
    end
    )

  let interface = Core.Interface.concat S1.interface S2.interface

  let print fmt (a, b) =
    Format.fprintf fmt "%a%a" S1.print a S2.print b


  (**************************************************************************)
  (**                 {2 Journaling utility functions}                      *)
  (**************************************************************************)

  let get_s1_log log =
    match log with
    | L_empty -> L_empty
    | L_compound [L_domain(_, log); _] -> log
    | _ -> assert false

  let get_s1_block log =
    match log with
    | L_empty -> []
    | L_compound [L_domain(b, _); _] -> b
    | _ -> assert false

  let get_s2_log log =
    match log with
    | L_empty -> L_empty
    | L_compound [_; L_domain(_, log)] -> log
    | _ -> assert false

  let get_s2_block log =
    match log with
    | L_empty -> []
    | L_compound [_; L_domain(b, _)] -> b
    | _ -> assert false

  let set_s1_log l log =
    L_compound [
      L_domain (get_s1_block log, l);
      L_domain (get_s2_block log, get_s2_log log)
    ]

  let set_s1_block b log =
    L_compound [
      L_domain (b, get_s1_log log);
      L_domain (get_s2_block log, get_s2_log log)
    ]

  let set_s2_log l log =
    L_compound [
      L_domain (get_s1_block log, get_s1_log log);
      L_domain (get_s2_block log, l)
    ]

  let set_s2_block b log =
    L_compound [
      L_domain (get_s1_block log, get_s1_log log);
      L_domain (b, get_s2_log log)
    ]

  let appens2_s1_block stmt post =
    Post.map_log (fun tk log ->
        match tk with
        | T_cur ->
          set_s1_block (stmt :: get_s1_block log) log
        | _ -> log
      ) post

  let appens2_s2_block stmt post =
    Post.map_log (fun tk log ->
        match tk with
        | T_cur ->
          set_s2_block (stmt :: get_s2_block log) log
        | _ -> log
      ) post




  (**************************************************************************)
  (**                        {2 Special values}                             *)
  (**************************************************************************)

  let bottom = S1.bottom, S2.bottom

  let top = S1.top, S2.top

  let is_bottom (a, b) =
    S1.is_bottom a || S2.is_bottom b

  let merge (pre1, pre2) ((post1, post2), log) ((post1', post2'), log') =
    let log = Log.get_domain_log log in
    let log' = Log.get_domain_log log' in
    S1.merge pre1 (post1, get_s1_log log) (post1', get_s1_log log'),
    S2.merge pre2 (post2, get_s2_log log) (post2', get_s2_log log')


  (**************************************************************************)
  (**                           {2 Managers}                                *)
  (**************************************************************************)

  (** Global manager of [S1] *)
  let s1_man (man:('a, t) man) : ('a, S1.t) man = {
    man with
    get = (fun flow -> man.get flow |> fst);
    set = (fun a flow -> man.set (a, man.get flow |> snd) flow);
  }

  (** Stack manager of [S1] *)
  let s1_sman (sman:('a, 's) stack_man) : ('a, 's) stack_man = {
    sman with
    get_log = get_s1_log;
    set_log = set_s1_log;
  }


  (** Global manager of [S2] *)
  let s2_man (man:('a, t) man) : ('a, S2.t) man = {
    man with
    get = (fun flow -> man.get flow |> snd);
    set = (fun b flow -> man.set (man.get flow |> fst, b) flow);
  }

  (** Stack manager of [S2] *)
  let s2_sman (sman:('a, 's) stack_man) : ('a, 's) stack_man = {
    sman with
    get_log = get_s2_log;
    set_log = set_s2_log;
  }


  (** Stack constructor *)
  module Make(Sub:Sig.Abstraction.ABSTRACTION) =
  struct

    (** Instantiate the two stacks over the same abstraction *)
    module SI1 = S1.Make(Sub)
    module SI2 = S2.Make(Sub)

    (**************************************************************************)
    (**                      {2 Lattice operators}                            *)
    (**************************************************************************)

    let subset ((a1,a2),s) ((a1',a2'),s') =
      let b1, s, s' = SI1.subset (a1,s) (a1',s') in
      let b2, s, s' = SI2.subset (a2,s) (a2',s') in
      b1 && b2, s, s'

    let join ((a1,a2),s) ((a1',a2'),s') =
      let a1, s, s' = SI1.join (a1,s) (a1',s') in
      let a2, s, s' = SI2.join (a2,s) (a2',s') in
      (a1,a2), s, s'

    let meet ((a1,a2),s) ((a1',a2'),s') =
      let a1, s, s' = SI1.meet (a1,s) (a1',s') in
      let a2, s, s' = SI2.meet (a2,s) (a2',s') in
      (a1,a2), s, s'

    let widen ctx ((a1,a2),s) ((a1',a2'),s') =
      let a1, stable1, s, s' = SI1.widen ctx (a1,s) (a1',s') in
      let a2, stable2, s, s' = SI2.widen ctx (a2,s) (a2',s') in
      (a1,a2), stable1 && stable2, s, s'



    (**************************************************************************)
    (**                      {2 Transfer functions}                           *)
    (**************************************************************************)

    (** Initialization function *)
    let init prog man sman flow =
      let flow1 =
        match SI1.init prog (s1_man man) (s1_sman sman) flow with
        | None -> flow
        | Some flow -> flow
      in
      SI2.init prog (s2_man man) (s2_sman sman) flow1

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
        let f = SI1.exec zone in
        (fun stmt man sman flow ->
           f stmt (s1_man man) (s1_sman sman) flow |>
           Option.lift (appens2_s1_block stmt)
        )

      | false, true ->
        (* Only [S2] provides an [exec] for such zone *)
        let f = SI2.exec zone in
        (fun stmt man sman flow ->
           f stmt (s2_man man) (s2_sman sman) flow |>
           Option.lift (appens2_s2_block stmt)
        )

      | true, true ->
        (* Both [S1] and [S2] provide an [exec] for such zone *)
        let f1 = SI1.exec zone in
        let f2 = SI2.exec zone in
        (fun stmt man sman flow ->
           match f1 stmt (s1_man man) (s1_sman sman) flow with
           | Some post ->
             Some (appens2_s1_block stmt post)

           | None ->
             f2 stmt (s2_man man) (s2_sman sman) flow |>
             Option.lift (appens2_s2_block stmt)
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
        let f = SI1.eval zone in
        (fun exp man sman flow ->
           f exp (s1_man man) (s1_sman sman) flow
        )

      | false, true ->
        (* Only [S2] provides an [eval] for such zone *)
        let f = SI2.eval zone in
        (fun exp man sman flow ->
           f exp (s2_man man) (s2_sman sman) flow
        )

      | true, true ->
        (* Both [S1] and [S2] provide an [eval] for such zone *)
        let f1 = SI1.eval zone in
        let f2 = SI2.eval zone in
        (fun exp man sman flow ->
           match f1 exp (s1_man man) (s1_sman sman) flow with
           | Some evl -> Some evl

           | None -> f2 exp (s2_man man) (s2_sman sman) flow
        )


    (** Query handler *)
    let ask query man sman flow =
      let reply1 = SI1.ask query (s1_man man) (s1_sman sman) flow in
      let reply2 = SI2.ask query (s2_man man) (s2_sman sman) flow in
      Option.neutral2 (Query.join query) reply1 reply2

  end

end
