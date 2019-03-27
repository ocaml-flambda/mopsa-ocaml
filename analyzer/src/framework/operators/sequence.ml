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

(** The [Sequence] combiner iterates over two domains successively and
    returns the result of the first answering domain *)

open Ast.All
open Core.All
open Log


module Make (D1:DOMAIN) (D2:DOMAIN) : DOMAIN =
struct

  (**************************************************************************)
  (**                       {2 Type declaration}                            *)
  (**************************************************************************)

  type t = D1.t * D2.t

  include GenDomainId(
    struct
      type typ = t
      let name = "framework.combiners.sequence"
    end
    )


  (**************************************************************************)
  (**                       {2 Zoning interface}                            *)
  (**************************************************************************)

  let exec_interface = Core.Sig.Interface.concat D1.exec_interface D2.exec_interface

  let eval_interface = Core.Sig.Interface.concat D1.eval_interface D2.eval_interface


  (**************************************************************************)
  (**                        {2 Special values}                             *)
  (**************************************************************************)

  let bottom = D1.bottom, D2.bottom

  let top = D1.top, D2.top

  let is_bottom (a, b) =
    D1.is_bottom a || D2.is_bottom b


  (**************************************************************************)
  (**                 {2 Journaling utility functions}                      *)
  (**************************************************************************)

  let get_d1_log log =
    match log with
    | L_empty -> L_empty
    | L_compound [L_domain(_, log); _] -> log
    | _ -> assert false

  let get_d1_block log =
    match log with
    | L_empty -> []
    | L_compound [L_domain(b, _); _] -> b
    | _ -> assert false

  let get_d2_log log =
    match log with
    | L_empty -> L_empty
    | L_compound [_; L_domain(_, log)] -> log
    | _ -> assert false

  let get_d2_block log =
    match log with
    | L_empty -> []
    | L_compound [_; L_domain(b, _)] -> b
    | _ -> assert false

  let set_d1_log l log =
    L_compound [
      L_domain (get_d1_block log, l);
      L_domain (get_d2_block log, get_d2_log log)
    ]

  let set_d1_block b log =
    L_compound [
      L_domain (b, get_d1_log log);
      L_domain (get_d2_block log, get_d2_log log)
    ]

  let set_d2_log l log =
    L_compound [
      L_domain (get_d1_block log, get_d1_log log);
      L_domain (get_d2_block log, l)
    ]

  let set_d2_block b log =
    L_compound [
      L_domain (get_d1_block log, get_d1_log log);
      L_domain (b, get_d2_log log)
    ]

  let append2_d1_block stmt post =
    Post.map_log (fun tk log ->
        match tk with
        | T_cur ->
          set_d1_block (stmt :: get_d1_block log) log
        | _ -> log
      ) post

  let append2_d2_block stmt post =
    Post.map_log (fun tk log ->
        match tk with
        | T_cur ->
          set_d2_block (stmt :: get_d2_block log) log
        | _ -> log
      ) post


  (**************************************************************************)
  (**                           {2 Managers}                                *)
  (**************************************************************************)

  (** Global manager of [S] *)
  let d1_man (man:('a, t) man) : ('a, D1.t) man = {
    man with
    get = (fun flow -> man.get flow |> fst);
    set = (fun a flow -> man.set (a, man.get flow |> snd) flow);
  }

  (** Global manager of [D] *)
  let d2_man (man:('a, t) man) : ('a, D2.t) man = {
    man with
    get = (fun flow -> man.get flow |> snd);
    set = (fun b flow -> man.set (man.get flow |> fst, b) flow);
  }

  (**************************************************************************)
  (**                      {2 Lattice operators}                            *)
  (**************************************************************************)

  let subset (a1,a2) (a1',a2') =
    D1.subset a1 a1' && D2.subset a2 a2'

  let join (a1,a2) (a1',a2') =
    (D1.join a1 a1', D2.join a2 a2')

  let meet (a1,a2) (a1',a2') =
    (D1.meet a1 a1', D2.meet a2 a2')

  let widen ctx (a1,a2) (a1',a2') =
    (D1.widen ctx a1 a1', D2.widen ctx a2 a2')


  let merge (pre1, pre2) ((post1, post2), log) ((post1', post2'), log') =
    let log = Log.get_domain_log log in
    let log' = Log.get_domain_log log' in
    D1.merge pre1 (post1, get_d1_log log) (post1', get_d1_log log'),
    D2.merge pre2 (post2, get_d2_log log) (post2', get_d2_log log')


  (**************************************************************************)
  (**                        {2 Pretty printer}                             *)
  (**************************************************************************)

  let print fmt (a, b) =
    Format.fprintf fmt "%a%a" D1.print a D2.print b


  (**************************************************************************)
  (**                      {2 Transfer functions}                           *)
  (**************************************************************************)

  (** Initialization function *)
  let init prog man flow =
    let flow1 =
      match D1.init prog (d1_man man) flow with
      | None -> flow
      | Some flow -> flow
    in
    D2.init prog (d2_man man) flow1

  (** Execution of statements *)
  let exec zone =
    match Core.Sig.Interface.sat_exec zone D1.exec_interface,
          Core.Sig.Interface.sat_exec zone D2.exec_interface
    with
    | false, false ->
      (* Both domains do not provide an [exec] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [S] provides an [exec] for such zone *)
      let f = D1.exec zone in
      (fun stmt man flow ->
         f stmt (d1_man man) flow |>
         Option.lift (append2_d1_block stmt)
      )

    | false, true ->
      (* Only [D] provides an [exec] for such zone *)
      let f = D2.exec zone in
      (fun stmt man flow ->
         f stmt (d2_man man) flow |>
         Option.lift (append2_d2_block stmt)
      )

    | true, true ->
      (* Both [S] and [D] provide an [exec] for such zone *)
      let f1 = D1.exec zone in
      let f2 = D2.exec zone in
      (fun stmt man flow ->
         match f1 stmt (d1_man man) flow with
         | Some post ->
           Some (append2_d1_block stmt post)

         | None ->
           f2 stmt (d2_man man) flow |>
           Option.lift (append2_d2_block stmt)
      )


  (** Evaluation of expressions *)
  let eval zone =
    match Core.Sig.Interface.sat_eval zone D1.eval_interface,
          Core.Sig.Interface.sat_eval zone D2.eval_interface
    with
    | false, false ->
      (* Both domains do not provide an [eval] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [S] provides an [eval] for such zone *)
      let f = D1.eval zone in
      (fun exp man flow ->
         f exp (d1_man man) flow
      )

    | false, true ->
      (* Only [D] provides an [eval] for such zone *)
      let f = D2.eval zone in
      (fun exp man  flow ->
         f exp (d2_man man) flow
      )

    | true, true ->
      (* Both [S] and [D] provide an [eval] for such zone *)
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
    Option.neutral2 (Query.join query) reply1 reply2


end
