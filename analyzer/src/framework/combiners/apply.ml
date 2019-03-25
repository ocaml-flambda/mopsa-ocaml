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

(** The [Apply] combiner implements the classic function application of a
    stack domain on a domain *)

open Ast.All
open Core.All
open Log


module Make (S:STACK) (D:DOMAIN) : DOMAIN =
struct

  (**************************************************************************)
  (**                       {2 Type declaration}                            *)
  (**************************************************************************)

  type t = S.t * D.t

  include GenDomainId(
    struct
      type typ = t
      let name = "framework.combiners.apply"
    end
    )


  (**************************************************************************)
  (**                       {2 Zoning interface}                            *)
  (**************************************************************************)

  let exec_interface = Core.Sig.Interface.concat S.exec_interface D.exec_interface

  let eval_interface = Core.Sig.Interface.concat S.eval_interface D.eval_interface


  (**************************************************************************)
  (**                        {2 Special values}                             *)
  (**************************************************************************)

  let bottom = S.bottom, D.bottom

  let top = S.top, D.top

  let is_bottom (a, b) =
    S.is_bottom a || D.is_bottom b


  (**************************************************************************)
  (**                 {2 Journaling utility functions}                      *)
  (**************************************************************************)

  let get_s_log log =
    match log with
    | L_empty -> L_empty
    | L_compound [L_domain(_, log); _] -> log
    | _ -> assert false

  let get_s_block log =
    match log with
    | L_empty -> []
    | L_compound [L_domain(b, _); _] -> b
    | _ -> assert false

  let get_d_log log =
    match log with
    | L_empty -> L_empty
    | L_compound [_; L_domain(_, log)] -> log
    | _ -> assert false

  let get_d_block log =
    match log with
    | L_empty -> []
    | L_compound [_; L_domain(b, _)] -> b
    | _ -> assert false

  let set_s_log l log =
    L_compound [
      L_domain (get_s_block log, l);
      L_domain (get_d_block log, get_d_log log)
    ]

  let set_s_block b log =
    L_compound [
      L_domain (b, get_s_log log);
      L_domain (get_d_block log, get_d_log log)
    ]

  let set_d_log l log =
    L_compound [
      L_domain (get_s_block log, get_s_log log);
      L_domain (get_d_block log, l)
    ]

  let set_d_block b log =
    L_compound [
      L_domain (get_s_block log, get_s_log log);
      L_domain (b, get_d_log log)
    ]

  let append_s_block stmt post =
    Post.map_log (fun tk log ->
        match tk with
        | T_cur ->
          set_s_block (stmt :: get_s_block log) log
        | _ -> log
      ) post

  let append_d_block stmt post =
    Post.map_log (fun tk log ->
        match tk with
        | T_cur ->
          set_d_block (stmt :: get_d_block log) log
        | _ -> log
      ) post


  (**************************************************************************)
  (**                           {2 Managers}                                *)
  (**************************************************************************)

  (** Global manager of [S] *)
  let s_man (man:('a, t) man) : ('a, S.t) man = {
    man with
    get = (fun flow -> man.get flow |> fst);
    set = (fun a flow -> man.set (a, man.get flow |> snd) flow);
  }

  (** Global manager of [D] *)
  let d_man (man:('a, t) man) : ('a, D.t) man = {
    man with
    get = (fun flow -> man.get flow |> snd);
    set = (fun b flow -> man.set (man.get flow |> fst, b) flow);
  }

  let d_lattice : D.t lattice = {
    bottom = D.bottom;
    top = D.top;
    is_bottom = D.is_bottom;
    subset = D.subset;
    join = D.join;
    meet = D.meet;
    widen = D.widen;
    print = D.print;
  }

  let d_sub_man : D.t sub_man =
    assert false


  (** Sub-tree manager of [S] *)
  let s_sman (man:('a,t) man) : ('a, S.t, D.t) stack_man = {
    sub_lattice = d_sub_man.sub_lattice;
    sub_get = (fun a -> man.get a |> snd);
    sub_set = (fun d a -> man.set (man.get a |> fst, d) a);
    sub_exec = (fun ?(zone=any_zone) stmt flow ->
        match D.exec zone stmt (d_man man) flow with
        | None ->
          Exceptions.panic_at stmt.srange ~loc:__LOC__
            "sub-domain %s returned nothing when analyzing statement %a"
            D.name
            pp_stmt stmt

        | Some post ->
          append_d_block stmt post
      );
    sub_merge = (fun pre (post,log) (post',log') -> assert false);
    get_log = get_s_log;
    set_log = set_s_log;
  }


  (**************************************************************************)
  (**                      {2 Lattice operators}                            *)
  (**************************************************************************)

  let subset (a1,a2) (a1',a2') =
    let b1, a2, a2' = S.subset (a1, a2) (a1', a2') d_sub_man in
    b1 && D.subset a2 a2'

  let join (a1,a2) (a1',a2') =
    let a1, a2, a2' = S.join (a1, a2) (a1', a2') d_sub_man in
    (a1, D.join a2 a2')

  let meet (a1,a2) (a1',a2') =
    let a1, a2, a2' = S.meet (a1, a2) (a1', a2') d_sub_man in
    (a1, D.meet a2 a2')

  let widen ctx (a1,a2) (a1',a2') =
    let a1, converged, a2, a2' = S.widen ctx (a1, a2) (a1', a2') d_sub_man in
    if not converged then
      a1, D.widen ctx a2 a2'
    else
      a1, D.join a2 a2'


  let merge (pre1, pre2) ((post1, post2), log) ((post1', post2'), log') =
    let log = Log.get_domain_log log in
    let log' = Log.get_domain_log log' in
    S.merge pre1 (post1, get_s_log log) (post1', get_s_log log'),
    D.merge pre2 (post2, get_d_log log) (post2', get_d_log log')


  (**************************************************************************)
  (**                        {2 Pretty printer}                             *)
  (**************************************************************************)

  let print fmt (a, b) =
    Format.fprintf fmt "%a%a" S.print a D.print b


  (**************************************************************************)
  (**                      {2 Transfer functions}                           *)
  (**************************************************************************)

  (** Initialization function *)
  let init prog man flow =
    let flow1 =
      match S.init prog (s_man man) flow with
      | None -> flow
      | Some flow -> flow
    in
    D.init prog (d_man man) flow1

  (** Execution of statements *)
  let exec zone =
    match Core.Sig.Interface.sat_exec zone S.exec_interface,
          Core.Sig.Interface.sat_exec zone D.exec_interface
    with
    | false, false ->
      (* Both domains do not provide an [exec] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [S] provides an [exec] for such zone *)
      let f = S.exec zone in
      (fun stmt man flow ->
         f stmt (s_man man) (s_sman man) flow |>
         Option.lift (append_s_block stmt)
      )

    | false, true ->
      (* Only [D] provides an [exec] for such zone *)
      let f = D.exec zone in
      (fun stmt man flow ->
         f stmt (d_man man) flow |>
         Option.lift (append_d_block stmt)
      )

    | true, true ->
      (* Both [S] and [D] provide an [exec] for such zone *)
      let f1 = S.exec zone in
      let f2 = D.exec zone in
      (fun stmt man flow ->
         match f1 stmt (s_man man) (s_sman man) flow with
         | Some post ->
           Some (append_s_block stmt post)

         | None ->
           f2 stmt (d_man man) flow |>
           Option.lift (append_d_block stmt)
      )


  (** Evaluation of expressions *)
  let eval zone =
    match Core.Sig.Interface.sat_eval zone S.eval_interface,
          Core.Sig.Interface.sat_eval zone D.eval_interface
    with
    | false, false ->
      (* Both domains do not provide an [eval] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [S] provides an [eval] for such zone *)
      let f = S.eval zone in
      (fun exp man flow ->
         f exp (s_man man) (s_sman man) flow
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
         match f1 exp (s_man man) (s_sman man) flow with
         | Some evl -> Some evl

         | None -> f2 exp (d_man man) flow
      )


  (** Query handler *)
  let ask query man flow =
    let reply1 = S.ask query (s_man man) flow in
    let reply2 = D.ask query (d_man man) flow in
    Option.neutral2 (Query.join query) reply1 reply2


end
