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

(** The [Compose] combiner implements the classic function composition between
    two stack domains *)

open Ast.All
open Core.All

type _ domain +=
  | D_compose : 'a domain * 'b domain -> ('a * 'b) domain

module Make (S1:STACK) (S2:STACK) : STACK =
struct

  (**************************************************************************)
  (**                       {2 Type declaration}                            *)
  (**************************************************************************)

  type t = S1.t * S2.t

  let name = "framework.combiners.compose"

  let id = D_compose (S1.id, S2.id)

  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_compose(id1, id2) ->
      begin match S1.identify id1, S2.identify id2 with
        | Some Eq, Some Eq -> Some Eq
        | _ -> None
      end

    | _ -> None


  (**************************************************************************)
  (**                       {2 Zoning interface}                            *)
  (**************************************************************************)

  let exec_interface = Interface.concat S1.exec_interface S2.exec_interface

  let eval_interface = Interface.concat S1.eval_interface S2.eval_interface


  (**************************************************************************)
  (**                        {2 Special values}                             *)
  (**************************************************************************)

  let bottom = S1.bottom, S2.bottom

  let top = S1.top, S2.top

  let is_bottom (a, b) =
    S1.is_bottom a || S2.is_bottom b


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

  let append_s1_block stmt sman post =
    Post.map_log (fun tk log ->
        match tk with
        | T_cur ->
          let local_log = sman.get_log log in
          let local_log' = set_s1_block (stmt :: get_s1_block local_log) local_log in
          sman.set_log local_log' log
        | _ -> log
      ) post


  let append_s2_block stmt sman post =
    Post.map_log (fun tk log ->
        match tk with
        | T_cur ->
          let local_log = sman.get_log log in
          let local_log' = set_s2_block (stmt :: get_s2_block local_log) local_log in
          sman.set_log local_log' log
        | _ -> log
      ) post


  (**************************************************************************)
  (**                           {2 Managers}                                *)
  (**************************************************************************)

  (** Global manager of [S1] *)
  let s1_man (man:('a, t) man) : ('a, S1.t) man = {
    man with
    get = (fun flow -> man.get flow |> fst);
    set = (fun a flow -> man.set (a, man.get flow |> snd) flow);
  }

  (** Global manager of [S2] *)
  let s2_man (man:('a, t) man) : ('a, S2.t) man = {
    man with
    get = (fun flow -> man.get flow |> snd);
    set = (fun b flow -> man.set (man.get flow |> fst, b) flow);
  }

  (** Sub-tree manager of [S2] *)
  let s2_sman (sman:('a,t,'s) sman) : ('a, S2.t, 's) sman = {
    man = s2_man sman.man;
    sub_man = sman.sub_man;
    sub_exec = sman.sub_exec;
    get_log = (fun glog -> get_s2_log @@ sman.get_log glog);
    set_log = (fun l glog -> sman.set_log (set_s2_log l @@ sman.get_log glog) glog);
  }

  let s1_sub_lattice (sman:('a,t,'s) sman) : (S2.t * 's) lattice = {
    bottom = S2.bottom, sman.sub_man.lattice.bottom;

    top = S2.top, sman.sub_man.lattice.top;

    is_bottom = (fun (a,s) ->
        S2.is_bottom a ||
        sman.sub_man.lattice.is_bottom s
      );

    subset = (fun (a1,s1) (a2,s2) ->
        let b, s1, s2 = S2.subset (a1,s1) (a2,s2) (s2_sman sman) in
        b && sman.sub_man.lattice.subset s1 s2
      );

    join = (fun (a1,s1) (a2,s2) ->
        let a, s1, s2 = S2.join (a1,s1) (a2,s2) (s2_sman sman) in
        a, sman.sub_man.lattice.join s1 s2
      );

    meet = (fun (a1,s1) (a2,s2) ->
        let a, s1, s2 = S2.meet (a1,s1) (a2,s2) (s2_sman sman) in
        a, sman.sub_man.lattice.meet s1 s2
      );

    widen = (fun ctx (a1,s1) (a2,s2) ->
        let a, b, s1, s2 = S2.widen ctx (a1,s1) (a2,s2) (s2_sman sman) in
        if b then a, sman.sub_man.lattice.join s1 s2
        else a, sman.sub_man.lattice.widen ctx s1 s2
      );

    merge = (fun (pr2,prs) ((ps2,pss),log) ((ps2',pss'),log') ->
        let s2_log log =
          let l = sman.get_log log in
          L_domain(get_s2_block l, get_s2_log l)
        in

        let a2 = S2.merge pr2 (ps2, s2_log log) (ps2', s2_log log') in
        let s = sman.sub_man.lattice.merge prs (pss, log) (pss', log') in

        a2, s
      );

    print = (fun fmt (a,s) ->
        Format.fprintf fmt "%a%a" S2.print a sman.sub_man.lattice.print s
      );
  }

  (** Local manager of [S2] inside [S1] sub-tree *)
  let rec s2_local_man (sman:('a, t, 's) sman) : (S2.t * 's, S2.t) man =
    let man = s1_sub_man sman in
    {
      man with
      get = (fun (a, _) -> a);
      set = (fun a (_, s) -> (a, s));
    }


  (** Local stack manager of [S2] within [S1] sub-tree *)
  and s2_local_sman (sman:('a, t, 's) sman) : (S2.t * 's, S2.t, 's) sman =
    let man = s2_local_man sman in
    {
    man;
    sub_man = sman.sub_man;
    sub_exec = (fun ?(zone=any_zone) stmt flow ->
        man.exec ~zone stmt flow |>
        Post.return
      );
    get_log = (fun glog -> assert false);
    set_log = (fun log glog -> assert false);
  }

  (** Sub-manager of [S1] *)
  and s1_sub_man (sman:('a,t,'s) sman) : (S2.t * 's, S2.t * 's) man =
    let lattice = s1_sub_lattice sman in {
      lattice;
      get = (fun a -> a);
      set = (fun a _ -> a);
      exec = (fun ?(zone=any_zone) stmt flow ->
          match S2.exec zone stmt (s2_local_sman sman) flow with
          | Some post -> Post.to_flow (s1_sub_lattice sman) post
          | None ->
            (* Downgrade flow type *)
            let sflow = Flow.map_flow (fun tk (a, s) -> s) Context.empty flow in
            let sflow = sman.sub_man.exec ~zone stmt sflow in
            (* Upgrade flow type *)
            Flow.map_flow (fun tk s ->
                let a,_ = Flow.get tk (s1_sub_lattice sman) flow in
                (a, s)
              ) (Flow.get_ctx flow) sflow
        );
      eval = (fun ?(zone=any_zone,any_zone) ?(via=any_zone) exp flow ->
          assert false
        );
      ask = (fun query flow ->
          assert false
        );

    }

  (** Sub-tree manager of [S1] *)
  let s1_sman (sman:('a,t,'s) sman) : ('a, S1.t, S2.t*'s) sman = {
    man = s1_man sman.man;
    sub_man = s1_sub_man sman;
    sub_exec = (fun ?(zone=any_zone) stmt flow ->
        match S2.exec zone stmt (s2_sman sman) flow with
        | None ->
          Exceptions.panic_at stmt.srange ~loc:__LOC__
            "sub-domain %s returned nothing when analyzing statement %a"
            S2.name
            pp_stmt stmt

        | Some post ->
          append_s2_block stmt sman post
      );

    get_log = (fun glog -> get_s1_log @@ sman.get_log glog);
    set_log = (fun log glog -> sman.set_log (set_s1_log log @@ sman.get_log glog) glog);
  }


  (**************************************************************************)
  (**                      {2 Lattice operators}                            *)
  (**************************************************************************)

  let subset ((a1,a2),s) ((a1',a2'),s') sman =
    let b1, (a2, s), (a2', s') = S1.subset (a1, (a2, s)) (a1', (a2', s')) (s1_sman sman) in
    let b2, s, s' = S2.subset (a2, s) (a2', s') (s2_sman sman) in
    b1 && b2, s, s'

  let join ((a1,a2),s) ((a1',a2'),s') sman =
    let a1, (a2, s), (a2', s') = S1.join (a1, (a2, s)) (a1', (a2', s')) (s1_sman sman) in
    let a2, s, s' = S2.join (a2, s) (a2', s') (s2_sman sman) in
    (a1, a2), s, s'


  let meet ((a1,a2),s) ((a1',a2'),s') sman =
    let a1, (a2, s), (a2', s') = S1.meet (a1, (a2, s)) (a1', (a2', s')) (s1_sman sman) in
    let a2, s, s' = S2.meet (a2, s) (a2', s') (s2_sman sman) in
    (a1, a2), s, s'

  let widen ctx ((a1,a2),s) ((a1',a2'),s') sman =
    let a1, converged, (a2, s), (a2', s') = S1.widen ctx (a1, (a2, s)) (a1', (a2', s')) (s1_sman sman) in
    if not converged then
      let a2, converged, s, s' = S2.widen ctx (a2, s) (a2', s') (s2_sman sman) in
      (a1, a2), converged, s, s'
    else
      let a2, s, s' = S2.join (a2, s) (a2', s') (s2_sman sman) in
      (a1, a2), true, s, s'


  let merge (pre1, pre2) ((post1, post2), log) ((post1', post2'), log') =
    let log = Log.get_domain_log log in
    let log' = Log.get_domain_log log' in
    S1.merge pre1 (post1, get_s1_log log) (post1', get_s1_log log'),
    S2.merge pre2 (post2, get_s2_log log) (post2', get_s2_log log')


  (**************************************************************************)
  (**                        {2 Pretty printer}                             *)
  (**************************************************************************)

  let print fmt (a, b) =
    Format.fprintf fmt "%a%a" S1.print a S2.print b


  (**************************************************************************)
  (**                      {2 Transfer functions}                           *)
  (**************************************************************************)

  (** Initialization function *)
  let init prog man flow =
    let flow1 =
      match S1.init prog (s1_man man) flow with
      | None -> flow
      | Some flow -> flow
    in
    S2.init prog (s2_man man) flow1

  (** Execution of statements *)
  let exec zone =
    match Interface.sat_exec zone S1.exec_interface,
          Interface.sat_exec zone S2.exec_interface
    with
    | false, false ->
      (* Both domains do not provide an [exec] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [S1] provides an [exec] for such zone *)
      let f = S1.exec zone in
      (fun stmt sman flow ->
         f stmt (s1_sman sman) flow |>
         Option.lift (append_s1_block stmt sman)
      )

    | false, true ->
      (* Only [S2] provides an [exec] for such zone *)
      let f = S2.exec zone in
      (fun stmt sman flow ->
         f stmt (s2_sman sman) flow |>
         Option.lift (append_s2_block stmt sman)
      )

    | true, true ->
      (* Both [S1] and [S2] provide an [exec] for such zone *)
      let f1 = S1.exec zone in
      let f2 = S2.exec zone in
      (fun stmt sman flow ->
         match f1 stmt (s1_sman sman) flow with
         | Some post ->
           Some (append_s1_block stmt sman post)

         | None ->
           f2 stmt (s2_sman sman) flow |>
           Option.lift (append_s2_block stmt sman)
      )


  (** Evaluation of expressions *)
  let eval zone =
    match Interface.sat_eval zone S1.eval_interface,
          Interface.sat_eval zone S2.eval_interface
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
      (fun exp man  flow ->
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
    Option.neutral2 (Query.join query) reply1 reply2


end
