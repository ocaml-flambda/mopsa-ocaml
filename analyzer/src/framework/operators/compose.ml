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
      let name = "framework.combiners.compose"
    end
    )


  (**************************************************************************)
  (**                       {2 Zoning interface}                            *)
  (**************************************************************************)

  let exec_interface =
    Core.Sig.Interface.concat S1.exec_interface S2.exec_interface

  let eval_interface =
    Core.Sig.Interface.concat S1.eval_interface S2.eval_interface


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

  (** Stack manager of [S2] *)
  let s2_stack_man (man:('a,t) man) (sman:('a,t,'s) stack_man) : ('a, S2.t, 's) stack_man = {
    sub_lattice = sman.sub_lattice;
    sub_get = sman.sub_get;
    sub_set = sman.sub_set;
    sub_exec = sman.sub_exec;
    sub_merge = sman.sub_merge;
    get_log = (fun glog -> get_s2_log @@ sman.get_log glog);
    set_log = (fun l glog -> sman.set_log (set_s2_log l @@ sman.get_log glog) glog);
  }

  (** Sub-tree lattice of [S1], containing [S2] and the argument sub-tree *)
  let s1_sub_lattice (man: 's sub_man) : (S2.t * 's) lattice = {
    bottom = S2.bottom, man.sub_lattice.bottom;
    top = S2.top, man.sub_lattice.top;
    is_bottom = (fun (a2,s) -> S2.is_bottom a2 || man.sub_lattice.is_bottom s);
    subset = (fun (a2,s) (a2',s') ->
        let b, s, s' = S2.subset (a2,s) (a2',s') man in
        b && man.sub_lattice.subset s s'
      );
    join = (fun (a2,s) (a2',s') ->
        let a2, s, s' = S2.join (a2,s) (a2',s') man in
        a2, man.sub_lattice.join s s'
      );
    meet = (fun (a2,s) (a2',s') ->
        let a2, s, s' = S2.meet (a2,s) (a2',s') man in
        a2, man.sub_lattice.meet s s'
      );
    widen = (fun ctx (a2,s) (a2',s') ->
        let a2, stable, s, s' = S2.widen ctx (a2,s) (a2',s') man in
        if stable
        then a2, man.sub_lattice.join s s'
        else a2, man.sub_lattice.widen ctx s s'
      );
    print = (fun fmt (a2,s) ->
        Format.fprintf fmt "%a%a" S2.print a2 man.sub_lattice.print s
      );
  }

  (** Stack manager of [S1] *)
  let s1_stack_man (man:('a,t) man) (sman:('a,t,'s) stack_man) : ('a, S1.t, S2.t*'s) stack_man = {
    sub_lattice = s1_sub_lattice (sub_man_of_stack_man man sman);
    sub_get = (fun a -> man.get a |> snd, sman.sub_get a);
    sub_set = (fun (a2,s) a -> man.set (man.get a |> fst, a2) a |>
                               sman.sub_set s);
    sub_exec = (fun ?(zone=any_zone) stmt flow ->
        match S2.exec zone stmt (s2_man man) (s2_stack_man man sman) flow with
        | None ->
          Exceptions.panic_at stmt.srange ~loc:__LOC__
            "sub-domain %s returned nothing when analyzing statement %a"
            S2.name
            pp_stmt stmt

        | Some post ->
          append_s2_block stmt sman post
      );
    sub_merge = (fun (pa2,ps) ((a2,s),log) ((a2',s'),log') ->
        let s2_log log =
          let l = sman.get_log log in
          L_domain(get_s2_block l, get_s2_log l)
        in

        let a2 = S2.merge pa2 (a2, s2_log log) (a2', s2_log log') in
        let s = sman.sub_merge ps (s, log) (s', log') in

        a2, s
      );
    get_log = (fun glog -> get_s1_log @@ sman.get_log glog);
    set_log = (fun log glog -> sman.set_log (set_s1_log log @@ sman.get_log glog) glog);
  }

  let rec s2_isolated_man (man:'s sub_man) : (S2.t*'s,S2.t) man =
    let lattice = s1_sub_lattice man in
    {
      lattice;
      get = (fun (a2,_) -> a2);
      set = (fun a2 (_,s) -> (a2,s));
      exec = (fun ?(zone=any_zone) stmt flow ->
          match S2.exec zone stmt (s2_isolated_man man) (s2_isolated_stack_man man) flow with
          | Some post -> Post.to_flow lattice post
          | None ->
            let a2,s = Flow.get T_cur lattice flow in
            let s' = man.sub_exec ~zone stmt s in
            Flow.set T_cur (a2,s') lattice flow
        );
      eval = (fun ?(zone=any_zone,any_zone) ?(via=any_zone) exp flow ->
          match S2.eval zone exp (s2_isolated_man man) (s2_isolated_stack_man man) flow with
          | Some evl -> evl
          | None -> Exceptions.panic_at exp.erange
                      "Expression %a was not evaluated by %s"
                      pp_expr exp
                      S2.name
        );
      ask = (fun query flow ->
          match S2.ask query (s2_isolated_man man) flow with
          | Some rep -> rep
          | None -> Exceptions.panic
                      "Query was not handled by %s"
                      S2.name
        );
    }

  and s2_isolated_stack_man (man:'s sub_man) : (S2.t*'s,S2.t,'s) stack_man =
    let lattice = s1_sub_lattice man in
    {
      sub_lattice = man.sub_lattice;
      sub_get = (fun (_,s) -> s);
      sub_set = (fun s (a2,_) -> (a2,s));
      sub_exec = (fun ?(zone=any_zone) stmt flow ->
          match S2.exec zone stmt (s2_isolated_man man) (s2_isolated_stack_man man) flow with
          | Some post -> post
          | None ->
            let a2,s = Flow.get T_cur lattice flow in
            let s' = man.sub_exec ~zone stmt s in
            Flow.set T_cur (a2,s') lattice flow |>
            Post.return
        );
      sub_merge = (fun pre (post,log) (post',log') -> assert false);
      get_log = (fun glog -> glog);
      set_log = (fun log glog -> log);
    }
    

  (** Sub-tree manager of [S1] *)
  let s1_sub_man (man:'s sub_man) : (S2.t*'s) sub_man =
    let sub_lattice = s1_sub_lattice man in
    {
      sub_lattice;
      sub_exec = (fun ?(zone=any_zone) stmt (a2,s) ->
          let flow = Flow.singleton Context.empty T_cur (a2,s) in
          match S2.exec zone stmt (s2_isolated_man man) (s2_isolated_stack_man man) flow with
          | Some post ->
            Post.to_flow sub_lattice post |>
            Flow.get T_cur sub_lattice

          | None ->
            let s' = man.sub_exec ~zone stmt s in
            (a2,s')
        );
    }

  (**************************************************************************)
  (**                      {2 Lattice operators}                            *)
  (**************************************************************************)

  let subset ((a1,a2),s) ((a1',a2'),s') man =
    let b1, (a2, s), (a2', s') = S1.subset (a1, (a2, s)) (a1', (a2', s')) (s1_sub_man man) in
    let b2, s, s' = S2.subset (a2, s) (a2', s') man in
    b1 && b2, s, s'

  let join ((a1,a2),s) ((a1',a2'),s') man =
    let a1, (a2, s), (a2', s') = S1.join (a1, (a2, s)) (a1', (a2', s')) (s1_sub_man man) in
    let a2, s, s' = S2.join (a2, s) (a2', s') man in
    (a1, a2), s, s'


  let meet ((a1,a2),s) ((a1',a2'),s') man =
    let a1, (a2, s), (a2', s') = S1.meet (a1, (a2, s)) (a1', (a2', s')) (s1_sub_man man) in
    let a2, s, s' = S2.meet (a2, s) (a2', s') man in
    (a1, a2), s, s'

  let widen ctx ((a1,a2),s) ((a1',a2'),s') man =
    let a1, converged, (a2, s), (a2', s') = S1.widen ctx (a1, (a2, s)) (a1', (a2', s')) (s1_sub_man man) in
    if not converged then
      let a2, converged, s, s' = S2.widen ctx (a2, s) (a2', s') man in
      (a1, a2), converged, s, s'
    else
      let a2, s, s' = S2.join (a2, s) (a2', s') man in
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
    match Core.Sig.Interface.sat_exec zone S1.exec_interface,
          Core.Sig.Interface.sat_exec zone S2.exec_interface
    with
    | false, false ->
      (* Both domains do not provide an [exec] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [S1] provides an [exec] for such zone *)
      let f = S1.exec zone in
      (fun stmt man sman flow ->
         f stmt (s1_man man) (s1_stack_man man sman) flow |>
         Option.lift (append_s1_block stmt sman)
      )

    | false, true ->
      (* Only [S2] provides an [exec] for such zone *)
      let f = S2.exec zone in
      (fun stmt man sman flow ->
         f stmt (s2_man man) (s2_stack_man man sman) flow |>
         Option.lift (append_s2_block stmt sman)
      )

    | true, true ->
      (* Both [S1] and [S2] provide an [exec] for such zone *)
      let f1 = S1.exec zone in
      let f2 = S2.exec zone in
      (fun stmt man sman flow ->
         match f1 stmt (s1_man man) (s1_stack_man man sman) flow with
         | Some post ->
           Some (append_s1_block stmt sman post)

         | None ->
           f2 stmt (s2_man man) (s2_stack_man man sman) flow |>
           Option.lift (append_s2_block stmt sman)
      )


  (** Evaluation of expressions *)
  let eval zone =
    match Core.Sig.Interface.sat_eval zone S1.eval_interface,
          Core.Sig.Interface.sat_eval zone S2.eval_interface
    with
    | false, false ->
      (* Both domains do not provide an [eval] for such zone *)
      raise Not_found

    | true, false ->
      (* Only [S1] provides an [eval] for such zone *)
      let f = S1.eval zone in
      (fun exp man stman flow ->
         f exp (s1_man man) (s1_stack_man man stman) flow
      )

    | false, true ->
      (* Only [S2] provides an [eval] for such zone *)
      let f = S2.eval zone in
      (fun exp man stman flow ->
         f exp (s2_man man) (s2_stack_man man stman) flow
      )

    | true, true ->
      (* Both [S1] and [S2] provide an [eval] for such zone *)
      let f1 = S1.eval zone in
      let f2 = S2.eval zone in
      (fun exp man stman flow ->
         match f1 exp (s1_man man) (s1_stack_man man stman) flow with
         | Some evl -> Some evl

         | None -> f2 exp (s2_man man) (s2_stack_man man stman) flow
      )


  (** Query handler *)
  let ask query man flow =
    let reply1 = S1.ask query (s1_man man) flow in
    let reply2 = S2.ask query (s2_man man) flow in
    Option.neutral2 (Query.join query) reply1 reply2


end
