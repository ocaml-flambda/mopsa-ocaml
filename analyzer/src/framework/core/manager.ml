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

(**
   Managers encapsulate an abstract domain into a record so that it can be
   passed to other abstract domains at runtime.
*)

open Context
open Lattice
open Token
open Flow
open Log
open Post
open Eval
open Ast.Stmt
open Ast.Expr
open Query
open Zone


(*==========================================================================*)
(**                         {2 Global manager}                              *)
(*==========================================================================*)


(** Global managers provide access to full analyzer, i.e. (i) the lattice
    operators of the global abstraction ['a], (ii) the transfer functions
    over ['a flow] and (iii) accessors to the domain's abstract element ['t]
    within ['a].
*)
type ('a, 't) man = {
  (* Lattice operators over global abstract elements ['a] *)
  lattice : 'a lattice;

  (* Accessors to the domain's abstract element ['t] within ['a] *)
  get : 'a -> 't;
  set : 't -> 'a -> 'a;

  (** Analyzer transfer functions *)
  exec : ?zone:zone -> stmt -> 'a flow -> 'a flow;
  eval : ?zone:(zone * zone) -> ?via:zone -> expr -> 'a flow -> (expr, 'a) eval;
  ask : 'r. 'r Query.query -> 'a flow -> 'r;
}



(*==========================================================================*)
(**                          {2 Stack manager}                              *)
(*==========================================================================*)

(** Stack managers are provided to stacked domains to access their parameter
    domain. Journaling functions in these managers allow stacked domains to log
    statements for eventual future merges.
*)
type ('a,'t,'s) stack_man = {
  (** Lattice of the sub-tree domain *)
  sub_lattice: 's lattice;

  sub_get : 'a -> 's;
  sub_set : 's -> 'a -> 'a;

  (** Journaling transfer function of the sub-tree domain *)
  sub_exec: ?zone:zone -> stmt -> 'a flow -> 'a post;

  (** Merge two post-conditions of the sub-tree domain *)
  sub_merge : 's -> 's * log -> 's * log -> 's;

  (** Accessors to the domain's log *)
  set_log : log -> log -> log;
  get_log : log -> log;
}


(*==========================================================================*)
(**                        {2 Sub-tree manager}                             *)
(*==========================================================================*)

(** Sub-tree manager provides a simplified interface to the argument
   sub-tree of a stacked domain. *)
type 's sub_man = {
  sub_lattice: 's lattice;
  sub_exec: ?zone:zone -> stmt -> 's -> 's;
}


(*==========================================================================*)
(**                        {2 Utility functions}                            *)
(*==========================================================================*)

let set_domain_env (tk:token) (env:'t) (man:('a,'t) man) (flow:'a flow) : 'a flow =
  Flow.set tk (man.set env (Flow.get tk man.lattice flow)) man.lattice flow

let get_domain_env (tk:token) (man:('a,'t) man) (flow:'a flow) : 't =
  man.get (Flow.get tk man.lattice flow)

let map_domain_env (tk:token) (f:'t -> 't) (man:('a,'t) man) (flow:'a flow) : 'a flow =
  set_domain_env tk (f (get_domain_env tk man flow)) man flow

let mem_domain_env (tk:token) (f:'t -> bool) (man:('a,'t) man) (flow:'a flow) : bool =
  get_domain_env tk man flow |>
  f
  

(** Create a sub-tree manager from a stacked manager *)
let sub_man_of_stack_man (man:('a,'t) man) (sman:('a,'t,'s) stack_man) : 's sub_man = {
  sub_lattice = sman.sub_lattice;
  sub_exec = (fun ?(zone=any_zone) stmt s ->
      let flow = sman.sub_set s man.lattice.top |>
                 Flow.singleton Context.empty T_cur
      in
      let post = sman.sub_exec ~zone stmt flow in
      let flow = Post.to_flow man.lattice post in
      Flow.get T_cur man.lattice flow |>
      sman.sub_get
    );
}

let assume cond ?(zone = any_zone)
    ~fthen ~felse ~fboth ~fnone
    man flow
  =
  let then_flow = man.exec ~zone (mk_assume cond cond.erange) flow in
  let flow = Flow.copy_ctx then_flow flow in
  let else_flow = man.exec ~zone (mk_assume (mk_not cond cond.erange) cond.erange) flow in
  let then_flow = Flow.copy_ctx else_flow then_flow in
  match man.lattice.is_bottom (Flow.get T_cur man.lattice then_flow),
        man.lattice.is_bottom (Flow.get T_cur man.lattice else_flow)
  with
  | false, true -> fthen then_flow
  | true, false -> felse else_flow
  | false, false -> fboth then_flow else_flow
  | true, true -> fnone (Flow.join man.lattice then_flow else_flow)

let assume_eval cond ?(zone = any_zone)
    ~fthen ~felse
    ?(fboth = (fun flow1 flow2 ->
        let fthen_r = fthen flow1 in
        let flow2 = Flow.set_ctx (Eval.choose_ctx fthen_r) flow2 in
        let felse_r = felse flow2 in
        Eval.join fthen_r felse_r))
    ?(fnone = (fun flow -> empty_singleton flow))
    man flow
  =
  assume cond ~zone ~fthen ~felse ~fboth ~fnone man flow

let assume_post cond ?(zone = any_zone)
    ~fthen ~felse
    ?(fboth = (fun flow1 flow2 ->
        let fthen_r = fthen flow1 in
        let flow2 = Flow.set_ctx (Post.choose_ctx fthen_r) flow2 in
        Post.join fthen_r (felse flow2)))
    ?(fnone = (fun flow -> Post.return flow))
    man flow
  : 'a post  =
  assume cond ~zone ~fthen ~felse ~fboth ~fnone man flow

let switch
    (cases : (((expr * bool) list) * ('a Flow.flow -> 'b)) list)
    ~join
    ?(zone = any_zone)
    man flow
  : 'b  =
  match cases with
  | [] -> assert false

  | (cond, t) :: q ->
    let one (cond : (expr * bool) list) t =
      List.fold_left (fun acc (x, b) ->
          let s =
            if b then (mk_assume x x.erange)
            else (mk_assume (mk_not x x.erange) x.erange)
          in
          man.exec ~zone s acc
        ) flow cond
      |> t
    in
    List.fold_left (fun acc (cond, t) -> join (one cond t) acc) (one cond t) q

let switch_eval = switch ~join:Eval.join

let switch_post = switch ~join:Post.join
