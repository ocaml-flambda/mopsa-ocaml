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
open Lattice.Sig
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
(**                        {2 Sub-tree manager}                             *)
(*==========================================================================*)

(** Sub-tree managers are provided to stacked domains to access their parameter
    domain. Journaling functions in these managers allow stacked domains to log
    statements for eventual future merges.
*)
type ('a,'t,'s) sman = {
  (* Global manager of the stacked domain *)
  man: ('a,'t) man;

  (** Manager of the sub-tree domain *)
  sub_man: ('s, 's) man;

  (** Journaling transfer function in the sub-tree domain *)
  sub_exec: ?zone:zone -> stmt -> 'a flow -> 'a post;

  (** Accessors to the domain's log *)
  set_log : log -> log -> log;
  get_log : log -> log;
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

let sub_exec ?(zone=any_zone) stmt (sman:('a,'t,'s) sman) (s:'s) : 's =
  assert false
