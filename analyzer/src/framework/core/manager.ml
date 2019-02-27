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
   Managers provide access to operators and transfer functions of the
   global abstract environment.
*)

open Annotation
open Lattice
open Flow
open Post
open Eval
open Ast
open Zone


(**
   An instance of type [('a, 't) man] encapsulates the lattice
   operators of the global environment abstraction ['a], the top-level
   transfer functions [exec], [eval] and [ask], and the accessor to
   the domain abstraction ['t] within ['a].
*)
type ('a, 't) man = {
  (* Functions on the global abstract element *)
  lattice : 'a lattice;

  (* Accessors to abstract element ['t] of the domain *)
  get : 'a -> 't;
  set : 't -> 'a -> 'a;

  (** Transfer functions *)
  exec : ?zone:zone -> stmt -> 'a flow -> 'a flow;
  eval : ?zone:(zone * zone) -> ?via:zone -> expr -> 'a flow -> (expr, 'a) eval;
  ask : 'r. 'r Query.query -> 'a flow -> 'r;
}

let set_domain_env (tk:token) (env:'t) (man:('a,'t) man) (flow:'a flow) : 'a flow =
  Flow.set tk (man.set env (Flow.get tk man.lattice flow)) man.lattice flow

let get_domain_env (tk:token) (man:('a,'t) man) (flow:'a flow) : 't =
  man.get (Flow.get tk man.lattice flow)

let map_domain_env (tk:token) (f:'t -> 't) (man:('a,'t) man) (flow:'a flow) : 'a flow =
  set_domain_env tk (f (get_domain_env tk man flow)) man flow


(*==========================================================================*)
(**                     {2 Argument domain manager }                        *)
(*==========================================================================*)

type 'a arg = {
  arg_exec : ?zone:zone -> stmt -> 'a flow -> 'a post;
  arg_ask : 'r. 'r Query.query -> 'a flow -> 'r;
}
