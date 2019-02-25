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
open Ast
open Zone

(*==========================================================================*)
(**                            {2 Flows}                                    *)
(*==========================================================================*)

type token = ..
(** Flow tokens are used to tag abstract elements when encountered in a
    relevant control point *)

type token += T_cur
(** Token of current (active) execution flow *)

type token_info = {
  compare : (token -> token -> int) -> token -> token -> int;
  print   : (Format.formatter -> token -> unit) -> Format.formatter -> token -> unit;
}

val register_token : token_info -> unit

val compare_token : token -> token -> int

val pp_token : Format.formatter -> token -> unit

module FlowMap : sig
  include MapExtSig.S with type key = token
  val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end
(** Map of flows binding tokens to abstract elements *)

type 'a fmap = 'a FlowMap.t Top.with_top

type 'a flow = {
  map   : 'a fmap;
  annot : 'a annot;
}
(** An abstract flow is a flow map augmented with an annotation *)




(*==========================================================================*)
(**                          {2 Post-states}                                *)
(*==========================================================================*)

(** Combiner logs *)
type clog =
   | L_leaf
   | L_product of clog list
   | L_compose of clog * log

(** Post-state logs *)
and log = Ast.stmt list * clog

(** Post-state case *)
type 'a post_case = {
  post_flow : 'a flow;
  post_log  : 'a flow;
}

(** Post-state *)
type 'a post = 'a post_case Dnf.t




(*==========================================================================*)
(**                             {2 Managers}                                *)
(*==========================================================================*)



(**
   An instance of type [('a, 't) man] encapsulates the lattice
   operators of the global environment abstraction ['a], the top-level
   transfer functions [exec], [eval] and [ask], and the accessor to
   the domain abstraction ['t] within ['a].
*)
type ('a, 't) man = {
  (* Functions on the global abstract element *)
  bottom    : 'a;
  top       : 'a;
  is_bottom : 'a -> bool;
  subset    : 'a -> 'a -> bool;
  join      : 'a -> 'a -> 'a;
  meet      : 'a -> 'a -> 'a;
  widen     : 'a annot -> 'a -> 'a -> 'a;
  print     : Format.formatter -> 'a -> unit;

  (* Accessors to abstract element ['t] of the domain *)
  get : 'a -> 't;
  set : 't -> 'a -> 'a;

  (** Transfer functions *)
  exec : ?zone:zone -> stmt -> 'a flow -> 'a flow;
  eval : ?zone:(zone * zone) -> ?via:zone -> expr -> 'a flow -> ('a, expr) eval;
  ask : 'r. 'r Query.query -> 'a flow -> 'r;
}


(*==========================================================================*)
(**                     {2 Argument domain manager }                        *)
(*==========================================================================*)

type 'a arg = {
  arg_exec : ?zone:zone -> stmt -> 'a flow -> 'a post;
  arg_ask : 'r. 'r Query.query -> 'a flow -> 'r;
}
