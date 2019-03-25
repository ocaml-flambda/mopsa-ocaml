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

open Lattice
open Flow
open Ast.Stmt
open Context

type ('e, 'a) eval

val empty : ('e, 'a) eval

val singleton : 'e -> ?cleaners:stmt list -> 'a flow -> ('e, 'a) eval

val empty_singleton : 'a flow -> ('e, 'a) eval

val join : ('e, 'a) eval  -> ('e, 'a) eval  -> ('e, 'a) eval

val join_list : ?empty:(('e, 'a) eval) -> ('e, 'a) eval list -> ('e, 'a) eval

val meet : ('e, 'a) eval  -> ('e, 'a) eval  -> ('e, 'a) eval

val meet_list : ?empty:(('e, 'a) eval) -> ('e, 'a) eval list -> ('e, 'a) eval

val print: pp:(Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, 'a) eval -> unit

val add_cleaners : stmt list -> ('e, 'a) eval  -> ('e, 'a) eval

val iter : ('e -> 'a flow -> unit) -> ('e, 'a) eval -> unit

val iter_all : ('e option -> 'a flow -> unit) -> ('e, 'a) eval -> unit

val map:
  ('e -> 'a flow -> 'e * 'a flow) ->
  ('e, 'a) eval -> ('e, 'a) eval

val map_flow:
  ('a flow -> 'a flow) ->
  ('e, 'a) eval -> ('e, 'a) eval

val reduce :
    ('e -> 'a flow -> 'b) ->
    ('b -> 'b -> 'b) ->
    ('b -> 'b -> 'b) ->
    ('b) ->
    ('e, 'a) eval ->
    'b

val choose : ('e, 'a) eval -> ('e option * 'a flow) option

val to_dnf : ('e, 'a) eval -> ('e option * 'a flow) Dnf.t

val bind : ('e -> 'a flow -> ('f, 'a) eval ) -> ('e, 'a) eval -> ('f, 'a) eval

val bind_return : ('e -> 'a flow -> ('f, 'a) eval ) -> ('e, 'a) eval -> ('f, 'a) eval option

val bind_opt : ('e -> 'a flow -> ('f, 'a) eval option) -> ('e, 'a) eval -> ('f, 'a) eval option

val eval_list : ('e -> 'a flow -> ('f, 'a) eval) -> 'e list -> 'a flow -> ('f list, 'a) eval

val eval_list_opt : ('e -> 'a flow -> ('f, 'a) eval option) -> 'e list -> 'a flow -> ('f list, 'a) eval option

val choose_ctx : ('e, 'a) eval -> 'a ctx
