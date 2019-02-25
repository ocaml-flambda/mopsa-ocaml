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

open Abstraction

type ('a, 'e) eval_case

type ('a, 'e) eval

val empty : ('a, 'e) eval

val case : ('a, 'e) eval_case -> ('a, 'e) eval

val singleton : 'e -> ?cleaners:Ast.stmt list -> 'a flow -> ('a, 'e) eval

val empty_singleton : 'a flow -> ('a, 'e) eval

val join : ('a, 'e) eval  -> ('a, 'e) eval  -> ('a, 'e) eval

val join_list : ?empty:(('a, 'e) eval) -> ('a, 'e) eval list -> ('a, 'e) eval

val meet : ('a, 'e) eval  -> ('a, 'e) eval  -> ('a, 'e) eval

val meet_list : ?empty:(('a, 'e) eval) -> ('a, 'e) eval list -> ('a, 'e) eval

val add_cleaners : Ast.stmt list -> ('a, 'e) eval  -> ('a, 'e) eval

val flip : ('e, 'a) eval -> ('e, 'a) eval

val fold :
    ('b -> ('a, 'e) eval_case -> 'b) ->
    ('b -> 'b -> 'b) ->
    ('b -> 'b -> 'b) ->
    'b -> ('a, 'e) eval ->
    'b

val fold2 :
    ('c -> ('a, 'e) eval_case -> 'b * 'c) ->
    ('b -> 'b -> 'b) ->
    ('b -> 'b -> 'b) ->
    'c -> ('a, 'e) eval ->
    'b * 'c

val substitute :
    ('e -> 'a flow -> 'b) ->
    ('b -> 'b -> 'b) ->
    ('b -> 'b -> 'b) ->
    ('b) ->
    ('a, 'e) eval ->
    'b

val iter : ('e -> 'a flow -> unit) -> ('a, 'e) eval -> unit

val iter_cases : (('a, 'e) eval_case -> unit) -> ('a, 'e) eval -> unit

val bind : ('e -> 'a flow -> ('a, 'f) eval ) -> ('a, 'e) eval -> ('a, 'f) eval

val bind_return : ('e -> 'a flow -> ('a, 'f) eval ) -> ('a, 'e) eval -> ('a, 'f) eval option

val bind_opt : ('e -> 'a flow -> ('a, 'f) eval option) -> ('a, 'e) eval -> ('a, 'f) eval option

val eval_list : 'e list -> ('e -> 'a flow -> ('a, 'c) eval) -> 'a flow ->  ('a, 'c list) eval

val eval_list_opt : 'e list -> ('e -> 'a flow -> ('a, 'c) eval option) -> 'a flow ->  ('a, 'c list) eval option

val assume :
  Ast.expr -> ?zone:Zone.zone ->
  fthen:('a flow -> ('a, 'e) eval ) ->
  felse:('a flow -> ('a, 'e) eval ) ->
  ?fboth:('a flow -> 'a flow -> ('a, 'e) eval ) ->
  ?fnone:('a flow -> ('a, 'e) eval) ->
  ('a, 'b) man -> 'a flow ->
  ('a, 'e) eval

val switch :
  ((Ast.expr * bool) list * ('a flow -> ('a, 'e) eval )) list ->
  ?zone:Zone.zone ->
  ('a, 'b) Manager.man -> 'a flow ->
  ('a, 'e) eval

val print: pp:(Format.formatter -> 'e -> unit) -> Format.formatter -> ('a, 'e) eval -> unit

val map:
  ('e -> 'a flow -> 'e * 'a flow) ->
  ('a, 'e) eval -> ('a, 'e) eval

val map_flow:
  ('a flow -> 'a flow) ->
  ('a, 'e) eval -> ('a, 'e) eval

val choose : ('a, 'e) eval -> ('e option * 'a flow) option

val to_dnf : ('a, 'e) eval -> ('a, 'e) eval_case Dnf.t

val return : ('a, 'e) eval -> ('a, 'e) eval option
