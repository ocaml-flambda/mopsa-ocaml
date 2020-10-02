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

(** Generic query mechanism for extracting information from domains. *)


type ('a,_) query = ..

type query_operator = {
  apply : 'a 'r.  ('a,'r) query -> ('a -> 'a -> 'a) -> 'r -> 'r -> 'r;
}

type query_info = {
  join : 'a 'r. query_operator -> ('a,'r) query -> ('a->'a->'a) -> 'r -> 'r -> 'r;
  meet : 'a 'r. query_operator -> ('a,'r) query -> ('a->'a->'a) -> 'r -> 'r -> 'r;
}

val register_query : query_info -> unit

val join_query : ('a,'r) query -> join:('a->'a->'a) -> 'r -> 'r -> 'r

val meet_query : ('a,'r) query -> meet:('a->'a->'a) -> 'r -> 'r -> 'r

type ('a, _) query += Q_variables_linked_to : Ast.Expr.expr -> ('a, Ast.Var.VarSet.t) query
