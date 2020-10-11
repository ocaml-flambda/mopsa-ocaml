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

(** Queries are a generic mechanism for extracting information from abstract
    domains without being coupled to their internal representation.
*)


open Lattice

type ('a,_) query_kind = ..

type ('a,'r) query = {
  query_kind : ('a,'r) query_kind;
  query_lattice : 'a lattice option;
}

let mk_query ?(lattice=None) query_kind = {
  query_kind;
  query_lattice = lattice;
}

let qkind q = q.query_kind

let qlattice q = q.query_lattice

type query_pool = {
  pool_join : 'a 'r. ('a,'r) query -> 'r -> 'r -> 'r;
  pool_meet : 'a 'r. ('a,'r) query -> 'r -> 'r -> 'r;
}


let pool = ref {
    pool_join = (fun _ _ _ -> Exceptions.panic "query_join: unknown query");
    pool_meet = (fun _ _ _ -> Exceptions.panic "query_meet: unknown query");
  }

let join_query q a b = !pool.pool_join q a b

let meet_query q a b = !pool.pool_meet q a b

type query_info = {
  join : 'a 'r. query_pool -> ('a,'r) query -> 'r -> 'r -> 'r;
  meet : 'a 'r. query_pool -> ('a,'r) query -> 'r -> 'r -> 'r;
}

let register_query info =
  let old_pool = !pool in
  pool := {
    pool_join = (fun q a b -> info.join old_pool q a b);
    pool_meet = (fun q a b -> info.meet old_pool q a b);
  }

type ('a, _) query_kind += Q_variables_linked_to : Ast.Expr.expr -> ('a, Ast.Var.VarSet.t) query_kind

let () =
  register_query {
      join = (
        let f : type a r. query_pool -> (a, r) query -> r -> r -> r =
          fun next query a b ->
          match query.query_kind with
          | Q_variables_linked_to _ -> Ast.Var.VarSet.union a b
          | _ -> next.pool_join query a b in f
      );
      meet = (
        let f : type a r. query_pool -> (a, r) query -> r -> r -> r =
          fun next query a b ->
          match query.query_kind with
          | Q_variables_linked_to _ -> Ast.Var.VarSet.inter a b
          | _ -> next.pool_meet query a b in f
      );
    }
