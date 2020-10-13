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
open Context
open Mopsa_utils


type ('a,_) query = ..

type query_pool = {
  pool_join : 'a 'r. ('a,'r) query -> 'r -> 'r -> 'r;
  pool_meet : 'a 'r. ('a,'r) query -> 'r -> 'r -> 'r;
}

let simple_pool = ref {
    pool_join = (fun _ _ _ -> raise Not_found);
    pool_meet = (fun _ _ _ -> raise Not_found);
  }

type lattice_query_pool = {
  pool_join : 'a 'r. 'a ctx -> 'a lattice -> ('a,'r) query -> 'r -> 'r -> 'r;
  pool_meet : 'a 'r. 'a ctx -> 'a lattice -> ('a,'r) query -> 'r -> 'r -> 'r;
}

let lattice_pool = ref {
    pool_join = (fun _ _ _ _ _ -> raise Not_found);
    pool_meet = (fun _ _ _ _ _ -> raise Not_found);
  }

let join_query ?(ctx=None) ?(lattice=None) q a b =
  try
    !simple_pool.pool_join q a b
  with Not_found ->
  match ctx,lattice with
  | Some ctx, Some lattice ->
    begin
      try !lattice_pool.pool_join ctx lattice q a b
      with Not_found -> Exceptions.panic "join_query: query not found"
    end
  | _ -> Exceptions.panic "join_query: query not found"


let meet_query ?(ctx=None) ?(lattice=None) q a b =
  try
    !simple_pool.pool_meet q a b
  with Not_found ->
  match ctx,lattice with
  | Some ctx, Some lattice ->
    begin
      try !lattice_pool.pool_meet ctx lattice q a b
      with Not_found -> Exceptions.panic "meet_query: query not found"
    end
  | _ -> Exceptions.panic "meet_query: query not found"


type query_info = {
  join : 'a 'r. query_pool -> ('a,'r) query -> 'r -> 'r -> 'r;
  meet : 'a 'r. query_pool -> ('a,'r) query -> 'r -> 'r -> 'r;
}

let register_query info =
  let old_pool = !simple_pool in
  simple_pool := {
    pool_join = (fun q a b -> info.join old_pool q a b);
    pool_meet = (fun q a b -> info.meet old_pool q a b);
  }

type lattice_query_info = {
  join : 'a 'r. lattice_query_pool -> 'a ctx -> 'a lattice -> ('a,'r) query -> 'r -> 'r -> 'r;
  meet : 'a 'r. lattice_query_pool -> 'a ctx -> 'a lattice -> ('a,'r) query -> 'r -> 'r -> 'r;
}

let register_lattice_query info =
  let old_pool = !lattice_pool in
  lattice_pool := {
    pool_join = (fun ctx lattice q a b -> info.join old_pool ctx lattice q a b);
    pool_meet = (fun ctx lattice q a b -> info.meet old_pool ctx lattice q a b);
  }


type ('a, _) query += Q_variables_linked_to : Ast.Expr.expr -> ('a, Ast.Var.VarSet.t) query

let () =
  register_query {
      join = (
        let f : type a r. query_pool -> (a, r) query -> r -> r -> r =
          fun next query a b ->
          match query with
          | Q_variables_linked_to _ -> Ast.Var.VarSet.union a b
          | _ -> next.pool_join query a b in f
      );
      meet = (
        let f : type a r. query_pool -> (a, r) query -> r -> r -> r =
          fun next query a b ->
          match query with
          | Q_variables_linked_to _ -> Ast.Var.VarSet.inter a b
          | _ -> next.pool_meet query a b in f
      );
    }
