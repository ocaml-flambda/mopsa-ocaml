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


type _ query = ..


type query_pool = {
  join : 'r. 'r query -> 'r -> 'r -> 'r;
  meet : 'r. 'r query -> 'r -> 'r -> 'r;
}

let chain = ref {
  join = (fun _ _ _ -> Exceptions.panic "unknown query");
  meet = (fun _ _ _ -> Exceptions.panic "unknown query");
}

let join : type r. r query -> r -> r -> r = fun q a b ->
  !chain.join q a b

let meet : type r. r query -> r -> r -> r = fun q a b ->
  !chain.meet q a b

type query_info = {
  query_join : 'r. query_pool -> 'r query -> 'r -> 'r -> 'r;
  query_meet : 'r. query_pool -> 'r query -> 'r -> 'r -> 'r;
}

let register_query info =
  let next = !chain in
  chain := {
    join = (fun q a b -> info.query_join next q a b);
    meet = (fun q a b -> info.query_meet next q a b);
  }
