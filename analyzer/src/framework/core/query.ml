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


type ('a,_) query = ..


type query_operator = {
  apply : 'a 'r.  ('a,'r) query -> ('a -> 'a -> 'a) -> 'r -> 'r -> 'r;
}

let join_chain = ref {
    apply = (fun _ _ _ _ -> Exceptions.panic "query_join: unknown query");
  }

let meet_chain = ref {
    apply = (fun _ _ _ _ -> Exceptions.panic "query_meet: unknown query");
  }

let join_query q ~join a b = !join_chain.apply q join a b

let meet_query q ~meet a b = !meet_chain.apply q meet a b

type query_info = {
  join : 'a 'r. query_operator -> ('a,'r) query -> ('a->'a->'a) -> 'r -> 'r -> 'r;
  meet : 'a 'r. query_operator -> ('a,'r) query -> ('a->'a->'a) -> 'r -> 'r -> 'r;
}

let register_query info =
  join_chain := { apply = (fun q join a b -> info.join !join_chain q join a b) };
  meet_chain := { apply = (fun q meet a b -> info.meet !meet_chain q meet a b) }
