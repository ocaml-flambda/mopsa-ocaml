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

(** Generic query mechanism for extracting information from abstract domains.

    Defining a new query requires the definition of the type of its reply and a
    manager on this type providing reply merging functions.

    Here is an example. Let us define a new interval query:
    {[type _ query += QInterval : var -> (int * int) query]}

    Next, an interval manager is registered as follows:
    {[register_query {
        eq = (let check : type a. a query -> (a, (int * int)) eq option =
                fun q ->
                  match q with
                  | QInterval _ -> Some Eq
                  | _ -> None
              in
              check
             );
        join = (fun (a1, a2) (b1, b2) -> (min a1 b1, max a2 b2));
        meet = (fun (a1, a2) (b1, b2) -> (max a1 b1, min a2 b2));
      };;]}

    For instance, the join of two intervals of a query [q] can be obtained simply by:
    {[join q (Some (1, 20)) (Some (-1, 5));;]}
    {v - : (int * int) option = Some (-1, 20) v}

*)

(** {2 Queries} *)

(** Type of a query, defined by domains and annotated with the type of the reply. *)
type _ query = ..

(** Query type equality witness. *)
type (_, _) eq = Eq : ('a, 'a) eq


(** {2 Managers} *)

(** Query manager defines merge operators on replies. *)
type 'r info = {
  eq : 'a. 'a query -> ('a, 'r) eq option;
  join: 'r -> 'r -> 'r;
  meet: 'r -> 'r -> 'r;
}

type pool =
  | [] : pool
  | (::) : 'r info * pool -> pool

let pool : pool ref = ref []

let register_query info =
  pool := info :: !pool

let find query =
  let rec aux : type a b. a query -> pool -> a info =
    fun query -> function
      | [] -> raise Not_found
      | hd :: tl ->
        match hd.eq query with
        | Some Eq -> hd
        | None -> aux query tl
  in
  aux query !pool


(** {2 Operators} *)

let join : type a. a query -> a -> a -> a =
  fun q r1 r2 ->
    let info = find q in
    info.join r1 r2

let meet : type a. a query -> a -> a  -> a =
  fun q r1 r2 ->
    let info = find q in
    info.meet r1 r2

let eq : type a b. a query -> b query -> (a, b) eq option =
  fun q1 q2 ->
    let info2 = find q2 in
    info2.eq q1
