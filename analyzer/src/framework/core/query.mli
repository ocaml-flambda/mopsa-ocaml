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
(** *********** *)

open Eq

(** Type of a query, defined by domains and annotated with the type of the reply. *)
type _ query = ..

type 'r query_info = {
  eq : 'a. 'a query -> ('a, 'r) eq option;
  join: 'r -> 'r -> 'r;
  meet: 'r -> 'r -> 'r;
}

val register_query : 'a query_info -> unit

val join : 'a query -> 'a -> 'a -> 'a

val meet : 'a query -> 'a -> 'a -> 'a


(** {Common queries} *)
(** **************** *)

type _ query +=
  | Q_print_var : (Format.formatter -> string -> unit) query
  (** Print the value of a variable *)
