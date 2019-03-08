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

(** Generic mechanism for extracting information from abstract domains. *)

open Eq

(** {2 Queries} *)
(** *********** *)

(** Type of a query, defined by domains and annotated with the type of the reply. *)
type _ query = ..


(** {2 Managers} *)
(** ************ *)

(** Query manager defines merge operators on replies. *)
type 'r query_info = {
  eq : 'a. 'a query -> ('a, 'r) eq option;
  join: 'r -> 'r -> 'r;
  meet: 'r -> 'r -> 'r;
}

type pool =
  | [] : pool
  | (::) : 'r query_info * pool -> pool

let pool : pool ref = ref []

let register_query info =
  pool := info :: !pool

module GenFunQuery(Q:sig
    type arg
    type ret
    val join : ret -> ret -> ret
    val meet : ret -> ret -> ret
  end)
  =
  struct
    type _ query += Query: Q.arg -> Q.ret query

    let query arg = Query arg

    let eq : type a. a query -> (a, Q.ret) eq option =
      fun q ->
        match q with
        | Query _ -> Some Eq
        | _ -> None

    let () =
      register_query {
        eq = eq;
        join = Q.join;
        meet = Q.meet;
      }
  end

module GenUnitQuery(Q:sig
    type ret
    val join : ret -> ret -> ret
    val meet : ret -> ret -> ret
  end)
  =
  struct
    type _ query += Query: Q.ret query

    let query = Query

    let eq : type a. a query -> (a, Q.ret) eq option =
      fun q ->
        match q with
        | Query -> Some Eq
        | _ -> None

    let () =
      register_query {
        eq = eq;
        join = Q.join;
        meet = Q.meet;
      }
  end


let find query =
  let rec aux : type a b. a query -> pool -> a query_info =
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



(** {2 Common queries} *)
(** ****************** *)

let print_var_query =
  let module Q = GenUnitQuery
      (struct
        type ret = Format.formatter -> string -> unit

        let join pp1 pp2  = fun fmt var ->
          Format.fprintf fmt "%a@,%a" pp1 var pp2 var

        let meet = join
      end)
  in
  Q.query
