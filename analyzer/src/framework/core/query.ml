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

open Eq


(** {2 Argument-less (unit) queries} *)
(** ******************************** *)

type _ uq = ..


(** Registration record defining new argument-less queries. *)
type 'r uinfo = {
  eq : 'a. 'a uq -> ('a, 'r) eq option;
  join: 'r -> 'r -> 'r;
  meet: 'r -> 'r -> 'r;
}


(** Pool of unit queries *)
type upool =
  | [] : upool
  | (::) : 'r uinfo * upool -> upool


let upool : upool ref = ref []


let uregister uinfo =
  upool := uinfo :: !upool


let ufind uq =
  let rec aux : type a b. a uq -> upool -> a uinfo =
    fun query -> function
      | [] -> raise Not_found
      | hd :: tl ->
        match hd.eq query with
        | Some Eq -> hd
        | None -> aux query tl
  in
  aux uq !upool



(** {2 Queries with arguments} *)
(** ************************** *)

type (_,_) aq = ..


type ('a,'r) ainfo = {
  eq : 'b 's. ('b,'s) aq -> ('b*'s, 'a*'r) eq option;
  join: 'r -> 'r -> 'r;
  meet: 'r -> 'r -> 'r;
}


type apool =
  | [] : apool
  | (::) : ('a,'r) ainfo * apool -> apool


let apool : apool ref = ref []


let aregister ainfo =
  apool := ainfo :: !apool


let afind aq =
  let rec aux : type a r. (a,r) aq -> apool -> (a,r) ainfo =
    fun query -> function
      | [] -> raise Not_found
      | hd :: tl ->
        match hd.eq query with
        | Some Eq -> hd
        | None -> aux query tl
  in
  aux aq !apool


(** {2 Queries} *)
(** *********** *)

type _ query =
  | Q_unit : 'r uq       -> 'r query
  | Q_arg  : ('a, 'r) aq * 'a -> 'r query

let join (query:'r query) (a:'r) (b:'r) =
  match query with
  | Q_unit uq ->
    let info = ufind uq in
    info.join a b

  | Q_arg (aq,_) ->
    let info = afind aq in
    info.join a b


let meet (query:'r query) (a:'r) (b:'r) =
  match query with
  | Q_unit uq ->
    let info = ufind uq in
    info.meet a b

  | Q_arg (aq,_) ->
    let info = afind aq in
    info.meet a b
    

(** {2 Generators of queries} *)
(** ************************* *)

module type UnitQuery =
sig
  type ret
  val query : ret query
  val handle : 'r query -> (unit -> ret) -> 'r option
end

module GenUnitQuery
    (Spec:
     sig
       type ret
       val join : ret -> ret -> ret
       val meet : ret -> ret -> ret
     end
    )
  : UnitQuery
    with type ret = Spec.ret
  =
  struct

    type ret = Spec.ret

    type _ uq +=
      | Q : Spec.ret uq

    let eq : type a. a uq -> (a, Spec.ret) eq option =
      fun q ->
        match q with
        | Q -> Some Eq
        | _ -> None             

    let () =
      uregister {
        eq = eq;
        join = Spec.join;
        meet = Spec.meet;
      }

    let query = Q_unit Q

    let handle : type r . r query -> (unit -> Spec.ret) -> r option =
      fun query f ->
        match query with
        | Q_unit q ->
          begin
            match eq q with
            | Some Eq -> Some (f ())
            | None -> None
          end
        | _ -> None
  end


module type ArgQuery =
sig
  type ret
  type arg
  val query : arg -> ret query
  val handle : 'r query -> (arg -> ret) -> 'r option
end

module GenArgQuery
    (
      Spec:
      sig
        type ret
        type arg
        val join : ret -> ret -> ret
        val meet : ret -> ret -> ret
      end
    )
  : ArgQuery
    with type ret = Spec.ret
    with type arg = Spec.arg
=
struct

  type ret = Spec.ret
  type arg = Spec.arg

    type (_,_) aq +=
      | Q : (Spec.arg,Spec.ret) aq

    let eq : type a r. (a,r) aq -> (a*r, Spec.arg*Spec.ret) eq option =
      fun q ->
        match q with
        | Q -> Some Eq
        | _ -> None             

    let () =
      aregister {
        eq = eq;
        join = Spec.join;
        meet = Spec.meet;
      }

    let query arg = Q_arg (Q,arg)

    let handle : type r . r query -> (Spec.arg -> Spec.ret) -> r option =
      fun query f ->
        match query with
        | Q_arg (q,arg) ->
          begin
            match eq q with
            | Some Eq -> Some (f arg)
            | None -> None
          end
        | _ -> None
  end

(** {2 Common queries} *)
(** ****************** *)

module PrintVarQuery = GenUnitQuery
    (struct
      type ret = Format.formatter -> string -> unit

      let join pp1 pp2  = fun fmt var ->
        Format.fprintf fmt "%a@,%a" pp1 var pp2 var

      let meet = join
    end)
