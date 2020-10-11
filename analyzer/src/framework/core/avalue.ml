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

(** Abstract value representation *)

open Query
open Ast.Constant
open Ast.Expr
open Ast.Typ
open Ast.Visitor
open Location


type _ avalue_kind = ..

type avalue_pool = {
  pool_typ : 'v. 'v avalue_kind -> typ;
  pool_bottom : 'v. 'v avalue_kind -> 'v;
  pool_top : 'v. 'v avalue_kind -> 'v;
  pool_join : 'v. 'v avalue_kind -> 'v -> 'v -> 'v;
  pool_meet : 'v. 'v avalue_kind -> 'v -> 'v -> 'v;
  pool_compare : 'v 'w. 'v avalue_kind -> 'v -> 'w avalue_kind -> 'w -> int;
  pool_print : 'v. 'v avalue_kind -> Format.formatter -> 'v -> unit;
}

type avalue_info = {
  typ : 'v. avalue_pool -> 'v avalue_kind -> typ;
  bottom : 'v. avalue_pool -> 'v avalue_kind -> 'v;
  top : 'v. avalue_pool -> 'v avalue_kind -> 'v;
  join : 'v. avalue_pool -> 'v avalue_kind -> 'v -> 'v -> 'v;
  meet : 'v. avalue_pool -> 'v avalue_kind -> 'v -> 'v -> 'v;  
  compare : 'v 'w. avalue_pool -> 'v avalue_kind -> 'v -> 'w avalue_kind -> 'w -> int;
  print : 'v. avalue_pool -> 'v avalue_kind -> Format.formatter -> 'v -> unit;
}

let pool = ref {
    pool_bottom = (
      let f : type a. a avalue_kind -> a =
        fun avk -> Exceptions.panic "Bottom of unregistered avalue"
      in f);
    pool_top = (
      let f : type a. a avalue_kind -> a =
        fun avk -> Exceptions.panic "Top of unregistered avalue"
      in f);
    pool_join = (
      let f : type a. a avalue_kind -> a -> a -> a =
        fun avk -> Exceptions.panic "Join of unregistered avalue"
      in f);
    pool_meet = (
      let f : type a. a avalue_kind -> a -> a -> a =
        fun avk -> Exceptions.panic "Meet of unregistered avalue"
      in f);
    pool_compare = (
      let f : type a b. a avalue_kind -> a -> b avalue_kind -> b -> int =
        fun avk1 av1 avk2 av2 ->
          compare
            (Obj.Extension_constructor.of_val avk1 |> Obj.Extension_constructor.id)
            (Obj.Extension_constructor.of_val avk2 |> Obj.Extension_constructor.id)
      in
      f);
    pool_print = (
      let f : type a. a avalue_kind -> Format.formatter -> a -> unit =
        fun avk fmt av -> 
          Exceptions.panic "Print of unregistered avalue"
      in
      f);
    pool_typ = (
      let f : type a. a avalue_kind -> typ =
        fun avk ->
          Exceptions.panic "Type of unregistered avalue"
      in
      f);
  }

let register_avalue info =
  let old_pool = !pool in
  pool := {
    pool_bottom = (
      let f : type a. a avalue_kind -> a =
        fun avk ->
          info.bottom old_pool avk
      in f);
    pool_top = (
      let f : type a. a avalue_kind -> a =
        fun avk ->
          info.top old_pool avk
      in f);
    pool_join = (
      let f : type a. a avalue_kind -> a -> a -> a =
        fun avk av1 av2 ->
          info.join old_pool avk av1 av2
      in f);
    pool_meet = (
      let f : type a. a avalue_kind -> a -> a -> a =
        fun avk av1 av2 ->
          info.meet old_pool avk av1 av2
      in f);
    pool_compare = (
      let f : type a b. a avalue_kind -> a -> b avalue_kind -> b -> int =
        fun avk1 av1 avk2 av2 ->
          info.compare old_pool avk1 av1 avk2 av2
      in f);
    pool_print = (
      let f : type a. a avalue_kind -> Format.formatter -> a -> unit =
        fun avk fmt av ->
          info.print old_pool avk fmt av
      in f);
    pool_typ = (
      let f : type a. a avalue_kind -> typ =
        fun avk ->
          info.typ old_pool avk
      in f);
  }

let type_of_avalue avk = !pool.pool_typ avk

let bottom_avalue avk = !pool.pool_bottom avk

let top_avalue avk = !pool.pool_top avk

let join_avalue avk av1 av2 = !pool.pool_join avk av1 av2

let meet_avalue avk av1 av2 = !pool.pool_meet avk av1 av2

let compare_avalue avk1 av1 avk2 av2 = !pool.pool_compare avk1 av1 avk2 av2

let pp_avalue avk fmt av = !pool.pool_print avk fmt av

type constant += C_avalue : 'v avalue_kind * 'v -> constant

let () =
  register_constant {
    print = (fun next fmt c ->
        match c with
        | C_avalue(avk,av) -> pp_avalue avk fmt av
        | _ -> next fmt c);
    compare = (fun next c1 c2 ->
        match c1, c2 with
        | C_avalue(avk1,av1), C_avalue(avk2,av2) -> compare_avalue avk1 av1 avk2 av2
        | _ -> next c1 c2);
  }
          
let mk_avalue_constant avk av =  C_avalue(avk,av)

let mk_avalue_expr avk av range = mk_constant (mk_avalue_constant avk av) ~etyp:(type_of_avalue avk) range

type ('a,_) query_kind += Q_avalue : expr * 'v avalue_kind -> ('a,'v) query_kind

let mk_avalue_query e avk = mk_query (Q_avalue(e,avk))
