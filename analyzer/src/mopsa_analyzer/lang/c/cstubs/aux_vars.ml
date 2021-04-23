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


(** Common auxiliary variables used by stubs transfer functions *)

open Mopsa
open Universal.Ast
open Stubs.Ast
open Ast
open Common.Base


(** {2 Bytes auxiliary variable} *)
(** ============================ *)

(** The size of stub resources are kept in a C variable `bytes(.)`  *)
type var_kind +=
  | V_c_bytes of addr

let pp_bytes fmt addr =
  Format.fprintf fmt "bytes⦃%a⦄" pp_addr addr

let () =
  register_var {
    print = (fun next fmt v ->
        match v.vkind with
        | V_c_bytes addr -> pp_bytes fmt addr
        | _ -> next fmt v
      );

    compare = (fun next v1 v2 ->
        match v1.vkind, v2.vkind with
        | V_c_bytes a1, V_c_bytes a2 -> compare_addr a1 a2
        | _ -> next v1 v2
      );
  }


let mk_bytes_var addr =
  let name = Format.asprintf "bytes⦃%s⦄" (addr_uniq_name addr) in
  mkv name (V_c_bytes addr) (T_c_integer C_unsigned_long)

let mk_bytes addr mode range =
  let v = mk_bytes_var addr in
  mk_var v ~mode range




(** Auxiliary variables of primed bases *)
(** =================================== *)


(** The post-state value of assigned bases are kept in primed copies *)
type var_kind +=
  | V_c_primed_base of base


let () = register_var {
    print = (fun next fmt v ->
        match v.vkind with
        | V_c_primed_base base -> Format.fprintf fmt "primed⦃%a⦄" pp_base base
        | _ -> next fmt v
      );
    compare = (fun next v1 v2 ->
        match v1.vkind, v2.vkind with
        | V_c_primed_base b1, V_c_primed_base b2 -> compare_base b1 b2
        | _ -> next v1 v2
      );
  }

let mk_primed_base_var base =
  let vkind = V_c_primed_base base in
  let vname = Format.asprintf "primed⦃%s⦄" (base_uniq_name base) in
  let vtyp = match base.base_kind with
    | Var v  -> v.vtyp
    | Addr a -> T_c_array(s8,C_array_no_length)
    | _      -> assert false
  in
  mkv vname vkind vtyp ~mode:STRONG


let mk_primed_base_expr base range =
  mk_var (mk_primed_base_var base) range


let mk_base_expr base range =
  match base.base_kind with
  | Var v  -> mk_var v range
  | Addr a -> mk_addr a range
  | _      -> assert false


(** Create the expression ( typ* )( ( char* )&base' + offset ) *)
let mk_primed_address base offset typ range =
  let primed = mk_primed_base_expr base range in
  mk_c_cast
    ( add
        (mk_c_cast (mk_c_address_of primed range) (T_c_pointer s8) range)
        offset
        ~typ:(T_c_pointer s8)
        range )
    (T_c_pointer typ)
    range

