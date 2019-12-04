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

(** Base storage of scalar values. *)

open Mopsa
open Universal.Ast
open Ast
open Zone

(** lv base *)
type base =
  | V of var (** program variable *)
  | A of addr (** resource address *)
  | D of addr * range (** deallocated resource address with deallocation range *)
  | S of string (** string literal *)
  | Z (** pointers with absolute numeric value *)

let pp_base fmt = function
  | V v -> pp_var fmt v
  | A (a) -> pp_addr fmt a
  | D (a,r) -> Format.fprintf fmt "✗%a" pp_addr a
  | S s -> Format.fprintf fmt "\"%s\"" s
  | Z -> Format.fprintf fmt "ℤ"


let base_uniq_name b =
  match b with
  | V v -> v.vname
  | A _
  | D _ ->
    let () = pp_base Format.str_formatter b in
    Format.flush_str_formatter ()
  | S s -> s
  | Z -> "ℤ"

let compare_base b b' = match b, b' with
  | V v, V v' -> compare_var v v'
  | A a, A a' -> compare_addr a a'
  | D (a,r), D(a',r') -> Compare.pair compare_addr compare_range (a,r) (a',r')
  | S s, S s' -> compare s s'
  | Z, Z -> 0
  | _ -> compare b b'


let base_size =
  function
  | V v -> sizeof_type v.vtyp
  | S s -> Z.of_int @@ String.length s
  | A _ -> panic ~loc:__LOC__ "base_size: addresses not supported"
  | D _ -> panic ~loc:__LOC__ "base_size: deallocated addresses not supported"
  | Z -> panic ~loc:__LOC__ "base_size: absolute pointers not supported"

let base_mode =
  function
  | V v -> STRONG
  | S s -> STRONG
  | A a -> a.addr_mode
  | D(a,_) -> a.addr_mode
  | Z -> panic ~loc:__LOC__ "base_mode: addresses not supported"


let is_base_readonly = function
  | S _ -> true
  | _ -> false


(** Evaluate the size of a base in bytes *)
let eval_base_size base ?(via=Z_any) range (man:('a,'t,'s) Core.Sig.Stacked.Lowlevel.man) flow =
  match base with
  | V var when is_c_variable_length_array_type var.vtyp ->
    let bytes_expr = mk_expr (Stubs.Ast.E_stub_builtin_call (BYTES, mk_var var range)) range ~etyp:ul in
    man.eval ~zone:(Z_c_low_level, Z_c_scalar) ~via bytes_expr flow

  | V var ->
    Eval.singleton (mk_z (sizeof_type var.vtyp) range ~typ:ul) flow

  | S str ->
    Eval.singleton (mk_int (String.length str + 1) range ~typ:ul) flow

  | A addr ->
    let bytes_expr = mk_expr (Stubs.Ast.E_stub_builtin_call (BYTES, mk_addr addr range)) range ~etyp:ul in
    man.eval ~zone:(Z_c_low_level, Z_c_scalar) ~via bytes_expr flow

  | Z ->
    Eval.singleton (mk_top ul range) flow

  | D _ -> panic ~loc:__LOC__ "eval_base_size: deallocated addresses not supported"

module Base =
struct
  type t = base
  let compare = compare_base
  let print = pp_base
end
