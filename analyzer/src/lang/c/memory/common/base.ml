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
  | S of string (** string literal *)
  | Z (** pointers with absolute numeric value *)

let pp_base fmt = function
  | V v -> pp_var fmt v
  | A (a) -> pp_addr fmt a
  | S s -> Format.fprintf fmt "\"%s\"" s
  | Z -> Format.fprintf fmt "ℤ"

let compare_base b b' = match b, b' with
  | V v, V v' -> compare_var v v'

  | A a, A a' -> compare_addr a a'

  | S s, S s' -> compare s s'

  | Z, Z -> 0

  | _ -> compare b b'

let base_uid = function
  | V v -> v.vuid
  | A a -> a.addr_uid
  | S _ -> panic ~loc:__LOC__ "base_uid: string literals not supported"
  | Z -> panic  ~loc:__LOC__ "base_uid: absolute pointers not supported"

let base_size =
  function
  | V v -> sizeof_type v.vtyp
  | S s -> Z.of_int @@ String.length s
  | A _ -> panic ~loc:__LOC__ "base_size: addresses not supported"
  | Z -> panic ~loc:__LOC__ "base_size: absolute pointers not supported"

let base_mode =
  function
  | V v -> STRONG
  | S s -> STRONG
  | A a -> a.addr_mode
  | Z -> panic ~loc:__LOC__ "base_mode: addresses not supported"

let base_scope =
  function
  | V { vkind = V_c {var_scope} } -> var_scope
  | _ -> assert false

let base_range =
  function
  | V { vkind = V_c {var_range} } -> var_range
  | _ -> assert false


(** Evaluate the size of a base *)
let eval_base_size base ?(via=Z_any) range (man:('a,'t,'s) Core.Sig.Stacked.Lowlevel.man) flow =
  match base with
  | V var -> Eval.singleton (mk_z (sizeof_type var.vtyp) range ~typ:ul) flow
  | S str -> Eval.singleton (mk_int (String.length str + 1) range ~typ:ul) flow
  | A addr ->
    let size_expr = mk_expr (Stubs.Ast.E_stub_builtin_call (SIZE, mk_addr addr range)) range ~etyp:ul in
    man.eval ~zone:(Z_c_low_level, Z_c_scalar) ~via size_expr flow
  | Z -> panic ~loc:__LOC__ "eval_base_size: addresses not supported"

module Base =
struct
  type t = base
  let compare = compare_base
  let print = pp_base
end
