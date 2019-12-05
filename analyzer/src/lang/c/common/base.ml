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
  | ValidVar    of var   (** valid variable in an active stack frame *)

  | InvalidVar  of var   (** invalid variable in a dead stack frame *) *
                  range  (** return location *)

  | ValidAddr   of addr  (** valid allocated address *)

  | InvalidAddr of addr  (** invalid deallocated address *) *
                   range (** deallocation location *)

  | String      of string (** string literal *)


let pp_base fmt = function
  | ValidVar v -> pp_var fmt v
  | InvalidVar (v,_) -> Format.fprintf fmt "✗%a" pp_var v
  | ValidAddr (a) -> pp_addr fmt a
  | InvalidAddr (a,r) -> Format.fprintf fmt "✗%a" pp_addr a
  | String s -> Format.fprintf fmt "\"%s\"" s


let base_uniq_name b =
  match b with
  | ValidVar v -> v.vname
  | InvalidVar (v,_) -> "✗" ^ v.vname
  | ValidAddr _ | InvalidAddr _ ->
    let () = pp_base Format.str_formatter b in
    Format.flush_str_formatter ()
  | String s -> s

let compare_base b b' = match b, b' with
  | ValidVar v, ValidVar v' -> compare_var v v'
  | InvalidVar(v,r), InvalidVar(v',r') -> Compare.pair compare_var compare_range (v,r) (v',r')
  | ValidAddr a, ValidAddr a' -> compare_addr a a'
  | InvalidAddr(a,r), InvalidAddr(a',r') -> Compare.pair compare_addr compare_range (a,r) (a',r')
  | String s, String s' -> compare s s'
  | _ -> compare b b'


let base_size =
  function
  | ValidVar v | InvalidVar(v,_) -> sizeof_type v.vtyp
  | String s -> Z.of_int @@ String.length s
  | ValidAddr _ | InvalidAddr _ -> panic ~loc:__LOC__ "base_size: addresses not supported"

let base_mode =
  function
  | ValidVar _ | InvalidVar _ | String _ -> STRONG
  | ValidAddr a | InvalidAddr(a,_) -> a.addr_mode


let is_base_readonly = function
  | String _ -> true
  | _ -> false


(** Evaluate the size of a base in bytes *)
let eval_base_size base ?(via=Z_any) range (man:('a,'t,'s) Core.Sig.Stacked.Lowlevel.man) flow =
  match base with
  | ValidVar var | InvalidVar(var,_)
    when is_c_variable_length_array_type var.vtyp ->
    let bytes_expr = mk_expr (Stubs.Ast.E_stub_builtin_call (BYTES, mk_var var range)) range ~etyp:ul in
    man.eval ~zone:(Z_c_low_level, Z_c_scalar) ~via bytes_expr flow

  | ValidVar var | InvalidVar(var,_) ->
    Eval.singleton (mk_z (sizeof_type var.vtyp) range ~typ:ul) flow

  | String str ->
    Eval.singleton (mk_int (String.length str + 1) range ~typ:ul) flow

  | ValidAddr addr | InvalidAddr(addr,_)  ->
    let bytes_expr = mk_expr (Stubs.Ast.E_stub_builtin_call (BYTES, mk_addr addr range)) range ~etyp:ul in
    man.eval ~zone:(Z_c_low_level, Z_c_scalar) ~via bytes_expr flow


module Base =
struct
  type t = base
  let compare = compare_base
  let print = pp_base
end
