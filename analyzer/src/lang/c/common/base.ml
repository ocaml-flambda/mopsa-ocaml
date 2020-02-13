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


(** Kinds of bases *)
type base_kind =
  | Var    of var    (** Stack variable *)
  | Addr   of addr   (** Heap address *)
  | String of string (** String literal *)

(** Bases *)
type base = {
  base_kind : base_kind;
  base_valid : bool;
  base_invalidation_range : range option;
}


let pp_base_kind fmt = function
  | Var v -> pp_var fmt v
  | Addr (a) -> pp_addr fmt a
  | String s -> Format.fprintf fmt "\"%s\"" s

let pp_base fmt b =
  Format.fprintf fmt "%s%a"
    (if b.base_valid then "" else "✗")
    pp_base_kind b.base_kind

let compare_base_kind b b' = match b, b' with
  | Var v, Var v' -> compare_var v v'
  | Addr a, Addr a' -> compare_addr a a'
  | String s, String s' -> compare s s'
  | _ -> compare b b'

let compare_base b b' =
  Compare.compose [
    (fun () -> compare_base_kind b.base_kind b'.base_kind);
    (fun () -> compare b.base_valid b'.base_valid);
    (fun () -> Compare.option compare_range b.base_invalidation_range b'.base_invalidation_range);
  ]

let mk_base ?(valid=true) ?(invalidation_range=None) kind =
  { base_kind = kind;
    base_valid = valid;
    base_invalidation_range = invalidation_range; }


let mk_var_base ?(valid=true) ?(invalidation_range=None) v =
  mk_base (Var v) ~valid ~invalidation_range


let mk_addr_base ?(valid=true) ?(invalidation_range=None) a =
  mk_base (Addr a) ~valid ~invalidation_range

let mk_string_base s =
  mk_base (String s) ~valid:true ~invalidation_range:None

let base_kind_uniq_name b =
  match b with
  | Var v -> v.vname
  | Addr a ->
    let () = pp_addr Format.str_formatter a in
    Format.flush_str_formatter ()
  | String s -> s


let base_uniq_name b =
  let name = base_kind_uniq_name b.base_kind in
  if b.base_valid then name else "✗" ^ name


let base_size b =
  match b.base_kind with
  | Var v -> sizeof_type v.vtyp
  | String s -> Z.of_int @@ String.length s
  | Addr a -> panic ~loc:__LOC__ "base_size: addresses not supported"

let base_mode b =
  match b.base_kind with
  | Var v -> v.vmode
  | Addr a -> a.addr_mode
  | String _ -> STRONG


let is_base_readonly b =
  match b.base_kind with
  | String _ -> true
  | _ -> false


let is_base_expr e =
  match ekind e with
  | E_var(v,_)                -> is_c_type v.vtyp
  | E_addr _                  -> true
  | E_constant (C_c_string _) -> true
  | _ -> false


let expr_to_base e =
  match ekind e with
  | E_var(v,_)                    -> mk_var_base v
  | E_addr a                      -> mk_addr_base a
  | E_constant (C_c_string (s,_)) -> mk_string_base s
  | _ -> assert false


(** Evaluate the size of a base in bytes *)
let eval_base_size base ?(via=Z_any) range (man:('a,'t,'s) Core.Sig.Stacked.Lowlevel.man) flow =
  match base.base_kind with
  | Var var
    when is_c_variable_length_array_type var.vtyp ||
         is_c_no_length_array_type var.vtyp
    ->
    let bytes_expr = mk_expr (Stubs.Ast.E_stub_builtin_call (BYTES, mk_var var range)) range ~etyp:ul in
    man.eval ~zone:(Z_c_low_level, Z_c_scalar) ~via bytes_expr flow

  | Var var ->
    Eval.singleton (mk_z (sizeof_type var.vtyp) range ~typ:ul) flow

  | String str ->
    Eval.singleton (mk_int (String.length str + 1) range ~typ:ul) flow

  | Addr addr ->
    let bytes_expr = mk_expr (Stubs.Ast.E_stub_builtin_call (BYTES, mk_addr addr range)) range ~etyp:ul in
    man.eval ~zone:(Z_c_low_level, Z_c_scalar) ~via bytes_expr flow


module Base =
struct
  type t = base
  let compare = compare_base
  let print = pp_base
end


module BaseSet = SetExt.Make(Base)
