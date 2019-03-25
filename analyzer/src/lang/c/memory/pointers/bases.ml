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

(** This module handles the base part of a pointer value. Bases
   represent the allocated memory blocks in which pointers can
   point. Bases are abstracted as powersets, which can be used as
   argument to the non-relational functor [Framework.Domains.Nonrel].
*)


open Mopsa
open Universal.Ast
open Common.Base


(** Pointer base *)
type pb =
  | PB_block of base         (** Memory blocks, such as program variables, mallocs and strings *)
  | PB_fun   of Ast.c_fundec (** Functions *)
  | PB_null                  (** Null pointers *)
  | PB_invalid               (** Invalid pointers: uninitialized or deallocated pointers *)


(** Bases are abstracted as powersets *)
module BaseSet = Framework.Lattices.Powerset.Make(
  struct

    type t = pb

    let print fmt = function
      | PB_block base -> pp_base fmt base
      | PB_fun f -> Format.pp_print_string fmt f.Ast.c_func_org_name
      | PB_null-> Format.pp_print_string fmt "NULL"
      | PB_invalid -> Format.pp_print_string fmt "Invalid"

    let compare p1 p2 =
      match p1, p2 with
      | PB_block b1, PB_block b2 -> compare_base b1 b2
      | PB_fun f1, PB_fun f2 -> compare f1.Ast.c_func_unique_name f2.Ast.c_func_unique_name
      | _, _ -> Pervasives.compare p1 p2
  end
  )

include BaseSet

include Framework.Core.Id.GenValueId(struct
    type typ = t
    let name = "c.memory.pointers"
    let display = "pointers"
  end)

let zone = Zone.Z_c_scalar

let null = singleton PB_null

let invalid = singleton PB_invalid

let block b = singleton (PB_block b)

let bfun f = singleton (PB_fun f)

let mem_block v =
  if is_top v then true
  else exists (function PB_block _ -> true | _ -> false) v

let of_constant _ c =
  match c with
  | C_int n when Z.equal n Z.zero -> null
  | _ -> top

let unop _ op v = top

let binop _ op v1 v2 = top

let bwd_unop = Framework.Core.Sig.Value.default_bwd_unop

let bwd_binop = Framework.Core.Sig.Value.default_bwd_binop

let filter _ v b =
  if b then diff v null
  else meet v null

let is_singleton v =
  not (is_top v) &&
  cardinal v == 1

let compare _ op v1 v2 r =
  let op = if r then op else negate_comparison op in
  match op with
  | O_eq ->
    let v = meet v1 v2 in
    v, v

  | O_ne ->
    if is_singleton v1 then v1, diff v2 v1
    else if is_singleton v2 then diff v1 v2, v2
    else v1, v2

  | _ -> v1, v2

module EvalQuery = Query.GenArgQuery(
  struct
    type arg = expr
    type ret = t
    let join = join
    let meet = meet
  end
  )
