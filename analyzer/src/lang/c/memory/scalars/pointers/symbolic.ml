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


(** Symbolic representation of pointer values with equality predicates *)


open Mopsa
open Universal.Ast
open Ast
open Common.Base


(** Symbolic pointer values *)
type t =
  | AddrOf of base * expr
  | Eq of var * mode * expr
  | Fun of c_fundec
  | Null
  | Invalid
  | Top


(** Advance the offset of a symbolic pointer *)
let advance_offset (op:operator) (ptr:t) (o:expr) typ range : t =
  (* Size of the pointed type *)
  let size =
    match under_type typ |> remove_typedef_qual with
    | T_c_void -> Z.one
    | tt -> sizeof_type tt
 in

 let advance oo =
   if Z.equal size Z.one then
     mk_binop oo op o range ~etyp:T_int
   else
     mk_binop oo op (mk_binop o O_mult (mk_z size range) range ~etyp:T_int) range ~etyp:T_int
  in

  match ptr with
  | AddrOf (b, oo) -> AddrOf (b, advance oo)

  | Eq (p, mode, oo) -> Eq (p, mode, advance oo)

  | Null -> Null

  | Invalid -> Invalid

  | Fun _ ->
    panic_at range
      "pointers.add_offset: pointer arithmetics on functions not supported"

  | Top -> Top



(** Symbolic evaluation of a pointer expression *)
let rec eval_opt exp : t option =
  match ekind exp with
  | E_constant(C_int n) when Z.equal n Z.zero ->
    Null |> Option.return

  | E_constant(C_c_invalid) ->
    Invalid |> Option.return

  | E_constant(C_top t) when is_c_pointer_type t ->
    Top |> Option.return

  | E_addr (addr) ->
    AddrOf(A addr, mk_zero exp.erange) |> Option.return

  | E_c_deref { ekind = E_c_address_of e } ->
    eval_opt e

  | E_c_address_of { ekind = E_c_deref p } ->
    eval_opt p

  | E_c_cast (e, _) ->
    eval_opt e

  | E_c_function f ->
    Fun f |> Option.return

  | E_constant (C_c_string (s, _)) ->
    AddrOf(S s, mk_zero exp.erange) |> Option.return

  | E_var (a, _) when is_c_array_type a.vtyp ->
    AddrOf(V a, mk_zero exp.erange) |> Option.return

  | E_c_deref a when is_c_array_type (under_type a.etyp) ->
    eval_opt a

  | E_c_address_of { ekind = E_var (v, _) } ->
    AddrOf (V v, mk_zero exp.erange) |> Option.return

  | E_c_address_of { ekind = E_c_function f } ->
    Fun f |> Option.return

  | E_binop(O_plus | O_minus as op, e1, e2) ->
    let p, i =
      if is_c_pointer_type e1.etyp || is_c_array_type e1.etyp
      then e1, e2
      else e2, e1
    in
    eval_opt p |>
    Option.lift @@ fun ptr ->
    advance_offset op ptr i p.etyp exp.erange

  | E_var (v, mode) when is_c_pointer_type v.vtyp ->
    Eq (v, mode, mk_zero exp.erange) |> Option.return

  | x when is_c_int_type exp.etyp ->
    AddrOf(Common.Base.Z, exp) |> Option.return

  | _ ->
    warn_at exp.erange "evaluation of pointer expression %a not supported" pp_expr exp;
    None


(** Symbolic evaluation of a pointer expression *)
let eval exp : t =
  match eval_opt exp with
  | Some ptr -> ptr
  | None -> panic_at exp.erange "evaluation of pointer expression %a not supported" pp_expr exp
