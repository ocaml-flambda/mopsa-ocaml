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

(** Zones for the C language. *)

open Mopsa
open Universal.Ast
open Stubs.Ast
open Ast

(** Zones of the C language *)
type zone +=
  | Z_c           (* Entire C language *)
  | Z_c_low_level (* C without aggregates (arrays and records) *)
  | Z_c_scalar    (* C with only scalars (no dereferences) *)

let () =
  register_zone {
    zone = Z_c;
    zone_subset = None;
    zone_name = "C";
    zone_eval = (fun exp ->
        match ekind exp with
        (* ------------------------------------------- *)
        | E_constant _
        | E_addr _
        | E_var _                            -> Keep
        (* ------------------------------------------- *)
        | E_stub_quantified _
        | E_stub_builtin_call _
        | E_stub_primed _
        | E_stub_resource_mem _
        | E_unop _
        | E_binop _                          -> Visit
        (* ------------------------------------------- *)
        | E_c_conditional _
        | E_c_array_subscript _
        | E_c_member_access _
        | E_c_function _
        | E_c_builtin_function _
        | E_c_predefined _
        | E_call _
        | E_c_builtin_call _
        | E_c_arrow_access _
        | E_c_assign _
        | E_c_compound_assign _
        | E_c_comma _
        | E_c_increment _
        | E_c_address_of _
        | E_c_deref _
        | E_c_cast _
        | E_c_statement _
        | E_c_var_args _
        | E_c_atomic _                      -> Keep
        (* ------------------------------------------- *)
        | _                                 -> Process
      );
    }

let () =
  register_zone {
    zone = Z_c_low_level;
    zone_subset = Some Z_c;
    zone_name = "C/LowLevel";
    zone_eval = (fun exp ->
        match ekind exp with
        (* ------------------------------------------- *)
        | E_constant _
        | E_addr _
        | E_var _
        | E_c_function _                     -> Keep
        (* ------------------------------------------- *)
        | E_binop((O_c_and | O_c_or), _, _)  -> Process
        (* ------------------------------------------- *)
        | E_stub_resource_mem _
        | E_stub_quantified _
        | E_stub_builtin_call _
        | E_stub_primed _
        | E_unop _
        | E_binop _
        | E_c_cast _
        | E_c_address_of _
        | E_c_deref _                        -> Visit
        (* ------------------------------------------- *)
        | _                                  -> Process
      );
    }


let () =
  register_zone {
    zone = Z_c_scalar;
    zone_subset = Some Z_c;
    zone_name = "C/Scalar";
    zone_eval = (fun exp ->
        match ekind exp with
        (* ------------------------------------------- *)
        | E_constant _
        | E_addr _
        | E_c_function _                     -> Keep
        (* ------------------------------------------- *)
        | E_var _                            -> Visit
        (* ------------------------------------------- *)
        | E_binop((O_c_and | O_c_or), _, _)  -> Process
        (* ------------------------------------------- *)
        | E_stub_quantified _
        | E_unop _
        | E_binop _
        | E_c_address_of _
        | E_c_cast _                         -> Visit
        (* ------------------------------------------- *)
        | E_c_deref p
          when p.etyp |> under_type |> is_c_array_type
                                             -> Visit
        (* ------------------------------------------- *)
        | _                                  -> Process
      );
  }
