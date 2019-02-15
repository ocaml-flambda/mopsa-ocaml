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

(** Zones for the Universal language. *)

open Mopsa
open Ast

type zone +=
  | Z_u
  | Z_u_num
  | Z_u_heap
  | Z_u_string
  | Z_u_tree

let () =
  register_zone {
    zone = Z_u;
    subset = None;
    name = "U";
    eval = (fun exp ->
        match ekind exp with
        (* ------------------------------------------- *)
        | E_constant _
        | E_var _
          when is_universal_type exp.etyp ->
          Keep

        (* ------------------------------------------- *)
        | E_function _
        | E_array _
        | E_subscript _
        | E_alloc_addr _
        | E_addr _
        | E_len _                            -> Keep
        (* ------------------------------------------- *)
        | E_unop _
        | E_binop _                          -> Visit
        (* ------------------------------------------- *)
        | _                                  -> Process
      );
  };

  register_zone {
    zone = Z_u_num;
    subset = Some Z_u;
    name = "U/Num";
    eval = (fun exp ->
        match ekind exp with
        (* ------------------------------------------- *)
        | E_constant _
        | E_var _
          when is_numeric_type (etyp exp) ->
          Keep
        (* ------------------------------------------- *)
        | E_unop _
        | E_binop _                          -> Visit
        (* ------------------------------------------- *)
        | _                                  -> Process
      );
  };

  register_zone {
    zone = Z_u_heap;
    subset = Some Z_u;
    name = "U/Heap";
    eval = (fun exp ->
        match ekind exp with
        (* ------------------------------------------- *)
        | E_alloc_addr _
        | E_addr _                           -> Keep
        (* ------------------------------------------- *)
        | _                                  -> Process
      );
  };

  register_zone {
    zone = Z_u_string;
    subset = Some Z_u;
    name = "U/String";
    eval = (fun exp ->
        match ekind exp with
        (* ------------------------------------------- *)
        | E_constant _
        | E_function _
        | E_array _
        | E_subscript _
        | E_addr _
        | E_len _                            -> Keep
        (* ------------------------------------------- *)
        | E_unop _
        | E_binop _                          -> Visit
        (* ------------------------------------------- *)
        | _                                  -> Process
      );
  };

  ()
