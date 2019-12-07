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


(** Utility functions for updating the scope due to jump statements *)

open Mopsa
open Sig.Domain.Manager
open Universal.Ast
open Ast


(* Update the scope by removing/adding variables as indicated by [upd] *)
let update_scope upd range man flow =
  let flow = List.fold_left (fun flow v ->
      man.exec (mk_remove_var v range) flow
    ) flow upd.c_scope_var_removed
  in
  List.fold_left (fun acc v ->
      man.exec (mk_c_declaration v None (var_scope v) range) acc
    ) flow upd.c_scope_var_added
