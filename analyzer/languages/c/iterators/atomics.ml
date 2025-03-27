(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(* Copyright (c) 2025 Jane Street Group LLC                                 *)
(* opensource-contacts@janestreet.com                                       *)
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

(** Turn atomic expressions into regular expressions. *)

open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open Ast
open Common
open Common.Runtime



(** {2 Domain definition} *)
(** ===================== *)

module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.iterators.atomics"
    end)

  let dependencies = []

  let checks = []


  (** Initialization *)
  (** ============== *)

  let init _ _ flow = None


  (** Statement transformation *)
  (** ========================== *)

  let exec stmt man flow = None


  (** Expression transformation *)
  (** ========================= *)
  let atomic_op_is_simple_load = function
  | AO__c11_atomic_load -> true
  | AO__atomic_load -> true
  | AO__scoped_atomic_load -> true
  | AO__opencl_atomic_load -> true
  | AO__hip_atomic_load -> true
  | _ -> false


  let atomic_op_is_simple_store = function
  | AO__c11_atomic_store -> true
  | AO__atomic_store -> true
  | AO__scoped_atomic_store -> true
  | AO__opencl_atomic_store -> true
  | AO__hip_atomic_store -> true
  | _ -> false



  let eval exp man flow  =
    match ekind exp with
    | E_c_atomic (op, [e; _]) when atomic_op_is_simple_load op ->
      man.eval (mk_c_deref e exp.erange) flow |> OptionExt.return
    | E_c_atomic (op, [e1; e2; _]) when atomic_op_is_simple_store op ->
      (man.exec (mk_assign e1 e2 exp.erange) flow >>% fun flow ->
      Eval.singleton (mk_unit exp.erange) flow) |> OptionExt.return
    | E_c_atomic _ ->
      let flow = raise_ffi_internal_error (Format.asprintf "unsupported atomic operation %a" pp_expr exp) exp.erange man flow in
      (Eval.empty flow) |> OptionExt.return
    | _ -> None


  (** Query handler *)
  (** ============= *)

  let ask _ _ _  = None


  (** Pretty printer *)
  (** ============== *)

  let print_expr _ _ _ _ = ()

end

let () = register_stateless_domain (module Domain)
