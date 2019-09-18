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

(** Simple packing strategy based on static scoping of variables.

    The idea is simple: global variables are kept in one pack and local
    variables of each function are kept in separate packs.

    The packs may overlap to preserve some relations between arguments passed
    to a function and its return value.
*)

open Mopsa
open Sig.Domain.Simplified
open Universal.Packing.Static
open Universal.Ast
open Ast
open Common.Base

module Strategy =
struct

  (** Name of the packing strategy *)
  let name = "c.memory.packing.static_scope"

  let debug fmt = Debug.debug ~channel:name fmt

  (** Packing key *)
  type pack =
    | Globals (** Pack of global variables *)
    | Locals of string (** Pack of local variables of a function *)


  (** Total order of packing keys *)
  let compare k1 k2 =
    match k1, k2 with
    | Globals, Globals -> 0
    | Locals f1, Locals f2 -> compare f1 f2
    | Globals, Locals _ -> 1
    | Locals _, Globals -> -1


  (** Pretty printer of packing keys *)
  let print fmt = function
    | Globals -> Format.pp_print_string fmt "[globals]"
    | Locals f -> Format.pp_print_string fmt f


  (** Initialization *)
  let init prog = ()


  (** Packs of a base memory block *)
  let packs_of_base ctx b =
    match b with
    | V { vkind = V_cvar {cvar_scope = Variable_global} }
    | V { vkind = V_cvar {cvar_scope = Variable_file_static _} } ->
      []

    | V { vkind = V_cvar {cvar_scope = Variable_local f} }
    | V { vkind = V_cvar {cvar_scope = Variable_func_static f} } ->
      [Locals f.c_func_unique_name]

    | V { vkind = V_cvar {cvar_scope = Variable_parameter f} } ->
      (* Parameters are part of the caller and the callee packs *)
      let cs = Context.ufind Callstack.ctx_key ctx in
      if Callstack.is_empty cs
      then []
      else
        let callee, cs' = Callstack.pop cs in
        if Callstack.is_empty cs'
        then []
        else
          let caller, _ = Callstack.pop cs' in
          [Locals caller.call_fun; Locals callee.call_fun]

    | V { vkind = Universal.Iterators.Interproc.Common.V_return call } ->
      (* Return variables are also part of the caller and the callee packs *)
      (* Note that the top of the callstack is not always the callee
         function, because the return variable is used after the function
         returns
      *)
      let cs = Context.ufind Callstack.ctx_key ctx in
      if Callstack.is_empty cs
      then []
      else
        let f1, cs' = Callstack.pop cs in
        let fname = match ekind call with
          | E_call ({ekind = E_function (User_defined f)},_) -> f.fun_name
          | Stubs.Ast.E_stub_call(f,_) -> f.stub_func_name
          | _ -> assert false
        in
        if Callstack.is_empty cs'
        then [Locals f1.call_fun]
        else if f1.call_fun <> fname
        then [Locals f1.call_fun]
        else
          let f2, _ = Callstack.pop cs' in
          [Locals f1.call_fun; Locals f2.call_fun]

    | V { vkind = V_tmp _ } ->
      (* Temporary variables are considered as locals *)
      let cs = Context.ufind Callstack.ctx_key ctx in
      if Callstack.is_empty cs
      then []
      else
        let callee, _ = Callstack.pop cs in
        [Locals callee.call_fun]

    | _ ->
      []


  (** Packing function returning packs of a variable *)
  let rec packs_of_var ctx v =
    match v.vkind with
    | V_cvar _ -> packs_of_base ctx (V v)
    | Lowlevel.Cells.Domain.V_c_cell {base} -> packs_of_base ctx base
    | Lowlevel.String_length.Domain.V_c_string_length (base,_) -> packs_of_base ctx base
    | Scalars.Pointers.Domain.Domain.V_c_ptr_offset vv -> packs_of_var ctx vv
    | Scalars.Machine_numbers.Domain.V_c_num vv -> packs_of_var ctx vv
    | _ -> []

end

(** Registration *)
let () =
  Universal.Packing.Static.register_strategy (module Strategy)
