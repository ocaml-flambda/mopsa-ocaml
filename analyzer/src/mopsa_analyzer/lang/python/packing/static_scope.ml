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

(** Simple packing strategy copying the C static packing.

    Global variables are kept in the same pack
    Functions have their own packs containing parameters, local variables and return value
    Addr attribute packs with addr_kind?
*)

open Mopsa
open Addr
open Universal.Packing.Static
open Universal.Ast
open Ast

module Strategy =
struct
  (** {2 Packs} *)
  (** ********* *)

  (** Packing key *)
  type pack =
    | Globals          (** Pack of global variables *)
    | Locals of var    (** Pack of local variables of a function *)
    (* | Addr of addr_kind   (\** ??? *\) *)

  (** Generate a unique ID for the strategy *)
  include GenDomainId(struct
      type t = pack
      let name = "python.packing.static_scope"
    end)

  (** Total order of packing keys *)
  let compare k1 k2 =
    match k1, k2 with
    | Globals, Globals -> 0
    | Locals f1, Locals f2 -> compare f1 f2
    | _ -> compare k1 k2


  (** Pretty printer of packing keys *)
  let print printer = function
    | Globals  -> pp_string printer "pack-[globals]"
    | Locals f -> pp_string printer (Format.asprintf "pack-%a" pp_var f)
    (* | User u  -> pp_user_pack printer u *)

  (** Initialization *)
  let init prog = ()

  let compare_var_chtype v v' =
    compare_var v {v' with vtyp = v.vtyp; vname = Format.asprintf "%s:%a" v'.vname pp_typ v.vtyp }

  (** Packing function returning packs of a variable *)
  let rec packs_of_var ctx v =
    let _, globals, body = find_ctx Ast.py_program_ctx ctx in
    let fundecs = Visitor.fold_stmt (fun acc e -> VisitParts acc)
                    (fun acc s ->
                      match skind s with
                      | S_py_function f ->
                         VisitParts (f :: acc)
                      | _ ->
                         VisitParts acc
                    ) [] body in
    match v.vkind with
    | V_uniq _ ->
       if List.exists (fun v' ->
              compare_var_chtype v v' = 0) globals then
         if String.rcontains_from (get_orig_vname v) 0 '_' then [] else [Globals]
        else begin
            match List.find_opt (fun f ->
                      List.exists (fun v' -> compare_var_chtype v v' = 0) f.py_func_parameters ||
                        List.exists (fun v' -> compare_var_chtype v v' = 0) f.py_func_locals ||
                          compare_var_chtype v f.py_func_ret_var = 0
                    ) fundecs with
            | None -> []
            | Some f -> [Locals f.py_func_var]
          end
    (* Search in body to find corresponding function *)
    (* | V_addr_attr({addr_kind = Python.Addr.A_py_instance {}}, _) -> {} *)

    | V_addr_attr({addr_kind = (Objects.Py_list.A_py_list | Objects.Py_list.A_py_iterator _ | A_py_instance {addr_kind = A_py_class (C_builtin "range", _) }); addr_partitioning = Universal.Heap.Policies.G_range r | Universal.Heap.Policies.G_stack_range (_, r)}, _) ->
       begin
         match List.find_opt (fun f -> subset_range r f.py_func_range) fundecs with
         | None -> [Globals]
         | Some f -> [Locals f.py_func_var]
       end

    | _ ->
       debug "packs_of_var %a: []" pp_var v;
       []
end

(** Registration *)
let () =
  Universal.Packing.Static.register_strategy (module Strategy);
  Universal.Packing.Intervals_static_scope.register_itv_packing_reduction (module Strategy)
