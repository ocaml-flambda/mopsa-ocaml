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

(** Range objects. *)

open Mopsa
open Sig.Domain.Stateless
open Ast
open Addr
open Universal.Ast

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.objects.range"
    end)

  let interface = {
    iexec = {provides = []; uses = []};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]}
  }

  let allocate_builtin ?(mode=STRONG) man range flow bltin oe =
    (* allocate addr, and map this addr to inst bltin *)
    let range = tag_range range "alloc_%s" bltin in
    let cls = fst @@ find_builtin bltin in
    man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) (mk_alloc_addr ~mode:mode (A_py_instance cls) range) flow |>
    Eval.bind (fun eaddr flow ->
        let addr = match ekind eaddr with
          | E_addr a -> a
          | _ -> assert false in
        man.exec ~zone:Zone.Z_py_obj (mk_add eaddr range) flow |>
        Eval.singleton (mk_py_object (addr, oe) range)
      )

  let alarms = []

  let rec eval zs exp man flow =
    let range = exp.erange in
    match ekind exp with
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("slice.__new__" as f, _))}, _)}, cls :: args, []) ->
      Utils.new_wrapper man range flow "slice" cls
        ~fthennew:(fun flow ->
            let intornone = ["int"; "NoneType"] in
            Utils.check_instances_disj f man flow range args
              [intornone; intornone; intornone]
              (fun _ flow -> allocate_builtin man range flow "slice" (Some exp))
          )

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__new__", _))}, _)} as call, cls :: [up], []) ->
      let args' = (mk_constant T_int (C_int (Z.of_int 0)) range)::up::(mk_constant T_int (C_int (Z.of_int 1)) range)::[] in
      man.eval {exp with ekind = E_py_call(call, cls :: args', [])} flow
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__new__", _))}, _)} as call, cls :: [down; up], []) ->
      let args' = down::up::(mk_constant T_int (C_int (Z.of_int 1)) range)::[] in
      man.eval {exp with ekind = E_py_call(call, cls :: args', [])} flow
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__new__" as f, _))}, _)}, cls :: args, []) ->
      Utils.new_wrapper man range flow "range" cls
        ~fthennew:(fun flow ->
            Utils.check_instances f man flow range args
              ["int"; "int"; "int"]
              (fun args flow -> allocate_builtin man range flow "range" (Some exp))
          )

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__contains__", _))}, _)}, args, []) ->
      (* isinstance(arg1, range) && isinstance(arg2, int) ? *)
      Exceptions.panic "todo: %a@\n" pp_expr exp

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__len__", _))}, _)}, [arg], []) ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) arg flow |>
      Eval.bind (fun arg flow ->
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range) flow
          (* TODO: which one is better? *)
          (* process_constant man flow range "int" addr_integers *)
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__iter__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["range"]
        (fun r flow -> allocate_builtin man range flow "range_iterator" (Some exp))
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__reversed__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["range"]
        (fun r flow -> allocate_builtin man range flow "range_iterator" (Some exp))
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range_iterator.__next__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["range_iterator"]
        (fun _ flow ->
           let res = man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range) flow in
           let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton in
           Eval.join_list (Eval.copy_ctx stopiteration res :: stopiteration :: []) ~empty:(fun () -> Eval.empty_singleton flow)
        )
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range_iterator.__iter__", _))}, _)}, [self], []) ->
      man.eval self flow |> Option.return

    | _ -> None

  let init _ _ flow = flow
  let exec _ _ _ _ = None
  let ask _ _ _ = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
