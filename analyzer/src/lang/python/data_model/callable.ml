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

(** Python data model for callables. *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Ast
open Addr
open Universal.Ast

module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.data_model.callable"
      end)

    let interface = {
      iexec = {provides = []; uses = []};
      ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]}
    }

    let alarms = []

    let init _ _ flow = flow

    let exec _ _ _ _ = None

    let eval zs exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_py_call({ekind = E_py_object _}, _, _) -> None

      | E_py_call(f, args, kwargs) when List.for_all (fun (so, _) -> so <> None) kwargs ->
        let start = Timing.start () in
        debug "Calling %a from %a" pp_expr exp pp_range exp.erange;
        let res =
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) f flow |>
          Eval.bind
            (fun f flow ->
               debug "f is now %a" pp_expr f;
               match ekind f with
               (* Calls on non-object variables and constants is not allowed *)
               | E_var _ | E_constant _ ->
                 let () = Format.fprintf Format.str_formatter "object is not callable"  in
                 let stmt = Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range in
                 let flow = man.exec stmt flow in
                 Eval.empty_singleton flow

               (* Calls on other kinds of addresses is handled by other domains *)
               | E_py_object ({addr_kind = A_py_class _}, _)
               | E_py_object ({addr_kind = A_py_function _}, _)
               | E_py_object ({addr_kind = A_py_method _}, _)
               | E_py_object ({addr_kind = A_py_module _}, _) ->
                 let exp = {exp with ekind = E_py_call(f, args, kwargs)} in
                 man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj) exp flow

               | _ ->
                 (* if f has attribute call, restart with that *)
                 assume
                   (mk_py_hasattr f "__call__" range)
                   ~fthen:(fun flow ->
                       man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_kall (mk_py_attr f "__call__" range) args kwargs range) flow)
                   ~felse:(fun flow ->
                       panic_at range "callable/E_py_call, on %a, exp=%a@\n" pp_expr f pp_expr exp
                     )
                   man flow
            ) in
        Debug.debug ~channel:"profiling" "Call %a: %.4f at range %a" pp_expr f (Timing.stop start) pp_range range;
        res |> Option.return

      | E_py_call(f, args, _) ->
        panic_at range "calls with **kwargs not supported"

      | _ -> None


    let ask _ _ _ = None

  end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
