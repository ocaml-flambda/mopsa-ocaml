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

    let name = "python.data_model.callable"
    let debug fmt = Debug.debug ~channel:name fmt

    let interface = {
      iexec = {provides = []; uses = []};
      ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]}
    }

    let init _ _ flow = flow

    let exec _ _ _ _ = None

    let eval zs exp man flow =
      let range = erange exp in
      match ekind exp with
      (* FIXME *)
      | E_py_call({ekind = E_py_object _}, _, _) -> None
      (*   (\* Calls to addresses should be captured by other domains. If we
       *      are here, then we are missing an implementation of the
       *      function *\)
       *   panic_at range "call %a can not be resolved" pp_expr exp *)

      | E_py_call(f, args, []) ->
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
                 let stmt = Utils.mk_builtin_raise "TypeError" range in
                 let flow = man.exec stmt flow in
                 Eval.empty_singleton flow

               (* Calls on other kinds of addresses is handled by other domains *)
               | E_py_object ({addr_kind = A_py_class _}, _)
               | E_py_object ({addr_kind = A_py_function _}, _)
               | E_py_object ({addr_kind = A_py_method _}, _)
               | E_py_object ({addr_kind = A_py_module _}, _) ->

                 (* Eval.eval_list args (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
                  * Eval.bind (fun args flow -> *)
                 let exp = {exp with ekind = E_py_call(f, args, [])} in
                 man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj) exp flow
               (* ) *)

               | _ ->
                 (* if f has attribute call, restart with that *)
                 assume_eval
                   (mk_py_hasattr f "__call__" range)
                   ~fthen:(fun flow ->
                       man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call (mk_py_attr f "__call__" range) args range) flow)
                   ~felse:(fun flow ->
                       debug "callable/E_py_call, on %a@\n" pp_expr f; assert false
                     )
                   man flow
            ) in
        Debug.debug ~channel:"profiling" "Call %a: %.4f at range %a" pp_expr f (Timing.stop start) pp_range range;
        res |> Option.return

      | E_py_call(f, args, _) ->
        panic_at range "calls with keyword arguments not supported"

      | _ -> None


    let ask _ _ _ = None

  end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
