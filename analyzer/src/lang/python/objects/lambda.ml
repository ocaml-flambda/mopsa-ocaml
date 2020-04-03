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

(** Definition of python functions and evaluation of their calls. *)

open Mopsa
open Sig.Domain.Stateless
open Ast
open Addr
open Universal.Ast


module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.objects.lambda"
      end)

    let interface = {
      iexec = {provides = [Zone.Z_py]; uses = []};
      ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]}
    }

    let alarms = []

    let init _ _ flow = flow

    let eval zs exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_py_lambda l ->
        let lname = Format.asprintf "λ%a" pp_range range in
        let fun_addr = F_user
            { py_func_var = mk_range_attr_var range lname T_any;
              py_func_parameters = l.py_lambda_parameters;
              py_func_defaults = l.py_lambda_defaults;
              py_func_vararg = None;
              py_func_kwonly_args = [];
              py_func_kwonly_defaults = [];
              py_func_kwarg = None;
              py_func_locals = [];
              py_func_body =
                mk_stmt (S_return (Some l.py_lambda_body)) range;
              py_func_is_generator = false;
              py_func_decors = [];
              py_func_types_in = [];
              py_func_type_out = None;
              py_func_range = range;
              py_func_ret_var = mk_range_attr_var range ("ret "^lname) T_any;
            } in
        eval_alloc man (A_py_function fun_addr) range flow |>
        bind_some (fun addr flow ->
            Eval.singleton (mk_py_object (addr, None) range) flow)
        |> OptionExt.return


      | _ -> None

    let exec zone stmt man flow = None

    let ask _ _ _ = None

  end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
