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

(** Inlining of anonymous functions defined through lambda *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast


module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.objects.lambda"
      end)

    let alarms = []

    let init _ _ flow = flow

    let eval exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_py_lambda l ->
        let lname = Format.asprintf "λ%a" pp_range range in
        let fun_addr = F_user
            { py_func_var = mk_range_attr_var range lname (T_py None);
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
              py_func_ret_var = mk_range_attr_var range ("ret "^lname) (T_py None);
            } in
        eval_alloc man (A_py_function fun_addr) range flow |>
        bind_some (fun addr flow ->
            Eval.singleton (mk_py_object (addr, None) range) flow)
        |> OptionExt.return


      | _ -> None

    let exec stmt man flow = None

    let ask _ _ _ = None

  end

let () =
  register_stateless_domain (module Domain)
