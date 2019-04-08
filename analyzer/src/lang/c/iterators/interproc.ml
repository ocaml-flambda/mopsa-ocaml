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

(** Abstraction of C function calls *)

open Mopsa
open Memory.Common.Points_to
open Universal.Ast
open Ast
open Zone


module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  let name = "c.iterators.interproc"
  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {
      provides = [];
      uses = []
    };

    ieval = {
      provides = [Z_c, Z_c_low_level];
      uses = [
        Z_c, Z_c_points_to;
        Universal.Zone.Z_u, any_zone;
        Stubs.Zone.Z_stubs, Z_any
      ]
    }
  }


  (** Initialization of environments *)
  (** ============================== *)

  let init _ _ flow =  flow


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow  = None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow =
    match ekind exp with
    | E_call(f, args) ->
      begin
        man.eval ~zone:(Zone.Z_c, Z_c_points_to) f flow |>
        Eval.bind @@ fun f flow ->

        match ekind f with
        | E_c_points_to (P_fun f)
          when Libs.Libmopsa.is_builtin_function f.c_func_org_name ||
               Libs.Libc.is_builtin_function f.c_func_org_name
          ->
          let exp' = {exp with ekind = E_c_builtin_call(f.c_func_org_name, args)} in
          man.eval ~zone:(Zone.Z_c, Zone.Z_c_low_level) exp' flow

        | E_c_points_to (P_fun ({c_func_body = Some body; c_func_stub = None; c_func_variadic = false} as fundec)) ->
          let open Universal.Ast in
          let ret_var = mktmp ~typ:fundec.c_func_return () in
          let fundec' = {
            fun_name = fundec.c_func_unique_name;
            fun_parameters = fundec.c_func_parameters;
            fun_locvars = fundec.c_func_local_vars;
            fun_body = {skind = S_c_goto_stab (body); srange = srange body};
            fun_return_type = Some fundec.c_func_return;
            fun_return_var = ret_var;
            fun_range = fundec.c_func_range;
          }
          in
          let exp' = mk_call fundec' args exp.erange in
          (* Universal will evaluate the call into a temporary variable containing the returned value *)
          man.eval ~zone:(Universal.Zone.Z_u, any_zone) exp' flow

        | E_c_points_to (P_fun ({c_func_variadic = true} as fundec)) ->
          let exp' = mk_c_call fundec args exp.erange in
          man.eval ~zone:(Zone.Z_c, Zone.Z_c_low_level) exp' flow

        | E_c_points_to (P_fun {c_func_stub = Some stub}) ->
          let exp' = Stubs.Ast.mk_stub_call stub args exp.erange in
          man.eval ~zone:(Stubs.Zone.Z_stubs, any_zone) exp' flow

        | E_c_points_to (P_fun {c_func_body = None; c_func_org_name}) ->
          panic_at exp.erange "no implementation found for function %s" c_func_org_name

        | _ -> assert false
      end |>
      Option.return


    | _ -> None



  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None

end

let () =
  Framework.Core.Sig.Stateless.Domain.register_domain (module Domain)
