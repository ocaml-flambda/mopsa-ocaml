(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2019 The MOPSA Project.                                    *)
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

open Mopsa
open Ast
open MapExt
open Addr
open Universal.Ast
(* gérer les appels sur int + constantes *)

module Domain =
  struct
    let name = "python.types.t_int"
    let debug fmt = Debug.debug ~channel:name fmt

    let interface = {
      iexec = {provides = []; uses = []};
      ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]}
    }

    let init _ _ flow = flow

    let is_arith_unop_fun = function
      | "int.__pos__"
        | "int.__neg__"
        | "int.__invert__" -> true
           | _ -> false

    let eval zs exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_class (C_builtin "bool", _)}, _)}, [arg], [])
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "bool.__new__")}, _)}, [_; arg], []) ->
         (* FIXME: check: According to the documentation: By default,
            an object is considered true unless its class defines
            either a __bool__() method that returns False or a __l
            en__() method that returns zero, when called with the
            object.  *)

         man.eval arg flow |>
           Eval.bind (fun earg flow ->
               assume_eval (mk_py_isinstance_builtin earg "bool" range)
                 ~fthen:(Eval.singleton earg)
                 ~felse:(fun flow ->
                   assume_eval
                     (mk_py_hasattr earg "__bool__" range)
                     ~fthen:(fun flow ->
                       let attr = mk_py_attr earg "__bool__" range in
                       man.eval (mk_py_call attr [] range) flow
                     )
                     ~felse:(fun flow ->
                       assume_eval
                         (mk_py_hasattr earg "__len__" range)
                         ~fthen:(fun flow ->
                           let attr = mk_py_attr earg "__len__" range in
                           let comp = mk_binop (mk_py_call attr [] range) O_ne (mk_zero range) range in
                           man.eval comp flow)
                         ~felse:(fun flow ->
                           man.eval (mk_py_true range) flow)
                         man flow
                     )
                     man flow
                 )
                 man flow
             )
         |> Option.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "int.__new__")}, _)}, [cls], []) ->
        man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range) flow |> Option.return

      | E_py_call(({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "int.__new__")}, _)} as f), [cls; arg], []) ->
         (* FIXME *)
         debug "ok, let's move on@\n";
         man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) {exp with ekind = E_py_call(f, [cls; arg; mk_int 10 range], [])} flow |> Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "int.__new__")}, _)}, [cls; str; base], []) ->
         (* FIXME?*)
         debug "ok@\n";
         man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range) flow |> Option.return

      (* 𝔼⟦ int.__op__(e1, e2) | op ∈ {==, !=, <, ...} ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e1; e2], [])
           when is_compare_op_fun "int" f ->
         Eval.eval_list (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) [e1; e2] flow |>
           Eval.bind (fun el flow ->
               let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
               assume_eval
                 (mk_py_isinstance_builtin e1 "int" range)
                 ~fthen:(fun true_flow ->
                   assume_eval
                     (mk_py_isinstance_builtin e2 "int" range)
                     ~fthen:(fun true_flow ->
                       man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_bool range) true_flow)
                     ~felse:(fun false_flow ->
                       let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                       man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) expr false_flow)
                     man true_flow
                 )
                 ~felse:(fun false_flow ->
                   let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow in
                   Eval.empty_singleton flow)
                 man flow
             )
         |>  Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e1; e2], [])
           when is_arith_binop_fun "int" f ->
         Eval.eval_list (man.eval~zone:(Zone.Z_py, Zone.Z_py_obj)) [e1; e2] flow |>
           Eval.bind (fun el flow ->
               let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
               assume_eval
                 (mk_py_isinstance_builtin e1 "int" range)
                 ~fthen:(fun true_flow ->
                   assume_eval
                     (mk_py_isinstance_builtin e2 "int" range)
                     ~fthen:(fun true_flow ->
                       match f with
                       | "int.__truediv__"
                         | "int.__rtruediv__" ->
                          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top (T_float F_DOUBLE) range) true_flow
                       | _ -> man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range) true_flow)
                     ~felse:(fun false_flow ->
                       let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                       man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) expr false_flow)
                     man true_flow
                 )
                 ~felse:(fun false_flow ->
                   let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow in
                   Eval.empty_singleton flow)
                 man flow
             )
         |>  Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e], [])
           when is_arith_unop_fun f ->
         man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
           Eval.bind (fun el flow ->
               assume_eval
                 (mk_py_isinstance_builtin e "int" range)
                 ~fthen:(fun true_flow ->
                   man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range) true_flow)
                 ~felse:(fun false_flow ->
                   let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                   man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) expr false_flow)
                 man flow
             )
         |> Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "int.__bool__")}, _)}, args, []) ->
        Utils.check_instances man flow range args
          ["int"]
          (fun _ flow -> man.eval (mk_py_top T_bool range) flow)
        |> Option.return

      | _ -> None

    let exec _ _ _ _ = None
    let ask _ _ _ = None
  end

let () = Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
