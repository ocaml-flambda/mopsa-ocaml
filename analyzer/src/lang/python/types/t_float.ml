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
open Sig.Domain.Stateless
open Ast
open MapExt
open Addr
open Universal.Ast
(* gérer les appels sur float + constantes *)

module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.types.t_float"
      end)

    let interface = {
      iexec = {provides = []; uses = []};
      ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]}
    }

    let init _ _ flow = flow

    let is_arith_unop_fun = function
      | "float.__pos__"
        | "float.__neg__" -> true
           | _ -> false

    let eval zs exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "float.__new__")}, _)}, [cls], []) ->
         (* FIXME?*)
         man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top (T_float F_DOUBLE) range) flow |> Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "float.__new__")}, _)}, [cls; arg], []) ->
         (* FIXME?*)
         man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) arg flow |>
           Eval.bind (fun el flow ->
               assume_eval
                 (mk_py_isinstance_builtin el "float" range)
                 ~fthen:(fun flow ->
                   man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top (T_float F_DOUBLE) range) flow)
                 ~felse:(fun flow ->
                   assume_eval
                     (mk_py_isinstance_builtin el "int" range)
                     ~fthen:(fun flow ->
                       man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top (T_float F_DOUBLE) range) flow)
                     ~felse:(fun flow ->
                       assume_eval
                         (mk_py_isinstance_builtin el "str" range)
                         ~fthen:(fun flow ->
                           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top (T_float F_DOUBLE) range) flow)
                         ~felse:(fun flow ->
                           man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
                             Eval.empty_singleton)
                         man flow)
                     man flow
                 )
                 man flow
             )
         |> Option.return


      (* 𝔼⟦ float.__op__(e1, e2) | op ∈ {==, !=, <, ...} ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e1; e2], [])
        when is_compare_op_fun "float" f ->
        Eval.eval_list (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) [e1; e2] flow |>
        Eval.bind (fun el flow ->
            let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
            assume_eval (mk_py_isinstance_builtin e1 "float" range) man flow
              ~fthen:(fun flow ->
                  (* let float_or_int = mk_binop
                   *     (mk_py_isinstance_builtin e2 "float" range)
                   *     O_py_or
                   *     (mk_py_isinstance_builtin e2 "int" range)
                   *     range
                   * in *)
                  assume_eval
                    (mk_py_isinstance_builtin e2 "float" range)
                    (* float_or_int *)
                    man flow
                    ~fthen:(man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_bool range))
                    ~felse:(fun flow ->
                        assume_eval (mk_py_isinstance_builtin e2 "int" range) man flow
                          ~fthen:(fun flow ->
                              (* Exceptions.panic_at range "eurk %a@\n" man.pri(Flow.print man.lattice) flow; *)
                              let res = man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_bool range) flow in
                              let overflow = man.exec (Utils.mk_builtin_raise "OverflowError" range) flow |> Eval.empty_singleton in
                              Eval.join_list (Eval.copy_ctx overflow res :: overflow :: [])
                            )
                          ~felse:(fun flow ->
                              debug "compare: %a at %a@\n" pp_expr exp pp_range exp.erange;
                              let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) expr flow)
                      )
                    (* ) *)
                )
              ~felse:(fun flow ->
                  man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton)
          )
        |>  Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, [e1; e2], [])
           when is_arith_binop_fun "float" f ->
         Eval.eval_list (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) [e1; e2] flow |>
           Eval.bind (fun el flow ->
               let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
               assume_eval
                 (mk_py_isinstance_builtin e1 "float" range)
                 ~fthen:(fun true_flow ->
                   assume_eval
                     (mk_py_isinstance_builtin e2 "float" range)
                     ~fthen:(fun true_flow ->
                       man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top (T_float F_DOUBLE) range) true_flow)
                     ~felse:(fun false_flow ->
                       assume_eval
                         (mk_py_isinstance_builtin e2 "int" range)
                         ~fthen:(fun flow -> man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top (T_float F_DOUBLE) range) true_flow)
                         ~felse:(fun false_flow ->
                           let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) expr false_flow)
                         man false_flow)
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
                 (mk_py_isinstance_builtin e "float" range)
                 ~fthen:(fun true_flow ->
                   man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top (T_float F_DOUBLE) range) true_flow)
                 ~felse:(fun false_flow ->
                   let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                   man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) expr false_flow)
                 man flow
             )
         |> Option.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "float.__hash__")}, _)}, args, []) ->
        Utils.check_instances man flow range args ["float"] (fun _ -> man.eval (mk_py_top T_int range))
        |> Option.return

      | _ -> None

    let exec _ _ _ _ = None
    let ask _ _ _ = None
  end

let () = Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
