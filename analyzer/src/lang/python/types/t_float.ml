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
      ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj; Z_any, Universal.Zone.Z_u_float]}
    }

    let alarms = []

    let init _ _ flow = flow

    let is_arith_unop_fun = function
      | "float.__pos__"
        | "float.__neg__" -> true
           | _ -> false

    let extract_oobject e = match ekind e with
      | E_py_object (_, Some a) -> a
      | _ -> assert false

    let eval zs exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_constant (C_top (T_float _))
      | E_constant (C_float _)
      | E_constant (C_float_interval _) ->
        Eval.singleton (mk_py_object (Addr_env.addr_float (), Some {exp with etyp=(T_float F_DOUBLE)}) range) flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("float.__new__", _))}, _)}, [cls], []) ->
        Utils.new_wrapper man range flow "float" cls
          ~fthennew:(man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top (T_float F_DOUBLE) range))

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("float.__new__", _))}, _)}, [cls; arg], []) ->
        Utils.new_wrapper man range flow "float" cls
          ~fthennew:(fun flow ->
              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) arg flow |>
              Eval.bind (fun el flow ->
                  assume
                    (mk_py_isinstance_builtin el "float" range)
                    ~fthen:(fun flow ->
                        man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top (T_float F_DOUBLE) range) flow)
                    ~felse:(fun flow ->
                        assume
                          (mk_py_isinstance_builtin el "int" range)
                          ~fthen:(fun flow ->
                              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top (T_float F_DOUBLE) range) flow)
                          ~felse:(fun flow ->
                              assume
                                (mk_py_isinstance_builtin el "str" range)
                                ~fthen:(fun flow ->
                                    man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top (T_float F_DOUBLE) range) flow)
                                ~felse:(fun flow ->
                                  let msg = Format.asprintf "float() argument must be a string or a number, not '%a'" pp_expr el in
                                    man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow |>
                                    Eval.empty_singleton)
                                man flow)
                          man flow
                      )
                    man flow
                )
            )

      (* 𝔼⟦ float.__op__(e1, e2) | op ∈ {==, !=, <, ...} ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (f, _))}, _)}, [e1; e2], [])
        when is_compare_op_fun "float" f ->
        bind_list [e1; e2] (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
        bind_some (fun el flow ->
            let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
            assume (mk_py_isinstance_builtin e1 "float" range) man flow
              ~fthen:(fun flow ->
                  assume
                    (mk_py_isinstance_builtin e2 "float" range)
                    man flow
                    ~fthen:(fun flow ->
                        assume
                          (mk_binop (extract_oobject e1) (Operators.methfun_to_binop f) (extract_oobject e2) ~etyp:(T_float F_DOUBLE) range) man flow
                          ~zone:Universal.Zone.Z_u_float
                          ~fthen:(fun flow -> man.eval (mk_py_true range) flow)
                          ~felse:(fun flow -> man.eval (mk_py_false range) flow)
                      )
                    ~felse:(fun flow ->
                        assume (mk_py_isinstance_builtin e2 "int" range) man flow
                          ~fthen:(fun flow ->
                              man.eval (mk_py_call (mk_py_attr e2 "__float__" range) [] range) flow |>
                              Eval.bind (fun e2 flow ->
                                  assume
                                    (mk_binop (extract_oobject e1) (Operators.methfun_to_binop f) (extract_oobject e2) ~etyp:(T_float F_DOUBLE) range) man flow
                                    ~zone:Universal.Zone.Z_u_float
                                    ~fthen:(fun flow -> man.eval (mk_py_true range) flow)
                                    ~felse:(fun flow -> man.eval (mk_py_false range) flow)
                                )
                            )
                          ~felse:(fun flow ->
                              debug "compare: %a at %a@\n" pp_expr exp pp_range exp.erange;
                              let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                              man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) expr flow)
                      )
                    (* ) *)
                )
              ~felse:(fun flow ->
                let msg = Format.asprintf "descriptor '%s' requires a 'float' object but received '%a'" f pp_expr e1 in
                man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow |> Eval.empty_singleton)
          )
        |>  OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (f, _))}, _)}, [e1; e2], [])
           when is_arith_binop_fun "float" f ->
         bind_list [e1; e2] (man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
           bind_some (fun el flow ->
               let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
               assume
                 (mk_py_isinstance_builtin e1 "float" range) man flow
                 ~fthen:(fun flow ->
                   assume
                     (mk_py_isinstance_builtin e2 "float" range) man flow
                     ~fthen:(fun flow ->
                       if is_reverse_operator f then
                         Eval.singleton (mk_py_object (Addr_env.addr_float (), Some (mk_binop (extract_oobject e2) (Operators.methfun_to_binop f) (extract_oobject e1) range ~etyp:(T_float F_DOUBLE))) range) flow
                       else
                         Eval.singleton (mk_py_object (Addr_env.addr_float (), Some (mk_binop (extract_oobject e1) (Operators.methfun_to_binop f) (extract_oobject e2) range ~etyp:(T_float F_DOUBLE))) range) flow
                     )
                     ~felse:(fun flow ->
                       assume
                         (mk_py_isinstance_builtin e2 "int" range) man flow
                         ~fthen:(fun flow ->
                             man.eval (mk_py_call (mk_py_attr e2 "__float__" range) [] range) flow |>
                             Eval.bind (fun e2 flow ->
                                 Eval.singleton (mk_py_object (Addr_env.addr_float (), Some (mk_binop (extract_oobject e1) (Operators.methfun_to_binop f) (extract_oobject e2) range ~etyp:(T_float F_DOUBLE))) range) flow)
                           )
                         ~felse:(fun flow ->
                           let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) expr flow)
                       )
                   )
                 ~felse:(fun false_flow ->
                   let msg = Format.asprintf "descriptor '%s' requires a 'float' object but received '%a'" f pp_expr e1 in
                   let flow = man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow in
                   Eval.empty_singleton flow)
             )
         |>  OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (f, _))}, _)}, [e], [])
           when is_arith_unop_fun f ->
         man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) e flow |>
           Eval.bind (fun el flow ->
               assume
                 (mk_py_isinstance_builtin e "float" range)
                 ~fthen:(fun true_flow ->
                     Eval.singleton (mk_py_object (Addr_env.addr_float (), Some (mk_unop (Operators.methfun_to_unop f) (extract_oobject el) range ~etyp:(T_float F_DOUBLE))) range) true_flow
                   )
                 ~felse:(fun false_flow ->
                   let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                   man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) expr false_flow)
                 man flow
             )
         |> OptionExt.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("float.__hash__" as f, _))}, _)}, args, []) ->
        Utils.check_instances f man flow range args ["float"] (fun _ -> man.eval (mk_py_top T_int range))
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("float.__bool__" as f, _))}, _)}, args, []) ->
        Utils.check_instances f man flow range args ["float"]
          (fun e flow ->
             (* FIXME: best way? *)
             assume
               (mk_binop (extract_oobject @@ List.hd e) O_eq (mk_float 0. range) ~etyp:(T_float F_DOUBLE) range)
               man flow
               ~zone:Universal.Zone.Z_u_float
               ~fthen:(fun flow -> man.eval (mk_py_false range) flow)
               ~felse:(fun flow -> man.eval (mk_py_true range) flow)
          )
        |> OptionExt.return


      | _ -> None

    let exec _ _ _ _ = None
    let ask _ _ _ = None
  end

let () = Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
