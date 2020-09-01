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

(** Python data model for arithmetic operators. *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Addr
open Operators
open Universal.Ast


module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.data_model.arith_ops"
      end)

    let alarms = []

    let init _ _ flow = flow

    let eval exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_binop(op, e1, e2) when is_arith_op op && etyp exp = (T_py None) ->
         (* CPython's equivalent code is located in abstract.c,
            binary_op1 and typeobject.c, SLOT1BINFULL *)
         let is_notimplemented x =
           let not_implemented_type = mk_py_object (find_builtin "NotImplementedType") range in
           mk_py_isinstance x not_implemented_type range in

         bind_list [e1; e2] (man.eval  ) flow |>
           bind_some
             (fun el flow ->
               let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
               let op_fun = binop_to_fun op in
               let rop_fun = binop_to_rev_fun op in

               man.eval   (mk_py_type e1 range) flow >>$
 (fun ocls1 flow ->
                     let cls1 = object_of_expr ocls1 in
                     man.eval   (mk_py_type e2 range) flow >>$
 (fun ocls2 flow ->
                           let cls2 = object_of_expr ocls2 in

                           let is_same_type = compare_py_object cls1 cls2 = 0 in
                           let typerr flow =
                             let msg = Format.asprintf "unsupported operand type(s) for '%a': '%a' and '%a'" pp_operator op pp_addr_kind (akind @@ fst cls1) pp_addr_kind (akind @@ fst cls2) in
                             man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow >>%
                             Eval.empty_singleton in
                           let call_radd man ocondtocheck flow ~felseradd =
                             let hasradd = Utils.mk_object_hasattr cls2 rop_fun range in
                             assume
                               (if ocondtocheck = None then hasradd else (mk_binop ~etyp:(T_py None) hasradd O_py_and (OptionExt.none_to_exn ocondtocheck) range))
                               man flow
                               ~fthen:(fun flow ->
                                 man.eval   (mk_py_call (mk_py_object_attr cls2 rop_fun range) [e2; e1] range) flow >>$
 (fun r flow ->
                                       assume
                                         (is_notimplemented r)
                                         man flow
                                         ~fthen:(typerr)
                                         ~felse:(Eval.singleton r)
                                     )
                               )
                               ~felse:felseradd in

                           assume
                             (Utils.mk_object_hasattr cls1 op_fun range)
                             man flow
                             ~fthen:(fun flow ->
                               let call_add flow =
                                 man.eval   (mk_py_call (mk_py_object_attr cls1 op_fun range) [e1; e2] range) flow >>$
 (fun r flow ->
                                       assume (is_notimplemented r)
                                         man flow
                                         ~fthen:(fun flow ->
                                           if is_same_type then typerr flow
                                           else call_radd man None flow ~felseradd:(typerr)
                                         )
                                         ~felse:(Eval.singleton r)
                                     ) in
                               if is_same_type then call_add flow else
                                 call_radd man (Some (mk_py_issubclass ocls2 ocls1 range)) flow
                                   ~felseradd:call_add
                             )
                             ~felse:(fun flow ->
                               if is_same_type then typerr flow
                               else call_radd man None flow ~felseradd:(typerr)
                             )
                         )
                   )
             )
         |> OptionExt.return

      | E_unop(op, e) when is_arith_op op && etyp exp = (T_py None) ->
         debug "Resolving unary operator %a" pp_operator op;
         man.eval   e flow >>$
 (fun e flow ->
               debug "Subexpression evaluated to %a(%a)" pp_expr e pp_typ e.etyp;
               let op_fun = unop_to_fun op in
               man.eval   (mk_py_type e range) flow >>$
 (fun cls flow ->
                     let cls = object_of_expr cls in
                     assume
                       (Utils.mk_object_hasattr cls op_fun range)
                       ~fthen:(fun true_flow ->
                         man.eval   (mk_py_call (mk_py_object_attr cls op_fun range) [e] range) true_flow
                       )
                       ~felse:(fun false_flow ->
                         let msg = Format.asprintf "bad operand type for unary '%s': '%a'" op_fun pp_addr_kind (akind @@ fst cls) in
                         man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) false_flow >>%
                         Eval.empty_singleton
                       )
                       man flow
                   )
             )
         |> OptionExt.return
      | _ -> None

    let exec _ _ _ = None
    let ask _ _ _ = None

  end

let () =
  register_stateless_domain (module Domain)
