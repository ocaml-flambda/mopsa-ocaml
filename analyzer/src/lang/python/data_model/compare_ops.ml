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

(** Python data model for comparison operators. *)


open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Addr
open Operators
open Universal.Ast


module Domain = struct

  include GenStatelessDomainId(struct
      let name = "python.data_model.compare_ops"
    end)

  let checks = []

  let init _ _ flow = flow

  let eval exp man flow =
    let range = erange exp in
    if is_py_exp exp then
    match ekind exp with
    | E_binop(op, e1, e2) when is_comp_op op ->
      (* CPython: object.c, function do_richcompare *)
      let is_notimplemented x =
        let not_implemented_type = mk_py_object (find_builtin "NotImplementedType") range in
        mk_py_isinstance x not_implemented_type range in

      bind_list [e1; e2] (man.eval   ) flow |>
      bind_result (fun el flow ->
      let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in

      let op_fun, rop_fun =
        match op with
        | O_eq -> "__eq__", "__eq__"
        | O_ne -> "__ne__", "__ne__"
        | O_lt -> "__lt__", "__gt__"
        | O_le -> "__le__", "__ge__"
        | O_gt -> "__gt__", "__lt__"
        | O_ge -> "__ge__", "__le__"
        | _ -> assert false
      in

      man.eval   (mk_py_type e1 range) flow >>$
      (fun ocls1 flow ->
      let cls1 = object_of_expr ocls1 in
      man.eval   (mk_py_type e2 range) flow >>$
      (fun ocls2 flow ->
      let cls2 = object_of_expr ocls2 in
      let is_same_type = compare_py_object cls1 cls2 = 0 in

      let call_op checked_reverse flow =
        let switch flow =
          match op with
          | O_eq | O_ne ->
            assume (mk_expr ~etyp:(T_py None) (E_binop(O_py_is, e1, e2)) range) man flow
              ~fthen:(man.eval   (mk_py_bool (O_eq =  op) range))
              ~felse:(man.eval   (mk_py_bool (O_eq <> op) range))
          | _ ->
            Format.fprintf Format.str_formatter "'%s' not supported between instances of '%a' and '%a'" op_fun pp_addr_kind (akind @@ fst cls1) pp_addr_kind (akind @@ fst cls2);
            man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.flush_str_formatter ()) range) flow >>%
            Eval.empty
            in
        let check_reverse flow =
          if not checked_reverse then
            assume (mk_py_hasattr ocls2 rop_fun range)
              man flow
              ~fthen:(fun flow ->
                  man.eval
                    (mk_py_call (mk_py_object_attr cls2 rop_fun range) [e2; e1] range) flow >>$
 (fun cmp flow ->
                      assume (is_notimplemented cmp) man flow
                        ~fthen:switch
                        ~felse:(Eval.singleton cmp)
                    )
                )
              ~felse:switch
          else
            switch flow in
        assume (mk_py_hasattr ocls1 op_fun range)
          man flow
          ~fthen:(fun flow ->
              man.eval
                (mk_py_call (mk_py_object_attr cls1 op_fun range) [e1; e2] range) flow >>$
 (fun cmp flow ->
                  assume (is_notimplemented cmp)
                    man flow
                    ~fthen:check_reverse
                    ~felse:(Eval.singleton cmp)

                )
            )
          ~felse:check_reverse
      in

      assume
        (mk_binop ~etyp:(T_py None) (mk_py_bool is_same_type range) O_py_and
           (mk_binop ~etyp:(T_py None) (mk_py_issubclass ocls2 ocls1 range) O_py_and
              (mk_py_hasattr ocls2 rop_fun range) range) range)
        man flow
        ~fthen:(fun flow ->
            man.eval
              (mk_py_call (mk_py_object_attr cls2 rop_fun range) [e2; e1] range) flow >>$
 (fun cmp flow ->
                assume (is_notimplemented cmp)
                  man flow
                  ~fthen:(call_op true)
                  ~felse:(Eval.singleton cmp)
              )
          )
        ~felse:(call_op false)
        )))
      |> OptionExt.return

    | _ -> None
    else None

  let exec _ _ _ = None
  let ask _ _ _ = None

end

let () =
  register_stateless_domain (module Domain)
