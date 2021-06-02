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

(** Data model for attribute access. *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast

module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.data_model.attribute"
      end)

    let checks = []

    let init _ _ flow = flow

    let search_mro man range flow attr c mro =
      let rec search mro =
        match mro with
        | [] ->
           let msg = Format.asprintf "'%s' object has no attribute '%s'" c attr in
           man.exec (Utils.mk_builtin_raise_msg "AttributeError" msg range) flow >>%
             Eval.empty
        | cls::tl ->
           if is_builtin_attribute cls attr then
             Flow.add_safe_check Alarms.CHK_PY_ATTRIBUTEERROR range flow |>
             Eval.singleton (mk_py_object (find_builtin_attribute cls attr) range)
           else
             search tl in
      search mro

    let eval expr man flow =
      let range = erange expr in
      match ekind expr with
      | E_py_attribute(e, ("__name__" as attr)) ->
         man.eval e flow >>$ (fun ee flow ->
         match ekind ee with
         | E_py_object ({addr_kind = A_py_class (c, _)}, _) ->
            let name = match c with
              | C_builtin c -> c
              | C_unsupported s -> s
              | C_user u -> get_orig_vname u.py_cls_var
              | C_annot a -> get_orig_vname a.py_cls_a_var in
            man.eval (mk_string ~etyp:(T_py None) name range) flow
         | _ ->
            panic_at range "Access to special attribute %s not supported (%a)" attr pp_expr ee
                             ) |> OptionExt.return

      (* Special attributes *)
      | E_py_attribute(obj, ("__dict__" as attr))
        | E_py_attribute(obj, ("__bases__" as attr))
        | E_py_attribute(obj, ("__qualname__" as attr))
        | E_py_attribute(obj, ("__mro__" as attr))
        | E_py_attribute(obj, ("mro" as attr))
        | E_py_attribute(obj, ("__subclass__" as attr)) ->
         panic_at range "Access to special attribute %s not supported (%a)" attr pp_expr obj

      | E_py_attribute(obj, "__class__") ->
         man.eval (mk_py_type obj range) flow |> OptionExt.return

      (* Attributes of builtin classes are static, so we can be more efficient *)
      | E_py_attribute ({ekind = E_py_object ({addr_kind = A_py_class (C_builtin c, mro)}, _)}, attr) ->
        search_mro man range flow attr c mro |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("getattr", _))}, _)}, e::attr::[], [])  ->
        man.eval attr flow >>$
          (fun eattr flow ->
            match ekind eattr with
            | E_py_object (_, Some {ekind = E_constant (C_string attr)}) ->
              man.eval (mk_py_attr e attr range) flow
            | _ ->
              assume (mk_py_isinstance_builtin eattr "str" range) man flow
                ~fthen:(fun flow -> man.eval (mk_py_top (T_py None) range) flow)
                ~felse:(fun flow ->
                    panic_at range "getattr with attr=%a" pp_expr eattr)
          )
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("getattr", _))}, _)}, obj::attr::default::[], [])  ->
         bind_list [obj; attr] man.eval flow >>$? (fun ee flow ->
           let obj, attr = match ee with [e1; e2] -> e1, e2 | _ -> assert false in
           match ekind attr with
           | E_py_object (_, Some {ekind = E_constant (C_string attr)}) ->
              Utils.try_eval_expr man (mk_py_attr obj attr range) flow
                ~on_empty:(fun exc_exp exc_str _ flow ->
                  OptionExt.return @@
                  if exc_str = "AttributeError" then
                    man.eval default flow
                  else
                    man.exec (mk_raise exc_exp range) flow >>% Eval.empty
                )
                ~on_result:(fun res flow -> Eval.singleton res flow)
           | _ ->
              panic " expected a string object, got %a" pp_expr attr
           )

      (* Other attributes *)
      | E_py_attribute (e, attr) ->
         debug "%a@\n" pp_expr expr;
         let c_attr = mk_constant ~etyp:(T_py (Some Str)) (C_string attr) range in
         man.eval e   flow >>$
           (fun exp flow ->
             match ekind exp with
             | E_py_object ({addr_kind = A_py_class (C_builtin c, mro)}, _) ->
                search_mro man range flow attr c mro

             | _ ->
               debug "other: %a@\n" pp_expr exp;
               (* In a bottom environment, the only thing that we can
                  do is to search for builtins attributes and resolve
                  them statically.
               *)
               if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom then
                 let oexp = object_of_expr exp in
                 let oname = oobject_name oexp in
                 if oname <> None && is_builtin_name (OptionExt.none_to_exn oname) && is_builtin_attribute oexp attr then
                   let rese = mk_py_object (find_builtin_attribute oexp attr) range in
                   Eval.singleton rese flow
                 else
                   Eval.empty flow
               else
                 man.eval   (mk_py_type exp range) flow >>$
                   (fun class_of_exp flow ->
                     let mro = mro (object_of_expr class_of_exp) in
                     debug "mro of %a: %a" pp_expr class_of_exp (Format.pp_print_list (fun fmt (a, _) -> pp_addr fmt a)) mro;
                     let rec search_mro ?(empty=(fun () -> assert false)) flow attr mro = match mro with
                       | [] -> empty ()
                       | cls::tl ->
                         assume
                           (mk_expr ~etyp:(T_py None) (E_py_ll_hasattr (mk_py_object cls range, mk_string attr range)) range)
                           ~fthen:(fun flow -> man.eval (mk_expr ~etyp:(T_py None) (E_py_ll_getattr (mk_py_object cls range, mk_string attr range)) range) flow)
                           ~felse:(fun flow -> search_mro ~empty:empty flow attr tl)
                           man flow in
                     search_mro flow "__getattribute__" mro >>$
                       (fun getattribute flow ->
                         bind_list [exp; c_attr] man.eval flow >>$ (fun ee flow ->
                          let exp, c_attr = match ee with [e1;e2] -> e1, e2 | _ -> assert false in
                          OptionExt.none_to_exn @@
                          Utils.try_eval_expr man (mk_py_call getattribute [exp; c_attr] range) flow
                            ~on_empty:(fun exc_exp exc_str exc_msg flow ->
                              OptionExt.return @@
                              if exc_str = "AttributeError" then
                                search_mro ~empty:(fun () ->
                                    man.exec (mk_raise exc_exp range) flow >>% Eval.empty
                                  ) flow "__getattr__" mro >>$ fun getattr flow ->
                                  man.eval (mk_py_call getattr [exp; c_attr] range) flow
                              else
                                man.exec (mk_raise exc_exp range) flow >>% Eval.empty
                            )
                            ~on_result:(fun res flow ->
                              Eval.singleton res flow
                            )
                        )
                       )
                   )
           )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("hasattr", _))}, _)}, [obj; attr], []) ->
         bind_list [obj; attr] man.eval flow >>$? (fun ee flow ->
           let obj, attr = match ee with [e1; e2] -> e1, e2 | _ -> assert false in
           match ekind attr with
           | E_py_object (_, Some {ekind = E_constant (C_string attr)}) ->
              Utils.try_eval_expr man (mk_py_attr obj attr range) flow
                ~on_empty:(fun exc_exp exc_str _ flow ->
                  OptionExt.return @@
                  if exc_str = "AttributeError" then
                    man.eval (mk_py_false range) flow
                  else
                    man.exec (mk_raise exc_exp range) flow >>% Eval.empty
                )
                ~on_result:(fun _ flow -> man.eval (mk_py_true range) flow)
           | _ ->
              panic " expected a string object, got %a" pp_expr attr
           )

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("setattr", _))}, _)}, [lval; attr; rval], []) ->
         man.eval lval flow >>$
           (fun lval flow ->
             man.eval (mk_py_call (mk_py_attr lval "__setattr__" range) [attr; rval] range) flow
           )
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("delattr", _))}, _)}, [lval; attr], []) ->
        man.eval   (mk_py_call (mk_py_attr lval "__delattr__" range) [attr] range) flow
        |> OptionExt.return


      | _ -> None

    let exec stmt man flow =
      let range = stmt.srange in
      match skind stmt with
      | S_assign({ekind = E_py_attribute(lval, attr)}, rval) ->
         man.eval rval flow >>$
           (fun rval flow ->
             man.eval lval flow >>$
               fun lval flow ->
               man.eval   (mk_py_call (mk_py_attr (mk_py_type lval range) "__setattr__" range) [lval; mk_constant ~etyp:(T_py None) (C_string attr) range; rval] range) flow
           >>$
             (fun e flow -> Post.return flow)
           )
        |> OptionExt.return

      | _ -> None

    let ask _ _ _ = None
    let print_expr _ _ _ _ = ()
  end


let () = register_stateless_domain (module Domain)
