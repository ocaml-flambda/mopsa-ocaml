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
open Framework.Core.Sig.Domain.Stateless
open Ast
open Addr
open Universal.Ast


type expr_kind +=
   (** low-level hasattribute working at the object level only *)
   | E_py_ll_hasattr of expr (** object *) * expr (** attribute name *)
   (** low-level attribute access working at the object level only *)
   | E_py_ll_getattr of expr (** object *) * expr (** attribute name *)
(* todo: change strings into expr *)


let () =
  register_expr_pp (fun default fmt exp ->
      match ekind exp with
      | E_py_ll_hasattr (e, attr) -> Format.fprintf fmt "E_py_ll_hasattr(%a, %a)" pp_expr e pp_expr attr
      | E_py_ll_getattr (e, attr) -> Format.fprintf fmt "E_py_ll_getattr(%a, %a)" pp_expr e pp_expr attr
      | _ -> default fmt exp);
  register_expr_visitor (fun default exp ->
      match ekind exp with
      | E_py_ll_hasattr(e1, e2) ->
         {exprs = [e1; e2]; stmts = [];},
         (fun parts -> let e1, e2 = match parts.exprs with
                         | [e1; e2] -> e1, e2
                         | _ -> assert false in
                       {exp with ekind = E_py_ll_hasattr(e1, e2)})
      | E_py_ll_getattr(e1, e2) ->
         {exprs = [e1; e2]; stmts = [];},
         (fun parts -> let e1, e2 = match parts.exprs with
                         | [e1; e2] -> e1, e2
                         | _ -> assert false in
                       {exp with ekind = E_py_ll_getattr(e1, e2)})
      | _ -> default exp
    )


module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.data_model.attribute"
      end)

    let interface = {
      iexec = {provides = [Zone.Z_py]; uses = []} (* TODO: add attribute assignment *);
      ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]}
    }

    let init _ _ flow = flow

    let eval zs expr man flow =
      let range = erange expr in
      match ekind expr with
      (* Special attributes *)
      | E_py_attribute(obj, ("__dict__" as attr))
        | E_py_attribute(obj, ("__class__" as attr))
        | E_py_attribute(obj, ("__bases__" as attr))
        | E_py_attribute(obj, ("__name__" as attr))
        | E_py_attribute(obj, ("__qualname__" as attr))
        | E_py_attribute(obj, ("__mro__" as attr))
        | E_py_attribute(obj, ("mro" as attr))
        | E_py_attribute(obj, ("__subclass__" as attr)) ->
         panic_at range "Access to special attribute %s not supported" attr

      (* Attributes of builtins classes are static, so we can be much more efficient *)
      | E_py_attribute ({ekind = E_py_object ({addr_kind = A_py_class (C_builtin c, mro)}, _)}, attr) ->
        let rec search_mro mro = match mro with
          | [] ->
            man.exec (Utils.mk_builtin_raise "AttributeError" range) flow |>
            Eval.empty_singleton
          | cls::tl ->
            if is_builtin_attribute cls attr then
              Eval.singleton (mk_py_object (find_builtin_attribute cls attr) range) flow
            else
              search_mro tl in
        search_mro mro |> Option.return

      (* Other attributes *)
      | E_py_attribute (e, attr) ->
         debug "%a@\n" pp_expr expr;
         let c_attr = mk_constant T_string (C_string attr) range in
         man.eval e ~zone:(Zone.Z_py, Zone.Z_py_obj) flow |>
         Eval.bind (fun exp flow ->
             match ekind exp with
             | E_py_object ({addr_kind = A_py_class (C_builtin c, mro)}, _) ->
               let rec search_mro mro = match mro with
                 | [] ->
                   man.exec (Utils.mk_builtin_raise "AttributeError" range) flow |>
                   Eval.empty_singleton
                 | cls::tl ->
                   if is_builtin_attribute cls attr then
                     Eval.singleton (mk_py_object (find_builtin_attribute cls attr) range) flow
                   else
                     search_mro tl in
               search_mro mro
             | _ ->
               debug "other: %a@\n" pp_expr exp;
               assume_eval (mk_expr (E_py_ll_hasattr (exp, c_attr)) range)
                 ~fthen:(fun flow ->
                   debug "instance attribute found locally@\n";
                   man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_ll_getattr(exp, c_attr)) range) flow |>
                   Eval.bind (fun exp flow ->
                       match akind @@ fst @@ object_of_expr exp with
                       | A_py_function (F_user f) when List.exists (fun x -> match ekind x with E_var (v, _) -> v.org_vname = "classmethod" | _ -> false) f.py_func_decors ->
                         eval_alloc ~mode:WEAK man (A_py_method (object_of_expr exp, e)) range flow |>
                         Eval.bind (fun addr flow ->
                             let obj = (addr, None) in
                             Eval.singleton (mk_py_object obj range) flow
                           )
                       | _ ->
                         Eval.singleton exp flow
                     )
                   )
                 ~felse:(fun flow ->
                     debug "searching in the classes now@\n";
                     (* if exp is a class, we just call the attribute
                        (after searching in the mro). if exp is an
                        instance, we take its class, search in the mro
                        and create a method *)
                     (* to test if an object o is a class, we call isinstance(o, type) *)
                     assume_eval
                       (mk_py_isinstance_builtin exp "type" range)
                       ~fthen:(fun flow ->
                           let mro = mro (object_of_expr exp) in
                           debug "mro = %a@\n" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr) (List.map (fun x -> mk_py_object x range) mro);
                           let rec search_mro flow mro = match mro with
                             | [] ->
                               debug "No attribute found for %a@\n" pp_expr expr;
                               let flow = man.exec (Utils.mk_builtin_raise "AttributeError" range) flow in
                               Eval.empty_singleton flow
                             | cls::tl ->
                               assume_eval
                                 (mk_expr (E_py_ll_hasattr (mk_py_object cls range, c_attr)) range)
                                 ~fthen:(fun flow ->
                                     man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_ll_getattr (mk_py_object cls range, c_attr)) range) flow
                                   )
                                 ~felse:(fun flow -> search_mro flow tl)
                                 man flow
                           in search_mro flow mro
                         )
                       ~felse:(fun flow ->
                           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_type exp range) flow |>
                           Eval.bind (fun class_of_exp flow ->
                               let mro = mro (object_of_expr class_of_exp) in
                               let rec search_mro flow mro = match mro with
                                 | [] ->
                                   debug "No attribute found for %a@\n" pp_expr expr;
                                   let flow = man.exec (Utils.mk_builtin_raise "AttributeError" range) flow in
                                   Eval.empty_singleton flow
                                 | cls::tl ->
                                   assume_eval
                                     (mk_expr (E_py_ll_hasattr (mk_py_object cls range, c_attr)) range)
                                     ~fthen:(fun flow ->
                                         (* FIXME: disjunction between instances an non-instances *)
                                         man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_ll_getattr (mk_py_object cls range, c_attr)) range) flow |>
                                         Eval.bind (fun obj' flow ->
                                             assume_eval
                                               (mk_py_isinstance_builtin obj' "function" range) man flow
                                               ~fthen:(fun flow ->
                                                   debug "obj'=%a; exp=%a@\n" pp_expr obj' pp_expr exp;
                                                   eval_alloc ~mode:WEAK man (A_py_method (object_of_expr obj', e)) range flow |>
                                                   Eval.bind (fun addr flow ->
                                                       let obj = (addr, None) in
                                                       Eval.singleton (mk_py_object obj range) flow)
                                                 )
                                               ~felse:(fun flow ->
                                                   (* assume_eval
                                                    *   (mk_py_isinstance_builtin obj' "method" range) man flow
                                                    *   ~fthen:(fun flow ->
                                                    *       match akind @@ fst @@ object_of_expr obj' with
                                                    *       | A_py_method (({addr_kind = A_py_function (F_user f)}, _), _) when List.exists (fun x -> match ekind x with E_var (v, _) -> v.org_vname = "classmethod" | _ -> false) f.py_func_decors ->
                                                    *         eval_alloc ~mode:WEAK man (A_py_method (object_of_expr obj', class_of_exp)) range flow |>
                                                    *       Eval.bind (fun addr flow ->
                                                    *           let obj = (addr, None) in
                                                    *           Eval.singleton (mk_py_object obj range) flow)
                                                    *       | _ -> Exceptions.panic "%a@\n" pp_expr obj'
                                                    *     )
                                                    *   ~felse:(Eval.singleton obj') *)
                                                   Eval.singleton obj' flow
                                                 )
                                           )
                                       )
                                     ~felse:(fun flow -> search_mro flow tl)
                                     man flow
                               in search_mro flow mro)
                         )
                       man flow
                   )
                 ~fnone:(fun flow ->
                     (* In a bottom environment, the only thing that we
                        can do is to search for builtins attributes and
                        resolve them statically *)
                     let oexp = object_of_expr exp in
                     if is_builtin_name (object_name oexp) && is_builtin_attribute oexp attr then
                       let rese = mk_py_object (find_builtin_attribute oexp attr) range in
                       Eval.singleton rese flow
                     else
                       Eval.empty_singleton flow
                   )
                 man flow
           )
         |> Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "hasattr")}, _)}, [obj; attr], []) ->
         man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) obj flow |>
           Eval.bind (fun eobj flow ->
             match ekind eobj with
             | E_py_object ({addr_kind = A_py_class (C_builtin c, mro)}, _) ->
               let attr = match ekind attr with
                 | E_constant (C_string s) -> s
                 | _ -> assert false in
               let rec search_mro mro = match mro with
                 | [] ->
                   man.eval (mk_py_false range) flow
                 | cls::tl ->
                   if is_builtin_attribute cls attr then
                     man.eval (mk_py_true range) flow
                   else
                     search_mro tl in
               search_mro mro
             | _ ->
               assume_eval (mk_expr (E_py_ll_hasattr (eobj, attr)) range)
                 ~fthen:(fun flow -> man.eval (mk_py_true range) flow)
                 ~felse:(fun flow ->
                   (* test with ll_hasattr and search in the MRO otherwise *)
                   let rec search_mro flow mro = match mro with
                     | [] -> man.eval (mk_py_false range) flow
                     | cls::tl ->
                        assume_eval
                          (mk_expr (E_py_ll_hasattr (mk_py_object cls range, attr)) range)
                          ~fthen:(fun flow ->
                            man.eval (mk_py_true range) flow)
                          ~felse:(fun flow -> search_mro flow tl)
                          man flow
                   in
                   assume_eval
                     (mk_py_isinstance_builtin eobj "type" range)
                     ~fthen:(fun flow ->
                       let mro = mro (object_of_expr eobj) in
                       search_mro flow mro)
                     ~felse:(fun flow ->
                       man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_type eobj range) flow |>
                         Eval.bind (fun class_of_exp flow ->
                             let mro = mro (object_of_expr class_of_exp) in
                             search_mro flow mro)
                     )
                     man flow
                 )
                 man flow
             )
         |> Option.return

      | _ -> None

    let exec _ _ _ _ = None
    let ask _ _ _ = None
  end


let () = Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
