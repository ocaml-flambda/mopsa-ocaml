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
open Framework.Visitor
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

    type _ domain += D_python_data_model_attribute : unit domain

    let id = D_python_data_model_attribute
    let name = "python.data_model.attribute"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_data_model_attribute -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = [Zone.Z_py]; import = []} (* TODO: add attribute assignment *)
    let eval_interface = {export = [Zone.Z_py, Zone.Z_py_obj]; import = [Zone.Z_py, Zone.Z_py_obj]}

    let init _ _ flow = Some flow

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

      (* Other attributes *)
      | E_py_attribute (e, attr) ->
         debug "%a@\n" pp_expr expr;
         let c_attr = mk_constant T_string (C_string attr) range in
         man.eval e ~zone:(Zone.Z_py, Zone.Z_py_obj) flow |>
           Eval.bind (fun exp flow ->
               Eval.assume (mk_expr (E_py_ll_hasattr (exp, c_attr)) range)
                 ~fthen:(fun flow ->
                   debug "instance attribute found locally@\n";
                   man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_ll_getattr(exp, c_attr)) range) flow
                 )
                 ~felse:(fun flow ->
                   (* if exp is a class, we just call the attribute
                      (after searching in the mro). if exp is an
                      instance, we take its class, search in the mro
                      and create a method *)
                   (* to test if an object o is a class, we call isinstance(o, type) *)
                   Eval.assume
                     (mk_py_isinstance_builtin exp "type" range)
                     ~fthen:(fun flow ->
                       let mro = mro (object_of_expr exp) in
                       let rec search_mro flow mro = match mro with
                         | [] ->
                            debug "No attribute found for %a@\n" pp_expr expr;
                            let flow = man.exec (Utils.mk_builtin_raise "AttributeError" range) flow in
                            Eval.empty_singleton flow
                         | cls::tl ->
                            Eval.assume
                              (mk_expr (E_py_ll_hasattr (mk_py_object cls range, c_attr)) range)
                              ~fthen:(fun flow ->
                                man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_expr (E_py_ll_getattr (mk_py_object cls range, c_attr)) range) flow)
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
                                  Eval.assume
                                    (mk_expr (E_py_ll_hasattr (mk_py_object cls range, c_attr)) range)
                                    ~fthen:(fun flow ->
                                      (* FIXME: disjunction between instances an non-instances *)
                                      (* FIXME: perf: optim this into a get_attr? *)
                                      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_object_attr cls attr range) flow |>
                                        Eval.bind (fun obj' flow ->
                                            Eval.assume
                                              (mk_py_isinstance_builtin obj' "function" range)
                                              ~fthen:(fun flow ->
                                                  debug "obj'=%a; exp=%a@\n" pp_expr obj' pp_expr exp;
                                                  eval_alloc man (A_py_method (object_of_expr obj', e)) range flow |>
                                                  Eval.bind (fun addr flow ->
                                                      let obj = (addr, None) in
                                                      Eval.singleton (mk_py_object obj range) flow)
                                               )
                                              ~felse:(fun flow ->
                                                Eval.singleton obj' flow
                                              )
                                              man flow
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
                   if is_builtin oexp && is_builtin_attribute oexp attr then
                     let rese = mk_py_object (find_builtin_attribute oexp attr) range in
                     Eval.singleton rese flow
                   else
                     Eval.empty_singleton flow
                 )
                 man flow
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "hasattr")}, _)}, [obj; attr], []) ->
         man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) obj flow |>
           Eval.bind (fun eobj flow ->
               Eval.assume (mk_expr (E_py_ll_hasattr (eobj, attr)) range)
                 ~fthen:(fun flow -> man.eval (mk_py_true range) flow)
                 ~felse:(fun flow ->
                   (* test with ll_hasattr and search in the MRO otherwise *)
                   let rec search_mro flow mro = match mro with
                     | [] -> man.eval (mk_py_false range) flow
                     | cls::tl ->
                        Eval.assume
                          (mk_expr (E_py_ll_hasattr (mk_py_object cls range, attr)) range)
                          ~fthen:(fun flow ->
                            man.eval (mk_py_true range) flow)
                          ~felse:(fun flow -> search_mro flow tl)
                          man flow
                   in
                   Eval.assume
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
         |> OptionExt.return

      | _ -> None

    let exec _ _ _ _ = None
    let ask _ _ _ = None
  end


let () = Framework.Domains.Stateless.register_domain (module Domain)
