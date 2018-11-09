(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Data model for attribute access. *)

open Framework.Essentials
open Framework.Visitor
open Universal.Ast
open Ast
open Addr


type expr_kind +=
   (** low-level hasattribute working at the object level only *)
   | E_py_ll_hasattr of expr (** object *) * expr (** attribute name *)
   (** low-level attribute access working at the object level only *)
   | E_py_ll_getattr of expr (** object *) * expr (** attribute name *)
(* todo: change strings into expr *)


let () =
  register_pp_expr (fun default fmt exp ->
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

    let exec_interface = {export = []; import = []}
    let eval_interface = {export = [any_zone, any_zone]; import = []}

    let init _ _ flow = Some flow

    let eval zs expr man flow =
      debug "eval %a@\n" pp_expr expr;
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
         Framework.Exceptions.panic_at range "Access to special attribute %s not supported" attr

      (* Other attributes *)
      | E_py_attribute (e, attr) ->
         debug "%a@\n" pp_expr expr;
         let c_attr = mk_constant T_string (C_string attr) range in
         man.eval e flow |>
           Eval.bind (fun exp flow ->
               Eval.assume (mk_expr (E_py_ll_hasattr (exp, c_attr)) range)
                 ~fthen:(fun flow ->
                   debug "instance attribute found locally@\n";
                   man.eval (mk_expr (E_py_ll_getattr(exp, c_attr)) range) flow
                 )
                 ~felse:(fun flow ->
                   (* if exp is a class, we just call the attribute
                      (after searching in the mro). if exp is an
                      instance, we take its class, search in the mro
                      and create a method *)
                   (* to test if an object o is a class, we call isinstance(o, type) *)
                   Eval.assume
                     (mk_py_call (mk_py_object (Addr.find_builtin "isinstance") range) [exp; mk_py_object (Addr.find_builtin "type") range] range)
                     ~fthen:(fun flow ->
                       let mro = Addr.mro (object_of_expr exp) in
                       let rec search_mro flow mro = match mro with
                         | [] ->
                            debug "No attribute found for %a@\n" pp_expr expr;
                            let flow = man.exec (Utils.mk_builtin_raise "AttributeError" range) flow in
                            Eval.empty_singleton flow
                         | cls::tl ->
                            Eval.assume
                              (mk_expr (E_py_ll_hasattr (mk_py_object cls range, c_attr)) range)
                              ~fthen:(fun flow ->
                                man.eval (mk_expr (E_py_ll_getattr (mk_py_object cls range, c_attr)) range) flow)
                              ~felse:(fun flow -> search_mro flow tl)
                              man flow
                       in search_mro flow mro
                     )
                     ~felse:(fun flow ->
                       man.eval (mk_py_call (mk_py_object (Addr.find_builtin "type") range) [exp] range) flow |>
                         Eval.bind (fun class_of_exp flow ->
                             let mro = Addr.mro (object_of_expr class_of_exp) in
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
                                      man.eval (mk_py_object_attr cls attr range) flow |>
                                        Eval.bind (fun obj' flow ->
                                            Eval.assume
                                              (mk_py_call (mk_py_object (Addr.find_builtin "isinstance") range) [obj'; mk_py_object (Addr.find_builtin "function") range] range)
                                              ~fthen:(fun flow ->
                                                (* Debug.fail "todo@\n"; *)
                                                debug "obj'=%a; exp=%a@\n" pp_expr obj' pp_expr exp;
                                                let exp = mk_expr (E_alloc_addr (A_py_method(object_of_expr obj', exp))) range in
                                                man.eval exp flow)
                                              ~felse:(fun flow ->
                                                let exp = mk_expr (E_py_ll_getattr (mk_py_object cls range, c_attr)) range in
                                                Eval.singleton exp flow)
                                              man flow
                                            (* let exp = mk_expr (E_py_ll_getattr (mk_py_object cls range, c_attr)) range in
                                             * man.eval exp flow *)
                                          )
                                    )
                                    ~felse:(fun flow -> search_mro flow tl)
                                    man flow
                             in search_mro flow mro)
                     )
                     man flow
                 )
                 man flow
             )
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "hasattr")}, _)}, [obj; attr], []) ->
         man.eval obj flow |>
           Eval.bind (fun eobj flow ->
               Eval.assume (mk_expr (E_py_ll_hasattr (eobj, attr)) range)
                 ~fthen:(fun flow -> Eval.singleton (mk_py_true range) flow)
                 ~felse:(fun flow ->
                   (* test with ll_hasattr and search in the MRO otherwise *)
                   let rec search_mro flow mro = match mro with
                     | [] -> Eval.singleton (mk_py_false range) flow
                     | cls::tl ->
                        Eval.assume
                          (mk_expr (E_py_ll_hasattr (eobj, attr)) range)
                          ~fthen:(fun flow ->
                            Eval.singleton (mk_py_true range) flow)
                          ~felse:(fun flow -> search_mro flow tl)
                          man flow
                   in
                   Eval.assume
                     (mk_py_call (mk_py_object (Addr.find_builtin "isinstance") range) [eobj; mk_py_object (Addr.find_builtin "type") range] range)
                     ~fthen:(fun flow ->
                       let mro = Addr.mro (object_of_expr eobj) in
                       search_mro flow mro)
                     ~felse:(fun flow ->
                       man.eval (mk_py_call (mk_py_object (Addr.find_builtin "type") range) [eobj] range) flow |>
                         Eval.bind (fun class_of_exp flow ->
                             let mro = Addr.mro (object_of_expr class_of_exp) in
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
