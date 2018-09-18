(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Handling of class definition and instantiation. *)

open Framework.Essentials
open Universal.Ast
open Ast
open Addr


module Domain =
  struct

    type _ domain += D_python_objects_class : unit domain

    let id = D_python_objects_class
    let name = "python.objects.class"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_objects_class -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = [Framework.Zone.Z_top]; import = []}
    let eval_interface = {export = []; import = []}

    let init _ _ flow = Some flow

    let create_builtin_class kind name cls bases range =
      let addr = {
          addr_kind = A_py_class(kind, bases);
          addr_uid = 0;
        }
      in
      Addr.add_builtin_class (addr, mk_py_empty range) ()


    let rec eval zones exp man flow =
      let range = erange exp in
      match ekind exp with
      (* 𝔼⟦ C() | isinstance(C, type) ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind=A_py_class (C_builtin "type", _)}, _)}, args, []) ->
         (* handled in myytypes *)
         (* FIXME: what about the value analysis? *)
         None
      | E_py_call({ekind = E_py_object cls}, args, []) when Addr.isclass cls ->
         (* Call __new__ *)
         man.eval (mk_py_call (mk_py_object_attr cls "__new__" range) ((mk_py_object cls range) :: args) range) flow |>
           Eval.bind
             (fun eobj flow ->
               Eval.assume
                 (mk_py_call (mk_py_object (Addr.find_builtin "isinstance") range) [eobj; exp] range)
                 ~fthen:(fun flow ->
                   debug "init!@\n";
                   man.eval (mk_py_call (mk_py_object_attr cls "__init__" range) (eobj :: args) range) flow |>
                     Eval.bind (fun r flow ->
                         Eval.assume
                           (mk_py_call (mk_py_object (Addr.find_builtin "isinstance") range) [r; mk_py_object (Addr.find_builtin "NoneType") range] range)
                           ~fthen:(fun flow -> Eval.singleton eobj flow)
                           ~felse:(fun flow ->
                             let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) flow in
                             Eval.empty_singleton flow
                           )
                           man flow
                 ))
                 ~felse:(fun flow -> Eval.singleton eobj flow)
                 man flow
             )
         |> Option.return

      | _ -> None

    let rec exec zone stmt (man:('a, unit) man) (flow:'a flow) : 'a post option =
      let range = srange stmt in
      match skind stmt with
      (* 𝕊⟦ class cls: body ⟧ *)
      | S_py_class cls ->
         debug "definition of class %a" pp_var cls.py_cls_var;
         Eval.eval_list cls.py_cls_bases man.eval flow |>
           Post.bind man
             (fun bases flow ->
               let bases' =
                 match bases with
                 | [] -> [Addr.find_builtin "object"]
                 | _ -> List.map object_of_expr  bases
               in
               if Libs.Mopsa.is_builtin_clsdec cls then
                 let name = Libs.Mopsa.builtin_clsdec_name cls in
                 create_builtin_class (C_builtin name) name cls bases' range;
                 Post.of_flow flow
               else
                 if Libs.Mopsa.is_unsupported_clsdec cls then
                   let name = cls.py_cls_var.vname in
                   create_builtin_class (C_unsupported name) name cls bases' range;
                   Post.of_flow flow
                 else
                   Addr.eval_alloc man (A_py_class (C_user cls, bases')) stmt.srange flow |>
                     Post.bind man
                       (fun addr flow ->
                         let obj = (addr, mk_py_empty range) in
                         let flow = man.exec (mk_assign (mk_var cls.py_cls_var range) (mk_py_object obj range) range) flow in
                         man.exec cls.py_cls_body flow |>
                           Post.of_flow
                       )
             )
         |> Option.return

      | _ -> None

    (* FIXME: unused? *)
    (* (\* Parse the body of the class *\)
     * let rec parse base stmt =
     *   match skind stmt with
     *   | S_py_class(cls) ->
     *      let name = mk_dot_name base cls.py_cls_var.vname in
     *      let bases = List.map (fun base ->
     *                      match ekind base with
     *                      | E_var v -> Addr.find_builtin v.vname
     *                      | _ -> assert false
     *                    ) cls.py_cls_bases
     *      in
     *      let kind =
     *        if Libs.Mopsa.is_unsupported_clsdec cls then C_unsupported name
     *        else C_builtin name
     *      in
     *      let addr = {
     *          addr_kind = A_py_class (kind, bases);
     *          addr_uid = 0;
     *        }
     *      in
     *      Addr.add_builtin_class (addr, mk_py_empty range) ();
     *      parse (Some name) cls.py_cls_body
     *
     *   | S_py_function(fundec) ->
     *      let name = mk_dot_name base fundec.py_func_var.vname in
     *      let fundec = {fundec with py_func_var = {fundec.py_func_var with vname = name}} in
     *      let kind =
     *        if Libs.Mopsa.is_builtin_fundec fundec then F_builtin name else
     *          if Libs.Mopsa.is_unsupported_fundec fundec then F_unsupported name
     *          else F_user fundec
     *      in
     *      let addr = {
     *          addr_kind = A_py_function kind;
     *          addr_uid = -1;
     *        }
     *      in
     *      Addr.add_builtin_function (addr, mk_py_empty range) ()
     *
     *   | S_block(block) ->
     *      List.iter (parse base) block
     *
     *   | _ -> Framework.Exceptions.fail "stmt %a not supported in builtin class definition" Framework.Ast.pp_stmt stmt
     *     in
     *     parse (Some name) cls.py_cls_body *)


    let ask _ _ _ = None

  end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
