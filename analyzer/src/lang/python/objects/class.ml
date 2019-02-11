(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Handling of class definition and instantiation. *)

open Mopsa
open Ast
open Addr
open Universal.Ast


module Domain =
  struct

    type _ domain += D_python_objects_class : unit domain

    let id = D_python_objects_class
    let name = "python.objects.class"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_objects_class -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = [Zone.Z_py]; import = []}
    let eval_interface = {export = [Zone.Z_py, Zone.Z_py_obj]; import = [Zone.Z_py, Zone.Z_py_obj]}

    let init _ _ flow = Some flow


    let rec eval zones exp man flow =
      let range = erange exp in
      match ekind exp with
      (* 𝔼⟦ C() | isinstance(C, type) ⟧ *)
      | E_py_call({ekind = E_py_object ({addr_kind=A_py_class (C_builtin "type", _)}, _)}, args, []) ->
         None

      | E_py_call({ekind = E_py_object cls} as ecls, args, []) when isclass cls ->
        debug "class call  %a@\n@\n" pp_expr exp;
        (* Call __new__ *)
        let new_call = mk_py_call (mk_py_object_attr cls "__new__" range) ((mk_py_object cls range) :: args) range in
        man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) new_call flow |>
        Eval.bind (fun inst flow ->
           Eval.assume
                 (mk_py_isinstance inst ecls range)
                 ~fthen:(fun flow ->
                   debug "init!@\n";
                   man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_call (mk_py_object_attr cls "__init__" range) (inst :: args) range) flow |>
                     Eval.bind (fun r flow ->
                         Eval.assume
                           (mk_py_isinstance_builtin r "NoneType" range)
                           ~fthen:(fun flow -> man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj) inst flow)
                           ~felse:(fun flow ->
                             let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) flow in
                             Eval.empty_singleton flow
                           )
                           man flow
                 ))
                 ~felse:(fun flow -> Eval.singleton inst flow)
                 man flow
          )
        |> OptionExt.return

      | _ -> None

    let rec exec zone stmt (man:('a, unit) man) (flow:'a flow) : 'a post option =
      let range = srange stmt in
      match skind stmt with
      (* 𝕊⟦ class cls: body ⟧ *)
      | S_py_class cls ->
         debug "definition of class %a" pp_var cls.py_cls_var;
         Eval.eval_list cls.py_cls_bases (man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj)) flow |>
           Post.bind man
             (fun bases flow ->
               let bases' =
                 match bases with
                 | [] -> [find_builtin "object"]
                 | _ -> List.map object_of_expr  bases
               in
               if Libs.Py_mopsa.is_builtin_clsdec cls then
                 let name = Libs.Py_mopsa.builtin_clsdec_name cls in
                 create_builtin_class (C_builtin name) name cls bases' range;
                 Post.of_flow flow
               else
                 if Libs.Py_mopsa.is_unsupported_clsdec cls then
                   let name = cls.py_cls_var.org_vname in
                   create_builtin_class (C_unsupported name) name cls bases' range;
                   Post.of_flow flow
                 else
                   try
                     let mro = c3_lin ({addr_kind= (A_py_class (C_user cls, bases')); addr_uid=(-1); addr_mode = STRONG}, None) in
                     debug "MRO of %a: %a@\n" pp_var cls.py_cls_var
                       (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                          (fun fmt x -> Format.fprintf fmt "%a" pp_expr (mk_py_object x (srange stmt))))
                       mro;

                     eval_alloc man (A_py_class (C_user cls, mro)) stmt.srange flow |>
                       Post.bind man
                         (fun addr flow ->
                           let obj = (addr, None) in
                           let flow = man.exec (mk_assign (mk_var cls.py_cls_var range) (mk_py_object obj range) range) flow in
                           debug "Body of class is %a@\n" pp_stmt cls.py_cls_body;
                           man.exec cls.py_cls_body flow |>
                             Post.of_flow
                         )
                   with C3_lin_failure ->
                     Exceptions.warn "C3 linearization failure during class declaration %a@\n" pp_var cls.py_cls_var;
                     man.exec (Utils.mk_builtin_raise "TypeError" range) flow
                     |> Post.of_flow
             )
         |> OptionExt.return

      | _ -> None


    let ask _ _ _ = None

  end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
