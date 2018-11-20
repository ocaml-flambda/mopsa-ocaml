(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** With statement and context managers *)

open Framework.Essentials
open Framework.Ast
open Universal.Ast
open Ast
open Addr

module Domain =
  struct

    type _ domain += D_python_desugar_with : unit domain

    let id = D_python_desugar_with
    let name = "python.desugar.with"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_desugar_with -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = [any_zone]; import = []}
    let eval_interface = {export = []; import = []}

    let init _ _ flow = Some flow
    let eval _ _ _ _ = None

    let exec zone stmt man flow =
      match skind stmt with
      | S_py_with(context, target, body) ->
         let srange = stmt.srange in
         let erange = context.erange in
         (* Evaluate the context *)
         man.eval context flow |>
           Post.bind man (fun econtext flow ->
               (* Enter the context *)
               man.eval (mk_py_type econtext econtext.erange) flow |>
                 Post.bind man (fun cls flow ->
                     let cls = object_of_expr cls in
                     let eenter = mk_py_call (mk_py_object_attr cls "__enter__" erange) [econtext] erange in
                     let flow =
                       match target with
                       | None -> man.exec (mk_stmt (S_expression eenter) srange)  flow
                       | Some x -> man.exec (mk_assign x eenter srange) flow
                     in

                     (* Execute body *)
                     let tmpexn = mk_tmp () in
                     let tmpret = mk_tmp () in
                     let eexit e1 e2 e3 = mk_py_call (mk_py_object_attr cls "__exit__" erange) [econtext; e1; e2; e3] erange in
                     let stmt =
                       mk_try
                         (mk_block [
                              body;
                              (* In case of normal execution, call context.__exit__(None, None, None) *)
                              (mk_stmt
                                 (S_expression (eexit
                                                  (mk_py_none srange)
                                                  (mk_py_none srange)
                                                  (mk_py_none srange)
                                 ))
                                 srange
                              )
                            ] srange)
                         [mk_except
                            (Some (mk_py_object (Addr.find_builtin "BaseException") srange))
                            (Some tmpexn)
                            (mk_block [
                                 (* In case of exception, call __exit__ and give the exception as argument *)
                                 (* FIXME : the type and the traceback are not implmented *)
                                 mk_assign (mk_var tmpret srange)
                                   (eexit
                                      (mk_py_none srange)
                                      (mk_var tmpexn srange)
                                      (mk_py_none srange)
                                   )
                                   srange
                               ;
                                 (* Check the return value of the __exit__ method and re-raise the exception when it is false *)
                                 mk_if
                                   (mk_var tmpret srange)
                                   (mk_block [] srange)
                                   (mk_stmt (S_py_raise (Some (mk_var tmpexn erange))) srange)
                                   srange
                               ] srange
                            )
                         ]
                         (mk_block [] srange)
                         (mk_block [] srange)
                         srange
                     in
                     man.exec stmt flow |>
                       man.exec (mk_remove_var tmpexn srange) |>
                       man.exec (mk_remove_var tmpret srange) |> Post.of_flow
                   )
             )
         |> OptionExt.return

      | _ -> None

    let ask _ _ _ = None


  end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
