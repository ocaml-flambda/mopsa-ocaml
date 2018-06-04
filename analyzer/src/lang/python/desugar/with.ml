(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** With statement and context managers *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Exec
open Framework.Ast
open Universal.Ast
open Ast

let name = "python.desugar.with"

module Domain =
struct


  let exec man ctx stmt flow =
    match skind stmt with
    | S_py_with(context, target, body) ->
      let srange = stmt.srange in
      let erange = context.erange in
      (* Evaluate the context *)
      man.eval ctx context flow |>
      eval_to_exec (fun context flow ->
          (* Enter the context *)
          let cls = Addr.classof @@ addr_of_expr context in
          let eenter = mk_py_call (mk_py_addr_attr cls "__enter__" erange) [context] erange in
          let flow =
            match target with
            | None -> debug "no target"; man.exec ctx (mk_stmt (S_expression eenter) srange)  flow
            | Some x -> debug "target %a" Framework.Pp.pp_expr x; man.exec ctx (mk_assign x eenter srange) flow
          in

          (* Execute body *)
          let tmpexn = mktmp () in
          let tmpret = mktmp () in
          let eexit e1 e2 e3 = mk_py_call (mk_py_addr_attr cls "__exit__" erange) [context; e1; e2; e3] erange in
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
                 (Some (mk_addr (Addr.find_builtin "BaseException") srange))
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
          man.exec ctx stmt flow |>
          man.exec ctx (mk_remove_var tmpexn srange) |>
          man.exec ctx (mk_remove_var tmpret srange)
        ) (man.exec ctx) man.flow |>
      return

    | _ -> None

  let init _ ctx _ flow = ctx, flow
  let eval _ _ _ _ = None
  let ask _ _ _ _ = None


end

let setup () =
  Stateless.register_domain name (module Domain)
