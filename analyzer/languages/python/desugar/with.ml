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

(** With statement and context managers *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast

module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.desugar.with"
      end)

    let checks = []

    let init _ _ flow = None
    let eval _ _ _ = None

    let exec stmt man flow =
      match skind stmt with
      | S_py_with(context, target, body) ->
         let srange = stmt.srange in
         let erange = context.erange in
         (* Evaluate the context *)
         man.eval context flow >>$?
         fun econtext flow ->

         (* Enter the context *)
         man.eval (mk_py_type econtext econtext.erange) flow >>$? fun cls flow ->

         let cls = object_of_expr cls in
         man.eval (mk_py_object_attr cls "__enter__" erange) flow >>$? fun enter flow ->
         man.eval (mk_py_object_attr cls "__exit__" erange) flow >>$? fun exit flow ->

         let eenter = mk_py_call enter [econtext] erange in
         let flow =
           match target with
           | None -> man.exec (mk_stmt (S_expression eenter) srange)  flow
           | Some x -> man.exec (mk_assign x eenter srange) flow
         in

         (* Execute body *)
         let tmpexn = mktmp ~typ:(T_py None) () in
         let eexit e1 e2 e3 = mk_py_call exit [econtext; e1; e2; e3] erange in
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
                (Some (mk_py_object (find_builtin "BaseException") srange))
                (Some tmpexn)
                (mk_block [
                    (* Check the return value of the __exit__ method and re-raise the exception when it is false *)
                      mk_stmt
                        (S_py_if
                           (
                             (* In case of exception, call __exit__ and give the exception as argument *)
                             (* FIXME : the type and the traceback are not implmented *)
                             (eexit
                               (mk_py_none srange)
                               (mk_var tmpexn srange)
                               (mk_py_none srange)
                            ),
                            mk_block [] srange,
                            mk_stmt (S_py_raise (Some (mk_var tmpexn erange))) srange)
                        )
                      srange
                  ] srange
                )
             ]
             (mk_block [] srange)
             (mk_block [] srange)
             srange
         in
         flow >>%
         man.exec stmt >>%
         man.exec (mk_remove_var tmpexn srange) >>%
         Post.return |>
         OptionExt.return

      | _ -> None

    let ask _ _ _ = None

    let print_expr _ _ _ _ = ()

  end

let () =
  register_stateless_domain (module Domain)
