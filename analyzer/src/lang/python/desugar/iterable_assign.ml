(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Assignments from iterables. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Exec
open Framework.Ast
open Universal.Ast
open Ast

let name = "python.desugar.iterable_assign"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct


  let rec exec man ctx stmt flow =
    let range = srange stmt in
    match skind stmt with
    | S_assign({ekind = E_py_tuple(el)}, exp, kind)
    | S_assign({ekind = E_py_list(el)}, exp, kind) ->
      man.eval ctx exp flow |>
      eval_to_exec
        (fun exp flow ->
          match ekind exp with
            | E_addr addr ->
              Framework.Exceptions.panic "python heap operations not supported"

            (* let iter_expr = mk_expr
             *     (E_py_call (
             *         (mk_expr
             *            (E_addr_attribute (addr, "__iter__"))
             *            (tag_range range "iter attr")),
             *         [],[]
             *       ))
             *     (tag_range range "iter call")
             * in
             * man.eval ctx iter_expr flow |>
             * eval_to_exec
             *   (fun iter flow ->
             *      match ekind iter with
             *      | E_addr(addr) -> assign_iter man ctx el addr kind range flow
             *      | _ -> assert false
             *   )
             *   (man.exec ctx) man.flow *)

          | _ ->
            man.exec ctx (Builtins.mk_builtin_raise "TypeError" (tag_range range "error")) flow
        )
        (man.exec ctx) man.flow  |>
      return

    | _ -> None

  and assign_iter man ctx el iter mode range flow =
    let stmtl =
      List.fold_left (fun acc e ->
          mk_assign
            e ~mode
            (mk_py_call
               (mk_addr (Builtins.from_string "next") (tag_range range "next addr"))
               [mk_addr iter (tag_range range "iter addr")]
               (tag_range range "next call")
            )
            (tag_range range "next assign")
          :: acc
        ) [] el |>
      List.rev
    in
    let block = mk_block stmtl (tag_range range "next assign block") in
    let stmt =
      mk_try
        block
        [mk_except
           (Some (mk_addr (Builtins.from_string "StopIteration") (tag_range range "stop iter")))
           None
           (Builtins.mk_builtin_raise "ValueError" (tag_range range "error raise"))
        ]
        (mk_nop (tag_range range "empty try else"))
        (mk_nop (tag_range range "empty try finally"))
        (tag_range range "try next")
    in

    man.exec ctx stmt flow


  let init _ ctx _ flow = ctx, flow
  let eval _ _ _ _ = None
  let ask _ _ _ _ = None


end

let setup () =
  Stateless.register_domain name (module Domain)
