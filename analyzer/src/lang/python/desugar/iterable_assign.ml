(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Assignments from iterables. *)

open Framework.Essentials
open Universal.Ast
open Ast

module Domain =
  struct

    type _ domain += D_python_desugar_iterable_assign : unit domain

    let id = D_python_desugar_iterable_assign
    let name = "python.desugar.iterable_assign"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_desugar_iterable_assign -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let zone = Framework.Zone.Z_top
    let import_exec = []
    let import_eval = []

    let init _ _ flow = Some flow

    let rec exec stmt man flow =
      let range = srange stmt in
      match skind stmt with
      | S_assign({ekind = E_py_tuple(el)}, exp, kind)
        | S_assign({ekind = E_py_list(el)}, exp, kind) ->
         man.eval (Utils.mk_builtin_call "iter" [exp] range) flow |>
           Post.bind man
             (fun iter flow ->
               assign_iter man el iter kind range flow
               |> Post.of_flow
             )
         |> Option.return

      | _ -> None

    and assign_iter man el iter mode range flow =
      let stmtl =
        List.fold_left (fun acc e ->
            mk_assign
              e ~mode
              (Utils.mk_builtin_call "next" [iter] range)
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
             (Some (mk_py_object (Addr.find_builtin "StopIteration") (tag_range range "stop iter")))
             None
             (Utils.mk_builtin_raise "ValueError" (tag_range range "error raise"))
          ]
          (mk_nop (tag_range range "empty try else"))
          (mk_nop (tag_range range "empty try finally"))
          (tag_range range "try next")
      in
      man.exec stmt flow


    let eval _ _ _ = None
    let ask _ _ _ = None

  end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
