(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Inliner of imported packages. *)

open Framework.Essentials
open Framework.Ast
open Universal.Ast
open Ast

module Domain =
  struct

    type _ domain += D_python_desugar_loops : unit domain

    let id = D_python_desugar_loops
    let name = "python.desugar.loops"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_desugar_loops -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let zone = Framework.Zone.Z_top
    let import_exec = []
    let import_eval = []

    let init _ _ flow = Some flow
    let eval _ _ _ = None


    let exec stmt man flow =
      let range = srange stmt in
      match skind stmt with
      | S_py_while (test, body, orelse) ->
         man.exec
           (mk_while
              (mk_one range)
              (mk_block [
                   mk_if
                     (mk_not test range)
                     (mk_block [
                          orelse;
                          mk_stmt S_break range
                        ] range)
                     (mk_block [] range)
                     range
                 ;
                   body
                 ] range)
              range
           ) flow
         |> Post.of_flow
         |> Option.return

      | S_py_for(target, iterable, body, orelse) ->
         man.eval (Utils.mk_builtin_call "iter" [iterable] range) flow |>
           Post.bind man (fun iter flow ->
               let stmt =
                 mk_while
                   (mk_one range)
                   (mk_block [
                        (Utils.mk_try_stopiteration
                           (mk_assign
                              target
                              (Utils.mk_builtin_call "next" [iter] range)
                              range
                           )
                           (mk_block [
                                orelse;
                                mk_stmt S_break range
                              ] range)
                           range)
                      ;
                        body
                      ] range)
                   range
               in
               man.exec stmt flow
               |> Post.of_flow
             )
         |> Option.return

      | _ -> None

    let ask _ _ _ = None

  end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
