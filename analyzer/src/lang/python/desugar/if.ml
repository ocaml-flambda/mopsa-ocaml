(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Transformation of conditional expressions. *)

open Framework.Essentials
open Universal.Ast
open Ast

module Domain =
  struct

    type _ domain += D_python_desugar_if : unit domain

    let id = D_python_desugar_if
    let name = "python.desugar.if"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_desugar_if -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let zone = Framework.Zone.Z_top
    let import_exec = []
    let import_eval = []

    let init _ _ flow = Some flow

    let eval exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_py_if(test, body, orelse) ->
         let tmp = mk_tmp () in
         let flow = man.exec
                      (mk_if
                         test
                         (mk_assign (mk_var tmp (tag_range range "true branch lval")) body (tag_range range "true branch assign"))
                         (mk_assign (mk_var tmp (tag_range range "false branch lval")) orelse (tag_range range "false branch assign"))
                         range
                      ) flow
         in
         let exp' = {exp with ekind = E_var tmp} in
         Eval.singleton exp' flow ~cleaners:[mk_remove_var tmp (tag_range range "cleaner")]
         |> Option.return

      | _ -> None


    let exec _ _ _ = None

    let ask _ _ _ = None

  end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
