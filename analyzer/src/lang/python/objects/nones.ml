(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** None constant. *)

open Mopsa
open Ast
open Addr
open Universal.Ast

module Domain =
  struct

    type _ domain += D_python_objects_nones : unit domain

    let id = D_python_objects_nones
    let name = "python.objects.nones"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_objects_nones -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = []; import = []}
    let eval_interface = {export = [Zone.Z_py, Zone.Z_py]; import = []}

    let rec eval zs exp man flow =
      let range = exp.erange in
      match ekind exp with
      (* 𝔼⟦ None ⟧ *)
      | E_constant (C_py_none) ->
         Eval.singleton (mk_py_none range) flow |> OptionExt.return
      | _ -> None

    let init _ _ flow = Some flow
    let exec _ _ _ _ = None
    let ask _ _ _ = None

  end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
