(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Inliner of imported packages. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Utils
open Framework.Ast
open Universal.Ast
open Ast

let name = "python.desugar.import"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct


  let exec stmt manager ctx flow =
    let range = srange stmt in
    match skind stmt with
    | S_py_import(modul, vasname, vroot)  when Builtins.is_builtin_module modul ->
      let addr = Builtins.from_string vroot.vname in
      manager.exec
        (mk_assign
           (mk_var vroot (tag_range range "import vroot"))
           (mk_addr addr (tag_range range "import vroot addr"))
           (tag_range range "import vroot assign")
        ) ctx flow
      |>
      (fun flow ->
         match vasname with
         | None -> flow
         | Some v ->
           let addr' = Builtins.from_attribute modul v.vname in
           manager.exec
             (mk_assign
                (mk_var v (tag_range range "import v"))
                (mk_addr addr' (tag_range range "import v addr"))
                (tag_range range "import v assign")
             ) ctx flow
      ) |>
      return

    | S_py_import(modul, vasname, vroot)  ->
      assert false

    | S_py_import_from(modul, name, vmodul) ->
      assert false
    | _ ->
      None

  let init _ _ ctx flow = ctx, flow
  let eval _ _ _ _ = None
  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
