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
open Framework.Exec
open Framework.Ast
open Universal.Ast
open Ast
open Addr

let name = "python.desugar.import"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let rec exec man ctx stmt flow =
    let range = srange stmt in
    match skind stmt with
    | S_py_import(modul, _, _) when String.contains modul '.'->
      Framework.Exceptions.panic_at range "import of sub-module %s not supported" modul

    | S_py_import_from(modul, _, _, _) when String.contains modul '.'->
      Framework.Exceptions.panic_at range "import from sub-module %s not supported" modul

    | S_py_import(modul, vasname, vroot) ->
      let addr, flow = import_module man ctx modul range flow in
      let v = match vasname with
        | None -> vroot
        | Some v -> v
      in
      man.exec ctx (mk_assign (mk_var v range) (mk_addr addr range) range) flow |>
      return

    | S_py_import_from(modul, name, _, vmodul) ->
      let addr, flow = import_module man ctx modul range flow in
      let e =
        match addr.addr_kind with
        | A_py_module(M_user(_, globals)) ->
          let v = List.find (fun v -> v.vname = name) globals in
          mk_var v range
        | A_py_module(M_builtin m) ->
          let addr = Addr.find_builtin_attribute m name in
          mk_addr addr range
        | _ -> assert false
      in
      man.exec ctx (mk_assign (mk_var vmodul range) e range) flow |>
      return

    | _ ->
      None


  (** Search for the module in the search path and parse its body *)
  and import_module man ctx name range flow =
    if Addr.is_builtin name then
      Addr.find_builtin name, flow
    else
      let dir =
        Framework.Options.(common_options.stubs) |> List.find_opt
          (fun dir ->
             let filename = dir ^ "/" ^ name ^ ".py" in
             Sys.file_exists filename
          )
      in
      match dir with
      | None -> Framework.Exceptions.panic_at range "module %s not found in stubs" name
      | Some dir ->
        let filename = dir ^ "/" ^ name ^ ".py" in
        let prog = Frontend.parse_program [filename] in
        let globals, body = match prog.prog_kind with Py_program(body, globals) -> body, globals | _ -> assert false in
        let addr = {
          addr_kind = A_py_module (M_user(name, globals));
          addr_range = mk_file_range filename;
          addr_uid = 0;
        }
        in
        let flow' = man.exec ctx body flow in
        addr, flow'


  (** Parse and import a builtin module *)
  and import_builtin_module base name =
    let file = name ^ ".py" in
    let dir = Framework.Options.(common_options.stubs) |> List.find_opt
                (fun dir ->
                   let filename = dir ^ "/" ^ file in
                   Sys.file_exists filename
                )
    in
    match dir with
    | None -> Framework.Exceptions.panic "builtin module %s not found" file
    | Some dir ->
      let path = dir ^ "/" ^ file in
      let range = mk_file_range path in
      let stmt = Frontend.parse_file path in

      let rec parse base stmt =
        match skind stmt with
        | S_py_class(cls) ->
          let name = mk_dot_name base cls.py_cls_var.vname in
          let bases = List.map (fun base ->
              match ekind base with
              | E_var v -> Addr.find_builtin v.vname
              | _ -> assert false
            ) cls.py_cls_bases
          in
          let kind =
            if Libs.Mopsa.is_unsupported_clsdec cls then C_unsupported name
            else C_builtin name
          in
          let addr = {
            addr_kind = A_py_class (kind, bases);
            addr_range = range;
            addr_uid = -1;
          }
          in
          Addr.add_builtin_class addr ();
          parse (Some name) cls.py_cls_body

        | S_py_function(fundec) ->
          let name = mk_dot_name base fundec.py_func_var.vname in
          let fundec = {fundec with py_func_var = {fundec.py_func_var with vname = name}} in
          let kind =
            if Libs.Mopsa.is_stub_fundec fundec then F_user fundec else
            if Libs.Mopsa.is_unsupported_fundec fundec then F_unsupported name
            else F_builtin name
          in
          let addr = {
            addr_kind = A_py_function kind;
            addr_range = range;
            addr_uid = -1;
          }
          in
          Addr.add_builtin_function addr ()

        | S_block(block) ->
          List.iter (parse base) block

        | S_py_import(name, _, _) when Addr.is_builtin name -> ()

        | _ -> Framework.Exceptions.fail "stmt %a not supported in %s" Framework.Pp.pp_stmt stmt file

      in
      parse base stmt;
      if name <> "stdlib" then
        let addr = {
          addr_kind = A_py_module(M_builtin name);
          addr_range = range;
          addr_uid = 0;
        }
        in
        Addr.add_builtin_module addr ()
      else
        ()

  let init man ctx prog flow =
    import_builtin_module (Some "mopsa") "mopsa";
    import_builtin_module None "stdlib";
    ctx, flow

  let eval _ _ _ _ = None
  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
