(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Main handler of standalone C programs. *)


open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.program"
let debug fmt = Debug.debug ~channel:name fmt


module Domain =
struct

  let init prog man flow =
    match prog.prog_kind with
    | C_program(globals, _) ->
      let range = mk_fresh_range () in
      globals |> List.fold_left (fun flow (v, init) ->
          if not (is_c_scalar_type v.vtyp) then
            flow
          else
            let v = mk_var v range in
            match init with
            | None -> flow
            | Some (C_init_expr e) -> man.exec (mk_assign v e range) Framework.Context.empty flow
            | Some (Ast.C_init_list (_,_)) -> assert false
            | Some (Ast.C_init_implicit _) -> assert false
        ) flow

    | _ -> flow

  let eval exp manager ctx flow = None

  let get_function_name fundec = fundec.c_func_var.vname

  let is_test fundec =
    String.sub (get_function_name fundec) 0 4 = "test"

  let get_test_functions functions =
    List.filter is_test functions

  let mk_c_unit_tests file tests =
    let range = mk_file_range file in
    let tests =
      tests |> List.map (fun test ->
          let name = test.c_func_var.vname in
          let range = tag_range range "test %s" name in
          let cleaners = List.mapi (fun i (v, _) ->
              let range = tag_range range "cleaner %d" i in
              mk_remove_var v range
            ) test.c_func_local_vars
          in
          let body = mk_block (test.c_func_body :: cleaners) range in
          (name, body)
        )
    in
    mk_stmt (Universal.Ast.S_unit_tests (file, tests)) range


  let exec stmt manager ctx flow  =
    match skind stmt with
    | S_program({prog_kind = C_program(globals, functions); prog_file})
      when not Framework.Options.(common_options.unit_test_mode) ->
      let main = List.find (function
            {c_func_var} -> c_func_var.vname = "main"
        ) functions
      in
      manager.exec main.c_func_body ctx flow |>
      Exec.return

    | S_program({prog_kind = C_program(globals, functions); prog_file})
      when Framework.Options.(common_options.unit_test_mode) ->
      let tests = get_test_functions functions in
      let stmt = mk_c_unit_tests prog_file tests in
      Exec.return (manager.exec stmt ctx flow)


    | _ -> None

  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
