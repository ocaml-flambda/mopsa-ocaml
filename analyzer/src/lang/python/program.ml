(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Main handler of Python programs. *)


open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Ast
open Universal.Ast
open Ast

let name = "python.program"
let debug fmt = Debug.debug ~channel:name fmt


module Domain =
struct

  let init _ ctx _ flow = ctx, flow

  let eval man ctx exp flow = None

  let init_globals man ctx filename globals flow =
    (* Initialize global variables with C_py_undefined constant *)
    let range = mk_file_range filename in
    let stmt =
      mk_block
        (List.mapi (fun i v ->
             let e =
               (* Initialize globals with the same name of a builtin with its address *)
               if Addr.is_builtin_name v.vname then (mk_py_object (Addr.find_builtin v.vname) range)
               else mk_expr (E_py_undefined true) range
             in
             mk_assign (mk_var v range) e range
           ) globals
        )
        range
    in
    let flow1 = man.exec ctx stmt flow in

    (** Initialize special variable __name__ *)
    let v = {
      vname = "__name__";
      vuid = 0;
      vtyp = T_any;
      vkind = V_orig;
    }
    in
    let stmt =
      mk_assign
        (mk_var v range)
        (mk_constant (Universal.Ast.C_string "__main__") ~etyp:Universal.Ast.T_string range)
        range
    in
    let flow2 = man.exec ctx stmt flow1 in

    (** Initialize special variable __file__ *)
    let v = {
      vname = "__file__";
      vuid = 0;
      vtyp = T_any;
      vkind = V_orig;
    }
    in
    let stmt =
        mk_assign
          (mk_var v range)
          (mk_constant (Universal.Ast.C_string filename) ~etyp:Universal.Ast.T_string range)
          range
    in
    let flow3 = man.exec ctx stmt flow2 in

    flow3


  let get_function_name fundec = fundec.py_func_var.vname

  let is_test fundec =
    let name = get_function_name fundec in
    if String.length name < 5 then false
    else String.sub name 0 4 = "test"

  let get_test_functions body =
    Framework.Visitor.fold_stmt
        (fun acc exp -> acc)
        (fun acc stmt ->
           match skind stmt with
           | S_py_function(fundec)
             when is_test fundec  ->
             fundec :: acc
           | _ -> acc
        ) [] body


  let mk_py_unit_tests file tests =
    let range = mk_file_range file in
    let tests =
      tests |> List.map (fun test ->
          (test.py_func_var.vname, {skind = S_expression (mk_py_call (mk_var test.py_func_var range) [] range); srange = range})
        )
    in
    mk_stmt (Universal.Ast.S_unit_tests (file, tests)) range


  let exec man ctx stmt flow  =
    match skind stmt with
    | S_program({prog_kind = Py_program(globals, body); prog_file})
      when not Framework.Options.(common_options.unit_test_mode) ->
      (* Initialize global variables *)
      init_globals man ctx prog_file globals flow |>
      (* Execute the body *)
      man.exec ctx body |>
      return

    | S_program({prog_kind = Py_program(globals, body); prog_file})
      when Framework.Options.(common_options.unit_test_mode) ->
      (* Initialize global variables *)
      let flow1 = init_globals man ctx prog_file globals flow in

      (* Execute the body *)
      let flow2 = man.exec ctx body flow1 in

      (* Collect test functions *)
      let tests = get_test_functions body in
      let stmt = mk_py_unit_tests prog_file tests in
      return (man.exec ctx stmt flow2)


    | _ -> None

  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
