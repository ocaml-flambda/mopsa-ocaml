(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Main handler of Python programs. *)


open Framework.Domains.Stateless
open Framework.Essentials
open Universal.Ast
open Ast

let name = "python.program"
let debug fmt = Debug.debug ~channel:name fmt


(** Store list of global variables in the context. *)
type _ Framework.Context.key +=
  | KPyGlobals: var list Framework.Context.key

module Domain =
struct

  let init prog man ctx flow =
    match prog.prog_kind with
    | Py_program(globals, _) ->
      let ctx' = Framework.Context.add KPyGlobals globals ctx in
      Some (ctx', flow)

    | _ -> None

  let init_globals filename globals man ctx flow =
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
    let flow1 = man.exec stmt ctx flow in

    (** Initialize special variable __name__ *)
    let v = {
      vname = "__name__";
      vuid = 0;
      vtyp = T_any;
    }
    in
    let stmt =
      mk_assign
        (mk_var v range)
        (mk_constant (Universal.Ast.C_string "__main__") ~etyp:Universal.Ast.T_string range)
        range
    in
    let flow2 = man.exec stmt ctx flow1 in

    (** Initialize special variable __file__ *)
    let v = {
      vname = "__file__";
      vuid = 0;
      vtyp = T_any;
    }
    in
    let stmt =
        mk_assign
          (mk_var v range)
          (mk_constant (Universal.Ast.C_string filename) ~etyp:Universal.Ast.T_string range)
          range
    in
    let flow3 = man.exec stmt ctx flow2 in

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

  let import_exec = [Zone.Z_py]
  let export_exec = [Zone.Z_py]

  let exec zone stmt man ctx flow  =
    match skind stmt with
    | S_program({prog_kind = Py_program(globals, body); prog_file})
      when not Framework.Utils.Options.(common_options.unit_test_mode) ->
      (* Initialize global variables *)
      init_globals prog_file globals man ctx flow |>
      (* Execute the body *)
      man.exec body ctx |>
      Post.of_flow |>
      return

    | S_program({prog_kind = Py_program(globals, body); prog_file})
      when Framework.Utils.Options.(common_options.unit_test_mode) ->
      (* Initialize global variables *)
      let flow1 = init_globals prog_file globals man ctx flow in

      (* Execute the body *)
      let flow2 = man.exec body ctx flow1 in

      (* Collect test functions *)
      let tests = get_test_functions body in
      let stmt = mk_py_unit_tests prog_file tests in
      man.exec stmt ctx flow2 |>
      Post.of_flow |>
      return

    | _ -> None


  let import_eval = []
  let export_eval = []

  let eval zpath exp man ctx flow = None


  let ask _ _ _ _ = None

end

let setup () =
  register_domain name (module Domain);
  Framework.Context.(register_key_equality {
    case = (let f : type a b. chain -> a key -> b key -> (a, b) eq option =
              fun chain k1 k2 ->
                match k1, k2 with
                | KPyGlobals, KPyGlobals -> Some Eq
                | _ -> chain.check k1 k2
            in
            f);
  })

