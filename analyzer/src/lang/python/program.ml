(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Main handler of Python programs. *)
(** This domain initializes global variables, creates special
   variables __name__, __main__, __file__, and collects unit-testing
   functions if required *)

open Framework.Essentials
open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Universal.Ast
open Ast


module Domain =
struct

  type _ domain += D_python_program : unit domain

  let id = D_python_program
  let name = "python.program"
  let identify : type a. a domain -> (unit, a) eq option = function
    | D_python_program -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  let exec_interface = {export = [Zone.Z_py]; import = []}
  let eval_interface = {export = []; import = []}

  let init _ _ flow = Some flow

  let eval _ _ _ _ = None

  let init_globals man globals range flow =
    (* Initialize global variables with C_py_undefined constant *)
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
    let flow1 = man.exec stmt flow in

    (** Initialize special variable __name__ *)
    let v = {
      vname = "__name__";
      vuid = 0;
      vtyp = T_any;
      (* vkind = V_orig; *)
    }
    in
    let stmt =
      mk_assign
        (mk_var v range)
        (mk_constant (Universal.Ast.C_string "__main__") ~etyp:Universal.Ast.T_string range)
        range
    in
    let flow2 = man.exec stmt flow1 in

    (** Initialize special variable __file__ *)
    let v = {
      vname = "__file__";
      vuid = 0;
      vtyp = T_any;
      (* vkind = V_orig; *)
    }
    in
    let stmt =
        mk_assign
          (mk_var v range)
          (mk_constant (Universal.Ast.C_string (get_range_file range)) ~etyp:Universal.Ast.T_string range)
          range
    in
    let flow3 = man.exec stmt flow2 in

    flow3


  let get_function_name fundec = fundec.py_func_var.vname

  let is_test fundec =
    let name = get_function_name fundec in
    if String.length name < 5 then false
    else String.sub name 0 4 = "test"

  let get_test_functions body =
    Framework.Visitor.fold_stmt
        (fun acc exp -> VisitParts acc)
        (fun acc stmt ->
           match skind stmt with
           | S_py_function(fundec)
             when is_test fundec  ->
             Keep (fundec :: acc)
           | _ -> VisitParts (acc)
        ) [] body


  let mk_py_unit_tests tests range =
    let tests =
      tests |> List.map (fun test ->
          (test.py_func_var.vname, {skind = S_expression (mk_py_call (mk_var test.py_func_var range) [] range); srange = range})
        )
    in
    mk_stmt (Universal.Ast.S_unit_tests (tests)) range


  let exec zone stmt man flow  =
    match skind stmt with
    | S_program(Py_program(globals, body))
      when not !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
      init_globals man globals (srange stmt) flow |>
      (* Execute the body *)
      man.exec body |>
      Post.return

    | S_program(Py_program(globals, body))
      when !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
      let flow1 = init_globals man  globals (srange stmt) flow in

      (* Execute the body *)
      let flow2 = man.exec body flow1 in

      (* Collect test functions *)
      let tests = get_test_functions body in
      let stmt = mk_py_unit_tests tests (srange stmt) in
      Post.return (man.exec stmt flow2)


    | _ -> None

  let ask _ _ _ = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
