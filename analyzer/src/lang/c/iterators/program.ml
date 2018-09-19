(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Main handler of standalone C programs. *)


open Framework.Essentials
open Universal.Ast
open Ast


(** Command line options *)
(** ==================== *)

(* Name of the entry function to be analyzed. *)
let opt_entry_function = ref "main"

let () =
  register_option (
    "-c-entry",
    Arg.Set_string opt_entry_function,
    " name of the entry function to be analyzed (default: main)"
  )


(** Iterator domain *)
(** =============== *)

module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_program : unit domain

  let id = D_c_program
  let name = "c.iterators.program"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_program -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {export = [Zone.Z_c]; import = []}
  let eval_interface = {export = []; import = []}


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow = None

  (** Computation of post-conditions *)
  (** ============================== *)

  let rec exec zone stmt man flow  =
    match skind stmt with
    | S_program({prog_kind = C_program(globals, functions); prog_file})
      when not !Universal.Iterators.Unittest.unittest_flag ->
      let range = mk_file_range prog_file in
      (* Initialize global variables *)
      let flow1 = init_globals globals man flow in
      (* Find entry function *)
      let entry =
        try
          List.find (function
                {c_func_var} -> c_func_var.vname = !opt_entry_function
            ) functions
        with Not_found ->
          Framework.Exceptions.panic "entry function %s not found" !opt_entry_function
      in
      (* Execute body of entry function *)
      let stmt = mk_c_call_stmt entry [] range in
      man.exec stmt flow1 |>
      Post.return

    | S_program({prog_kind = C_program(globals, functions); prog_file})
      when !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
      let flow1 = init_globals globals man flow in

      let get_function_name fundec =
        fundec.c_func_var.vname
      in

      let is_test fundec =
        let name = get_function_name fundec in
        if String.length name < 5 then false
        else String.sub name 0 4 = "test"
      in

      let get_test_functions functions =
        List.filter is_test functions
      in

      let mk_c_unit_tests file tests =
        let range = mk_file_range file in
        let tests =
          tests |> List.map (fun test ->
              let name = test.c_func_var.vname in
              let range = tag_range range "test %s" name in
              let stmt = mk_c_call_stmt test [] range in
              (name, stmt)
            )
        in
        mk_stmt (Universal.Ast.S_unit_tests (file, tests)) range
      in

      let tests = get_test_functions functions in
      let stmt = mk_c_unit_tests prog_file tests in
      man.exec stmt flow1 |>
      Post.return

    | _ -> None

  and init_globals globals man flow =
    globals |>
    List.fold_left (fun flow (v, init, range) ->
        let stmt = mk_stmt (S_c_global_declaration (v, init)) range in
        man.exec stmt flow
      ) flow

  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow = None


  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
