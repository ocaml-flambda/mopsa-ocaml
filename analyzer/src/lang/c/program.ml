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
  let name = "c.program"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_program -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let zone = Zone.Z_c
  let import_exec = []
  let import_eval = []


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow = None

  (** Computation of post-conditions *)
  (** ============================== *)

  let exec stmt man flow  =
    match skind stmt with
    | S_program({prog_kind = C_program(globals, functions); prog_file})
      when not Framework.Options.(common_options.unit_test_mode) ->
      begin
        try
          let entry = List.find (function
                {c_func_var} -> c_func_var.vname = !opt_entry_function
            ) functions
          in
          let range = mk_file_range prog_file in
          let stmt = mk_c_call_stmt entry [] range in
          man.exec stmt flow |>
          Post.return
        with Not_found ->
          Framework.Exceptions.panic "entry function %s not found" !opt_entry_function
      end

    | S_program({prog_kind = C_program(globals, functions); prog_file})
      when Framework.Options.(common_options.unit_test_mode) ->

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
      man.exec stmt flow |>
      Post.return

    | _ -> None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval exp man flow = None


  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
