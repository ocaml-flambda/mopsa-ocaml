(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Main handler of standalone C programs. *)


open Mopsa
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

  let exec_interface = {export = [Zone.Z_c]; import = [Zone.Z_c]}
  let eval_interface = {export = []; import = []}


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow = None


  (** Computation of post-conditions *)
  (** ============================== *)

  (** Initialize global variables *)
  let init_globals globals range man flow =
    globals |>
    List.fold_left (fun flow v ->
        let cvar =
          match v.vkind with
          | V_c cvar -> cvar
          | _ -> assert false
        in
        if cvar.var_scope = Variable_extern then flow
        else
          let stmt = mk_stmt (S_c_declaration v) cvar.var_range in
          man.exec stmt flow
      ) flow

  let find_function f functions =
    List.find (function
          {c_func_org_name} -> c_func_org_name = f
      ) functions

  let find_global v globals =
    List.find (fun v' -> v'.org_vname = v) globals

  let call f args man flow =
    let stmt = mk_c_call_stmt f args f.c_func_range in
    man.exec ~zone:Zone.Z_c stmt flow

  (** Initialize argc and argv and execute the body of main *)
  let exec_main main globals functions man flow =
    if main.c_func_parameters = [] then
      (* main takes no argument => just call its body *)
      let stmt = mk_c_call_stmt main [] main.c_func_range in
      man.exec ~zone:Zone.Z_c stmt flow
    else
      (* initialize argc and argv *)
      let f = find_function "_mopsa_main" functions in
      let flow = call f [] man flow in

      (* call main with argc and argv *)
      let argc = find_global "_argc" globals in
      let argv = find_global "_argv" globals in

      call main [
        mk_var argc main.c_func_range;
        mk_var argv main.c_func_range
      ] man flow

  let exec zone stmt man flow =
    match skind stmt with
    | S_program { prog_kind = C_program {c_globals; c_functions} }
      when not !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
      let flow1 = init_globals c_globals (srange stmt) man flow in

      (* Find entry function *)
      let entry =
        try find_function !opt_entry_function c_functions
        with Not_found ->
          panic "entry function %s not found" !opt_entry_function
      in
      debug "entering %s" !opt_entry_function;
      (* Special processing for main for initializing argc and argv*)
      if !opt_entry_function = "main" then
        exec_main entry c_globals c_functions man flow1 |>
        Post.return
      else
        (* Otherwise execute the body *)
        call entry [] man flow1 |>
        Post.return

    | S_program { prog_kind = C_program{ c_globals; c_functions } }
      when !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
      let flow1 = init_globals c_globals (srange stmt) man flow in

      let is_test fundec =
        let name = fundec.c_func_org_name in
        if String.length name < 5 then false
        else String.sub name 0 4 = "test"
      in

      let get_test_functions functions =
        List.filter is_test functions
      in

      let mk_c_unit_tests tests =
        let tests =
          tests |> List.map (fun test ->
              let name = test.c_func_org_name in
              let stmt = mk_c_call_stmt test [] test.c_func_range in
              (name, stmt)
            )
        in
        mk_stmt (Universal.Ast.S_unit_tests tests) (srange stmt)
      in

      let tests = get_test_functions c_functions in
      let stmt = mk_c_unit_tests tests in
      man.exec stmt flow1 |>
      Post.return

    | _ -> None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow = None


  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
