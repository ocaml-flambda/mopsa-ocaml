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


  (** Command line options *)
  (** ==================== *)

  (* Name of the entry function to be analyzed. *)
  let opt_entry_function = ref "main"

  let () =
    register_domain_option name {
      key = "-c-entry";
      doc = " name of the entry function to be analyzed";
      spec = Arg.Set_string opt_entry_function;
      default = "main";
    }

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

  let exec zone stmt man flow =
    match skind stmt with
    | S_program { prog_kind = C_program {c_globals; c_functions} }
      when not !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
      let flow1 = init_globals c_globals (srange stmt) man flow in
      (* Find entry function *)
      let entry =
        try
          List.find (function
                {c_func_org_name} -> c_func_org_name = !opt_entry_function
            ) c_functions
        with Not_found ->
          Exceptions.panic "entry function %s not found" !opt_entry_function
      in
      (* Execute body of entry function *)
      let stmt = mk_c_call_stmt entry [] (srange stmt) in
      man.exec ~zone:Zone.Z_c stmt flow1 |>
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
