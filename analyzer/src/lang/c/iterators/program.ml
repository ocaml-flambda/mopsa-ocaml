(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
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
  let name = "c.iterators.program"
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

  let interface = {
    iexec = {
      provides= [Zone.Z_c];
      uses = [Zone.Z_c]
    };

    ieval = {
      provides = [];
      uses = []
    }
  }


  (** Initialization of environments *)
  (** ============================== *)

  let init _ _ flow =  flow


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
      let f = find_function "_mopsa_init_symbolic_argc_argv" functions in
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
        Post.return |>
        Option.return
      else
        (* Otherwise execute the body *)
        call entry [] man flow1 |>
        Post.return |>
        Option.return

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
      Post.return |>
      Option.return

    | _ -> None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow = None


  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None

end

let () =
  Framework.Core.Sig.Stateless.Domain.register_domain (module Domain)
