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
open Framework.Core.Sig.Domain.Stateless
open Universal.Ast
open Stubs.Ast
open Ast


(** Iterator domain *)
(** =============== *)

module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.iterators.program"
    end)


  (** Command line options *)
  (** ==================== *)

  (* Name of the entry function to be analyzed. *)
  let opt_entry_function = ref "main"

  let () =
    register_domain_option name {
      key = "-c-entry";
      category = "C";
      doc = " name of the entry function to be analyzed";
      spec = ArgExt.Set_string opt_entry_function;
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
    List.fold_left (fun flow (v, init) ->
        let cvar =
          match v.vkind with
          | V_cvar cvar -> cvar
          | _ -> assert false
        in
        if cvar.cvar_scope = Variable_extern then flow
        else
          let stmt =
            match init with
            | Some (C_init_stub stub)->
              mk_block [
                mk_add_var v cvar.cvar_range;
                Stubs.Ast.mk_stub_init v stub cvar.cvar_range
              ] cvar.cvar_range

            | _ ->
              mk_c_declaration v init cvar.cvar_scope cvar.cvar_range
          in
          man.exec stmt flow
      ) flow


  (** Fund a function by name *)
  let find_function f functions =
    List.find (function
          {c_func_org_name} -> c_func_org_name = f
      ) functions


  (** Find a global variable by name *)
  let find_global v globals =
    List.find (function
        | ({vkind = (V_cvar {cvar_orig_name})},_) -> cvar_orig_name = v
        | _ -> false
      ) globals |> fst


  (** Call a function with a list of arguments *)
  let call f args man flow =
    let stmt = mk_c_call_stmt f args f.c_func_range in
    man.exec ~zone:Zone.Z_c stmt flow


  (** Address of the program name string *)
  let program_name_addr = {
    addr_kind = A_stub_resource "String";
    addr_group = G_all;
    addr_mode = STRONG;
  }


  (** Initialize argc and argv with concrete values and execute the body of main *)
  let call_main_with_concrete_args main args range man flow =
    (* Create the argc variable and initialize it to |args| + 1 *)
    let argc = mkfresh (fun uid ->
        let vname = "_argc" in
        let vkind = V_cvar {
            cvar_scope = Variable_global;
            cvar_range = range;
            cvar_uid = uid;
            cvar_orig_name = vname;
            cvar_uniq_name = vname;
          }
        in
        vname, vkind
      ) s32 ()
    in
    let decl =
      mk_c_declaration
        argc
        (Some (C_init_expr (mk_int (List.length args + 1) ~typ:s32 range)))
        Variable_global
        range
    in
    let flow = man.exec decl flow in

    (* Create the argv variable and initialize it with the given arguments *)
    let argv = mkfresh (fun uid ->
        let vname = "_argv" in
        let vkind = V_cvar {
            cvar_scope = Variable_global;
            cvar_range = range;
            cvar_uid = uid;
            cvar_orig_name = vname;
            cvar_uniq_name = vname;
          }
        in
        vname, vkind
      ) (array_type (pointer_type s8) (List.length args + 2 |> Z.of_int)) ()
    in
    let decl =
      mk_c_declaration
        argv
        (Some (C_init_list (
             (
               (("a.out" :: args) |>
                List.map (fun str -> C_init_expr (mk_c_string str range))
               )
               @ [C_init_expr (mk_zero range)]
             ), None
           )))
        Variable_global
        range
    in
    let flow = man.exec decl flow in

    (* call main with argc and argv *)
    call main [
      mk_var argc main.c_func_range;
      mk_var argv main.c_func_range
    ] man flow



  (** Initialize argc and argv with symbolic values and execute the body of main *)
  let call_main_with_symbolic_args main functions range man flow =
    (* Create the argc variable *)
    let argc = mkfresh (fun uid ->
        let vname = "_argc" in
        let vkind = V_cvar {
            cvar_scope = Variable_global;
            cvar_range = range;
            cvar_uid = uid;
            cvar_orig_name = vname;
            cvar_uniq_name = vname;
          }
        in
        vname, vkind
      ) s32 ()
    in
    let decl = mk_c_declaration argc None Variable_global range in
    let init_func = find_function "_mopsa_init_symbolic_argc" functions in
    let flow = man.exec decl flow |>
               man.exec (mk_assign (mk_var argc range) (mk_c_call init_func [] range) range)
    in

    (* Create the argv variable *)
    let argv = mkfresh (fun uid ->
        let vname = "_argv" in
        let vkind = V_cvar {
            cvar_scope = Variable_global;
            cvar_range = range;
            cvar_uid = uid;
            cvar_orig_name = vname;
            cvar_uniq_name = vname;
          }
        in
        vname, vkind
      ) (pointer_type (pointer_type s8)) ()
    in
    let decl = mk_c_declaration argv None Variable_global range in
    let init_func = find_function "_mopsa_init_symbolic_argv" functions in

    man.exec decl flow |>
    man.exec (mk_assign (mk_var argv range) (mk_c_call init_func [mk_var argc range] range) range) |>

    (* Initialize argv[0] and argv[1 .. argc] with fixed heap addresses of symbolic strings *)
    panic "c.iterators.program: initialization of symbolic argv not implemented"




  let exec zone stmt man flow =
    match skind stmt with
    | S_program ({ prog_kind = C_program {c_globals; c_functions} }, args)
      when not !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
      let flow1 = init_globals c_globals (srange stmt) man flow in

      (* Find entry function *)
      let entry =
        try find_function !opt_entry_function c_functions
        with Not_found ->
          panic "entry function %s not found" !opt_entry_function
      in

      (* Special processing for main for initializing argc and argv*)
      if !opt_entry_function = "main" && List.length entry.c_func_parameters = 2 then
        let flow2 =
          match args with
          | Some args -> call_main_with_concrete_args entry args entry.c_func_range man flow1
          | None      -> call_main_with_symbolic_args entry c_functions entry.c_func_range man flow1
        in
        Post.return flow2 |>
        Option.return
      else

      if List.length entry.c_func_parameters = 0 then
        (* Otherwise execute the body *)
        call entry [] man flow1 |>
        Post.return |>
        Option.return

      else panic "entry functions with arguments not supported"

    | S_program ({ prog_kind = C_program{ c_globals; c_functions } }, _)
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
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
