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
open Zone
open Universal.Zone


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

  (** Name of the entry function to be analyzed. *)
  let opt_entry_function = ref "main"

  let () =
    register_domain_option name {
      key = "-c-entry";
      category = "C";
      doc = " name of the entry function to be analyzed";
      spec = ArgExt.Set_string opt_entry_function;
      default = "main";
    }


  (** {2 Auxiliary variables} *)
  (** ======================= *)

  (** Symbolic variable representing the number of command-line arguments *)
  type var_kind += V_c_argn

  let () = register_var {
      print = (fun next fmt v ->
          match v.vkind with
          | V_c_argn -> Format.fprintf fmt "|args|"
          | _ -> next fmt v
        );
      compare = (fun next v1 v2 ->
          match v1.vkind, v2.vkind with
          | V_c_argn,V_c_argn -> 0
          | _ -> next v1 v2
        );
    }

  let mk_c_argn range =
    mk_var (mkv "|args|" V_c_argn s32) range


  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {
      provides= [Z_c];
      uses = [Z_c;Z_c_scalar]
    };

    ieval = {
      provides = [];
      uses = [Z_c,Z_c_scalar]
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
          let stmt = mk_c_declaration v init cvar.cvar_scope cvar.cvar_range in
          man.exec stmt flow
      ) flow


  (** Execute stub directives *)
  let exec_stub_directives directives range man flow =
    directives |>
    List.fold_left (fun flow directive ->
        let stmt = mk_stub_directive directive range in
        man.exec stmt flow
      ) flow


  (** Fund a function by name *)
  let find_function f functions =
    List.find (function
          {c_func_org_name} -> c_func_org_name = f
      ) functions


  (** Call a function with a list of arguments *)
  let call f args man flow =
    let stmt = mk_c_call_stmt f args f.c_func_range in
    man.exec ~zone:Z_c stmt flow |>
    Post.return


  (** Create the address of the array pointed by argv *)
  let mk_c_argv range =
    mk_addr
      {
        addr_kind = Stubs.Ast.A_stub_resource "argv";
        addr_group = G_all;
        addr_mode = STRONG;
      }
      ~etyp:(T_c_pointer (T_c_pointer s8))
      range


  (** Create the address of an argument string *)
  let mk_c_arg ~mode range =
    mk_addr
      {
        addr_kind = Stubs.Ast.A_stub_resource "arg";
        addr_group = G_all;
        addr_mode = mode;
      }
      ~etyp:(T_c_pointer s8)
      range


  (** Initialize argc and argv with concrete values and execute the body of main *)
  let call_main_with_concrete_args main args man flow =
    let range = main.c_func_range in

    (* argc is set to |args| + 1 *)
    let argc = mk_int (List.length args + 1) range in

    (* Create the memory block pointed by argv. *)
    let argv = mk_c_argv range in

    (* Initialize its size to |args| + 2 *)
    man.eval ~zone:(Z_c,Z_c_scalar) (mk_expr (Stubs.Ast.E_stub_builtin_call (BYTES, argv)) range ~etyp:ul) flow >>$ fun bytes flow ->
    let flow = man.exec ~zone:Z_c_scalar (mk_add bytes range) flow |>
               man.exec ~zone:Z_c_scalar (mk_assign bytes (mk_z
                                                             (Z.mul
                                                                (sizeof_type (T_c_pointer s8))
                                                                (Z.of_int (List.length args + 2))
                                                             ) range
                                                          ) range) |>
               man.exec (mk_add argv range)
    in


    (* Initialize argv[0] with the name of the program *)
    let flow = man.exec
        (mk_assign
           (mk_c_subscript_access argv (mk_zero range) range)
           (mk_c_string "a.out" range)
           range
        )
        flow
    in

    (* Initialize argv[i | 1 <= i < argc] with command-line arguments *)
    let rec iter i flow =
      if i >= List.length args + 1
      then flow
      else
        let range = tag_range range "argv[%d]" i in
        let argvi = mk_c_subscript_access argv (mk_int i range) range in
        let arg = mk_c_string (List.nth args (i-1)) range in
        let flow = man.exec (mk_assign argvi arg range) flow in
        iter (i + 1) flow
    in
    let flow = iter 1 flow in

    (* Put NULL in argv[argc + 1] *)
    let last = mk_c_subscript_access argv argc range in
    let flow = man.exec (mk_assign last (mk_zero range) range) flow in

    (* call main with argc and argv *)
    call main [argc; argv] man flow




  (** Initialize argc and argv with symbolic arguments *)
  let call_main_with_symbolic_args main functions range man flow =
    let range = main.c_func_range in

    (* Add the symbolic variable argn representing the number of arguments *)
    let argn = mk_c_argn range in
    let flow = man.exec (mk_add argn range) flow |>
               man.exec (mk_assign argn (mk_z_interval Z.one (rangeof s32 |> snd |> Z.pred) range) range)
    in

    (* Initialize argc *)
    let argc = add argn (mk_one range) range in

    (* Create the memory block pointed by argv. *)
    let argv = mk_c_argv range in

    (* Initialize its size to argc + 1 *)
    man.eval ~zone:(Z_c,Z_c_scalar) (mk_expr (Stubs.Ast.E_stub_builtin_call (BYTES, argv)) range ~etyp:ul) flow >>$ fun bytes flow ->
    man.eval ~zone:(Z_c,Z_c_scalar) argc flow >>$ fun scalar_argc flow ->
    let flow = man.exec ~zone:Z_c_scalar (mk_add bytes range) flow |>
               man.exec ~zone:Z_c_scalar (mk_assign bytes (mul
                                                             (mk_z (sizeof_type (T_c_pointer s8)) range)
                                                             (add scalar_argc (mk_one range) range)
                                                             range
                                                          ) range) |>
               man.exec (mk_add argv range)
    in

    (* Initialize argv[0] with the name of the program *)
    let flow = man.exec
        (mk_assign
           (mk_c_subscript_access argv (mk_zero range) range)
           (mk_c_string "a.out" range)
           range
        )
        flow
    in

    (* Create a symbolic argument *)
    let arg = mk_c_arg ~mode:STRONG range in

    (* Initialize the size of the argument *)
    man.eval ~zone:(Z_c,Z_c_scalar) (mk_expr (Stubs.Ast.E_stub_builtin_call (BYTES, arg)) range ~etyp:ul) flow >>$ fun bytes flow ->
    let flow = man.exec ~zone:Z_c_scalar (mk_add bytes range) flow |>
               man.exec ~zone:Z_c_scalar (mk_assign bytes (mk_z (rangeof s32 |> snd) range) range) |>
               man.exec (mk_add arg range)
    in

    (* Ensure that the argument is a valid string *)
    let some_arg_cell = mk_c_subscript_access arg (mk_z_interval Z.zero (rangeof s32 |> snd |> Z.pred) range) range in
    let flow = man.exec (mk_assign some_arg_cell (mk_zero range) range) flow in

    (* Make the address weak *)
    let arg_weak = mk_c_arg ~mode:WEAK range in
    let flow = man.exec (mk_rename arg arg_weak range) flow in

    (* Put the symbolic argument in argv[1 : argc-1] *)
    let i = mktmp ~typ:s32 () in
    let ii = mk_var i range in
    let l = mk_one range in
    let u = argn in
    let flow = man.exec (mk_add ii range) flow |>
               man.exec (mk_assign ii (mk_c_builtin_call "_mopsa_range_s32" [l;u] s32 range) range)
    in
    let every_argv_cell = mk_c_subscript_access argv (mk_expr (Stubs.Ast.E_stub_quantified(FORALL,i,S_interval(l,u))) ~etyp:s32 range) range in
    let flow = man.exec (mk_assume (mk_binop every_argv_cell O_eq arg_weak range) range) flow |>
               man.exec (mk_remove ii range)
    in

    (* Put NULL in argv[argc] *)
    let last = mk_c_subscript_access argv argc range in
    let flow = man.exec (mk_assign last (mk_zero range) range) flow in

    call main [argc; argv] man flow


  let exec zone stmt man flow =
    match skind stmt with
    | S_program ({ prog_kind = C_program {c_globals; c_functions; c_stub_directives} }, args)
      when not !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
      let flow1 = init_globals c_globals (srange stmt) man flow in

      (* Execute stub directives *)
      let flow1 = exec_stub_directives c_stub_directives (srange stmt) man flow1 in

      (* Find entry function *)
      let entry =
        try find_function !opt_entry_function c_functions
        with Not_found ->
          panic "entry function %s not found" !opt_entry_function
      in

      (* Special processing for main for initializing argc and argv*)
      if !opt_entry_function = "main" && List.length entry.c_func_parameters = 2 then
        let post =
          match args with
          | Some args -> call_main_with_concrete_args entry args man flow1
          | None      -> call_main_with_symbolic_args entry c_functions entry.c_func_range man flow1
        in
        Some post
      else

      if List.length entry.c_func_parameters = 0 then
        (* Otherwise execute the body *)
        call entry [] man flow1 |>
        Option.return

      else panic "entry functions with arguments not supported"

    | S_program ({ prog_kind = C_program{ c_globals; c_functions; c_stub_directives } }, _)
      when !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
      let flow1 = init_globals c_globals (srange stmt) man flow in

      (* Execute stub directives *)
      let flow1 = exec_stub_directives c_stub_directives (srange stmt) man flow1 in

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
