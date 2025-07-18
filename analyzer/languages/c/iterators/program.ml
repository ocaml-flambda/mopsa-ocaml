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
open Sig.Abstraction.Stateless
open Universal.Ast
open Stubs.Ast
open Ast
open Common
open Common.Points_to
open Common.Base
open Common.Runtime
open Universal.Numeric.Common
module StringMap = MapExt.StringMap


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
      spec = Set_string (opt_entry_function, ArgExt.empty);
      default = "main";
    }

  let opt_check_memory = ref false

  let () =
    register_domain_option name {
      key = "-c-check-unreachable-memory";
      category = "C";
      doc = " check for unreachable allocated memory";
      spec = Set opt_check_memory;
      default = "main";
    }


  (** Symbolic main arguments. *)
  let opt_symbolic_args = ref None

  let parse_symbolic_args_spec spec : int * int option =
    if not Str.(string_match (regexp "\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?") spec 0) then
      panic "incorrect argument '%s' for option -c-symbolic-args" spec
    ;
    let lo = Str.matched_group 1 spec |> int_of_string in
    let hi = try Some (Str.matched_group 3 spec |> int_of_string) with Not_found -> None in
    lo,hi

  let () =
    register_domain_option name {
      key = "-c-symbolic-args";
      category = "C";
      doc = " set the number of symbolic arguments given to main (syntax: min[:max])";
      spec = String ((fun s -> opt_symbolic_args := Some (parse_symbolic_args_spec s)), ArgExt.empty);
      default = "";
    }

  let opt_arg_max_size = ref None

  let () =
    register_domain_option name {
      key = "-c-symbolic-args-max-size";
      category = "C";
      doc = " set the maximum allocated size of all symbolic arguments";
      spec = Int ((fun s -> opt_arg_max_size := Some s), ArgExt.empty);
      default = "18446744073709551615";
    }


  let opt_arg_min_size = ref None

  let () =
    register_domain_option name {
      key = "-c-symbolic-args-min-size";
      category = "C";
      doc = " set the maximum allocated size of all symbolic arguments";
      spec = Int ((fun s -> opt_arg_min_size := Some s), ArgExt.empty);
      default = "1"
    }
  (** Runtime testing options *)
  let ffitest_flag = ref false
  module StringSet = Set.Make(String)
  let ffitest_extfuns : Type_shapes.extfun_desc StringMap.t ref = ref (StringMap.empty)
  let ffitest_missing_funs : StringSet.t ref = ref (StringSet.empty)
  let ffitest_unimplemented_funs : StringSet.t ref = ref (StringSet.empty)



  let externals_version = "v0.1"

  let read_file str =
    let file = try open_in str with Sys_error _ -> failwith (Format.asprintf "cannot open file %s" str) in
    try
      let text = In_channel.input_all file in
      close_in file;
      text
    with e ->
      (* some unexpected exception occurs *)
      close_in_noerr file;
      (* emergency closing *)
      raise e


  let () = register_domain_option name {
    key = "-external-functions";
    category="Runtime";
    doc=" file containing function names of functions to test (if contained in the file)";
    spec = ArgExt.String((fun s ->
      let funs = read_file s in
      match Type_shapes.deserialize_extfuns funs with
      | Some { version; extfuns=funs } when version = externals_version ->
        let funs_map: (string * Type_shapes.extfun_desc) list = List.map (fun (f: Type_shapes.extfun) -> (f.name, f.desc)) funs in
        ffitest_extfuns := StringMap.of_list funs_map
      | Some {version; _} ->
          failwith (Format.asprintf "version mismatch: expected version %s, but got %s" externals_version version)
      | None ->
        (* FIXME: perhaps we want a different error here? *)
        failwith (Format.asprintf "input error (current version %s): cannot parse external function declaration %s" externals_version funs)
   ), (fun _ -> []));
    default=""
  }

  let () = register_domain_option name {
    key = "-runtimetest";
    category="Runtime";
    doc=" test C stubs";
    spec = ArgExt.Set ffitest_flag;
    default=""
  }




  let checks = []

  (** Utility functions *)
  (** ================= *)

  let mk_lowlevel_subscript_access a i t range =
    mk_c_deref (mk_binop a O_plus i ~etyp:(pointer_type t) range) range

  let assign_array a i t e r range =
    let lval = mk_lowlevel_subscript_access a i t r in
    let stmt = mk_assign lval e range in
    stmt

  let function_report_outcome (rep: report) =
    let total, safe, error, warning, info, unimplemented, checks_map = Output.Text.construct_checks_summary ~print:false rep None in
    if error > 0 || warning > 0 then
      Error
    else if unimplemented > 0 then
      Unimplemented
    else if info > 0 then Info
    else Safe


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow =
    match prog.prog_kind with
    | C_program p ->
      set_c_program p flow |>
      set_c_target_info !Frontend.target_info |>
      Post.return |>
      Option.some

    | _ -> None


  (** OCaml runtime function test machinery *)
  (** ===================================== *)

  let value_shape_of_function (f: c_fundec) : Type_shapes.fn_value_shapes option =
    match StringMap.find_opt f.c_func_org_name (!ffitest_extfuns) with
    | Some { shape = Some ty } -> Some ty
    (* a runtime function we should test of unknown shape *)
    | Some { shape = None } ->
      let default_shape_args = List.map (fun _ -> Type_shapes.Value) (f.c_func_parameters) in
      let default_shape_ret = Type_shapes.Value in
      Some { arguments=default_shape_args; return=default_shape_ret }
    (* not a runtime function we should test *)
    | None -> None

  let virtual_runtime_arguments array tys range : expr list =
    let len = List.length tys in
    if len > 5 then [mk_c_address_of array range; mk_int len range]
    else List.init len (fun i -> mk_lowlevel_subscript_access array (mk_int i range) ffi_value_typ range)

  let virtual_runtime_argument_array tys range : (stmt list) * (expr list) =
    let size = C_array_length_cst (Z.of_int (List.length tys)) in
    let tmp_active_var = mktmp ~typ:(T_c_array (ffi_value_typ, size)) () in
    let tmp_active = mk_var tmp_active_var range in
    let declare_var = mk_c_declaration tmp_active_var None Variable_global range in
    let assignments = List.mapi (fun i ty ->
      [
        assign_array tmp_active (mk_int i range) ffi_value_typ (mk_top ffi_value_typ range) range range;
        mk_ffi_init_with_shape (mk_lowlevel_subscript_access tmp_active (mk_int i range) ffi_value_typ range) ty range
      ]
    ) tys in
    let args = virtual_runtime_arguments tmp_active tys range in
    declare_var :: List.concat assignments, args

  let eval_is_ret_val range retval man flow =
    match ekind retval with
    | E_constant C_unit ->
      let flow = raise_ffi_void_return range man flow in
      Cases.singleton false flow
    | _ ->
      Cases.singleton true flow


  let exec_arity_check f (ty: Type_shapes.fn_value_shapes) range man flow =
    let actual = List.length (f.c_func_parameters) in
    let expected = List.length (ty.arguments) in
    if actual != expected && actual = 0 then
      (* Taking zero arguments is fine. *)
      Post.return flow
    else if actual != expected && expected <= 5  then
      let flow = raise_ffi_arity_mismatch range ~actual ~expected man flow in
      Post.return flow
    else if expected > 5 && actual != 2 then
      let flow = raise_ffi_arity_mismatch range ~actual ~expected:2 man flow in
      Post.return flow
    else
      let flow = safe_ffi_arity_check range man flow in
      Post.return flow

  let exec_virtual_runtime_function_test_exn (f: c_fundec) (ty: Type_shapes.fn_value_shapes) man flow =
    let range = f.c_func_name_range in
    let {Type_shapes.arguments = arg_shapes; return = ret_shape } = ty in
    let stmts, args = virtual_runtime_argument_array arg_shapes range in
    (* check the OCaml side arity and the C side arity agree *)
    exec_arity_check f ty f.c_func_range man flow >>% fun flow ->
    (* allocate the runtime arguments *)
    man.exec (mk_block stmts range) flow >>% fun flow ->
    (* execute the external function *)
    man.eval (mk_c_call f args range) flow >>$ fun exp flow ->
    (* check the return value is not void *)
    eval_is_ret_val f.c_func_range exp man flow >>$ fun has_ret flow ->
    if has_ret && not (Flow.is_empty flow) then
      (* assert the result shape *)
      man.exec (mk_ffi_assert_shape exp ret_shape f.c_func_range) flow
    else
      Post.return flow

  let exec_virtual_runtime_function_test f ty man flow =
    try
      exec_virtual_runtime_function_test_exn f ty man flow
    with e ->
      (* something went wrong in the analysis *)
      let flow = raise_ffi_internal_error "An unexpected exception was raised during the analysis." f.c_func_range man flow in
      Post.return flow


  let rec exec_all_runtime_functions (fs: (c_fundec * Type_shapes.fn_value_shapes) list) man (flows: 'a flow list) (results: (string * diagnostic_kind) list) (flow: 'a flow) =
    match fs with
    | [] ->
      (* eventually, we reverse the results to be again in program order *)
      Cases.singleton (List.rev results) (Flow.join_list man.lattice ~empty: (fun () -> flow) flows)
    | (f, ty) :: fs ->
        let () = Debug.debug ~channel:"runtime_functions" "analyzing %s at %a" (f.c_func_org_name) pp_range f.c_func_range in
        (* NOTE: This is tricky. This bind only works, because even if the flow is empty,
           the monad will still execute the subsequent part. This is crucially different
          from the usual [>>$], which silently drops empty.  *)
        exec_virtual_runtime_function_test f ty man flow >>% fun flow' ->
        let report = Flow.get_report flow' in
        let res = function_report_outcome report in
        exec_all_runtime_functions fs man (flow' :: flows) ((f.c_func_org_name, res) :: results) flow


  let output_results skipped_functions unimplemented_functions results to_test =
    let pp_markdown_function fmt name = Format.fprintf fmt "`%s`" name in
    let pp_unknown_functions fmt set =
      Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ","; Format.pp_print_space fmt ()) pp_markdown_function fmt (StringSet.elements set)
    in
    let pp_runtime_analysis_results fmt results =
      List.iter (fun (f, res) -> Format.fprintf fmt "- %a (%s)\n" pp_markdown_function f (Output.Text.icon_of_diag res)) results
    in
    let pp_missing_functions fmt map =
      Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ","; Format.pp_print_space fmt ()) pp_markdown_function fmt (StringMap.fold (fun name _ names -> name :: names) map [])
    in
    let pp_unimplemented_functions fmt set =
      Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ","; Format.pp_print_space fmt ()) pp_markdown_function fmt (StringSet.elements set)
    in
    let results_map = StringMap.of_list results in
    let missing_functions = StringMap.filter (fun name _ -> not (StringMap.mem name results_map)) to_test in
      if not (!Output.Text.opt_no_ffi_report) then
        (Format.printf "Analysis completed successfully.\n\n";
          Format.printf
          "**Analyzed functions:**\n%a\n**Skipped C functions:**@\n\n@[%a@]@\n\n**Missing external functions:**@\n@[%a@]@\n\n**Unimplemented OCaml FFI functions:**@\n@[%a@]@\n\n"
          pp_runtime_analysis_results
          results
          pp_unknown_functions
          skipped_functions
          pp_missing_functions
          missing_functions
          pp_unimplemented_functions
          unimplemented_functions)

  let exec_runtime_tests c_functions man flow =
    (* Determine the runtime functions to execute; we sort them by program order *)
    let c_functions = List.sort (fun f g -> compare_range f.c_func_range g.c_func_range) c_functions in
    let ffi_functions = List.concat_map (fun f -> match value_shape_of_function f with None -> [] | Some sh -> [(f, sh)]) c_functions in
    (* Execute all the runtime functions, yielding a list of results for each function *)
    exec_all_runtime_functions ffi_functions man [] [] flow >>$ fun results flow ->
    (* we output the results, the functions that were assumed to be missing,
       and compute the functions that should have been checked but were not. *)
    output_results (!ffitest_missing_funs) (!ffitest_unimplemented_funs) results (!ffitest_extfuns);
    Post.return flow


  (** Computation of post-conditions *)
  (** ============================== *)


  (** Initialize global variables *)
  let init_globals globals man flow =
    globals |>
    List.fold_left (fun acc (v, init) ->
        let cvar =
          match v.vkind with
          | V_cvar cvar -> cvar
          | _ -> assert false
        in
        if cvar.cvar_scope = Variable_extern then acc
        else
          let decl = mk_c_declaration v init cvar.cvar_scope cvar.cvar_range in
          let stmt =
            match cvar.cvar_before_stmts, cvar.cvar_after_stmts with
            | [], [] -> decl
            | before, [] -> mk_block (before @ [decl]) cvar.cvar_range
            | [], after -> mk_block (decl::after) cvar.cvar_range
            | before, after -> mk_block (before @ [decl] @ after) cvar.cvar_range
          in
          acc >>% man.exec stmt
      ) (Post.return flow)


  (** Execute stub directives *)
  let exec_stub_directives directives man flow =
    directives |>
    List.fold_left (fun acc directive ->
        let stmt = mk_stub_directive directive directive.stub_directive_range in
        acc >>% man.exec stmt
      ) (Post.return flow)


  (** Fund a function by name *)
  let find_function f functions =
    List.find (function
          {c_func_org_name} -> c_func_org_name = f
      ) functions

  (** Find a global variable by name *)
  let find_variable v vars =
    List.find (fun (vv,_) ->
        match vv.vkind with
        | V_cvar {cvar_orig_name} -> cvar_orig_name = v
        | _ -> false
      ) vars
    |> fst

  let check_memory_allocation man range flow =
    if not !opt_check_memory then flow else
    (* debug "%a" (format @@ Flow.print man.lattice.print) flow; *)
    let open Universal.Heap.Recency in
    let dead_addrs =
      let alive_addrs = ask_and_reduce man.ask Q_alive_addresses_aspset flow in
      let all_addrs = ask_and_reduce man.ask Q_allocated_addresses_aspset flow in
      debug "All addrs: %a@.Alive addrs %a" (format Pool.print) all_addrs (format Pool.print) alive_addrs;
      Pool.diff all_addrs alive_addrs in
    let interesting_dead_addrs =
      Pool.filter (fun a ->
          (* FIXME: we could also check the range... *)
          match akind a with
          | A_stub_resource m -> m = "Memory"
          | _ -> false
        ) dead_addrs in
    (* FIXME add safe check otherwise *)
    Pool.fold (fun addr flow -> Common.Alarms.raise_c_unreachable_memory addr range man flow) interesting_dead_addrs flow

  (** Execute exit functions using a loop *)
  let exec_exit_functions_with_loop a b buf range man flow =
    let rec aux i flow =
      if i < Z.zero then
        (* We have reached the end of the array *)
        Post.return flow
      else
        (* Resolve the pointed function *)
        let fp = mk_c_subscript_access buf (mk_z i range) range in
        resolve_pointer fp man flow >>$ fun p flow ->
        let input = flow in
        match p with
        | P_fun f ->
          (* Execute the function *)
          let stmt = mk_c_call_stmt f [] range in
          man.exec stmt flow >>% fun flow' ->
          (* Function to return the correct flow depending on the
             position in the array. If index [i] is above [a], this
             means that function [f] hasn't been registered in some
             traces, so we put the input flow also *)
          let fix_flow f = if Z.(i < a) then f else Flow.join man.lattice input f in
          (* If the output is empty, this was due to calls to _exit or due to alarms. So we stop the loop *)
          if man.lattice.is_bottom (Flow.get T_cur man.lattice flow') then
            Post.return (fix_flow flow')
          else
            aux (Z.pred i) flow' >>% fun flow'' ->
            Post.return (fix_flow flow'')

        | P_top ->
          Flow.add_global_assumption
            Soundness.A_ignore_undetermined_exit_functions
            flow |>
          Post.return

        | _ -> assert false
    in
    aux (Z.pred b) flow >>% fun flow ->
    let flow = check_memory_allocation man range flow in
    Post.return (Flow.remove T_cur flow)



  (** Execute functions registered with atexit *)
  let exec_exit_functions name range man flow =
    let prog = get_c_program flow in
    try
      let nb = mk_var (find_variable ("_next_"^name^"_fun_slot") prog.c_globals) range in
      let buf = mk_var (find_variable ("_"^name^"_fun_buf") prog.c_globals) range in
      let nb_itv =
        let evl = man.eval nb flow ~translate:"Universal" in
        Cases.fold_result
          (fun acc nb flow ->
             ask_and_reduce man.ask (mk_int_interval_query nb) flow |>
             I.join_bot acc
          ) Bot.BOT evl
      in
      match nb_itv with
      | Bot.BOT ->
        Post.return flow

      | Bot.Nb itv when I.is_bounded itv ->
        let a,b = match itv with I.B.Finite a, I.B.Finite b -> (a,b) | _ -> assert false in
        exec_exit_functions_with_loop a b buf range man flow

      | _ ->
        Flow.add_global_assumption
          Soundness.A_ignore_undetermined_exit_functions
          flow |>
        Flow.remove T_cur |>
        Post.return

    (* Variables _next_exit_fun_slot and _exit_fun_buf not found,
       probably because <stdlib.h> not included. In this case, do nothing *)
    with Not_found -> Post.return flow


  let exec_entry_body f man flow =
    match f.c_func_body with
    | None -> panic "entry function %s is not defined" f.c_func_org_name
    | Some stmt ->
      let f' = { f with c_func_parameters = [] } in
      let stmt = mk_c_call_stmt f' [] f.c_func_name_range in
      man.exec stmt flow


  (** {2 Creation of argc and argv} *)
  (** ***************************** *)

  let argv_resource = "argv"
  let arg_single_resource i = "arg#"^(string_of_int i)
  let arg_smash_resource i = "arg#+"^(string_of_int i)

  let is_argv_resource r = (r=argv_resource)

  let is_arg_single_resource r =
    Str.string_match (Str.regexp "arg#[0-9]+") r 0

  let is_arg_smash_resource r =
    Str.string_match (Str.regexp "arg#\\+[0-9]+") r 0

  let is_arg_resource r =
    is_arg_single_resource r ||
    is_arg_smash_resource r

  let () =
    Universal.Heap.Policies.register_mk_addr
      (fun next a ->
         match a with
         | A_stub_resource r
           when is_argv_resource r
             || is_arg_resource r ->
           Universal.Heap.Policies.mk_addr_all a
         | a -> next a )

  (** Allocate the argv array *)
  let alloc_argv range man flow =
    let arange = tag_range range "%s" argv_resource in
    man.eval (mk_stub_alloc_resource argv_resource arange) flow >>$ fun addr flow ->
    Eval.singleton { addr with etyp = T_c_pointer (T_c_pointer s8) } flow


  (** Allocate an address for a concrete argument *)
  let alloc_concrete_arg i range man flow =
    let resource = arg_single_resource i in
    let irange = tag_range range "%s" resource in
    man.eval (mk_stub_alloc_resource resource irange) flow >>$ fun addr flow ->
    Eval.singleton { addr with etyp =  T_c_pointer s8 } flow


  (** Initialize an argument with a concrete string *)
  let init_concrete_arg arg str range man flow =
    let n = String.length str in
    let rec aux i flow =
      let argi = mk_c_subscript_access arg (mk_int i range) range in
      if i = n then man.exec (mk_assign argi zero range) flow
      else
        man.exec (mk_assign argi (mk_c_character (String.get str i) range (etyp argi)) range) flow >>% fun flow ->
        aux (i + 1) flow
    in
    aux 0 flow

  let eval_bytes obj range man flow =
    man.eval (mk_expr (Stubs.Ast.E_stub_builtin_call (BYTES, [obj])) range) flow

  (** Initialize argc and argv with concrete values and execute the body of main *)
  let call_main_with_concrete_args main args man flow =
    let range = main.c_func_name_range in

    (* argc is set to |args| + 1 *)
    let nargs = List.length args in
    let argc = mk_int (nargs + 1) range in

    (* Create the memory block pointed by argv. *)
    alloc_argv range man flow >>$ fun argv flow ->

    (* Initialize its size to (|args| + 2)*sizeof(ptr) *)
    eval_bytes argv range man flow >>$ fun argv_bytes flow ->
    let n = Z.mul (sizeof_type (T_c_pointer s8) flow) (Z.of_int (nargs + 2)) in
    man.exec (mk_assign argv_bytes (mk_z n range) range) flow >>% fun flow ->

    (* Initialize argv[0] with the name of the program *)
    alloc_concrete_arg 0 range man flow >>$ fun arg flow ->
    eval_bytes arg range man flow >>$ fun arg0_bytes flow ->
    let program_name = "a.out" in
    let min = mk_int (String.length program_name + 1) range in
    let max = mk_z (snd (rangeof (size_type flow) flow)) range in
    man.exec (mk_assume (mk_in arg0_bytes min max range) range) flow >>% fun flow ->
    init_concrete_arg arg program_name range man flow >>% fun flow ->
    let argv0 = mk_c_subscript_access argv (mk_zero range) range in
    man.exec (mk_assign argv0 arg range) flow >>% fun flow ->

    (* Initialize argv[i | 1 <= i < argc] with command-line arguments *)
    let rec iter i flow =
      if i >= nargs + 1
      then Post.return flow
      else
        let str = List.nth args (i-1) in
        alloc_concrete_arg i range man flow >>$ fun arg flow ->
        eval_bytes arg range man flow >>$ fun argi_bytes flow ->
        let min = mk_int (String.length str + 1) range in
        man.exec (mk_assume (mk_in argi_bytes min max range) range) flow >>% fun flow ->
        init_concrete_arg arg str range man flow >>% fun flow ->
        let argvi = mk_c_subscript_access argv (mk_int i range) range in
        man.exec (mk_assign argvi arg range) flow >>% fun flow ->
        iter (i + 1) flow
    in
    iter 1 flow >>% fun flow ->

    (* Put NULL in argv[argc + 1] *)
    let last = mk_c_subscript_access argv argc range in
    man.exec (mk_assign last (mk_c_null range) range) flow >>% fun flow ->

    (* assign main parameters and call the body *)
    let argcv, argvv = match main.c_func_parameters with
      | [v1;v2] -> mk_var v1 range, mk_var v2 range
      | _ -> assert false
    in
    man.exec (mk_add argcv range) flow >>% fun flow ->
    man.exec (mk_assign argcv argc range) flow >>% fun flow ->
    man.exec (mk_add argvv range) flow >>% fun flow ->
    man.exec (mk_assign argvv argv range) flow >>% fun flow ->
    exec_entry_body main man flow


  (** Allocate addresses for symbolic arguments *)
  let alloc_symbolic_args lo hi range man flow =
    (* Allocate concrete args for indices in [0,lo-1] *)
    let rec iter args i flow =
      if i >= lo then Cases.singleton (List.rev args) flow
      else
        alloc_concrete_arg i range man flow >>$ fun arg flow ->
        iter (arg::args) (i+1) flow in
    iter [] 0 flow >>$ fun args flow ->

    (* Allocate a smashed block for the remaining arguments *)
    if lo = hi then
      Cases.singleton (args,None) flow
    else
      let resource = arg_smash_resource lo in
      let arange = tag_range range "%s" resource in
      man.eval (mk_stub_alloc_resource resource ~mode:WEAK arange) flow >>$ fun addr flow ->
      let smashed = { addr with etyp =  T_c_pointer s8 } in
      Cases.singleton (args, Some smashed) flow


  let assume_valid_string arg ivar range man flow =
    let i = mk_var ivar range in
    (* ∃ i∈[0, sizeof(arg)-1]: arg[i] == 0 *)
    let l = mk_zero range in
    eval_bytes arg range man flow >>$ fun bytes flow ->
    let u = mk_pred bytes range in
    man.exec (mk_add i range) flow >>% fun flow ->
    man.exec (mk_assume (mk_in i l u range) range) flow >>% fun flow ->
    let some_arg_cell = mk_c_subscript_access arg i range in
    man.exec (mk_assume (mk_stub_quantified_formula
                           [EXISTS,ivar,S_interval(l,u)]
                           (eq some_arg_cell (mk_zero range) range) range) range) flow >>% fun flow ->
    man.exec (mk_remove i range) flow

  let memset_argv_with_smash argc argv smash l ivar range man flow =
    let l = mk_int l range in
    let u = sub argc (mk_one range) range in
    let i = mk_var ivar range in
    let argvi = mk_c_subscript_access argv i range in
    assume (gt l u range) man flow
      ~fthen:(fun flow -> Post.return flow)
      ~felse:(fun flow ->
          man.exec (mk_add i range) flow >>% fun flow ->
          man.exec (mk_assume (mk_in i l u range) range) flow >>% fun flow ->
          man.exec (mk_assume (mk_stub_quantified_formula
                                 [FORALL,ivar,S_interval(l,u)]
                                 (eq argvi smash range ~etyp:s32) range) range) flow >>% fun flow ->
          man.exec (mk_remove i range) flow
        ) |>
    Post.remove_duplicates man.lattice

  (** Initialize argc and argv with symbolic arguments *)
  let call_main_with_symbolic_args main (lo:int) (hi:int option) man flow =
    (* FIXME: we may generate here false alarms. Since we are sure everything is
       safe, we can remove these alarms. *)
    let report = Flow.get_report flow in
    let range = main.c_func_name_range in

    let argc_var, argv_var = match main.c_func_parameters with
      | [v1;v2] -> v1,v2
      | _ -> assert false
    in

    (* Add the symbolic variable argc representing the number of
       arguments. It should greater than 1 due to the presence of the
       program name. Also, it should not exceed INT_MAX - 1, because
       the argument argv[argc + 1] contains terminating NULL pointer
       (i.e. to avoid integer overflow).
    *)
    let argc = mk_var argc_var range in
    man.exec (mk_add argc range) flow >>% fun flow ->
    let lo' = lo + 1 in
    let hi' =
      match hi with
      | None -> lo'
      | Some hi -> hi + 1 in
    let int_max = rangeof s32 flow |> snd |> Z.to_int in
    if (lo' > hi') || (hi' > int_max - 1) then
      panic "incorrect argc value [%d,%d]" lo' hi'
    ;
    man.exec (mk_assume (mk_in argc (mk_int lo' range) (mk_int hi' range) range) range) flow >>% fun flow ->

    (* Create the memory block pointed by argv. *)
    alloc_argv range man flow >>$ fun argv flow ->
    let argvv = mk_var argv_var range in
    man.exec (mk_add argvv range) flow >>% fun flow ->
    man.exec (mk_assign argvv argv range) flow >>% fun flow ->

    (* Initialize its size to argc + 1 *)
    eval_bytes argv range man flow >>$ fun argv_bytes flow ->
    let n =
      mul (add argc (mk_one range) range)
          (mk_z (sizeof_type (T_c_pointer s8) flow) range)
          range
    in
    man.exec (mk_assume (eq argv_bytes n range) range) flow >>% fun flow ->

    (* Create a symbolic argument *)
    alloc_symbolic_args lo' hi' range man flow >>$ fun (args,smash_arg) flow ->

    (* Initialize the size of arguments *)
    let min_size =
      match !opt_arg_min_size with
      | None -> mk_one range
      | Some s -> mk_int s range in
    let max_size =
      match !opt_arg_max_size with
      | None -> mk_z (rangeof (size_type flow) flow |> snd) range
      | Some s -> mk_int s range in
    let init_size arg flow =
      eval_bytes arg range man flow >>$ fun bytes flow ->
      man.exec (mk_assume (mk_in bytes min_size max_size range) range) flow
    in
    List.fold_left
      (fun acc arg -> acc >>% init_size arg
      ) (Post.return flow) args
    >>% fun flow ->
    (* We need to strongify the address to apply the constraints on all concrete arguments *)
    let strong_smash_arg =
      match smash_arg with
      | None -> None
      | Some ({ekind = E_addr(addr,None)} as e) -> Some { e with ekind = E_addr(addr,Some STRONG)}
      | _ -> assert false
    in
    ( match strong_smash_arg with
      | None     -> Post.return flow
      | Some arg -> init_size arg flow
    ) >>% fun flow ->

    (* Ensure that arguments are valid string with at least one character *)
    (* Create a quantifier variable *)
    (* FIXME: When using the packing domain, relations on this variable can't be
       represented because it's not a local variable of any function (`main`
       hasn't been called yet). So, for the moment, we consider it as a local
       variable of `main`. *)
    let qi = mkv "#i" (V_cvar {
        cvar_scope = Variable_local main;
        cvar_range = range;
        cvar_uid = 0;
        cvar_orig_name = "#i";
        cvar_uniq_name = "#i";
        cvar_before_stmts = [];
        cvar_after_stmts = [];
      }) (size_type flow) in
    List.fold_left
      (fun acc arg -> acc >>% assume_valid_string arg qi range man)
      (Post.return flow) args
    >>% fun flow ->
    ( match strong_smash_arg with
      | None -> Post.return flow
      | Some arg -> assume_valid_string arg qi range man flow
    ) >>% fun flow ->

    (* Initialize argv[0] = program_name *)
    let program_name,other_args = match args with hd::tl -> hd,tl | [] -> assert false in
    let first = mk_c_subscript_access argv zero range in
    man.exec (mk_assign first program_name range) flow >>% fun flow ->

    (* Put the symbolic arguments in argv[1 : argc-1] *)
    let post,n =
      List.fold_left
        (fun (acc,i) arg ->
           let argvi = mk_c_subscript_access argv (mk_int i range) range in
           let acc' = acc >>% man.exec (mk_assign argvi arg range) in
           acc',(i+1)
        ) (Post.return flow,1) other_args
    in
    ( match smash_arg with
      | None -> post
      | Some smash -> post >>% memset_argv_with_smash argc argv smash n qi range man
    ) >>% fun flow ->

    (* Put the terminating NULL pointer in argv[argc] *)
    let last = mk_c_subscript_access argv argc range in
    man.exec (mk_assign last (mk_c_null range) range) flow >>% fun flow ->
    (* Put the initial alarms report *)
    let flow = Flow.set_report report flow in

    exec_entry_body main man flow


  let call_main main args functions man flow =
    (if List.length main.c_func_parameters = 2 then
      match !opt_symbolic_args, args with
      | None, Some args   ->
        call_main_with_concrete_args main args man flow
      | Some(lo,hi), None ->
        if lo = 0 && (hi = Some 0 || hi = None) then
          call_main_with_concrete_args main [] man flow
        else
          call_main_with_symbolic_args main lo hi man flow
      | None, None ->
        let hi = rangeof s16 flow |> snd |> Z.to_int in
        call_main_with_symbolic_args main 0 (Some (hi-2)) man flow
      | Some(lo,hi), Some args  -> panic "-c-symbolic-main-args used with concrete arguments"
    else
      exec_entry_body main man flow) >>% fun flow ->
      exec_exit_functions "exit" main.c_func_name_range man flow

  let exec stmt man flow =
    match skind stmt with
    | S_program  ({ prog_kind = C_program{ c_globals; c_functions; c_stub_directives } }, _) when !ffitest_flag ->
      (* Initialize global variables *)
      init_globals c_globals man flow >>%? fun flow ->
      (* Execute stub directives *)
      exec_stub_directives c_stub_directives man flow >>%? fun flow ->
      (* Execute all runtime functions *)
      exec_runtime_tests c_functions man flow |>
      OptionExt.return

    | S_c_ext_call (f, exprs) ->
      ffitest_missing_funs := StringSet.add (f.c_func_org_name) (!ffitest_missing_funs);
      man.exec ~route:(Below name) stmt flow |> OptionExt.return

    | S_unimplemented_ffi_function f ->
      ffitest_unimplemented_funs := StringSet.add f (!ffitest_unimplemented_funs);
     OptionExt.return (Post.return flow)

    | S_program ({ prog_kind = C_program {c_globals; c_functions; c_stub_directives} }, args)
      when not !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
      init_globals c_globals man flow >>%? fun flow ->

      (* Execute stub directives *)
      exec_stub_directives c_stub_directives man flow >>%? fun flow ->

      (* Find entry function *)
      let entry =
        try find_function !opt_entry_function c_functions
        with Not_found ->
          panic "entry function %s not found" !opt_entry_function
      in

      (* Special processing for main for initializing argc and argv*)
      if !opt_entry_function = "main" then
        call_main entry args c_functions man flow |>
        OptionExt.return
      else
      if List.length entry.c_func_parameters = 0 then
        (* Otherwise execute the body *)
        exec_entry_body entry man flow |>
        OptionExt.return
      else
        panic "entry function %s with arguments not supported" entry.c_func_org_name

    | S_program ({ prog_kind = C_program{ c_globals; c_functions; c_stub_directives } }, _)
      when !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
      init_globals c_globals man flow >>%? fun flow1 ->

      (* Execute stub directives *)
      exec_stub_directives c_stub_directives man flow1 >>%? fun flow1 ->

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
              let stmt = mk_c_call_stmt test [] test.c_func_name_range in
              (name, stmt)
            )
        in
        mk_stmt (Universal.Ast.S_unit_tests tests) (srange stmt)
      in

      let tests = get_test_functions c_functions in
      let stmt = mk_c_unit_tests tests in
      man.exec stmt flow1 |>
      OptionExt.return

    | _ -> None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval_exit name code range man flow =
    man.eval code flow >>$ fun _ flow ->
    exec_exit_functions name range man flow >>% fun flow ->
    Cases.empty flow

  let eval_abort range man flow =
    Cases.empty flow


  let eval exp man flow =
    match ekind exp with
    | E_c_builtin_call("exit", [code]) ->
      eval_exit "exit" code exp.erange man flow |>
      OptionExt.return
    | E_c_builtin_call("quick_exit", [code]) ->
      eval_exit "quick_exit" code exp.erange man flow |>
      OptionExt.return
    | E_c_builtin_call("abort", []) ->
      eval_abort exp.erange man flow |>
      OptionExt.return
    | _ -> None


  (** Handler of queries *)
  (** ================== *)

  let ask : type r. ('a,r) query -> _ man -> _ flow -> ('a, r) cases option = fun query man flow ->
    let get_locals prog call =
      let f = find_function call prog.c_functions in
      f.c_func_local_vars @ f.c_func_parameters in
    let open Framework.Engines.Interactive in
    match query with
    (* Get the list of variables in the current scope *)
    | Q_defined_variables None ->
      let prog = get_c_program flow in
      let cs = Flow.get_callstack flow in
      (* Get global variables *)
      let globals = List.map fst prog.c_globals in
      (* Get local variables of all functions in the callstack, following the same order *)
      let locals = List.fold_right (fun call acc -> (get_locals prog call.Callstack.call_fun_orig_name) @ acc
                     ) cs []
      in
      let all_vars = globals @ locals in
      let records = List.fold_left (fun recmap var ->
                        match vtyp var with
                        | T_c_record r ->
                           StringMap.add r.c_record_unique_name r recmap
                        | _ -> recmap) StringMap.empty all_vars in
      debug "records cheatsheet:@.%a"
        (StringMap.fprint {print_begin="";print_arrow=": "; print_sep="\n"; print_end=""; print_empty=""} Format.pp_print_string
           (fun fmt r ->
             Format.fprintf fmt "%a"
               (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                  (fun fmt rf ->
                    Format.fprintf fmt "%d: %s" rf.c_field_offset rf.c_field_name))
                    r.c_record_fields
             ))
        records;
      Some (Cases.singleton all_vars flow)

    | Q_defined_variables (Some call) ->
      let prog = get_c_program flow in
      Cases.singleton (get_locals prog call) flow
      |> OptionExt.return

    (* Get the value of a variable *)
    | Framework.Engines.Interactive.Query.Q_debug_variable_value var ->
      let open Framework.Engines.Interactive.Query in
      let open Universal.Numeric.Common in
      let module StringSet = SetExt.StringSet in
      let range = mk_fresh_range () in

      (* Get the direct value of an expression *)
      let rec get_value e =
        if is_c_int_type e.etyp then int_value e else
        if is_c_float_type e.etyp then float_value e else
        if is_c_array_type e.etyp then array_value e else
        if is_c_record_type e.etyp then record_value e else
        if is_c_pointer_type e.etyp then pointer_value e
        else assert false

      (* Get the direct value of an integer expression *)
      and int_value e =
        let evl = man.eval e flow ~translate:"Universal" in
        let itv = Cases.fold_result
            (fun acc ee flow ->
              let itv = ask_and_reduce man.ask (mk_int_interval_query ~fast:false ee) flow in
              I.join_bot itv acc
            ) Bot.BOT evl
        in
        match itv with
        | Bot.BOT -> None
        | Bot.Nb (I.B.Finite a, I.B.Finite b) when Z.equal a b -> Some (Z.to_string a)
        | Bot.Nb i -> Some (I.to_string i)

      (* Get the direct value of a float expression *)
      and float_value e =
        let evl = man.eval e flow ~translate:"Universal" in
        let itv = Cases.fold_result
            (fun acc ee flow ->
               let itv = ask_and_reduce man.ask (mk_float_interval_query ee) flow in
               ItvUtils.FloatItvNan.join itv acc
            ) ItvUtils.FloatItvNan.bot evl
        in
        if ItvUtils.FloatItvNan.is_bot itv then None
        else Some ItvUtils.FloatItvNan.(to_string dfl_fmt itv)

      (* Arrays have no direct value *)
      and array_value e = None

      (* Records have no direct value *)
      and record_value e = None

      (* Get the direct value a pointer expression *)
      and pointer_value e =
        let evl = resolve_pointer e man flow in
        let s = Cases.fold_result
            (fun acc pt flow ->
               match pt with
               | P_null -> StringSet.add "NULL" acc
               | P_invalid -> StringSet.add "INVALID" acc
               | P_block (base,offset,_) ->
                 begin match int_value offset with
                   | None -> acc
                   | Some o ->
                     let v = Format.asprintf "&%a + %s" pp_base base o in
                     StringSet.add v acc
                 end
               | P_fun f -> StringSet.add f.c_func_org_name acc
               | P_top -> StringSet.add "⊤" acc
            ) StringSet.empty evl
        in
        match StringSet.elements s with
        | []  -> None
        | [x] -> Some x
        | l   -> Some Format.(asprintf "{%a}"
                                (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")
                                   pp_print_string
                                ) l)
      in

      (* Get the sub-values of an expressions *)
      let rec get_sub_value past v e =
        if is_c_int_type e.etyp then int_sub_value v e else
        if is_c_float_type e.etyp then float_sub_value v e else
        if is_c_array_type e.etyp then array_sub_value past v e else
        if is_c_record_type e.etyp then record_sub_value past v e else
        if is_c_pointer_type e.etyp then pointer_sub_value past v e
        else panic "get_sub_value: unsupported expression %a of type %a" pp_expr e pp_typ e.etyp

      (* Integer expression have no sub-values *)
      and int_sub_value v e = None

      (* Float expression have no sub-values *)
      and float_sub_value v e = None

      (* Get the sub-values of an array *)
      and array_sub_value past v e =
        match e.etyp |> remove_typedef_qual with
        | T_c_array(t,C_array_length_cst n) ->
          let rec aux i =
            if Z.(i = n) then []
            else
              let vv = Format.asprintf "[%a]" Z.pp_print i in
              get_var_value past vv (mk_c_subscript_access e (mk_z i range) range) :: (aux Z.(succ i))
          in
          let l = aux Z.zero in
          Some (Indexed_sub_value l)

        | _ -> None

      (* Get the sub-values of a record *)
      and record_sub_value past v e =
        match e.etyp |> remove_typedef_qual with
        | T_c_record r ->
          let l = List.fold_right
              (fun field acc ->
                 (field.c_field_org_name, get_var_value past field.c_field_org_name (mk_c_member_access e field range)) :: acc
              ) r.c_record_fields []
          in
          Some (Named_sub_value l)

        | _ -> None

      (* Get the sub-values of a pointer *)
      and pointer_sub_value past v e =
        if is_c_pointer_type e.etyp && is_c_void_type (under_pointer_type e.etyp) then None else
        let evl = resolve_pointer e man flow in
        (* Get sub-values of pointers only if they are valid + not already processed *)
        let pts = Cases.fold_result
            (fun acc pt flow ->
               match pt with
               | P_block _ when not (PointsToSet.mem pt past) ->
                 PointsToSet.add pt acc
               | _ -> acc
            ) PointsToSet.empty evl
        in
        if PointsToSet.is_empty pts then
          None
        else
          let vv = "*" ^ v in
          let past' = PointsToSet.union pts past in
          match get_var_value past' vv (mk_c_deref e range) with
          | {var_value = None; var_sub_value = None} -> None
          | x -> Some (Named_sub_value [vv,x])

      (* Get the value of an expression *)
      and get_var_value past v e =
        { var_value = get_value e;
          var_value_type = e.etyp;
          var_sub_value = get_sub_value past v e; }
      in

      (* All together! *)
      let vname = Format.asprintf "%a" pp_var var in
      Some (Cases.singleton (get_var_value PointsToSet.empty vname (mk_var var range)) flow)

    | _ -> None


  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
