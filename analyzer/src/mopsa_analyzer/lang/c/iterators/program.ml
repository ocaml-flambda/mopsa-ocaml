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
open Common.Points_to
open Common.Base
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
      spec = ArgExt.Set_string opt_entry_function;
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
      spec = ArgExt.String (fun s -> opt_symbolic_args := Some (parse_symbolic_args_spec s));
      default = "";
    }


  let checks = []


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow =
    match prog.prog_kind with
    | C_program p -> set_c_program p flow
    | _ -> flow


  (** Computation of post-conditions *)
  (** ============================== *)


  (** Initialize global variables *)
  let init_globals globals range man flow =
    globals |>
    List.fold_left (fun acc (v, init) ->
        let cvar =
          match v.vkind with
          | V_cvar cvar -> cvar
          | _ -> assert false
        in
        if cvar.cvar_scope = Variable_extern then acc
        else
          let stmt = mk_c_declaration v init cvar.cvar_scope cvar.cvar_range in
          acc >>% man.exec stmt
      ) (Post.return flow)


  (** Execute stub directives *)
  let exec_stub_directives directives range man flow =
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
             man.ask (mk_int_interval_query nb) flow |>
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
      let stmt = mk_c_call_stmt f' [] f.c_func_range in
      man.exec stmt flow


  (** Allocate the argv array *)
  let alloc_argv range man flow =
    let arange = tag_range range "argv" in
    man.eval (mk_stub_alloc_resource "argv" arange) flow >>$ fun addr flow ->
    Eval.singleton { addr with etyp = T_c_pointer (T_c_pointer s8) } flow


  (** Allocate an address for a concrete argument *)
  let alloc_concrete_arg i range man flow =
    let irange = tag_range range "argv[%d]" i in
    man.eval (mk_stub_alloc_resource "arg" irange) flow >>$ fun addr flow ->
    Eval.singleton { addr with etyp =  T_c_pointer s8 } flow


  (** Set the minimal size of the argument block *)
  let set_arg_min_size arg min range man flow =
    let max = snd (rangeof ul) in
    man.exec (mk_assume (mk_in (mk_stub_builtin_call BYTES [arg] ~etyp:ul range) min (mk_z max range ) range) range) flow


  (** Initialize an argument with a concrete string *)
  let init_concrete_arg arg str range man flow =
    let n = String.length str in
    let rec aux i flow =
      let argi = mk_c_subscript_access arg (mk_int i range) range in
      if i = n then man.exec (mk_assign argi zero range) flow
      else
        man.exec (mk_assign argi (mk_c_character (String.get str i) range) range) flow >>% fun flow ->
        aux (i + 1) flow
    in
    aux 0 flow


  (** Initialize argc and argv with concrete values and execute the body of main *)
  let call_main_with_concrete_args main args man flow =
    let range = main.c_func_name_range in

    (* argc is set to |args| + 1 *)
    let nargs = List.length args in
    let argc = mk_int (nargs + 1) range in

    (* Create the memory block pointed by argv. *)
    alloc_argv range man flow >>$ fun argv flow ->
    man.exec (mk_add argv range) flow >>% fun flow ->

    (* Initialize its size to (|args| + 2)*sizeof(ptr) *)
    let bytes = mk_stub_builtin_call BYTES [argv] ~etyp:size_type range in
    let n = Z.mul (sizeof_type (T_c_pointer s8)) (Z.of_int (nargs + 2)) in
    man.exec (mk_assign bytes (mk_z n range) range) flow >>% fun flow ->

    (* Initialize argv[0] with the name of the program *)
    alloc_concrete_arg 0 range man flow >>$ fun arg flow ->
    let program_name = "a.out" in
    set_arg_min_size arg (mk_int (String.length program_name + 1) range) range man flow >>% fun flow ->
    man.exec (mk_add arg range) flow >>% fun flow ->
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
        set_arg_min_size arg (mk_int (String.length str + 1) range) range man flow >>% fun flow ->
        man.exec (mk_add arg range) flow >>% fun flow ->
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
    man.exec (mk_assign argcv argc range) flow >>% fun flow ->
    man.exec (mk_assign argvv argv range) flow >>% fun flow ->
    exec_entry_body main man flow


  (** Allocate addresses for symbolic arguments *)
  let alloc_symbolic_args lo hi range man flow =
    (* Allocate concrete args for indices in [1,lo] *)
    let rec iter args i flow =
      if i > lo then Cases.singleton (List.rev args) flow
      else
        alloc_concrete_arg i range man flow >>$ fun arg flow ->
        iter (arg::args) (i+1) flow in
    iter [] 0 flow >>$ fun args flow ->

    (* Allocate a smashed block for the remaining arguments *)
    if lo = hi then
      Cases.singleton (args,None) flow
    else
      let arange = tag_range range "argv[%d-%d]" (lo+1) hi in
      man.eval (mk_stub_alloc_resource "arg" ~mode:WEAK arange) flow >>$ fun addr flow ->
      let smashed = { addr with etyp =  T_c_pointer s8 } in
      Cases.singleton (args, Some smashed) flow


  (** Initialize argc and argv with symbolic arguments *)
  let call_main_with_symbolic_args main (lo:int) (hi:int option) functions man flow =
    (* FIXME: functions call_main_* may generate false alarms. Since
       we are sure they are safe, we can remove these alarms *)
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
    let int_max = rangeof s32 |> snd |> Z.to_int in
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
    let bytes = mk_expr (Stubs.Ast.E_stub_builtin_call (BYTES, [argv])) range ~etyp:size_type in
    let n =
      mul (add argc (mk_one range) range)
          (mk_z (sizeof_type (T_c_pointer s8)) range)
          range
    in
    man.exec (mk_assume (eq bytes n range) range) flow >>% fun flow ->

    (* Create a symbolic argument *)
    alloc_symbolic_args lo' hi' range man flow >>$ fun (args,smash_arg) flow ->

    (* Initialize the size of arguments *)
    (* FIXME The size is assumed to be a constant to avoid false out-of-bound alarms! *)
    let max_int = rangeof s32 |> snd in
    let emax_int = mk_z max_int range in
    let init_size arg n flow =
      let bytes = mk_expr (E_stub_builtin_call (BYTES, [arg])) range ~etyp:size_type in
      man.exec (mk_assume (eq bytes n range) range) flow
    in
    List.fold_left
      (fun acc arg -> acc >>% init_size arg emax_int
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
      | Some arg -> init_size arg emax_int flow
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
      }) s32 in
    let qii = mk_var qi range in
    let assume_valid_string arg flow =
      (* arg[0] = [1,255]; *)
      let first_arg_cell = mk_c_subscript_access arg (mk_zero range) range in
      let not_zero = mk_z_interval Z.one (rangeof s8 |> snd) range in
      man.exec (mk_assign first_arg_cell not_zero range) flow >>% fun flow ->
      (* ∃ i∈[1, sizeof(arg)-1]: arg[i] == 0 *)
      let l = mk_one range in
      let u = mk_z (Z.pred max_int) range in
      man.exec (mk_add qii range) flow >>% fun flow ->
      man.exec (mk_assume (mk_in qii l u range) range) flow >>% fun flow ->
      let some_arg_cell = mk_c_subscript_access arg qii range in
      man.exec (mk_assume (mk_stub_quantified_formula
                             [EXISTS,qi,S_interval(l,u)]
                             (eq some_arg_cell (mk_zero range) range) range) range) flow >>% fun flow ->
      man.exec (mk_remove_var qi range) flow
    in
    List.fold_left
      (fun acc arg -> acc >>% assume_valid_string arg)
      (Post.return flow) args
    >>% fun flow ->
    ( match strong_smash_arg with
      | None -> Post.return flow
      | Some arg -> assume_valid_string arg flow
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
      | Some arg ->
        let l = mk_int n range in
        let u = sub argc (mk_one range) range in
        let argvi = mk_c_subscript_access argv qii range in
        post >>%
        man.exec (mk_add qii range) >>% fun flow ->
        man.exec (mk_assume (mk_in qii l u range) range) flow >>% fun flow ->
        man.exec (mk_assume (mk_stub_quantified_formula
                               [FORALL,qi,S_interval(l,u)]
                               (eq argvi arg range ~etyp:s32) range) range) flow >>% fun flow ->
        man.exec (mk_remove qii range) flow
    ) >>% fun flow ->

    (* Put the terminating NULL pointer in argv[argc] *)
    let last = mk_c_subscript_access argv argc range in
    man.exec (mk_assign last (mk_c_null range) range) flow >>% fun flow ->

    (* Put the initial alarms report *)
    let flow = Flow.set_report report flow in

    exec_entry_body main man flow



  let call_main main args functions man flow =
    if List.length main.c_func_parameters = 2 then
      match !opt_symbolic_args, args with
      | None, None      -> call_main_with_concrete_args main [] man flow
      | None, Some args -> call_main_with_concrete_args main args man flow
      | Some(lo,hi), None       -> call_main_with_symbolic_args main lo hi functions man flow
      | Some(lo,hi), Some args  -> panic "-c-symbolic-main-args used with concrete arguments"
    else
      exec_entry_body main man flow >>% fun flow ->
      exec_exit_functions "exit" main.c_func_name_range man flow


  let exec stmt man flow =
    match skind stmt with
    | S_program ({ prog_kind = C_program {c_globals; c_functions; c_stub_directives} }, args)
      when not !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
      init_globals c_globals (srange stmt) man flow >>%? fun flow ->

      (* Execute stub directives *)
      exec_stub_directives c_stub_directives (srange stmt) man flow >>%? fun flow ->

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
      init_globals c_globals (srange stmt) man flow >>%? fun flow1 ->

      (* Execute stub directives *)
      exec_stub_directives c_stub_directives (srange stmt) man flow1 >>%? fun flow1 ->

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
    Eval.singleton (mk_unit range) flow

  let eval exp man flow =
    match ekind exp with
    | E_c_builtin_call("exit", [code]) ->
      eval_exit "exit" code exp.erange man flow |>
      OptionExt.return
    | E_c_builtin_call("quick_exit", [code]) ->
      eval_exit "quick_exit" code exp.erange man flow |>
      OptionExt.return

    | _ -> None


  (** Handler of queries *)
  (** ================== *)

  let ask : type r. ('a,r) query -> _ man -> _ flow -> r option = fun query man flow ->
    let open Framework.Engines.Interactive in
    match query with
    (* Get the list of variables in the current scope *)
    | Q_debug_variables ->
      let prog = get_c_program flow in
      let cs = Flow.get_callstack flow in
      (* Get global variables *)
      let globals = List.map fst prog.c_globals in
      (* Get local variables of all functions in the callstack, following the same order *)
      let locals = List.fold_right (fun call acc ->
          let f = find_function call.Callstack.call_fun_orig_name prog.c_functions in
          f.c_func_local_vars @ f.c_func_parameters @ acc
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
      Some all_vars


    (* Get the value of a variable *)
    | Q_debug_variable_value var ->
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
              let itv = man.ask (mk_int_interval_query ~fast:false ee) flow in
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
               let itv = man.ask (mk_float_interval_query ee) flow in
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
      Some (get_var_value PointsToSet.empty vname (mk_var var range))

    | _ -> None


  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
