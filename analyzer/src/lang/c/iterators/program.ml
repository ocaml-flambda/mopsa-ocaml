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
open Common.Points_to
open Common.Base


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
  let opt_symbolic_args = ref false

  let () =
    register_domain_option name {
      key = "-c-symbolic-args";
      category = "C";
      doc = " call main with symbolic argc and argv";
      spec = ArgExt.Set opt_symbolic_args;
      default = "false";
    }



  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {
      provides= [Z_c];
      uses = [Z_c;Z_c_scalar]
    };

    ieval = {
      provides = [];
      uses = [Z_c,Z_c_scalar;
              Z_c,Z_u_num;
              Z_c,Z_c_points_to]
    }
  }

  let alarms = []

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



  (** Create the address of the array pointed by argv *)
  let mk_c_argv range =
    mk_addr
      {
        addr_kind = Stubs.Ast.A_stub_resource "argv";
        addr_partitioning = G_all;
        addr_mode = STRONG;
      }
      ~etyp:(T_c_pointer (T_c_pointer s8))
      range


  (** Create the address of an argument string *)
  let mk_c_arg ~mode range =
    mk_addr
      {
        addr_kind = Stubs.Ast.A_stub_resource "arg";
        addr_partitioning = G_all;
        addr_mode = mode;
      }
      ~etyp:(T_c_pointer s8)
      range

  let exec_entry_body f man flow =
    match f.c_func_body with
    | None -> panic "entry function %s is not defined" f.c_func_org_name
    | Some stmt ->
      let f' = { f with c_func_parameters = [] } in
      let stmt = mk_c_call_stmt f' [] f.c_func_range in
      man.post stmt flow


  (** Initialize argc and argv with concrete values and execute the body of main *)
  let call_main_with_concrete_args main args man flow =
    let range = main.c_func_name_range in

    (* argc is set to |args| + 1 *)
    let nargs = List.length args in
    let argc = mk_int (nargs + 1) range in

    (* Create the memory block pointed by argv. *)
    let argv = mk_c_argv range in

    (* Initialize its size to |args| + 2 *)
    man.eval ~zone:(Z_c,Z_c_scalar) (mk_expr (Stubs.Ast.E_stub_builtin_call (BYTES, argv)) range ~etyp:ul) flow >>$ fun bytes flow ->
    let flow = man.exec ~zone:Z_c_scalar (mk_add bytes range) flow |>
               man.exec ~zone:Z_c_scalar (mk_assign bytes (mk_z
                                                             (Z.mul
                                                                (sizeof_type (T_c_pointer s8))
                                                                (Z.of_int (nargs + 2))
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
      if i >= nargs + 1
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
    let flow = man.exec (mk_assign last (mk_c_null range) range) flow in

    (* assign main parameters and call the body *)
    let argcv, argvv = match main.c_func_parameters with
      | [v1;v2] -> mk_var v1 range, mk_var v2 range
      | _ -> assert false
    in
    man.exec (mk_assign argcv argc range) flow |>
    man.exec (mk_assign argvv argv range) |>
    exec_entry_body main man




  (** Initialize argc and argv with symbolic arguments *)
  let call_main_with_symbolic_args main functions man flow =
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
    let flow = man.exec (mk_add argc range) flow |>
               man.exec (mk_assign argc (mk_z_interval Z.one (rangeof s32 |> snd |> Z.pred) range) range)
    in


    (* Create the memory block pointed by argv. *)
    let argv = mk_c_argv range in
    let argvv = mk_var argv_var range in
    let flow = man.exec (mk_add argvv range) flow |>
               man.exec (mk_assign argvv argv range)
    in

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

    (* Create a symbolic argument *)
    let arg = mk_c_arg ~mode:STRONG range in

    (* Initialize the size of the argument *)
    man.eval ~zone:(Z_c,Z_c_scalar) (mk_expr (Stubs.Ast.E_stub_builtin_call (BYTES, arg)) range ~etyp:ul) flow >>$ fun bytes flow ->
    let flow = man.exec ~zone:Z_c_scalar (mk_add bytes range) flow |>
               man.exec ~zone:Z_c_scalar (mk_assign bytes (mk_z (rangeof s32 |> snd) range) range) |>
               man.exec (mk_add arg range)
    in

    (* Ensure that the argument is a valid string with at least one character *)
    let first_arg_cell = mk_c_subscript_access arg (mk_zero range) range in
    let flow = man.exec (mk_assign first_arg_cell (mk_z_interval Z.one (rangeof s8 |> snd) range) range) flow in
    let some_arg_cell = mk_c_subscript_access arg (mk_z_interval Z.one (rangeof s32 |> snd |> Z.pred) range) range in
    let flow = man.exec (mk_assign some_arg_cell (mk_zero range) range) flow in

    (* Make the address weak *)
    let arg_weak = mk_c_arg ~mode:WEAK range in
    let flow = man.exec (mk_rename arg arg_weak range) flow in

    (* Put the symbolic argument in argv[0 : argc-1] *)
    let i = mktmp ~typ:s32 () in
    let ii = mk_var i range in
    let l = mk_zero range in
    let u = sub argc (mk_one range) range in
    let flow = man.exec (mk_add ii range) flow |>
               man.exec (mk_assign ii (mk_c_builtin_call "_mopsa_range_s32" [l;u] s32 range) range)
    in
    let every_argv_cell = mk_c_subscript_access argv (mk_stub_quantified FORALL i (S_interval(l,u)) range) range in
    let flow = man.exec (mk_assume (mk_binop every_argv_cell O_eq arg_weak ~etyp:u8 range) range) flow |>
               man.exec (mk_remove ii range)
    in

    (* Put the terminating NULL pointer in argv[argc] *)
    let last = mk_c_subscript_access argv argc range in
    let flow = man.exec (mk_assign last (mk_c_null range) range) flow in

    exec_entry_body main man flow



  let call_main main args functions man flow =
    if List.length main.c_func_parameters = 2 then
      match !opt_symbolic_args, args with
      | false, None      -> call_main_with_concrete_args main [] man flow
      | false, Some args -> call_main_with_concrete_args main args man flow
      | true, None       -> call_main_with_symbolic_args main functions man flow
      | true, Some args  -> panic "-c-symbolic-main-args used with concrete arguments"
    else
      exec_entry_body main man flow


  let exec zone stmt man flow =
    match skind stmt with
    | S_program ({ prog_kind = C_program {c_globals; c_functions; c_stub_directives} }, args)
      when not !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
      let flow = init_globals c_globals (srange stmt) man flow in

      (* Execute stub directives *)
      let flow = exec_stub_directives c_stub_directives (srange stmt) man flow in

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
              let stmt = mk_c_call_stmt test [] test.c_func_name_range in
              (name, stmt)
            )
        in
        mk_stmt (Universal.Ast.S_unit_tests tests) (srange stmt)
      in

      let tests = get_test_functions c_functions in
      let stmt = mk_c_unit_tests tests in
      man.exec stmt flow1 |>
      Post.return |>
      OptionExt.return

    | _ -> None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow = None


  (** Handler of queries *)
  (** ================== *)

  let ask : type r. r query -> _ man -> _ flow -> r option = fun query man flow ->
    let open Framework.Engines.Interactive in
    match query with
    (* Get the list of variables in the current scope *)
    | Q_debug_variables ->
      let prog = get_c_program flow in
      let cs = Flow.get_callstack flow in
      (* Get global variables *)
      let globals = List.map fst prog.c_globals in
      (* Get local variables of all functions in the callstack *)
      let locals = List.fold_left (fun acc call ->
          let f = find_function call.Callstack.call_fun_orig_name prog.c_functions in
          f.c_func_local_vars @ f.c_func_parameters @ acc
        ) [] cs
      in
      Some (globals @ locals)
      

    (* Get the value of a variable *)
    | Q_debug_variable_value var ->
      let open Universal.Numeric.Common in
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
      and int_value ?(zone=Z_c) e =
        let evl = man.eval e ~zone:(zone,Z_u_num) flow in
        let itv = Cases.fold_some
            (fun ee flow acc ->
              let itv = man.ask (mk_int_interval_query ~fast:false ee) flow in
              I.join_bot itv acc
            ) evl Bot.BOT
        in
        match itv with
        | Bot.BOT -> None
        | Bot.Nb (I.B.Finite a, I.B.Finite b) when Z.equal a b -> Some (Z.to_string a)
        | Bot.Nb i -> Some (I.to_string i)

      (* Get the direct value of a float expression *)
      and float_value e =
        let evl = man.eval e ~zone:(Z_c,Z_u_num) flow in
        let itv = Cases.fold_some
            (fun ee flow acc ->
               let itv = man.ask (mk_float_interval_query ee) flow in
               ItvUtils.FloatItvNan.join itv acc
            ) evl ItvUtils.FloatItvNan.bot
        in
        if ItvUtils.FloatItvNan.is_bot itv then None
        else Some ItvUtils.FloatItvNan.(to_string dfl_fmt itv)

      (* Arrays have no direct value *)
      and array_value e = None

      (* Records have no direct value *)
      and record_value e = None

      (* Get the direct value a pointer expression *)
      and pointer_value e =
        let evl = man.eval e ~zone:(Z_c,Z_c_points_to) flow in
        let l = Cases.fold_some
            (fun pt flow acc ->
               match ekind pt with
               | E_c_points_to P_null -> "NULL" :: acc
               | E_c_points_to P_invalid -> "INVALID" :: acc
               | E_c_points_to P_block (base,offset,_) ->
                 begin match int_value ~zone:Z_c_scalar offset with
                   | None -> acc
                   | Some o ->
                     let v = Format.asprintf "&%a + %s" pp_base base o in
                     v :: acc
                 end
               | E_c_points_to P_fun f -> f.c_func_org_name :: acc
               | E_c_points_to P_top -> "⊤" :: acc
               | _ -> assert false
            ) evl []
        in
        match l with
        | [x] -> Some x
        | []  -> None
        | _   -> Some Format.(asprintf "{%a}"
                                (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")
                                   pp_print_string
                                ) l)

      (* Get the sub-values of an expressions *)
      and get_sub_value v e =
        if is_c_int_type e.etyp then int_sub_value v e else
        if is_c_float_type e.etyp then float_sub_value v e else
        if is_c_array_type e.etyp then array_sub_value v e else
        if is_c_record_type e.etyp then record_sub_value v e else
        if is_c_pointer_type e.etyp then pointer_sub_value v e
        else panic "get_sub_value: unsupported expression %a of type %a" pp_expr e pp_typ e.etyp

      (* Integer expression have no sub-values *)
      and int_sub_value v e = None

      (* Float expression have no sub-values *)
      and float_sub_value v e = None

      (* Get the sub-values of an array *)
      and array_sub_value v e =
        match e.etyp |> remove_typedef_qual with
        | T_c_array(t,C_array_length_cst n) ->
          let rec aux i =
            if Z.(i = n) then []
            else
              let vv = Format.asprintf "[%a]" Z.pp_print i in
              get_var_value vv (mk_c_subscript_access e (mk_z i range) range) :: (aux Z.(succ i))
          in
          let l = aux Z.zero in
          Some (Indexed_sub_value l)

        | _ -> None

      (* Get the sub-values of a record *)
      and record_sub_value v e =
        match e.etyp |> remove_typedef_qual with
        | T_c_record r ->
          let l = List.fold_right
              (fun field acc ->
                 (field.c_field_org_name, get_var_value field.c_field_org_name (mk_c_member_access e field range)) :: acc
              ) r.c_record_fields []
          in
          Some (Named_sub_value l)

        | _ -> None

      (* Get the sub-values of a pointer *)
      and pointer_sub_value v e =
        let evl = man.eval e ~zone:(Z_c,Z_c_points_to) flow in
        let l = Cases.fold_some
            (fun pt flow acc ->
               match ekind pt with
               | E_c_points_to P_block (base,offset,_) ->
                 let vv = "*" ^ v in
                 begin match get_var_value vv (mk_c_deref e range) with
                   | {var_value = None; var_sub_value = None} -> acc
                   | x -> [Named_sub_value [vv,x]]
                 end
               | _ -> acc
            ) evl []
        in
        match l with
        | [] -> None
        | [v] -> Some v
        | _ -> assert false

      (* Get the value of an expression *)
      and get_var_value v e =
        { var_value = get_value e;
          var_value_type = e.etyp;
          var_sub_value = get_sub_value v e; }
      in

      (* All together! *)
      let vname = Format.asprintf "%a" pp_var var in
      Some (get_var_value vname (mk_var var range))

    | _ -> None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
