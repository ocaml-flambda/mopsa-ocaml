(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(* FIXME FIXME FIXME *)

(** MOPSA Python library. *)

open Framework.Essentials
open Framework.Ast
open Universal.Ast
open Ast
open Addr
open Alarms

let check man cond range flow =
  let flow = man.exec (mk_assert cond range) flow in
  Eval.singleton (mk_py_none range) flow


(*==========================================================================*)
(**                               {2 Domain }                               *)
(*==========================================================================*)


module Domain =
  struct
    type _ domain += D_python_libs_mopsa : unit domain

    let id = D_python_libs_mopsa
    let name = "python.libs.mopsa"
    let identify : type a. a domain -> (unit, a) eq option =
      function
      | D_python_libs_mopsa -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = []; import = []}
    let eval_interface = {export = [Framework.Zone.Z_any, Framework.Zone.Z_any]; import = []}

    (*==========================================================================*)
    (**                       {2 Transfer functions }                           *)
    (*==========================================================================*)
    let exec _ _ _ _ = None

    let init prog man flow = Some flow

    let eval zs exp man flow =
      debug "eval %a@\n" pp_expr exp;
      let range = erange exp in
      match ekind exp with
      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.random_bool")}, _)}, [], []) ->
         Eval.singleton (mk_py_top T_bool range) flow |> OptionExt.return

      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.random_float")}, _)}, [], []) ->
         Eval.singleton (mk_py_top (T_float F_DOUBLE) range) flow |> OptionExt.return

      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.random_string")}, _)}, [], []) ->
         Eval.singleton (mk_py_top T_string range) flow |> OptionExt.return

      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.random_int")}, _)}, [], []) ->
         Eval.singleton (mk_py_top T_int range) flow |> OptionExt.return

      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.random_int")}, _)}, [
                     {ekind = E_constant (C_int l)}; {ekind = E_constant (C_int u)}
                   ], []) ->
         Eval.singleton (mk_py_z_interval l u range) flow |> OptionExt.return

      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.random_int")}, _)}, [l; u], []) ->
         begin
           match ekind l, ekind u with
           | E_constant (C_int l), E_constant (C_int u) -> Eval.singleton (mk_py_z_interval l u range) flow
           | _ ->
              let tmp = mk_tmp () in
              let l = Utils.mk_builtin_call "int" [l] range in
              let u = Utils.mk_builtin_call "int" [u] range in
              let flow = man.exec (mk_assign (mk_var tmp range) (mk_top T_int range) range) flow |>
                           man.exec (mk_assume (mk_in (mk_var tmp range) l u range) range)
              in
              Eval.singleton (mk_var tmp range) ~cleaners:[mk_remove_var tmp range] flow
         end
         |> OptionExt.return

      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.random_float")}, _)}, [l; u], []) ->
         begin
           match ekind l, ekind u with
           | E_constant (C_float l), E_constant (C_float u) -> Eval.singleton (mk_py_float_interval l u range) flow
           | E_constant (C_float l), E_constant (C_int u) -> Eval.singleton (mk_py_float_interval l (Z.to_float u) range) flow
           | E_constant (C_int l), E_constant (C_float u) -> Eval.singleton (mk_py_float_interval (Z.to_float l) u range) flow
           | E_constant (C_int l), E_constant (C_int u) -> Eval.singleton (mk_py_float_interval (Z.to_float l) (Z.to_float u) range) flow
           | _ ->
              let tmp = mk_tmp () in
              let l = Utils.mk_builtin_call "float" [l] range in
              let u = Utils.mk_builtin_call "float" [u] range in
              (* FIXME: T_float *)
              let flow = man.exec (mk_assign (mk_var tmp range) (mk_top (T_float F_DOUBLE) range) range) flow |>
                           man.exec (mk_assume (mk_in (mk_var tmp range) l u range) range)
              in
              Eval.singleton (mk_var tmp range) ~cleaners:[mk_remove_var tmp range] flow
         end
         |> OptionExt.return

      (* Calls to mopsa.assert_equal function *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_equal")}, _)}, [x; y], []) ->
         let range = erange exp in
         check man (mk_binop x O_eq y (tag_range range "eq")) range flow
         |> OptionExt.return

      (* Calls to mopsa assert function *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.massert")}, _)}, [x], []) ->
         let range = erange exp in
         check man x range flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_exists")}, _)}, [cond], [])  ->
         let stmt = {skind = S_simple_assert(cond,false,true); srange = exp.erange} in
         let flow = man.exec stmt flow in
         (* FIXME:  mk_py_int ?*)
         Eval.singleton (mk_py_true exp.erange) flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_safe")}, _)}, [], [])  ->
         begin
           let annot = Flow.get_all_annot flow in
           let error_env = Flow.fold (fun acc tk env ->
                               match tk with
                               | T_alarm {alarm_kind = APyException _} -> man.join annot acc env
                               | _ -> acc
                             ) man.bottom man flow in
           let exception BottomFound in
           try
             debug "cond = %b %b@\n" (Flow.get T_cur man flow |> man.is_bottom) (man.is_bottom error_env);
             let cond =
               match Flow.get T_cur man flow |> man.is_bottom,
                     man.is_bottom error_env with
               | false, true -> mk_py_true
               | true, false -> mk_py_false
               | false, false -> mk_top T_bool
               | true, true -> raise BottomFound
             in
             let stmt = mk_assert (cond exp.erange) exp.erange in
             let cur = Flow.get T_cur man flow in
             let flow = Flow.set T_cur man.top man flow |>
                          man.exec stmt |>
                          Flow.set T_cur cur man
             in
             debug "Flow is now %a@\n" (Flow.print man) flow;
               (* FIXME:  mk_py_int ?*)
             Eval.singleton (mk_py_true exp.erange) flow
             |> OptionExt.return
           with BottomFound ->
             Eval.empty_singleton flow
             |> OptionExt.return
         end

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_unsafe")}, _)}, [], [])  ->
         begin
           Exceptions.panic "Unsure this is correclty implemented; FIXME: move to bools@\n"
           (* let annot = Flow.get_all_annot flow in
            * let error_env = Flow.fold (fun acc tk env -> match tk with
            *                                              | T_alarm {alarm_kind = APyException _} -> man.join annot acc env
            *                                              | _ -> acc
            *                   ) man.bottom man flow in
            * let cond =
            *   match Flow.get T_cur man flow |> man.is_bottom,
            *         man.is_bottom error_env with
            *   | false, true -> mk_zero
            *   | true, false -> mk_one
            *   | false, false -> mk_int_interval 0 1
            *   | true, true -> mk_zero
            * in
            * let stmt = mk_assert (cond exp.erange) exp.erange in
            * let cur = Flow.get T_cur man flow in
            * let flow = Flow.set T_cur man.top man flow in
            * let flow = man.exec stmt flow |>
            *              Flow.filter (fun tk _ -> match tk with T_alarm {alarm_kind = APyException _} -> false | _ -> true) man |>
            *              Flow.set T_cur cur man
            * in
            * (\* FIXME:  mk_py_int ?*\)
            * Eval.singleton (mk_int 0 exp.erange) flow |> OptionExt.return *)
         end
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_exception")}, _)}, [{ekind = cls} as assert_exn], []) ->
         debug "begin assert_exception";
         let annot = Flow.get_all_annot flow in
         let this_error_env, good_exns = Flow.fold (fun (acc_env, acc_good_exn) tk env -> match tk with
                                                                                          | T_alarm {alarm_kind = APyException exn} ->
                                                                                             let flow1 = Flow.bottom annot in
                                                                                             let flow1 = Flow.set T_cur env man flow1 in
                                                                                             let flow2 = man.exec (mk_assume (mk_py_isinstance exn assert_exn range) range) flow1 in
                                                                                             if not @@ Flow.is_cur_bottom man flow2 then
                                                                                               man.join annot acc_env env, exn :: acc_good_exn
                                                                                             else
                                                                                               acc_env, acc_good_exn
                                                                                          | _ -> acc_env, acc_good_exn) (man.bottom, []) man flow
         in
         debug "this_error_env = %a@\n" man.print this_error_env;
         let cond =
           match Flow.get T_cur man flow |> man.is_bottom,
                 man.is_bottom this_error_env with
           | true, false -> mk_py_true
           | _, true -> mk_py_false
           | false, false ->  mk_top T_bool
         in
         let stmt = mk_assert (cond exp.erange) exp.erange in
         let cur = Flow.get T_cur man flow in
         let flow = Flow.set T_cur man.top man flow in
         let flow = man.exec stmt flow |>
                      Flow.filter (fun tk _ -> match tk with T_alarm {alarm_kind = APyException exn} when List.mem exn good_exns -> debug "Foundit@\n"; false | _ -> true) man |>
                      Flow.set T_cur cur man
         in
         (* FIXME:  mk_py_int ?*)
         Eval.singleton (mk_py_false exp.erange) flow
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.ignore_exception")}, _)}, [{ekind = cls} as assert_exn], []) ->
         debug "begin ignore_exception";
         let annot = Flow.get_all_annot flow in
         let none = mk_py_none range in
         let flow = Flow.fold (fun acc tk env -> match tk with
                                                 | T_alarm {alarm_kind = APyException exn} ->
                                                    let flow1 =  Flow.bottom annot |> Flow.set T_cur env man in
                                                    let flow2 = man.exec (mk_assume (mk_py_isinstance exn assert_exn range) range) flow1 in
                                                    if Flow.is_cur_bottom man flow2 then
                                                      Flow.set tk env man acc
                                                    else
                                                      acc
                                                 | _ -> Flow.set tk env man acc) (Flow.bottom annot) man flow in
         man.eval none flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_exception_exists")}, _)}, [{ekind = E_py_object cls}], [])  ->
         Exceptions.panic "todo: fixme"
         (* begin
          *   let annot = Flow.get_all_annot flow in
          *   let error_env = Flow.fold (fun acc tk env -> match tk with
          *                                                     (\* FIXME: addr.isinstance *\)
          *                                                | T_alarm {alarm_kind = APyException exn} (\*when Addr.isinstance exn cls*\) -> Exceptions.panic "todo"; man.join annot acc env
          *                       | _ -> acc
          *                     ) man.bottom man flow in
          *   let cur = Flow.get T_cur man flow in
          *   let cur' = if man.is_bottom cur then man.top else cur in
          *   let cond = if man.is_bottom error_env then mk_zero exp.erange else mk_one exp.erange in
          *   let stmt = mk_assert cond exp.erange in
          *   let flow' = Flow.set T_cur cur' man flow |>
          *                 man.exec stmt |>
          *                                                     (\* FIXME: addr.isinstance *\)
          *                 Flow.filter (fun tk _ -> match tk with T_alarm {alarm_kind = APyException exn} (\*when Addr.isinstance exn cls*\) -> Exceptions.panic "todo"; false | _ -> true) man |>
          *                 Flow.set T_cur cur man
          *   in
          *   (\* FIXME:  mk_py_int ?*\)
          *   Eval.singleton (mk_py_true exp.erange) flow'
          *   |> OptionExt.return
          * end *)
      | _ ->
         None

    let ask _ _ _ = None
  end



(*==========================================================================*)
(**                          {2 Decorators}                                 *)
(*==========================================================================*)


let is_stub_fundec = function
  | {py_func_decors = [{ekind = E_py_attribute({ekind = E_var( {vname = "mopsa"}, _)}, "stub")}]} -> true
  | _ -> false

let is_builtin_fundec = function
  | {py_func_decors = [{ekind = E_py_call({ekind = E_py_attribute({ekind = E_var( {vname = "mopsa"}, _)}, "builtin")}, _, [])}]} -> true
  | _ -> false

let is_builtin_clsdec = function
  | {py_cls_decors = [{ekind = E_py_call({ekind = E_py_attribute({ekind = E_var( {vname = "mopsa"}, _)}, "builtin")}, _, [])}]} -> true
  | _ -> false

let is_unsupported_fundec = function
  | {py_func_decors = [{ekind = E_py_attribute({ekind = E_var( {vname = "mopsa"}, _)}, "unsupported")}]} -> true
  | _ -> false

let is_unsupported_clsdec = function
  | {py_cls_decors = [{ekind = E_py_attribute({ekind = E_var( {vname = "mopsa"}, _)}, "unsupported")}]} -> true
  | _ -> false


let builtin_fundec_name = function
  | {py_func_decors = [{ekind = E_py_call({ekind = E_py_attribute({ekind = E_var( {vname = "mopsa"}, _)}, "builtin")}, [{ekind = E_constant (C_string name)}], [])}]} -> name
  | _ -> raise Not_found

let builtin_clsdec_name = function
  | {py_cls_decors = [{ekind = E_py_call({ekind = E_py_attribute({ekind = E_var( {vname = "mopsa"}, _)}, "builtin")}, [{ekind = E_constant (C_string name)}], [])}]} -> name
  | _ -> raise Not_found


(*==========================================================================*)
(**                             {2 Setup }                                  *)
(*==========================================================================*)


let () =
  Framework.Domains.Stateless.register_domain (module Domain)
