(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** MOPSA Python library. *)

open Framework.Domains.Stateless
open Framework.Essentials
open Universal.Ast
open Ast
open Addr

let name = "python.libs.mopsa"
let debug fmt = Debug.debug ~channel:name fmt


let check cond range man ctx flow =
  let flow = man.exec (mk_stmt (Universal.Ast.S_assert cond) range) ctx flow in
  Eval.return (Some (mk_py_none range)) flow


(*==========================================================================*)
(**                               {2 Domain }                               *)
(*==========================================================================*)


module Domain =
struct


  (*==========================================================================*)
  (**                       {2 Transfer functions }                           *)
  (*==========================================================================*)


  let init prog man ctx flow = None

  let import_exec = [Zone.Z_py]
  let export_exec = []

  let exec zone stmt man ctx flow = None

  let import_eval = [Zone.Z_py, Zone.Z_py_object]
  let export_eval = [Zone.Z_py, Zone.Z_py_object]

  let eval zpath exp man ctx flow =
    let range = erange exp in
    match ekind exp with
    | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.random_int")}, _)}, [], []) ->
      Eval.singleton (Some (mk_py_top T_int range)) flow |>
      return

    | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.random_int")}, _)}, [l; u], []) ->
      bind_eval (Zone.Z_py, Zone.Z_py_object) l man ctx flow @@ fun l flow ->
      bind_eval (Zone.Z_py, Zone.Z_py_object) u man ctx flow @@ fun u flow ->
      let lv = object_of_expr l |> value_of_object in
      let uv = object_of_expr u |> value_of_object in
      begin match ekind lv, ekind uv with
        | E_constant (C_int l), E_constant (C_int u) -> Eval.singleton (Some (mk_py_z_interval l u range)) flow |>
                                                        return
        | _ ->
          let tmp = mktmp () in
          let l = Utils.mk_builtin_call "int" [l] range in
          let u = Utils.mk_builtin_call "int" [u] range in
          let flow = man.exec ~zone:Zone.Z_py (mk_assign (mk_var tmp range) (mk_py_top T_int range) range) ctx flow |>
                     man.exec ~zone:Zone.Z_py (mk_assume (mk_in (mk_var tmp range) l u range) range) ctx
          in
          man.eval ~zpath:(Zone.Z_py, Zone.Z_py_object) (mk_var tmp range) ctx flow |>
          Eval.add_cleaners [mk_remove_var tmp range] |>
          return
      end

    | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.random_float")}, _)}, [l; u], []) ->
      bind_eval (Zone.Z_py, Zone.Z_py_object) l man ctx flow @@ fun l flow ->
      bind_eval (Zone.Z_py, Zone.Z_py_object) u man ctx flow @@ fun u flow ->
      let lv = object_of_expr l |> value_of_object in
      let uv = object_of_expr u |> value_of_object in
      begin match ekind lv, ekind uv with
        | E_constant (C_float l), E_constant (C_float u) -> Eval.return (Some (mk_py_float_interval l u range)) flow
        | E_constant (C_float l), E_constant (C_int u) -> Eval.return (Some (mk_py_float_interval l (Z.to_float u) range)) flow
        | E_constant (C_int l), E_constant (C_float u) -> Eval.return (Some (mk_py_float_interval (Z.to_float l) u range)) flow
        | E_constant (C_int l), E_constant (C_int u) -> Eval.return (Some (mk_py_float_interval (Z.to_float l) (Z.to_float u) range)) flow
        | _ ->
          let tmp = mktmp () in
          let l = Utils.mk_builtin_call "float" [l] range in
          let u = Utils.mk_builtin_call "float" [u] range in
          let flow = man.exec ~zone:Zone.Z_py (mk_assign (mk_var tmp range) (mk_py_top T_float range) range) ctx flow |>
                     man.exec ~zone:Zone.Z_py (mk_assume (mk_in (mk_var tmp range) l u range) range) ctx
          in
          man.eval ~zpath:(Zone.Z_py, Zone.Z_py_object) (mk_var tmp range) ctx flow |>
          Eval.add_cleaners [mk_remove_var tmp range] |>
          return
      end

    | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.random_float")}, _)}, [], []) ->
      Eval.return (Some (mk_py_top T_float range)) flow

    | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.random_bool")}, _)}, [], []) ->
      Eval.return (Some (mk_py_top T_py_bool range)) flow

    | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.random_string")}, _)}, [], []) ->
      Eval.return (Some (mk_py_top T_string range)) flow

    (* Calls to mopsa.assert_equal function *)
    | E_py_call(
        {ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_equal")}, _)},
        [x; y], []
      ) ->
      let range = erange exp in
      check (mk_binop x O_eq y range) range man ctx flow

    (* Calls to mopsa.assert_true function *)
    | E_py_call(
        {ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_true")}, _)},
        [x], []
      ) ->
      let range = erange exp in
      check x range man ctx flow

    (* Calls to mopsa.assert_false function *)
    | E_py_call(
        {ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_false")}, _)},
        [x], []
      )  ->
      let range = erange exp in
      check (mk_not x range) range man ctx flow

    | E_py_call(
        {ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_exists")}, _)},
        [cond], []
      )  ->
      let stmt = {skind = S_simple_assert(cond,false,true); srange = exp.erange} in
      let flow = man.exec stmt ctx flow in
      Eval.return (Some (mk_py_none exp.erange)) flow

    | E_py_call(
        {ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_safe")}, _)},
        [], []
      )  ->
      begin
        let error_env = man.flow.fold (fun tk env acc ->
            match tk with
            | Flows.Exceptions.TExn _ -> man.env.join acc env
            | _ -> acc
          ) flow man.env.bottom in
        let exception BottomFound in
        try
          let cond =
            match is_cur_bottom man flow, man.env.is_bottom error_env with
            | false, true -> mk_one
            | true, false -> mk_zero
            | false, false -> mk_int_interval 0 1
            | true, true -> raise BottomFound
          in
          let stmt = mk_assert (cond exp.erange) exp.erange in
          let cur = man.flow.get Flow.TCur flow in
          let flow = man.flow.set Flow.TCur man.env.top flow |>
                     man.exec stmt ctx |>
                     man.flow.set Flow.TCur cur
          in
          Eval.return (Some (mk_py_none exp.erange)) flow
        with BottomFound ->
          Eval.return None flow
      end

    | E_py_call(
        {ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_unsafe")}, _)},
        [], []
      )  ->
      begin
        let error_env = man.flow.fold (fun tk env acc ->
            match tk with
            | Flows.Exceptions.TExn _ -> man.env.join acc env
            | _ -> acc
          ) flow man.env.bottom in
        let cond =
          match is_cur_bottom man flow, man.env.is_bottom error_env with
          | false, true -> mk_zero
          | true, false -> mk_one
          | false, false -> mk_int_interval 0 1
          | true, true -> mk_zero
        in
        let stmt = mk_assert (cond exp.erange) exp.erange in
        let cur = man.flow.get Flow.TCur flow in
        let flow = man.flow.set Flow.TCur man.env.top flow in
        let flow = man.exec stmt ctx flow |>
                   man.flow.filter (fun tk _ -> match tk with Flows.Exceptions.TExn _ -> false | _ -> true) |>
                   man.flow.set Flow.TCur cur
        in
        Eval.return (Some (mk_py_none exp.erange)) flow
      end

    | E_py_call(
        {ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_exception")}, _)},
        [{ekind = E_py_object cls}], []
      )  ->
      begin
        debug "begin assert_exception";
        let this_error_env = man.flow.fold (fun tk env acc ->
            match tk with
            | Flows.Exceptions.TExn exn when Addr.isinstance exn cls -> man.env.join acc env
            | _ -> acc
          ) flow man.env.bottom in
        let cond =
          match is_cur_bottom man flow, man.env.is_bottom this_error_env with
          | true, false -> mk_one
          | _, true -> mk_zero
          | false, false ->  mk_int_interval 0 1
        in
        let stmt = mk_assert (cond exp.erange) exp.erange in
        let cur = man.flow.get Flow.TCur flow in
        let flow = man.flow.set Flow.TCur man.env.top flow in
        let flow = man.exec stmt ctx flow |>
                   man.flow.filter (fun tk _ -> match tk with Flows.Exceptions.TExn exn when Addr.isinstance exn cls -> false | _ -> true) |>
                   man.flow.set Flow.TCur cur
        in
        Eval.return (Some (mk_py_none exp.erange)) flow
      end


    | E_py_call(
        {ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_exception_exists")}, _)},
        [{ekind = E_py_object cls}], []
      )  ->
      begin
        let error_env = man.flow.fold (fun tk acc env ->
            match tk with
            | Flows.Exceptions.TExn exn when Addr.isinstance exn cls -> man.env.join acc env
            | _ -> acc
          ) flow man.env.bottom in
        let cur = man.flow.get Flow.TCur flow in
        let cur' = if man.env.is_bottom cur then man.env.top else cur in
        let cond = if man.env.is_bottom error_env then mk_zero exp.erange else mk_one exp.erange in
        let stmt = mk_assert cond exp.erange in
        let flow' = man.flow.set Flow.TCur cur' flow |>
                   man.exec stmt ctx |>
                   man.flow.filter (fun tk _ -> match tk with Flows.Exceptions.TExn exn when Addr.isinstance exn cls -> false | _ -> true) |>
                   man.flow.set Flow.TCur cur
        in
        Eval.return (Some (mk_py_none exp.erange)) flow'
      end


    | _ ->
      None

  let ask _ _ _ _ = None

end



(*==========================================================================*)
(**                          {2 Decorators}                                 *)
(*==========================================================================*)


let is_stub_fundec = function
  | {py_func_decors = [{ekind = E_py_attribute({ekind = E_var {vname = "mopsa"}}, "stub")}]} -> true
  | _ -> false

let is_builtin_fundec = function
  | {py_func_decors = [{ekind = E_py_call({ekind = E_py_attribute({ekind = E_var {vname = "mopsa"}}, "builtin")}, _, [])}]} -> true
  | _ -> false

let is_builtin_clsdec = function
  | {py_cls_decors = [{ekind = E_py_call({ekind = E_py_attribute({ekind = E_var {vname = "mopsa"}}, "builtin")}, _, [])}]} -> true
  | _ -> false

let is_unsupported_fundec = function
  | {py_func_decors = [{ekind = E_py_attribute({ekind = E_var {vname = "mopsa"}}, "unsupported")}]} -> true
  | _ -> false

let is_unsupported_clsdec = function
  | {py_cls_decors = [{ekind = E_py_attribute({ekind = E_var {vname = "mopsa"}}, "unsupported")}]} -> true
  | _ -> false


let builtin_fundec_name = function
  | {py_func_decors = [{ekind = E_py_call({ekind = E_py_attribute({ekind = E_var {vname = "mopsa"}}, "builtin")}, [{ekind = E_constant (C_string name)}], [])}]} -> name
  | _ -> raise Not_found

let builtin_clsdec_name = function
  | {py_cls_decors = [{ekind = E_py_call({ekind = E_py_attribute({ekind = E_var {vname = "mopsa"}}, "builtin")}, [{ekind = E_constant (C_string name)}], [])}]} -> name
  | _ -> raise Not_found


(*==========================================================================*)
(**                             {2 Setup }                                  *)
(*==========================================================================*)


let setup () =
  register_domain name (module Domain)
