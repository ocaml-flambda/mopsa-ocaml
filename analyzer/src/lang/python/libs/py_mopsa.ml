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

(** MOPSA Python library. *)

open Mopsa
open Sig.Abstraction.Stateless
open Addr
open Ast
open Universal.Ast
open Alarms


let check man cond range flow =
  man.exec (mk_assert cond range) flow >>%
  man.eval (mk_py_none range)


(*==========================================================================*)
(**                               {2 Domain }                               *)
(*==========================================================================*)


module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.libs.mopsa"
      end)

    let checks = []

    (*==========================================================================*)
    (**                       {2 Transfer functions }                           *)
    (*==========================================================================*)
    let exec _ _ _ = None

    let init prog man flow = flow

    let eval exp man flow =
      let range = erange exp in
      match ekind exp with
      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.random_bool", _))}, _)}, [], []) ->
         man.eval (mk_py_top (T_py (Some Bool)) range) flow |> OptionExt.return

      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.random_float", _))}, _)}, [], []) ->
         man.eval (mk_py_top (T_py (Some (Float F_DOUBLE))) range) flow |> OptionExt.return

      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.random_string", _))}, _)}, [], []) ->
         man.eval (mk_py_top (T_py (Some Str)) range) flow |> OptionExt.return

      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.random_int", _))}, _)}, [], []) ->
         man.eval (mk_py_top T_int range) flow |> OptionExt.return

      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.random_int", _))}, _)}, [l; u], []) ->
              let tmp = mktmp ~typ:(T_py None) () in
              let l = Utils.mk_builtin_call "int" [l] range in
              let u = Utils.mk_builtin_call "int" [u] range in
              man.exec (mk_assign (mk_var tmp range) (mk_py_top T_int range) range) flow >>%
              man.exec (mk_assume (mk_py_in (mk_var tmp range) l u range) range) >>%
              man.eval (mk_var tmp range) |>
              Cases.add_cleaners [mk_remove_var tmp range] |>
              OptionExt.return

      | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.random_float", _))}, _)}, [l; u], []) ->
         begin
           match ekind l, ekind u with
           | E_constant (C_float l), E_constant (C_float u) -> man.eval  (mk_py_float_interval l u range) flow
           | E_constant (C_float l), E_constant (C_int u) -> man.eval (mk_py_float_interval l (Z.to_float u) range) flow
           | E_constant (C_int l), E_constant (C_float u) -> man.eval (mk_py_float_interval (Z.to_float l) u range) flow
           | E_constant (C_int l), E_constant (C_int u) -> man.eval (mk_py_float_interval (Z.to_float l) (Z.to_float u) range) flow
           | _ ->
              let tmp = mktmp ~typ:(T_py None) () in
              let l = Utils.mk_builtin_call "float" [l] range in
              let u = Utils.mk_builtin_call "float" [u] range in
              man.exec (mk_assign (mk_var tmp range) (mk_py_top (T_float F_DOUBLE) range) range) flow >>%
              man.exec (mk_assume (mk_py_in (mk_var tmp range) l u range) range) >>%
              man.eval (mk_var tmp range) |>
              Cases.add_cleaners [mk_remove_var tmp range]
         end
         |> OptionExt.return

      (* Calls to mopsa.assert_equal function *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.assert_equal", _))}, _)}, [x; y], []) ->
         let range = erange exp in
         assume (mk_binop ~etyp:(T_py None) x O_eq y (tag_range range "eq"))
           man flow
           ~fthen:(check man (mk_true range) range)
           ~felse:(check man (mk_false range) range)
           ~fnone:(check man (mk_false range) range)
         |> OptionExt.return

      (* Calls to mopsa assert function *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.massert", _))}, _)}, [x], []) ->
         let range = erange exp in
         assume x man flow
           ~fthen:(check man (mk_true range) range)
           ~felse:(check man (mk_false range) range)
           ~fnone:(check man (mk_false range) range)
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.assert_exists", _))}, _)}, [cond], [])  ->
         let stmt = {skind = S_satisfy(cond); srange = exp.erange} in
         man.exec stmt flow >>%
         Eval.singleton (mk_py_true exp.erange)
         |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.assert_safe", _))}, _)}, [], [])  ->
         begin
           let error_env = Flow.fold (fun acc tk env ->
               match tk with
               | T_py_exception _ -> man.lattice.join (Flow.get_ctx flow) acc env
               | _ -> acc
             ) man.lattice.bottom flow
           in
           let exception BottomFound in
           try
             debug "cond = %b %b@\n" (Flow.get T_cur man.lattice flow |> man.lattice.is_bottom) (man.lattice.is_bottom error_env);
             let cond =
               match Flow.get T_cur man.lattice flow |> man.lattice.is_bottom,
                     man.lattice.is_bottom error_env with
               | false, true -> mk_true
               | true, false -> mk_false
               | false, false -> mk_top T_bool
               | true, true -> raise BottomFound
             in
             let conde = {(cond exp.erange) with etyp=T_bool} in
             let stmt = mk_assert conde exp.erange in
             let cur = Flow.get T_cur man.lattice flow in
             let flow = Flow.set T_cur man.lattice.top man.lattice flow |>
                          man.exec stmt |> post_to_flow man |>
                          Flow.set T_cur cur man.lattice
             in
             debug "Flow is now %a@\n" (Flow.print man.lattice.print) flow;
             man.eval (mk_py_true exp.erange) flow
             |> OptionExt.return
           with BottomFound ->
             Eval.empty flow
             |> OptionExt.return
         end

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.assert_unsafe", _))}, _)}, [], [])  ->
         begin
           let error_env = Flow.fold (fun acc tk env -> match tk with
                                                        | T_py_exception _ -> man.lattice.join (Flow.get_ctx flow) acc env
                                                        | _ -> acc
             ) man.lattice.bottom flow in
           let exception BottomFound in
           try
             let cond =
               match Flow.get T_cur man.lattice flow |> man.lattice.is_bottom,
                     man.lattice.is_bottom error_env with
               | false, true -> mk_py_false
               | true, false -> mk_py_true
               | false, false -> mk_top T_bool
               | true, true -> raise BottomFound
             in
             let conde = {(cond exp.erange) with etyp = T_bool} in
             let stmt = mk_assert conde exp.erange in
             let cur = Flow.get T_cur man.lattice flow in
             let flow = Flow.set T_cur man.lattice.top man.lattice flow |>
                        man.exec stmt |> post_to_flow man |>
                        Flow.filter (fun tk _ -> match tk with T_py_exception _ -> false | _ -> true) |>
                        Flow.set T_cur cur man.lattice
             in
             Eval.singleton (mk_py_true exp.erange) flow |> OptionExt.return
           with BottomFound -> Eval.empty flow |> OptionExt.return
         end

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.assert_exception", _))}, _)}, [{ekind = cls} as assert_exn], []) ->
        debug "begin assert_exception, flow=%a" (Flow.print man.lattice.print) flow;
        let ctx = Flow.get_ctx flow in
        let report = Flow.get_report flow in
        let this_error_env, good_exns = Flow.fold (fun (acc_env, acc_good_exn) tk env ->
            match tk with
            | T_py_exception (exn, _, _) ->
              let flow1 = Flow.bottom ctx report in
              let flow1 = Flow.set T_cur env man.lattice flow1 in
              let flow2 = man.exec (mk_assume (mk_py_isinstance exn assert_exn range) range) flow1 |> post_to_flow man in
              if not @@ (Flow.get T_cur man.lattice flow2 |> man.lattice.is_bottom) then
                man.lattice.join (Flow.get_ctx flow2) acc_env env, exn :: acc_good_exn
              else
                acc_env, acc_good_exn
            | _ -> acc_env, acc_good_exn) (man.lattice.bottom, []) flow
        in
        debug "this_error_env = %a@\n" man.lattice.print this_error_env;
        let cond =
          match Flow.get T_cur man.lattice flow |> man.lattice.is_bottom,
                man.lattice.is_bottom this_error_env with
          | true, false -> mk_py_true
          | _, true -> mk_py_false
          | false, false ->  mk_top T_bool
        in
        let conde = {(cond exp.erange) with etyp = T_bool} in
        let stmt = mk_assert conde exp.erange in
        let cur = Flow.get T_cur man.lattice flow in
        let flow = Flow.set T_cur man.lattice.top man.lattice flow in
        let flow = man.exec stmt flow |> post_to_flow man |>
                   Flow.filter (fun tk _ -> match tk with T_py_exception (exn, _, _) when List.mem exn good_exns -> debug "Foundit@\n"; false | _ -> true) |>
                     Flow.set T_cur (man.lattice.join ctx cur this_error_env) man.lattice
        in
        debug "flow = %a@\n" (Flow.print man.lattice.print) flow;
        man.eval (mk_py_false exp.erange) flow
        |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.ignore_exception", _))}, _)}, [{ekind = cls} as assert_exn], []) ->
         debug "begin ignore_exception(%a) on %a" pp_expr assert_exn (Flow.print man.lattice.print) flow;
         let ctx = Flow.get_ctx flow in
         let report = Flow.get_report flow in
         let none = mk_py_none range in
         let flow = Flow.fold (fun acc tk env -> match tk with
             | T_py_exception (exn, _, _) ->
               let flow1 = Flow.bottom ctx report |> Flow.set T_cur env man.lattice in
               debug "assert_exn = %a, exn = %a" pp_expr assert_exn pp_expr exn;
               let flow2 = man.exec (mk_assume (mk_py_isinstance exn assert_exn range) range) flow1 |> post_to_flow man in
               debug "flow2 = %a" (Flow.print man.lattice.print) flow2;
               if Flow.get T_cur man.lattice flow2 |> man.lattice.is_bottom then
                 Flow.set tk env man.lattice acc
               else
                 acc
             | _ -> Flow.set tk env man.lattice acc) (Flow.bottom ctx report) flow
         in
         debug "Final flow = %a" (Flow.print man.lattice.print) flow;
         man.eval none flow |> OptionExt.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.assert_exception_exists", _))}, _)}, [{ekind = cls} as assert_exn], [])  ->
        debug "begin assert_exception_exists";
        let ctx = Flow.get_ctx flow in
        let report = Flow.get_report flow in
        let error_env = Flow.fold (fun acc tk env -> match tk with
            | T_py_exception (exn, _, _) ->
              let flow1 = Flow.bottom ctx report in
              let flow1 = Flow.set T_cur env man.lattice flow1 in
              let flow2 = man.exec (mk_assume (mk_py_isinstance exn assert_exn range) range) flow1 |> post_to_flow man in
              if not @@ (Flow.get T_cur man.lattice flow2 |> man.lattice.is_bottom) then
                man.lattice.join (Flow.get_ctx flow) acc env
              else acc
            | _ -> acc
          ) man.lattice.bottom flow in
        debug "error_env = %a" man.lattice.print error_env;
        let cond = mk_bool (not (man.lattice.is_bottom error_env)) range in
        let conde = {cond with etyp = T_bool} in
        let stmt = mk_assert conde exp.erange in
        let cur = Flow.get T_cur man.lattice flow in
        let flow = Flow.set T_cur man.lattice.top man.lattice flow |>
                   man.exec stmt |> post_to_flow man |> Flow.set T_cur cur man.lattice in
        debug "flow = %a" (Flow.print man.lattice.print) flow;
        man.eval (mk_py_true exp.erange) flow
        |> OptionExt.return

      | _ ->
         None

    let ask _ _ _ = None
  end



(*==========================================================================*)
(**                          {2 Decorators}                                 *)
(*==========================================================================*)


let is_stub_fundec fundec =
  List.exists (fun exp -> match ekind exp with
      | E_py_attribute({ekind = E_var( {vkind = V_uniq ("mopsa",_)}, _)}, "stub") -> true
      | _ -> false
    ) fundec.py_func_decors

let is_builtin_fundec fundec =
  List.exists (fun exp -> match ekind exp with
      | E_py_call({ekind = E_py_attribute({ekind = E_var( {vkind = V_uniq ("mopsa",_)}, _)}, "builtin")}, _, []) -> true
      | _ -> false
    )
    fundec.py_func_decors

let is_builtin_clsdec clsdec =
  List.exists (fun exp -> match ekind exp with
      | E_py_call({ekind = E_py_attribute({ekind = E_var( {vkind = V_uniq ("mopsa",_)}, _)}, "builtin")}, _, []) -> true
      | _ -> false
    )
    clsdec.py_cls_decors

let is_unsupported_fundec fundec =
  List.exists (fun exp -> match ekind exp with
      | E_py_attribute({ekind = E_var( {vkind = V_uniq ("mopsa",_)}, _)}, "unsupported") -> true
      | _ -> false)
    fundec.py_func_decors

let is_unsupported_clsdec clsdec =
  List.exists (fun exp -> match ekind exp with
      | E_py_attribute({ekind = E_var( {vkind = V_uniq ("mopsa",_)}, _)}, "unsupported") -> true
      | _ -> false)
    clsdec.py_cls_decors

let builtin_fundec_name fundec =
  let decor = List.find (fun exp -> match ekind exp with
      | E_py_call({ekind = E_py_attribute({ekind = E_var( {vkind = V_uniq ("mopsa",_)}, _)}, "builtin")}, [{ekind = E_constant (C_string name)}], []) -> true
      | _ -> false) fundec.py_func_decors  in
  match ekind decor with
  | E_py_call({ekind = E_py_attribute({ekind = E_var( {vkind = V_uniq ("mopsa",_)}, _)}, "builtin")}, [{ekind = E_constant (C_string name)}], []) -> name
  | _ -> assert false

let builtin_clsdec_name clsdec =
  let decor = List.find (fun exp -> match ekind exp with
      | E_py_call({ekind = E_py_attribute({ekind = E_var( {vkind = V_uniq ("mopsa",_)}, _)}, "builtin")}, [{ekind = E_constant (C_string name)}], []) -> true
      | _ -> false) clsdec.py_cls_decors in
  match ekind decor with
  | E_py_call({ekind = E_py_attribute({ekind = E_var( {vkind = V_uniq ("mopsa",_)}, _)}, "builtin")}, [{ekind = E_constant (C_string name)}], []) -> name
  | _ -> assert false


let builtin_type_name default fundec =
  let decor = List.find_opt (fun exp -> match ekind exp with
      | E_py_call({ekind = E_py_attribute({ekind = E_var( {vkind = V_uniq ("mopsa",_)}, _)}, "type")}, [{ekind = E_constant (C_string name)}], []) -> true
      | _ -> false) fundec.py_func_decors  in
  match decor with
  | None -> default
  | Some {ekind = E_py_call({ekind = E_py_attribute({ekind = E_var( {vkind = V_uniq ("mopsa",_)}, _)}, "type")}, [{ekind = E_constant (C_string name)}], [])} -> name
  | _ -> assert false


(*==========================================================================*)
(**                             {2 Setup }                                  *)
(*==========================================================================*)


let () =
  register_stateless_domain (module Domain)
