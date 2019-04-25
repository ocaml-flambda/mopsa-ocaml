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

(** Python standard library. *)

open Mopsa
open Addr
open Ast
open Universal.Ast
open MapExt

module Domain =
  struct

    let name = "python.libs.stdlib"
    let debug fmt = Debug.debug ~channel:name fmt

    let interface = {
      iexec = { provides = []; uses = [Zone.Z_py] };
      ieval = { provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj] }
    }

    type stub_signature = {in_args: string list;
                           out_type: Mopsa.typ}
    type stub_db = stub_signature StringMap.t

    let add_signature funname in_args out_type db =
      let out_type = match out_type with
        | "bool" -> T_bool
        | "int" -> T_int
        | "float" -> T_float F_DOUBLE
        | "str" -> T_string
        | "NoneType" -> T_py_none
        | "NotImplementedType" -> T_py_not_implemented
        | _ -> assert false in
      StringMap.add funname {in_args; out_type} db

    let stub_base =
      StringMap.empty |>
      add_signature "bin" ["int"] "str" |>
      add_signature "chr" ["int"] "str" |>
      add_signature "ord" ["str"] "int"

    let process_simple man flow range exprs instances return =
      Utils.check_instances man flow range exprs instances (fun _ flow -> man.eval (mk_py_top return range) flow)

    let init _ _ flow = flow

    let exec _ _ _ _ = None

    let eval zones exp man flow =
      let range = exp.erange in
      match ekind exp with
      (* Calls to iter built-in function *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "iter")}, _)},
                  [obj], []) ->
         (* Check that the class of obj has an attribute __iter__ *)
         man.eval obj flow |>
           Eval.bind (fun eobj flow ->
               man.eval (mk_py_type eobj range) flow |>
                 Eval.bind (fun cls' flow ->
                     let cls = object_of_expr cls' in
                     assume_eval
                       (Utils.mk_object_hasattr cls "__iter__" range)
                       ~fthen:(fun true_flow ->
                         (* Call iter and check that it returns an object with an attribute __next__ *)
                         man.eval (mk_py_call (mk_py_object_attr cls "__iter__" range) [eobj] range) true_flow |>
                           Eval.bind (fun iter flow ->
                               assume_eval
                                 (Utils.mk_hasattr iter "__next__" range)
                                 ~fthen:(fun true_flow -> Eval.singleton iter true_flow)
                                 ~felse:(fun false_flow ->
                                   man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow |>
                                     Eval.empty_singleton)
                                 man flow
                             )
                       )
                       ~felse:(fun false_flow ->
                         man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow |>
                           Eval.empty_singleton)
                       man flow
                   )
             )
         |> Option.return

      (* Calls to len built-in function *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "len")}, _)},
                  [obj], [])  ->
         (* Check that the class of obj has an attribute __len__ *)
         man.eval obj flow |>
           Eval.bind (fun eobj flow ->
               man.eval (mk_py_type eobj range) flow |>
                 Eval.bind (fun cls flow ->
                     let cls = object_of_expr cls in
                     assume_eval
                       (Utils.mk_object_hasattr cls "__len__" range)
                       ~fthen:(fun true_flow ->
                         (* Call __len__ and check that it returns an integer *)
                         man.eval (mk_py_call (mk_py_object_attr cls "__len__" range) [eobj] range) true_flow |>
                           Eval.bind (fun len flow ->
                               assume_eval
                                 (mk_py_isinstance_builtin len "int" range)
                                 ~fthen:(fun true_flow ->
                                   Eval.singleton len true_flow)
                                 ~felse:(fun false_flow ->
                                   man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow |>
                                     Eval.empty_singleton)
                                 man flow
                             )
                       )
                       ~felse:(fun false_flow ->
                         man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow |>
                           Eval.empty_singleton)
                       man flow
                   )
             )
         |> Option.return

      (* Calls to built-in function next *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "next")}, _)},
                  [obj], [])  ->
         (* Check that the class of obj has an attribute __next__ *)
         man.eval obj flow |>
           Eval.bind (fun eobj flow ->
               man.eval (mk_py_type eobj range) flow |>
                 Eval.bind (fun cls flow ->
                     let cls = object_of_expr cls in
                     assume_eval
                       (Utils.mk_object_hasattr cls "__next__" range)
                       ~fthen:(fun true_flow ->
                         man.eval (mk_py_call (mk_py_object_attr cls "__next__" range) [obj] range) true_flow
                       )
                       ~felse:(fun false_flow ->
                         man.exec (Utils.mk_builtin_raise "TypeError" range) false_flow |>
                           Eval.empty_singleton)
                       man flow
                   )
             )
         |> Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "input")}, _)}, args, [])  ->
         let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
         if List.length args <= 1 then
           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_string range) flow |> Option.return
         else
           tyerror flow |> Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "sum")}, _)} as call, [els], [])  ->
         let args' = els :: (mk_constant T_int (C_int (Z.of_int 0)) range) :: [] in
         man.eval {exp with ekind = E_py_call(call, args', [])} flow |> Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "sum")}, _)}, [ els; start ], [])  ->
      (* let's desugar sum into tmp = 0; for x in els: tmp = tmp + x; tmp *)
         let counter = mktmp () in
         let counter_var = mk_var counter range in
         let target = mktmp () in
         let target_var = mk_var target range in
         let assign = mk_assign counter_var (mk_constant T_int (C_int (Z.of_int 0)) range) range in
         let pass = mk_block [] range in
         let for_loop = mk_stmt (S_py_for (target_var, els,
                                           mk_assign counter_var (mk_binop counter_var O_plus target_var range) range,
                                           pass)) range in
         let stmt = mk_block (assign :: for_loop :: []) range in
         debug "Rewriting %a into %a@\n" pp_expr exp pp_stmt stmt;
         man.exec stmt flow |>
           man.eval counter_var |>
           Eval.add_cleaners [mk_remove_var counter range; mk_remove_var target range] |>
           Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin s)}, _)}, [iterable], []) when s = "max" || s = "min" ->
         (* desugaring max(iterable) into:
          *    iter_var = iter(iterable)
          *    maxi_var = next(iter_var)
          *    for target_var in iter_var:
          *        if target_var > maxi_var:
          *            maxi_var = target_var *)
        let comp_op = if s = "max" then O_gt else if s = "min" then O_lt else assert false in
        let iter = mktmp () in
        let iter_var = mk_var iter range in
        let maxi = mktmp () in
        let maxi_var = mk_var maxi range in
        let target = mktmp () in
        let target_var = mk_var target range in

        let cleaners = List.map (fun x -> mk_remove_var x range) [iter; maxi; target] in
        let pass = mk_block [] range in

        let assign_iter = mk_assign iter_var (Utils.mk_builtin_call "iter" [iterable] range) range in
        let assign_max =
          Utils.mk_try_stopiteration
            (mk_assign maxi_var (Utils.mk_builtin_call "next" [iter_var] range) range)
            (Utils.mk_builtin_raise "ValueError" range)
            range in
        let for_stmt = mk_stmt (S_py_for (target_var, iter_var,
                                          mk_if (mk_binop target_var comp_op maxi_var range)
                                            (mk_assign maxi_var target_var range)
                                            pass range
                                         , pass)) range in
        let stmt = mk_block (assign_iter :: assign_max :: for_stmt :: []) range in
        debug "Rewriting %a into %a@\n" pp_expr exp pp_stmt stmt;
        man.exec stmt flow |>
        man.eval maxi_var |>
        Eval.add_cleaners cleaners |>
        Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "max")}, _)}, [e1; e2], []) ->
        (* desugaring max(e1, e2) into if e1 > e2 then e1 else e2 *)
        let expr = mk_expr (E_py_if (mk_binop e1 O_gt e2 range, e1, e2)) range in
        debug "Rewriting %a into %a@\n" pp_expr exp pp_expr expr;
        man.eval expr flow |> Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "print")}, _)}, [obj], [])  ->
        man.eval obj flow |>
        Eval.bind (fun eobj flow ->
            man.eval (mk_py_none range) flow)
        |> Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "hash")}, _)}, args, []) ->
        let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
        Eval.eval_list man.eval args flow |>
        Eval.bind (fun eargs flow ->
            if List.length eargs <> 1 then tyerror flow else
              let el = List.hd eargs in
              man.eval (mk_py_call (mk_py_object_attr (object_of_expr el) "__hash__" range) [] range) flow
          )
        |> Option.return



      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, args, []) when StringMap.mem f stub_base ->
        debug "function %s in stub_base, processing@\n" f;
        let {in_args; out_type} = StringMap.find f stub_base in
        process_simple man flow range args in_args out_type
        |> Option.return

      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "sorted")}, _)}, [obj], [])  ->
        (* todo: call list on obj first *)
        let seq = mktmp () in
        let flow = man.exec (mk_assign (mk_var seq range) obj range) flow in
        man.eval (Utils.mk_builtin_call "list.sort" [mk_var seq range] range) flow |>
        Eval.bind (fun _ flow ->
            man.eval (mk_var seq range) flow |>
            Eval.add_cleaners [mk_remove_var seq range]
          )
        |> Option.return



      (* | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "bin")}, _)}, args, [])  ->
       *   Utils.check_instances man flow range args ["int"]
       *     (fun _ flow -> man.eval (mk_py_top T_string range) flow)
       *   |> Option.return *)

      | _ ->
         None

    let ask _ _ _ = None

  end

let () = Framework.Core.Sig.Stateless.Domain.register_domain (module Domain)
