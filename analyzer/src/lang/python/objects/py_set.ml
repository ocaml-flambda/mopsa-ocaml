(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2018-2019 The MOPSA Project.                               *)
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

(* A general smashing abstraction for Python sets, (hopefully)
   irrelevant of the value/type domain *)

open Mopsa
open Sig.Domain.Stateless
open Ast
open Addr
open Universal.Ast
open Data_container_utils


type addr_kind +=
  | A_py_set of Rangeset.t (* variable where the smashed elements are stored *)

let () =
  register_join_akind (fun default ak1 ak2 ->
      match ak1, ak2 with
      | A_py_set r1, A_py_set r2 -> A_py_set (Rangeset.union r1 r2)
      | _ -> default ak1 ak2);
  register_is_data_container (fun default ak -> match ak with
      | A_py_set _ -> true
      | _ -> default ak)


let () =
  Format.(register_addr_kind {
      print = (fun default fmt a ->
          match a with
          | A_py_set r -> fprintf fmt "set[%a]" (fun fmt -> Rangeset.iter (fun ra -> pp_range fmt ra)) r
          | _ -> default fmt a);
      compare = (fun default a1 a2 ->
          match a1, a2 with
          | A_py_set v1, A_py_set v2 -> Rangeset.compare v1 v2
          | _ -> default a1 a2);})


module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.objects.set"
    end)


  let interface = {
    iexec = {provides = [Zone.Z_py_obj]; uses = [Zone.Z_py_obj]};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj; Universal.Zone.Z_u_heap, Z_any]}
  }

  let init (prog:program) man flow = flow

  let var_of_addr a = match akind a with
    | A_py_set _ -> mk_addr_attr a "set" T_any
    | _ -> assert false

  let var_of_eobj e = match ekind e with
    | E_py_object (a, _) -> var_of_addr a
    | _ -> assert false

  let addr_of_expr exp = match ekind exp with
    | E_addr a -> a
    | _ -> Exceptions.panic "%a@\n" pp_expr exp

  let rec eval zones exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_set ls ->
      debug "Skipping set.__new__, set.__init__ for now@\n";

      let addr_set = mk_alloc_addr (A_py_set (Rangeset.singleton range)) range in
      man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_set flow |>
      Eval.bind (fun eaddr_set flow ->
          let addr_set = addr_of_expr eaddr_set in
          let els_var = var_of_addr addr_set in
          let flow = List.fold_left (fun acc el ->
              man.exec ~zone:Zone.Z_py (mk_assign (mk_var ~mode:WEAK els_var range) el range) acc) flow ls in
          Eval.singleton (mk_py_object (addr_set, None) range) flow
        )
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.__new__")}, _)}, args, []) ->
      (* todo: check that first arg is set class *)
      man.eval (mk_expr (E_py_set []) range) flow
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.__init__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["set"]
        (fun eargs flow ->
           man.eval (mk_py_none range) flow
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.clear")}, _)}, args, []) ->
      Utils.check_instances man flow range args ["set"]
        (fun args flow ->
           let set = List.hd args in
           let var_els = var_of_eobj set in
           man.exec (mk_remove_var var_els range) flow |>
           man.eval (mk_py_none range)
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.__contains__")}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 man flow range args ["set"]
        (fun args flow ->
           man.eval (mk_py_top T_bool range) flow)
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, args, [])
      when is_compare_op_fun "set" f ->
      Utils.check_instances ~arguments_after_check:1 man flow range args ["set"]
        (fun eargs flow ->
           let e1, e2 = match args with [l; r] -> l, r | _ -> assert false in
           assume (mk_py_isinstance_builtin e2 "set" range) man flow
             ~fthen:(man.eval (mk_py_top T_bool range))
             ~felse:(fun flow ->
                 let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                 man.eval expr flow)
        )
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.__iter__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["set"]
        (fun args flow ->
           let set = match args with | [l] -> l | _ -> assert false in
           let set_addr = match ekind set with
             | E_py_object ({addr_kind = A_py_set _} as a, _) -> a
             | _ -> assert false in
           let a = mk_alloc_addr (Py_list.A_py_iterator ("set_iterator", [set_addr], None)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) a flow |>
           Eval.bind (fun eaddr_it flow ->
               let addr_it = match ekind eaddr_it with | E_addr a -> a | _ -> assert false in
               Eval.singleton (mk_py_object (addr_it, None) range) flow
             )
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set_iterator.__next__")}, _)}, [iterator], []) ->
      (* todo: checks ? *)
      man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj) iterator flow |>
      Eval.bind (fun iterator flow ->
          let set_addr = match ekind iterator with
            | E_py_object ({addr_kind = Py_list.A_py_iterator (s, [a], _)}, _) when s = "set_iterator" -> a
            | _ -> assert false in
          let var_els = var_of_addr set_addr in
          let els = man.eval (mk_var var_els ~mode:WEAK range) flow in
          let flow = Flow.set_ctx (Eval.get_ctx els) flow in
          let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton in
          Eval.join_list ~empty:(Eval.empty_singleton flow) (Eval.copy_ctx stopiteration els::stopiteration::[])
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set_iterator.__iter__")}, _)}, [iterator], []) ->
      (* todo: checks ? *)
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) iterator flow |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.__len__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["set"]
        (fun args flow ->
           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range) flow
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "set.add")}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 man flow range args
        ["set"]
        (fun args flow ->
           let set, element = match args with | [l; e] -> l, e | _ -> assert false in
           debug "set: %a@\n" pp_expr set;
           let var_els = var_of_eobj set in
           man.exec (mk_assign (mk_var var_els ~mode:WEAK range) element range) flow |>
           man.eval (mk_py_none range))
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_set_of")}, _)}, args, []) ->
      bind_list args man.eval flow |>
      bind_some (fun eargs flow ->
          let set, set_v = match eargs with [d;e] -> d,e | _ -> assert false in
          assume (mk_py_isinstance_builtin set "set" range) man flow
            ~fthen:(fun flow ->
                let var = var_of_eobj set in
                Libs.Py_mopsa.check man
                  (mk_py_isinstance (mk_var ~mode:WEAK var range) set_v range)
                  range flow
              )
            ~felse:(Libs.Py_mopsa.check man (mk_py_false range) range)
        )
      |> Option.return


    | _ -> None


  let exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_rename ({ekind = E_addr ({addr_kind = A_py_set _} as a)}, {ekind = E_addr a'}) ->
      let va = var_of_addr a in
      let va' = var_of_addr a' in
      debug "renaming %a into %a@\n" pp_var va pp_var va';
      man.exec ~zone:Zone.Z_py (mk_rename_var va va' range) flow
      |> Post.return |> Option.return

    | _ -> None

  let ask _ _ _ = None
end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
