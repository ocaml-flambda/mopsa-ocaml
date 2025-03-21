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
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast
open Data_container_utils

let name = "python.objects.set"

type addr_kind +=
  | A_py_set

let () = register_addr_kind_nominal_type (fun default ak ->
             match ak with
             | A_py_set -> "set"
             | _ -> default ak);
         register_addr_kind_structural_type (fun default ak s ->
             match ak with
             | A_py_set -> false
             | _ -> default ak s)



let () =
  register_is_data_container (fun default ak -> match ak with
      | A_py_set -> true
      | _ -> default ak)


let () =
  Format.(register_addr_kind {
      print = (fun default fmt a ->
          match a with
          | A_py_set -> fprintf fmt "set"
          | _ -> default fmt a);
      compare = (fun default a1 a2 ->
          match a1, a2 with
          | _ -> default a1 a2);})

let opt_py_set_allocation_policy : string ref = ref "all"
let () = Universal.Heap.Policies.register_option opt_py_set_allocation_policy name "-py-set-alloc-pol" "for smashed sets"
           (fun default ak -> match ak with
                              | A_py_set -> (Universal.Heap.Policies.of_string !opt_py_set_allocation_policy) ak
                              | _ -> default ak)


module Domain =
struct

  include GenStatelessDomainId(struct
      let name = name
    end)

  let checks = []

  let init (prog:program) man flow = None

  let var_of_addr a = match akind a with
    | A_py_set -> {(mk_addr_attr a "set" (T_py None)) with vmode = WEAK}
    | _ -> assert false

  let var_of_eobj e = match ekind e with
    | E_py_object (a, _) -> var_of_addr a
    | _ -> assert false

  let rec eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_set ls ->
      debug "Skipping set.__new__, set.__init__ for now@\n";

      let addr_set = mk_alloc_addr A_py_set range in
      man.eval   addr_set flow >>$
 (fun eaddr_set flow ->
          let addr_set = Addr.from_expr eaddr_set in
          let els_var = var_of_addr addr_set in
          let flow = List.fold_left (fun acc el ->
                         acc >>% man.exec   (mk_assign (mk_var ~mode:(Some WEAK) els_var range) el range)) (Post.return flow) ls in
          flow >>% Eval.singleton (mk_py_object (addr_set, None) range)
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("set.__new__", _))}, _)}, cls::args, []) ->
      Utils.new_wrapper man range flow "set" cls
        ~fthennew:(man.eval (mk_expr ~etyp:(T_py None) (E_py_set []) range))

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("set.__init__" as f, _))}, _)}, [arg], []) ->
      Utils.check_instances f man flow range [arg]
        ["set"]
        (fun eargs flow ->
           man.eval (mk_py_none range) flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("set.__init__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["set"; "list"]
        (fun eargs flow ->
          let eaddr_set, eaddr_list = match eargs with
            | [a; b] -> a, b
            | _ -> assert false in
          let els_var = var_of_eobj eaddr_set in
          let list_els = Py_list.Domain.var_of_eobj eaddr_list in
          man.exec (mk_assign (mk_var ~mode:(Some WEAK) els_var range) (mk_var ~mode:(Some WEAK) list_els range) range) flow >>%
           man.eval (mk_py_none range)
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("set.clear" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args ["set"]
        (fun args flow ->
           let set = List.hd args in
           let var_els = var_of_eobj set in
           man.exec (mk_remove_var var_els range) flow >>%
           man.eval (mk_py_none range)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("set.__contains__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f ~arguments_after_check:1 man flow range args ["set"]
        (fun args flow ->
           man.eval (mk_py_top (T_py (Some Bool)) range) flow)
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (f, _))}, _)}, args, [])
      when is_compare_op_fun "set" f ->
      Utils.check_instances ~arguments_after_check:1 f man flow range args ["set"]
        (fun eargs flow ->
           let e1, e2 = match args with [l; r] -> l, r | _ -> assert false in
           assume (mk_py_isinstance_builtin e2 "set" range) man flow
             ~fthen:(man.eval (mk_py_top (T_py (Some Bool)) range))
             ~felse:(fun flow ->
                 let expr = mk_constant ~etyp:(T_py (Some NotImplemented)) C_py_not_implemented range in
                 man.eval expr flow)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("set.discard" as f, _))}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 f man flow range args
        ["set"]
        (fun args flow ->
          man.eval (mk_py_none range) flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("set.__iter__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["set"]
        (fun args flow ->
           let set = match args with | [l] -> l | _ -> assert false in
           let a = mk_alloc_addr (Py_list.A_py_iterator ("set_iterator", None)) range in
           man.eval   a flow >>$
 (fun eaddr_it flow ->
               let addr_it = Addr.from_expr eaddr_it in
               man.exec   (mk_assign (mk_var (Py_list.Domain.itseq_of_addr addr_it) range) set range) flow >>%
               Eval.singleton (mk_py_object (addr_it, None) range)
             )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("set_iterator.__next__", _))}, _)}, [iterator], []) ->
      (* todo: checks ? *)
       man.eval    iterator flow >>$
 (fun iterator flow ->
             man.eval   (mk_var (Py_list.Domain.itseq_of_eobj iterator) range) flow >>$
 (fun set_eobj flow ->
                   let var_els = var_of_eobj set_eobj in
                   let els = man.eval (mk_var var_els ~mode:(Some WEAK) range) flow in
                   let flow = Flow.set_ctx (Cases.get_ctx els) flow in
                   let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow >>% Eval.empty in
                   Eval.join_list ~empty:(fun () -> Eval.empty flow) (Cases.copy_ctx stopiteration els::stopiteration::[])
                 )
           )
       |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("set_iterator.__iter__", _))}, _)}, [iterator], []) ->
      (* todo: checks ? *)
      man.eval   iterator flow |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("set.__len__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["set"]
        (fun args flow ->
           man.eval   (mk_py_top T_int range) flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("set.add" as f, _))}, _)}, args, []) ->
      Utils.check_instances f ~arguments_after_check:1 man flow range args
        ["set"]
        (fun args flow ->
           let set, element = match args with | [l; e] -> l, e | _ -> assert false in
           debug "set: %a@\n" pp_expr set;
           let var_els = var_of_eobj set in
           man.exec (mk_assign (mk_var var_els ~mode:(Some WEAK) range) element range) flow >>%
           man.eval (mk_py_none range))
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.assert_set_of", _))}, _)}, args, []) ->
      bind_list args man.eval flow |>
      bind_result (fun eargs flow ->
          let set, set_v = match eargs with [d;e] -> d,e | _ -> assert false in
          assume (mk_py_isinstance_builtin set "set" range) man flow
            ~fthen:(fun flow ->
                let var = var_of_eobj set in
                Utils.check man
                  (mk_py_isinstance (mk_var ~mode:(Some WEAK) var range) set_v range)
                  range flow
              )
            ~felse:(Utils.check man (mk_py_false range) range)
        )
      |> OptionExt.return


    | _ -> None


  let exec stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_add {ekind = E_addr ({addr_kind = A_py_set}, _)} ->
       Post.return flow |> OptionExt.return

    | S_remove {ekind = E_addr ({addr_kind = A_py_set} as a, _)} ->
       let va = var_of_addr a in
       flow |>
         man.exec   (mk_remove_var va range) |>
         OptionExt.return

    | S_invalidate {ekind = E_addr ({addr_kind = A_py_set} as a, _)} ->
       let va = var_of_addr a in
       flow |>
         man.exec   (mk_remove_var va range) |>
         OptionExt.return

    | S_fold ({ekind = E_addr ({addr_kind = A_py_set} as a, _)}, addrs) ->
       let va = var_of_addr a in
       let vas = List.map (fun ea' -> match ekind ea' with
                                      | E_addr ({addr_kind = A_py_set} as a', _) -> var_of_addr a'
                                      | _ -> assert false) addrs in
       man.exec   (mk_fold_var va vas range) flow |>  OptionExt.return

    | S_expand ({ekind = E_addr ({addr_kind = A_py_set} as a, _)}, addrs) ->
       let va = var_of_addr a in
       let vas = List.map (fun ea' -> match ekind ea' with
                                      | E_addr ({addr_kind = A_py_set} as a', _) -> var_of_addr a'
                                      | _ -> assert false) addrs in
       man.exec   (mk_expand_var va vas range) flow |> OptionExt.return


    | S_rename ({ekind = E_addr ({addr_kind = A_py_set} as a, _)}, {ekind = E_addr (a', _)}) ->
      let va = var_of_addr a in
      let va' = var_of_addr a' in
      debug "renaming %a into %a@\n" pp_var va pp_var va';
      man.exec   (mk_rename_var va va' range) flow
      |> OptionExt.return

    | _ -> None

  let ask : type r. ('a, r) query -> ('a, unit) man -> 'a flow -> ('a, r) cases option =
    fun query man flow ->
    match query with
    | Framework.Engines.Interactive.Query.Q_debug_addr_value ({addr_kind = A_py_set} as addr) ->
       let open Framework.Engines.Interactive.Query in
       let content_set = ask_and_reduce man.ask (Q_debug_variable_value (var_of_addr addr)) flow in
       Some (Cases.singleton {
           var_value = None;
           var_value_type = T_any;
           var_sub_value = Some (Named_sub_value
                                   ["set content", content_set])
         } flow)


    | _ -> None

  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
