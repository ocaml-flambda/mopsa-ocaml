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

(* A general smashing abstraction for Python lists, (hopefully)
   irrelevant of the value/type domain *)

(* currently, lists are smashed into one variable abstracting all
   its elements. To avoid allocating a new variable each time
   (especially during loops), we allocate them only if there has
   been not other allocation at the same point. This is quite
   similar to the recency abstraction, and is probably not optimal
*)

(* TODO: add length for lists and position for iterator? *)

open Mopsa
open Sig.Domain.Stateless
open Ast
open Addr
open Universal.Ast

type addr_kind +=
  | A_py_list of var (* variable where the smashed elements are stored *)
  | A_py_iterator of string (* iterator kind (list_iterator, ...) *) * addr list  (* addr of the containers iterated on *) * int option (* potential position in the iterator *)

let () =
  Format.(register_addr {
      print = (fun default fmt a ->
          match a with
          | A_py_list var -> fprintf fmt "list[%a]" pp_var var
          | A_py_iterator (s, addr, d) ->
            begin match d with
            | None -> fprintf fmt "%s[%a]" s (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_addr) addr
            | Some d -> fprintf fmt "%s[%a, %d]" s (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_addr) addr d end
          | _ -> default fmt a);
      compare = (fun default a1 a2 ->
          match a1, a2 with
          | A_py_list v1, A_py_list v2 -> compare_var v1 v2
          | A_py_iterator (s1, a1, d1), A_py_iterator (s2, a2, d2) ->
            Compare.compose [
              (fun () -> Pervasives.compare s1 s2);
              (fun () -> Compare.list compare_addr a1 a2);
              (fun () -> Compare.option Pervasives.compare d1 d2);
            ]
          | _ -> default a1 a2);})


module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.objects.list"
    end)


  module VarInfo = struct type t = var let compare = compare_var let print = pp_var end
  module ListInfo = struct
    type t = Callstack.cs * range
    let compare (cs, r) (cs', r') =
      Compare.compose
        [
          (fun () -> Callstack.compare cs cs');
          (fun () -> compare_range r r')
        ]
    let print fmt (cs, r) =
      Format.fprintf fmt "(%a, %a)"
        Callstack.pp_call_stack cs
        pp_range r
  end

  module Equiv = Equiv.Make(ListInfo)(VarInfo)

  let ctx_key =
    let module K = Context.GenUnitKey(
      struct
        type t = Equiv.t
        let print fmt m =
          Format.fprintf fmt "List annots: @[%a@]" (Equiv.print ?pp_sep:None) m
      end
      )
    in
    K.key


  let fresh_smashed_var =  mkfresh_common (fun uid -> "$l*"  ^ (string_of_int uid), "$l*" ^ (string_of_int uid)) T_any

  let get_var_equiv (info: ListInfo.t) (e: Equiv.t) =
    try
      Equiv.find_l info e, e
    with Not_found ->
      let var = fresh_smashed_var () in
      let new_eq = Equiv.add (info, var) e in
      var, new_eq

  let get_var_flow (info: ListInfo.t) (f: 'a flow) : var * 'a flow =
    let a = Flow.get_ctx f |>
            Context.find_unit ctx_key
    in
    let var, a = get_var_equiv info a in
    var, Flow.set_ctx (Flow.get_ctx f |> Context.add_unit ctx_key a) f

  let interface = {
    iexec = {provides = []; uses = [Zone.Z_py_obj]};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj; Universal.Zone.Z_u_heap, Z_any]}
  }

  let init (prog:program) man flow =
    Flow.set_ctx (
      Flow.get_ctx flow |>
      Context.add_unit ctx_key Equiv.empty
    ) flow


  let rec eval zones exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__add__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["list"; "list"]
        (fun args flow ->
           let listl, listr = match args with [l; r] -> l, r | _ -> assert false in
           let elsl_var = match ekind listl with
             | E_py_object ({addr_kind = A_py_list a}, _) -> a
             | _ -> assert false in
           let elsr_var = match ekind listr with
             | E_py_object ({addr_kind = A_py_list a}, _) -> a
             | _ -> assert false in
           (* FIXME: try to reuse other functions? in the impl, list_concat (Objects/listobject.c) is not reusing anything *)
           (* First, allocate new addr for the list, and new addr for the list elements *)
           (* Then assign the el addr to both addresses above *)
           let els_res_var, flow = get_var_flow (Callstack.get flow, range) flow in
           let flow = List.fold_left (fun acc el ->
               man.exec ~zone:Zone.Z_py (mk_assign (mk_var ~mode:WEAK els_res_var range) el range) acc)
               flow [mk_var ~mode:WEAK elsl_var range;
                     mk_var ~mode:WEAK elsr_var range] in
           let addr_list = mk_alloc_addr (A_py_list els_res_var) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_list flow |>
           Eval.bind (fun list_addr flow ->
               let alist_addr = match ekind list_addr with | E_addr a -> a | _ -> assert false in
               Eval.singleton (mk_py_object (alist_addr, None) range) flow
             )
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin f)}, _)}, args, [])
      when is_compare_op_fun "list" f ->
      Utils.check_instances ~arguments_after_check:1 man flow range args ["list"]
        (fun eargs flow ->
           let e1, e2 = match args with [l; r] -> l, r | _ -> assert false in
           assume_eval (mk_py_isinstance_builtin e2 "list" range) man flow
             ~fthen:(man.eval (mk_py_top T_bool range))
             ~felse:(fun flow ->
                 let expr = mk_constant ~etyp:T_py_not_implemented C_py_not_implemented range in
                 man.eval expr flow)
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__mul__")}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__rmul__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["list"; "int"]
        (fun args flow ->
           let list, int = match args with [l; r] -> l, r | _ -> assert false in
           let els_list = match ekind list with
             | E_py_object ({addr_kind = A_py_list a}, _) -> a
             | _ -> assert false in
           let els_var, flow = get_var_flow (Callstack.get flow, range) flow in
           let flow = man.exec ~zone:Zone.Z_py (mk_assign (mk_var ~mode:WEAK els_var range) (mk_var ~mode:WEAK els_list range) range) flow in
           let addr_list = mk_alloc_addr (A_py_list els_var) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_list flow |>
           Eval.bind (fun eaddr_list flow ->
               let addr_list = match ekind eaddr_list with
                 | E_addr a -> a
                 | _ -> assert false in
               Eval.singleton (mk_py_object (addr_list, None) range) flow
             )
        )
      |> Option.return

    | E_py_list ls ->
      debug "Skipping list.__new__, list.__init__ for now@\n";
      (* TODO: handle empty lists *)
      let els_var, flow = get_var_flow (Callstack.get flow, range) flow in
      let flow = List.fold_left (fun acc el ->
          man.exec ~zone:Zone.Z_py (mk_assign (mk_var ~mode:WEAK els_var range) el range) acc) flow ls in
      let addr_list = mk_alloc_addr (A_py_list els_var) range in
      man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_list flow |>
      Eval.bind (fun eaddr_list flow ->
          let addr_list = match ekind eaddr_list with
            | E_addr a -> a
            | _ -> assert false in
          Eval.singleton (mk_py_object (addr_list, None) range) flow
        )
      |> Option.return


    | E_py_object ({addr_kind = A_py_list _}, e) ->
      Eval.singleton exp flow |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.append")}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 man flow range args
        ["list"]
        (fun args flow ->
           let list, element = match args with | [l; e] -> l, e | _ -> assert false in
           debug "list: %a@\nelement = %a@\nflow = %a@\n" pp_expr list pp_expr element (Flow.print man.lattice) flow;
           let var_els = match ekind list with
             | E_py_object ({addr_kind = A_py_list a}, _) -> a
             | _ -> assert false in
           man.exec (mk_assign (mk_var var_els ~mode:WEAK range) element range) flow |>
           man.eval (mk_py_none range))
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.insert")}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 man flow range args
        ["list"; "int"]
        (fun args flow ->
           let list, index, element = match args with | [l; i; e] -> l, i, e | _ -> assert false in
           let var_els = match ekind list with
             | E_py_object ({addr_kind = A_py_list a}, _) -> a
             | _ -> assert false in
           man.exec (mk_assign (mk_var var_els ~mode:WEAK range) element range) flow |>
           man.eval (mk_py_none range))
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__new__")}, _)}, args, []) ->
      (* todo: check that first arg is list class *)
      man.eval (mk_expr (E_py_list []) range) flow
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__init__")}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.extend")}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 man flow range args
        ["list"]
        (fun eargs flow ->
           (* FIXME: check manually (with ekind list) that we have a list or list_iterator as we are in the same abstract domain? *)
           let list, other = match eargs with e1::e2::[] -> e1, e2 | _ -> assert false in
           let var_els = match ekind list with
             | E_py_object ({addr_kind = A_py_list a}, _) -> a
             | _ -> Exceptions.panic_at range "%a@\n" pp_expr list in
           assume_eval (mk_py_isinstance_builtin other "list" range) man flow
             ~fthen:(fun flow ->
                 let var_sndels = match ekind other with
                   | E_py_object ({addr_kind = A_py_list a}, _) -> a
                   | _ -> assert false in
                 man.exec (mk_assign (mk_var var_els ~mode:WEAK range) (mk_var var_sndels ~mode:WEAK range) range) flow |>
                 man.eval (mk_py_none range)
               )
             ~felse:(fun flow ->
                 assume_eval (mk_py_isinstance_builtin other "range" range) man flow
                   ~fthen:(fun flow ->
                       (* TODO: more precision on top (for range) *)
                       man.exec (mk_assign (mk_var var_els ~mode:WEAK range) (mk_py_top T_int range) range) flow  |>
                       man.eval (mk_py_none range)
                     )
                   (* TODO: if object has iter field call it and then call next *)
                   ~felse:(fun flow ->
                       assume_eval (mk_py_isinstance_builtin other "list_reverseiterator" range) man flow
                         ~fthen:(fun flow ->
                             let var_sndels = match ekind other with
                               | E_py_object ({addr_kind = A_py_iterator (_, [{addr_kind = A_py_list a}], _)}, _) -> a
                               | _ -> assert false in
                             man.exec (mk_assign (mk_var var_els ~mode:WEAK range) (mk_var var_sndels ~mode:WEAK range) range) flow |>
                             man.eval (mk_py_none range)
                           )
                         ~felse:(fun flow ->
                             man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton)
                     )
               )
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.count")}, _)}, args, []) ->
      (* TODO: something smarter depending on the occurence of \gamma(element) in \gamma(list elements) ? *)
      Utils.check_instances ~arguments_after_check:1 man flow range args
        ["list"]
        (fun _ flow -> man.eval (mk_py_top T_int range) flow)
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.clear")}, _)}, args, []) ->
      Utils.check_instances man flow range args ["list"]
        (fun args flow ->
           let list = List.hd args in
           let var_els = match ekind list with
             | E_py_object ({addr_kind = A_py_list a}, _) ->a
             | _ -> Exceptions.panic_at range "%a@\n" pp_expr list in
           man.exec (mk_remove_var var_els range) flow |>
           man.eval (mk_py_none range)
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.index")}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 man flow range args
        ["list"]
        (fun args flow ->
           let eval_verror_f = man.exec (Utils.mk_builtin_raise "ValueError" range) flow in
           let flow = Flow.copy_ctx eval_verror_f flow in
           let eval_verror = Eval.empty_singleton eval_verror_f in
           let eval_res = man.eval (mk_py_top T_int range) flow in
           Eval.join_list (eval_res :: eval_verror :: []))
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.pop")}, _)} as call, [arg], []) ->
      let args' = arg :: (mk_constant T_int (C_int (Z.of_int (-1))) range) :: [] in
      man.eval {exp with ekind = E_py_call(call, args', [])} flow
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.pop")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["list"; "int"]
        (fun args flow ->
           let list = match args with l::_::[] -> l | _ -> assert false in
           let var_els = match ekind list with
             | E_py_object ({addr_kind = A_py_list a}, _) -> a
             | _ -> assert false in
           let eval_indexerror = man.exec (Utils.mk_builtin_raise "IndexError" range) flow
                                 |> Eval.empty_singleton in
           let eval_el = man.eval (mk_var ~mode:WEAK var_els range) flow in
           Eval.join_list (Eval.copy_ctx eval_indexerror eval_el :: eval_indexerror :: [])
        )
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.remove")}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 man flow range args
        ["list"]
        (fun args flow ->
           let eval_verror_f = man.exec (Utils.mk_builtin_raise "ValueError" range) flow in
           let eval_verror = Eval.empty_singleton eval_verror_f in
           let flow = Flow.copy_ctx eval_verror_f flow in
           let eval_none = man.eval (mk_py_none range) flow in
           Eval.join_list (eval_none :: eval_verror :: [])
        )
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.reverse")}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.sort")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["list"]
        (fun _ flow -> man.eval (mk_py_none range) flow)
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__getitem__")}, _)}, [list; index], []) ->
      let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
      Eval.eval_list man.eval [list; index] flow |>
      Eval.bind (fun exprs flow ->
          let list, index = match exprs with [l; i] -> l, i | _ -> assert false in
          assume_eval (mk_py_isinstance_builtin list "list" range) man flow
            ~fthen:(fun flow ->
                let var_els = match ekind list with
                  | E_py_object ({addr_kind = A_py_list a}, _) -> a
                  | _ -> assert false in
                assume_eval (mk_py_isinstance_builtin index "int" range) man flow
                  ~fthen:(fun flow ->
                      let indexerror_f = man.exec (Utils.mk_builtin_raise "IndexError" range) flow in
                      let indexerror = Eval.empty_singleton indexerror_f in
                      let flow = Flow.copy_ctx indexerror_f flow in
                      let evals = man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var var_els range) flow in
                      Eval.join_list (evals :: Eval.copy_ctx evals indexerror :: [])
                    )
                  ~felse:(fun flow ->
                      assume_eval (mk_py_isinstance_builtin index "slice" range) man flow
                        ~fthen:(fun flow ->
                            let slicedlist_var, flow = get_var_flow (Callstack.get flow, range) flow in
                            let flow = man.exec ~zone:Zone.Z_py (mk_assign (mk_var ~mode:WEAK slicedlist_var range) (mk_var ~mode:WEAK var_els range) range) flow in
                            let addr_list = mk_alloc_addr (A_py_list slicedlist_var) range in
                            man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_list flow |>
                            Eval.bind (fun eaddr_list flow ->
                                let addr_list = match ekind eaddr_list with
                                  | E_addr a -> a
                                  | _ -> assert false in
                                Eval.singleton (mk_py_object (addr_list, None) range) flow
                              )
                          )
                        ~felse:tyerror
                    )
              )
            ~felse:tyerror
        )
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__setitem__")}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:2 man flow range args
        ["list"]
        (fun args flow ->
           let list, index, value = match args with | [l; i; v] -> l, i, v | _ -> assert false in
           assume_eval (mk_py_isinstance_builtin index "int" range) man flow
             ~fthen:(fun flow ->
                 let var_els = match ekind list with
                   | E_py_object ({addr_kind = A_py_list a}, _) -> a
                   | _ -> assert false in
                 let indexerror_f = man.exec (Utils.mk_builtin_raise "IndexError" range) flow in
                 let flow = Flow.copy_ctx indexerror_f flow in

                 let assignment_f = man.exec (mk_assign (mk_var ~mode:WEAK var_els range) value range) flow in
                 let indexerror_f = Flow.copy_ctx assignment_f indexerror_f in

                 let assignment = man.eval (mk_py_none range) assignment_f in
                 let indexerror = Eval.empty_singleton indexerror_f in
                 Eval.join_list (assignment :: (Eval.copy_ctx assignment indexerror) ::[])
               )
             ~felse:(fun flow ->
                 assume_eval (mk_py_isinstance_builtin index "slice" range) man flow
                   ~fthen:(fun flow ->
                       man.eval (mk_py_call (mk_py_object (find_builtin "list.extend") range) [list; value] range) flow
                     )
                   ~felse:(fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |>
                                       Eval.empty_singleton)
               )
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__iter__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["list"]
        (fun args flow ->
           let list = match args with | [l] -> l | _ -> assert false in
           let list_addr = match ekind list with
             | E_py_object ({addr_kind = A_py_list _} as a, _) -> a
             | _ -> assert false in
           let a = mk_alloc_addr (A_py_iterator ("list_iterator", [list_addr], None)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) a flow |>
           Eval.bind (fun eaddr_it flow ->
               let addr_it = match ekind eaddr_it with | E_addr a -> a | _ -> assert false in
               Eval.singleton (mk_py_object (addr_it, None) range) flow
             )
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list_reverseiterator.__next__" as s))}, _)}, [iterator], [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list_iterator.__next__" as s))}, _)}, [iterator], []) ->
      (* todo: checks ? *)
      let it_name = String.sub s 0 (String.index s '.') in
      man.eval  ~zone:(Zone.Z_py, Zone.Z_py_obj) iterator flow |>
      Eval.bind (fun iterator flow ->
          let list_addr = match ekind iterator with
            | E_py_object ({addr_kind = A_py_iterator (s, [a], _)}, _) when s = it_name -> a
            | _ -> assert false in
          let var_els = match akind list_addr with
            | A_py_list a -> a
            | _ -> assert false in
          let els = man.eval (mk_var var_els ~mode:WEAK range) flow in
          let flow = Flow.set_ctx (Eval.choose_ctx els) flow in
          let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton in
          Eval.join_list (Eval.copy_ctx stopiteration els::stopiteration::[])
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list_reverseiterator.__iter__")}, _)}, [iterator], [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list_iterator.__iter__")}, _)}, [iterator], []) ->
      (* todo: checks ? *)
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) iterator flow |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__reversed__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["list"]
        (fun args flow ->
           let list = match args with | [l] -> l | _ -> assert false in
           let list_addr = match ekind list with
             | E_py_object ({addr_kind = A_py_list _} as a, _) -> a
             | _ -> assert false in
           let a = mk_alloc_addr (A_py_iterator ("list_reverseiterator", [list_addr], None)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) a flow |>
           Eval.bind (fun eaddr_it flow ->
               let addr_it = match ekind eaddr_it with | E_addr a -> a | _ -> assert false in
               Eval.singleton (mk_py_object (addr_it, None) range) flow
             )
        )
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__len__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["list"]
        (fun args flow ->
           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_top T_int range) flow
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "list.__contains__")}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 man flow range args ["list"]
        (fun args flow ->
           man.eval (mk_py_top T_bool range) flow)
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "math.fsum")}, _)}, args, []) ->
      let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
      Eval.eval_list man.eval args flow |>
      Eval.bind (fun args flow ->
          if List.length args >= 1 then
            let in_ty = List.hd args in
            assume_eval (mk_py_isinstance_builtin in_ty "list" range) man flow
              ~fthen:(fun flow ->
                  (* FIXME: we're assuming that we use the list abstraction *)
                  let var_els_in_ty = match ekind in_ty with
                    | E_py_object ({addr_kind = A_py_list a}, _) -> a
                    | _ -> assert false in
                  assume_eval (mk_py_isinstance_builtin (mk_var ~mode:WEAK var_els_in_ty range) "float" range) man flow
                    ~fthen:(man.eval (mk_py_top (T_float F_DOUBLE) range))
                    ~felse:tyerror)
              ~felse:tyerror
          else tyerror flow)
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "enumerate.__new__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        (* FIXME: first argument should be subclass of enumerate *)
        ["type"; "list"]
        (fun args flow ->
           let list = match args with | [_; l] -> l | _ -> assert false in
           let list_addr = match ekind list with
             | E_py_object ({addr_kind = A_py_list _} as a, _) -> a
             | _ -> assert false in
           let a = mk_alloc_addr (A_py_iterator ("enumerate", [list_addr], None)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) a flow |>
           Eval.bind (fun eaddr_it flow ->
               let addr_it = match ekind eaddr_it with | E_addr a -> a | _ -> assert false in
               Eval.singleton (mk_py_object (addr_it, None) range) flow
             )
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "enumerate.__iter__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["enumerate"]
        (fun args flow ->
           Eval.singleton (List.hd args) flow)
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "enumerate.__next__")}, _)}, [iterator], []) ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) iterator flow |>
      Eval.bind (fun iterator flow ->
          let list_addr = match ekind iterator with
            | E_py_object ({addr_kind = A_py_iterator (s, [a], _)}, _) when s = "enumerate" -> a
            | _ -> Exceptions.panic "%a@\n" pp_expr iterator in
          let var_els = match akind list_addr with
            | A_py_list a -> a
            | _ -> assert false in
          let els = man.eval (mk_expr (E_py_tuple [mk_top T_int range;
                                                   mk_var var_els ~mode:WEAK range]) range) flow in
          let flow = Flow.set_ctx (Eval.choose_ctx els) flow in
          let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton in
          Eval.join_list (Eval.copy_ctx stopiteration els::stopiteration::[])
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "zip.__new__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        (* FIXME: first argument should be subclass of enumerate *)
        ["type"; "list"; "list"]
        (fun args flow ->
           let list1, list2 = match args with | [_; l1; l2] -> l1, l2 | _ -> assert false in
           let list1_addr = match ekind list1 with
             | E_py_object ({addr_kind = A_py_list _} as a, _) -> a
             | _ -> assert false in
           let list2_addr = match ekind list2 with
             | E_py_object ({addr_kind = A_py_list _} as a, _) -> a
             | _ -> assert false in
           let a = mk_alloc_addr (A_py_iterator ("zip", [list1_addr; list2_addr], None)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) a flow |>
           Eval.bind (fun eaddr_it flow ->
               let addr_it = match ekind eaddr_it with
                 | E_addr a -> a
                 | _ -> assert false in
               Eval.singleton (mk_py_object (addr_it, None) range) flow
             )
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "zip.__iter__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["zip"]
        (fun args flow ->
           Eval.singleton (List.hd args) flow)
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "zip.__next__")}, _)}, [iterator], []) ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) iterator flow |>
      Eval.bind (fun iterator flow ->
          let list1_addr, list2_addr = match ekind iterator with
            | E_py_object ({addr_kind = A_py_iterator (s, [a1; a2], _)}, _) when s = "zip" -> a1, a2
            | _ -> Exceptions.panic "%a@\n" pp_expr iterator in
          let var_els1 = match akind list1_addr with
            | A_py_list a -> a
            | _ -> assert false in
          let var_els2 = match akind list2_addr with
            | A_py_list a -> a
            | _ -> assert false in
          let els = man.eval (mk_expr (E_py_tuple [mk_var var_els1 ~mode:WEAK range;
                                                   mk_var var_els2 ~mode:WEAK range]) range) flow in
          let flow = Flow.set_ctx (Eval.choose_ctx els) flow in
          let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton in
          Eval.join_list (Eval.copy_ctx stopiteration els::stopiteration::[])
        )
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.join")}, _)}, args, []) ->
      let tyerror = fun flow -> man.exec (Utils.mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
      Utils.check_instances man flow range args
        ["str"; "list"]
        (fun eargs flow ->
           let toinsert, iterable = match eargs with [t; i] -> t, i | _ -> assert false in
           let var_els_iterable = match ekind iterable with
             | E_py_object ({addr_kind = A_py_list a}, _) -> a
             | _ -> assert false in
           assume_eval (mk_py_isinstance_builtin (mk_var ~mode:WEAK var_els_iterable range) "str" range) man flow
             ~fthen:(man.eval (mk_py_top T_string range))
             ~felse:tyerror
        )
      |> Option.return

      (* the last case of str.split uses this list abstraction so every case is here... *)
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.split")}, _)} as call, [str], []) ->
        (* rewrite into str.split(str, " ", -1) *)
        let args' = (mk_constant T_string (C_string " ") range) :: (mk_constant T_int (C_int (Z.of_int 1)) range) :: [] in
        man.eval {exp with ekind = E_py_call(call, str :: args', [])} flow
        |> Option.return


      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.split")}, _)} as call , [str; split], []) ->
        (* rewrite into str.split(str, split, -1) *)
        let args' = (mk_constant T_int (C_int (Z.of_int 1)) range) :: [] in
        man.eval {exp with ekind = E_py_call(call, str :: split :: args', [])} flow
        |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.split")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["str"; "str"; "int"]
        (fun eargs flow ->
           man.eval (mk_expr (E_py_list [mk_py_top T_string range]) range) flow
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.splitlines")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["str"]
        (fun eargs flow ->
           man.eval (mk_expr (E_py_list [mk_py_top T_string range]) range) flow
        )
      |> Option.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "dir")}, _)}, args, []) ->
      Utils.check_instances man flow range args []
        (fun _ -> man.eval (mk_expr (E_py_list [mk_py_top T_string range]) range))
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "mopsa.assert_list_of")}, _)}, args, []) ->
      Eval.eval_list man.eval args flow |>
      Eval.bind (fun eargs flow ->
          let list, type_v = match eargs with [d;e] -> d,e | _ -> assert false in
          assume_eval (mk_py_isinstance_builtin list "list" range) man flow
            ~fthen:(fun flow ->
                let var = match ekind list with
                  | E_py_object ({addr_kind = A_py_list a}, _) -> a
                  | _ -> assert false in
                Libs.Py_mopsa.check man
                  (mk_py_isinstance (mk_var ~mode:WEAK var range) type_v range)
                  range flow
              )
            ~felse:(Libs.Py_mopsa.check man (mk_py_false range) range)
        )
      |> Option.return

    | _ -> None


  let exec zone stmt man flow = None

  let ask _ _ _ = None
end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
