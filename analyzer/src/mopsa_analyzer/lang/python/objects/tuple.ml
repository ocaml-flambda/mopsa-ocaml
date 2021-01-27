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

(** A general expansion-based abstraction for Python tuples *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast
open Data_container_utils

let name = "python.objects.tuple"

type addr_kind +=
  | A_py_tuple of int (* number of elements *)

let () = register_addr_kind_nominal_type (fun default ak ->
             match ak with
             | A_py_tuple _ -> "tuple"
             | _ -> default ak);
         register_addr_kind_structural_type (fun default ak s ->
             match ak with
             | A_py_tuple _ -> false
             | _ -> default ak s)


let () =
  register_is_data_container (fun default ak -> match ak with
      | A_py_tuple _ -> true
      | _ -> default ak)

let () =
  Format.(register_addr_kind {
      print = (fun default fmt a ->
          match a with
          | A_py_tuple i -> fprintf fmt "tuple[%d]" i
          | _ -> default fmt a);
      compare = (fun default a1 a2 ->
          match a1, a2 with
          | A_py_tuple t1, A_py_tuple t2 ->
             Stdlib.compare t1 t2
          | _ -> default a1 a2);})

let opt_py_tuple_allocation_policy : string ref = ref "all"
let () = Universal.Heap.Policies.register_option opt_py_tuple_allocation_policy name "-py-tuple-alloc-pol" "for expanded tuples"
           (fun default ak -> match ak with
                              | A_py_tuple _ ->
                                 (Universal.Heap.Policies.of_string !opt_py_tuple_allocation_policy) ak
                              | _ -> default ak)


module Domain =
struct

  include GenStatelessDomainId(struct
              let name = name
    end)

  let checks = []

  let init (prog:program) man flow = flow

  let var_of_addr a = match akind a with
    | A_py_tuple s ->
       let rec process i aux =
         if i = -1 then aux
         else process (i-1) ((mk_addr_attr a ("tuple[" ^ string_of_int i ^ "]") (T_py None))::aux)
       in process (s-1) []
    | _ -> assert false

  let var_of_eobj e = match ekind e with
    | E_py_object (a, _) -> var_of_addr a
    | _ -> assert false

  let get_eobj_itv man flow e =
    let mk_itv_from_z z  = Bot.Nb (ItvUtils.IntItv.cst z) in
    match ekind @@ Utils.extract_oobject e with
    | E_constant (C_int z) -> mk_itv_from_z z
    | E_unop (O_minus, {ekind=E_constant (C_int z)}) -> mk_itv_from_z (Z.neg z)
    | _ ->
       let e_num = Utils.extract_oobject e in
       debug "need to ask the numerical domain for the value of %a" pp_expr e_num;
       man.ask (Universal.Numeric.Common.mk_int_interval_query e_num) flow

  let rec eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_tuple els ->
      let addr_tuple = mk_alloc_addr (A_py_tuple (List.length els)) range in
      man.eval   addr_tuple flow >>$
 (fun eaddr_tuple flow ->
          let addr_tuple = Addr.from_expr eaddr_tuple in
          let els_vars = var_of_addr addr_tuple in
          let flow = List.fold_left2 (fun acc vari eli ->
              acc >>% man.exec
                (mk_assign (mk_var ~mode:(Some STRONG) vari range) eli range)) (Post.return flow) els_vars els in
          flow >>% Eval.singleton (mk_py_object (addr_tuple, None) range)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("tuple.__new__", _))}, _)}, cls::args, []) ->
      Utils.new_wrapper man range flow "tuple" cls
        ~fthennew:(man.eval (mk_expr ~etyp:(T_py None) (E_py_tuple []) range))


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("tuple.__contains__" as f, _))}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 f man flow range args
        ["tuple"]
        (fun eargs flow ->
           let tuple = List.hd eargs in
           let isin = List.hd (List.tl eargs) in
           let tuple_vars = var_of_eobj tuple in
           let mk_comp var = mk_binop ~etyp:(T_py None) (mk_var ~mode:(Some STRONG) var range) O_eq isin range in
           if List.length tuple_vars = 0 then
             man.eval   (mk_py_false range) flow
           else
             let or_expr = List.fold_left (fun acc var ->
                 mk_binop ~etyp:(T_py None) acc O_py_or (mk_comp var) range
               ) (mk_comp (List.hd tuple_vars)) (List.tl tuple_vars) in
           man.eval   or_expr flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("tuple.__getitem__" as f, _))}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 f man flow range args
        ["tuple"]
        (fun eargs flow ->
          let tuple = List.hd eargs in
          let tuple_vars = var_of_eobj tuple in
          let pos = List.hd @@ List.tl eargs in
          assume
            (mk_py_isinstance_builtin pos "int" range)
            man flow
            ~fthen:(fun flow ->
              let itv = get_eobj_itv man flow pos in
              let mk_itv_bound = ItvUtils.IntItv.of_bound_bot in
              let mk_itv = ItvUtils.IntItv.of_int_bot in
              debug "itv = %a" ItvUtils.IntItv.fprint_bot itv;
              let map_itv (f: int -> 'a) (itv: ItvUtils.IntItv.t_with_bot) : 'a list =
                match itv with
                | Bot.BOT -> []
                | Bot.Nb itv ->
                   List.map (fun x -> f (Z.to_int x)) (ItvUtils.IntItv.to_list itv) in
              let pos_accesses = ItvUtils.IntItv.meet_bot itv (mk_itv 0 (List.length tuple_vars - 1)) |>
                                   map_itv (fun ppos -> man.eval (mk_var ~mode:(Some STRONG) (List.nth tuple_vars ppos) range) flow) in
              let neg_accesses = ItvUtils.IntItv.meet_bot itv (mk_itv (- List.length tuple_vars) (-1)) |>
                                   map_itv (fun npos -> man.eval (mk_var ~mode:(Some STRONG) (List.nth tuple_vars (List.length tuple_vars + npos)) range) flow) in
              let ootb_accesses =
                if (ItvUtils.IntItv.intersect_bot itv (mk_itv_bound (ItvUtils.IntItv.B.MINF) (ItvUtils.IntItv.B.of_int (- List.length tuple_vars - 1)))) ||
                     (ItvUtils.IntItv.intersect_bot itv (mk_itv_bound (ItvUtils.IntItv.B.of_int (List.length tuple_vars)) (ItvUtils.IntItv.B.PINF))) then
                  (man.exec   (Utils.mk_builtin_raise_msg "IndexError" "tuple index out of range" range) flow >>%
                     Eval.empty) :: []
                else []
              in
              Eval.join_list ~empty:(fun () -> assert false)
                (ootb_accesses @ pos_accesses @ neg_accesses)
            )
            ~felse:(fun flow ->
              assume (mk_py_isinstance_builtin pos "slice" range) man flow
                ~fthen:(fun flow ->
                  let tuple_length = List.length tuple_vars in
                  man.eval (mk_py_call (mk_py_attr pos "indices" range) [mk_int tuple_length ~typ:(T_py None) range] range) flow >>$
                    fun tuple_indices flow ->
                    let get_nth n =
                      mk_py_call (mk_py_attr tuple_indices "__getitem__" range) [mk_int ~typ:(T_py None) n range] range in
                    Cases.bind_list [get_nth 0; get_nth 1; get_nth 2] (man.eval  ) flow |>
                      Cases.bind_result (fun sss flow ->
                          let itvs = List.map (get_eobj_itv man flow) sss in
                          if List.for_all (Bot.bot_apply (fun _ -> ItvUtils.IntItv.is_singleton) false) itvs then
                            let start, stop, step = match List.map (function Bot.Nb (ItvUtils.IntBound.Finite l, _) -> Z.to_int l | _ -> assert false) itvs with
                              | [a;b;c] -> a,b,c
                              | _ -> assert false in
                            let () = debug "slice, start=%d, stop=%d, step=%d" start stop step in
                            let sliced_tuple =
                              let rec slice pos acc =
                                if pos >= stop then List.rev acc
                                else
                                  slice (pos+step) (mk_var (List.nth tuple_vars pos) range :: acc)
                              in
                              slice start [] in
                            let () = debug "sliced_tuple = %a" (Format.pp_print_list pp_expr) sliced_tuple in
                            man.eval (mk_expr ~etyp:(T_py None) (E_py_tuple sliced_tuple) range) flow
                          else
                            panic_at range "FIXME: handle non-constant slices"
                        )
                )
                ~felse:(fun flow ->
                  let msg = Format.asprintf "tuple indices must be integers or slices, not %a" pp_addr_kind (akind @@ fst @@ object_of_expr pos) in
                  man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow >>%
                    Eval.empty
                )
            )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("tuple.__len__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["tuple"]
        (fun eargs flow ->
          let tuple = List.hd eargs in
          let tuple_vars = var_of_eobj tuple in
          man.eval (mk_int ~typ:(T_py (Some Int)) (List.length tuple_vars) range) flow
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("tuple.__iter__" as f, _))}, _)}, args, []) ->
       Utils.check_instances f man flow range args
         ["tuple"]
         (fun args flow ->
           let tuple = List.hd args in
           let addr_iterator = mk_alloc_addr (Py_list.A_py_iterator ("tuple_iterator", Some 0)) range in
           man.eval   addr_iterator flow >>$
             (fun addr_it flow ->
               let addr_it = Addr.from_expr addr_it in
               man.exec   (mk_assign (mk_var (Py_list.Domain.itseq_of_addr addr_it) range) tuple range) flow >>%
                 Eval.singleton (mk_py_object (addr_it, None) range)
             )
         )
       |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("tuple_iterator.__next__", _))}, _)}, [iterator], []) ->
      (* todo: checks? *)
      (* ugly assign iterator = iterator at pos+1... *)
      man.eval   iterator flow >>$
 (fun eiterator flow ->
          let tuple_it_addr, tuple_pos = match ekind eiterator with
            | E_py_object ({addr_kind = Py_list.A_py_iterator (s, d)} as addr, _) when s = "tuple_iterator" -> addr, d
            | _ -> assert false in
          man.eval   (mk_var (Py_list.Domain.itseq_of_eobj eiterator) range) flow >>$
 (fun tuple_eobj flow ->
                let vars_els = var_of_eobj tuple_eobj in
                match tuple_pos with
                | Some d when d < List.length vars_els ->
                   man.exec
                                (mk_rename (mk_addr tuple_it_addr range)
                                   (mk_addr {tuple_it_addr with addr_kind = Py_list.A_py_iterator ("tuple_iterator", Some (d+1))} range) range) flow >>%
                   man.eval (mk_var ~mode:(Some STRONG) (List.nth vars_els d) range)
                | _ ->
                   man.exec   (Utils.mk_builtin_raise "StopIteration" range) flow >>% Eval.empty
              )
        )
      |> OptionExt.return

    | E_py_annot {ekind = E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) } when get_orig_vname c.py_cls_a_var = "Tuple" ->
      let i = match ekind i with
        | E_py_tuple i -> i
        | _ -> assert false in
      let addr_tuple = mk_alloc_addr (A_py_tuple (List.length i)) range in
      man.eval   addr_tuple flow >>$
 (fun eaddr_tuple flow ->
          let addr_tuple = Addr.from_expr eaddr_tuple in
          let els_var = var_of_addr addr_tuple in
          let flow = List.fold_left2 (fun flow vari eli ->
                         flow >>% man.exec
                (mk_stmt (S_py_annot (mk_var ~mode:(Some STRONG) vari range, mk_expr ~etyp:(T_py None) (E_py_annot eli) range)) range)
            ) (Post.return flow) els_var i in
          flow >>% Eval.singleton (mk_py_object (addr_tuple, None) range)
        )
      |> OptionExt.return

    | _ -> None


  let exec stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_remove {ekind = E_addr ({addr_kind = A_py_tuple _} as a, _)} ->
       let vas = var_of_addr a in
       List.fold_left (fun flow v -> flow >>% man.exec   (mk_remove_var v range)) (Post.return flow) vas |> OptionExt.return

    | S_invalidate {ekind = E_addr ({addr_kind = A_py_tuple _} as a, _)} ->
       let vas = var_of_addr a in
       List.fold_left (fun flow v -> flow >>% man.exec   (mk_remove_var v range)) (Post.return flow) vas |> OptionExt.return

    | S_rename ({ekind = E_addr ({addr_kind = A_py_tuple _} as a, _)}, {ekind = E_addr (a', _)}) ->
      let vas = var_of_addr a in
      let vas' = var_of_addr a' in
      List.fold_left2 (fun flow v v' ->
          flow >>% man.exec   (mk_rename_var v v' range) )
        (Post.return flow) vas vas'
      |> OptionExt.return

    | S_fold ({ekind = E_addr ({addr_kind = A_py_tuple _} as a, _)}, [{ekind = E_addr (a', _)}]) ->
      let vas = var_of_addr a in
      let vas' = var_of_addr a' in
      List.fold_left2 (fun flow v v' ->
          flow >>% man.exec   (mk_fold_var v [v'] range))
        (Post.return flow) vas vas'
      |> OptionExt.return

    | S_expand ({ekind = E_addr ({addr_kind = A_py_tuple _} as a, _)}, [{ekind = E_addr (a', _)}]) ->
      let vas = var_of_addr a in
      let vas' = var_of_addr a' in
      List.fold_left2 (fun flow v v' ->
          flow >>% man.exec   (mk_expand_var v [v'] range))
        (Post.return flow) vas vas'
      |> OptionExt.return

    | _ -> None



  let ask : type r. ('a, r) query -> ('a, unit) man -> 'a flow -> r option =
    fun query man flow ->
    match query with
    | Q_variables_linked_to ({ekind = E_addr ({addr_kind = A_py_tuple _} as addr, _)} as e) ->
       let range = erange e in
       OptionExt.return @@
         List.fold_left (fun vset var ->
             VarSet.union (VarSet.add var vset) (man.ask (Q_variables_linked_to (mk_var var range)) flow)
           ) VarSet.empty (var_of_addr addr)

    | Universal.Ast.Q_debug_addr_value ({addr_kind = A_py_tuple _} as addr) ->
       let open Framework.Engines.Interactive in
       let vars_tuple = var_of_addr addr in
       let contents = List.map (fun var ->
                          match vkind var with
                          | V_addr_attr (_, attr) -> (attr, man.ask (Q_debug_variable_value var) flow)
                          | _ -> assert false) vars_tuple in
       Some {var_value = None;
             var_value_type = (T_py None);
             var_sub_value = Some (Named_sub_value contents)}

    | _ -> None

  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
