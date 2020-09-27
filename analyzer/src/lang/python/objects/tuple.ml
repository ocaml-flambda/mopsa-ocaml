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

  let addr_of_expr exp = match ekind exp with
    | E_addr a -> a
    | _ -> Exceptions.panic "%a@\n" pp_expr exp


  let rec eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_tuple els ->
      let addr_tuple = mk_alloc_addr (A_py_tuple (List.length els)) range in
      man.eval   addr_tuple flow >>$
 (fun eaddr_tuple flow ->
          let addr_tuple = addr_of_expr eaddr_tuple in
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
      Utils.check_instances f man flow range args
        ["tuple"; "int"]
        (fun eargs flow ->
          (* FIXME: ask the intervals rather than do this hack *)
           let exception Nonconstantinteger  in
           let tuple = List.hd eargs in
           let tuple_vars = var_of_eobj tuple in
           try
             debug "expr = %a" pp_expr (List.hd (List.tl args));
             let pos = match ekind (List.hd (List.tl args)) with
               | E_constant (C_int z) -> Z.to_int z
               | E_unop (O_minus, {ekind=E_constant (C_int z)}) -> - Z.to_int z
               | _ -> raise Nonconstantinteger in
             if 0 <= pos && pos < List.length tuple_vars then
               man.eval   (mk_var ~mode:(Some STRONG) (List.nth tuple_vars pos) range) flow
             else if 0 > pos && pos >= - List.length tuple_vars then
               man.eval (mk_var ~mode:(Some STRONG) (List.nth tuple_vars (List.length tuple_vars + pos)) range) flow
             else
               man.exec   (Utils.mk_builtin_raise_msg "IndexError" "tuple index out of range" range) flow >>%
               Eval.empty
           with Nonconstantinteger ->
             Eval.join_list ~empty:(fun () -> assert false)
               ((man.exec   (Utils.mk_builtin_raise_msg "IndexError" "tuple index out of range" range) flow >>%
                 Eval.empty)
                :: (List.map (fun var -> man.eval   (mk_var ~mode:(Some STRONG) var range) flow) tuple_vars))
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
               let addr_it = match ekind addr_it with
                 | E_addr a -> a
                 | _ -> assert false in
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
          let addr_tuple = addr_of_expr eaddr_tuple in
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
    | S_remove {ekind = E_addr ({addr_kind = A_py_tuple _} as a)} ->
       let vas = var_of_addr a in
       List.fold_left (fun flow v -> flow >>% man.exec   (mk_remove_var v range)) (Post.return flow) vas |> OptionExt.return

    | S_invalidate {ekind = E_addr ({addr_kind = A_py_tuple _} as a)} ->
       let vas = var_of_addr a in
       List.fold_left (fun flow v -> flow >>% man.exec   (mk_remove_var v range)) (Post.return flow) vas |> OptionExt.return

    | S_rename ({ekind = E_addr ({addr_kind = A_py_tuple _} as a)}, {ekind = E_addr a'}) ->
      let vas = var_of_addr a in
      let vas' = var_of_addr a' in
      List.fold_left2 (fun flow v v' ->
          flow >>% man.exec   (mk_rename_var v v' range) )
        (Post.return flow) vas vas'
      |> OptionExt.return

    | S_fold ({ekind = E_addr ({addr_kind = A_py_tuple _} as a)}, [{ekind = E_addr a'}]) ->
      let vas = var_of_addr a in
      let vas' = var_of_addr a' in
      List.fold_left2 (fun flow v v' ->
          flow >>% man.exec   (mk_fold_var v [v'] range))
        (Post.return flow) vas vas'
      |> OptionExt.return

    | S_expand ({ekind = E_addr ({addr_kind = A_py_tuple _} as a)}, [{ekind = E_addr a'}]) ->
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

  let pretty_print _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
