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

(* A general smashing abstraction for Python lists, independent from
   the value/type domain *)

(* currently, lists are smashed into one variable abstracting all
   its elements. To avoid allocating a new variable each time
   (especially during loops), we allocate them only if there has
   been not other allocation at the same point. This is quite
   similar to the recency abstraction.
*)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast
open Callstack
open Data_container_utils

let name = "python.objects.list"

type addr_kind +=
  | A_py_list
  | A_py_iterator of string (* iterator kind (list_iterator, ...) *)
                     * int option (* potential position in the iterator, just used for tuples I think... FIXME *)


let () =
  register_is_data_container (fun default ak -> match ak with
      | A_py_list -> true
      | A_py_iterator _ -> true
      | _ -> default ak)


let () =
  Format.(register_addr_kind {
      print = (fun default fmt a ->
          match a with
          | A_py_list -> fprintf fmt "list"
          | A_py_iterator (s, d) ->
            begin match d with
            | None -> fprintf fmt "%s" s
            | Some d -> fprintf fmt "%s#%d" s d end
          | _ -> default fmt a);
      compare = (fun default a1 a2 ->
          match a1, a2 with
          | A_py_iterator (s1, d1), A_py_iterator (s2, d2) ->
            Compare.compose [
              (fun () -> Stdlib.compare s1 s2);
              (fun () -> Compare.option Stdlib.compare d1 d2);
            ]
          | _ -> default a1 a2);})


let () = register_addr_kind_nominal_type (fun default ak ->
             match ak with
             | A_py_list -> "list"
             | A_py_iterator (s, _) -> s
             | _ -> default ak);
         register_addr_kind_structural_type (fun default ak s ->
             match ak with
             | A_py_list | A_py_iterator _ -> false
             | _ -> default ak s)


let opt_py_list_allocation_policy : string ref = ref "all"
let () = Universal.Heap.Policies.register_option opt_py_list_allocation_policy name "-py-list-alloc-pol" "for smashed lists"
           (fun default ak -> match ak with
                              | A_py_list
                              | A_py_iterator _ ->
                                 (Universal.Heap.Policies.of_string !opt_py_list_allocation_policy) ak
                              | _ -> default ak)

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = name
    end)

  let checks = []

  let init (prog:program) man flow = flow

  let itindex_var_of_addr a =
    let v = match akind a with
    | A_py_iterator ("list_iterator", _) -> mk_addr_attr a "it_index" (T_py None)
    | A_py_iterator ("list_reverseiterator", _) -> mk_addr_attr a "it_index" (T_py None)
    | _ -> assert false in
    Utils.change_var_type T_int v

  let itseq_of_addr a = mk_addr_attr a "it_seq" (T_py None)

  let itseq_of_eobj e = match ekind e with
    | E_py_object (a, _) -> itseq_of_addr a
    | _ -> assert false

  (* FIXME: ugly fix for zip  iterators *)
  let itseq2_of_addr a = mk_addr_attr a "it_seq2" (T_py None)

  let itseq2_of_eobj e = match ekind e with
    | E_py_object (a, _) -> itseq2_of_addr a
    | _ -> assert false


  let length_var_of_addr a = match akind a with
    | A_py_list ->
       mk_addr_attr a "list_length" (T_py None) |> Utils.change_var_type T_int
    | _ -> assert false

  let var_of_addr a = match akind a with
    | A_py_list ->
       {(mk_addr_attr a "list" (T_py None)) with vmode = WEAK}
    | _ -> assert false

  let var_of_eobj e = match ekind e with
    | E_py_object (a, _) -> var_of_addr a
    | _ -> assert false

  let length_var_of_eobj e = match ekind e with
    | E_py_object (a, _) -> length_var_of_addr a
    | _ -> assert false

  let itindex_var_of_eobj e = match ekind e with
    | E_py_object (a, _) -> itindex_var_of_addr a
    | _ -> assert false

  let rec eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_list ls ->
      debug "Skipping list.__new__, list.__init__ for now@\n";

      let addr_list = mk_alloc_addr A_py_list range in
      man.eval addr_list flow >>$
        (fun eaddr_list flow ->
            let addr_list = Addr.from_expr eaddr_list in
            let els_var = var_of_addr addr_list in
            let flow = List.fold_left (fun acc el ->
                           let stmt = mk_assign (mk_var els_var range) el range in
                           acc >>% man.exec   stmt) (Post.return flow) ls in
            flow >>%
            man.exec  (mk_assign (mk_var (length_var_of_addr addr_list) range) (mk_int (List.length ls) ~typ:T_int range) range) >>%
              Eval.singleton (mk_py_object (addr_list, None) range)
          )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__getitem__", _))}, _)}, args, []) ->
       Utils.bind_list_args man args flow range (fun args flow ->
           let eargs = List.map (fun x -> mk_var x range) args in
           let list, index = match eargs with [l; i] -> l, i | _ -> assert false in
           assume (mk_py_isinstance_builtin list "list" range) man flow
             ~fthen:(fun flow ->
               assume (mk_py_isinstance_builtin index "int" range) man flow
                 ~fthen:(fun flow ->
                   Cases.bind_list [list; index] man.eval flow |>
                     Cases.bind_result (fun eargs flow ->
                         let list, index = match eargs with [l; i] -> l, i | _ -> assert false in
                         let var_els = var_of_eobj list in
                         let length_list = length_var_of_eobj list in
                         assume
                           (mk_binop ~etyp:T_bool
                              (mk_binop ~etyp:T_bool (Utils.extract_oobject index) O_lt (mk_var length_list range) range)
                              O_log_and
                              (mk_binop ~etyp:T_bool (mk_unop O_minus (mk_var length_list range) ~etyp:T_int range) O_le (Utils.extract_oobject index)  range)
                              range
                           )
                           man flow
                           ~fthen:(man.eval (mk_var var_els range))
                           ~felse:(fun flow ->
                             man.exec (Utils.mk_builtin_raise "IndexError" range) flow >>%
                               Eval.empty
                           )
                       )
                 )
                 ~felse:(fun flow ->
                   assume (mk_py_hasattr index "__index__" range) man flow
                     ~fthen:(fun flow ->
                       Cases.bind_list [list; mk_py_call (mk_py_attr index "__index__" range) [] range] (man.eval  ) flow |>
                         Cases.bind_result (fun eargs flow ->
                             let list, index = match eargs with [l; i] -> l, i | _ -> assert false in
                             debug "list = %a, index = %a" pp_expr list pp_expr index;
                             let var_els = var_of_eobj list in
                             let length_list = length_var_of_eobj list in
                             assume
                               (mk_binop ~etyp:T_bool
                                  (mk_binop ~etyp:T_bool (Utils.extract_oobject index) O_lt (mk_var length_list range) range)
                                  O_log_and
                                  (mk_binop ~etyp:T_bool (mk_unop O_minus (mk_var length_list range) ~etyp:T_int range) O_le (Utils.extract_oobject index)  range)
                                  range
                               )
                               man flow
                               ~fthen:(man.eval (mk_var var_els range))
                               ~felse:(fun flow ->
                                 man.exec (Utils.mk_builtin_raise "IndexError" range) flow >>%
                                   Eval.empty
                               )
                           )
                     )
                     ~felse:(fun flow ->
                       assume (mk_py_isinstance_builtin index "slice" range) man flow
                         ~fthen:(fun flow ->
                           debug "slice!";
                           let addr_list = mk_alloc_addr A_py_list range in
                           man.eval   addr_list flow >>$
                             (fun eaddr_list flow ->
                               let addr_list = Addr.from_expr eaddr_list in
                               let slicedlist_var = var_of_addr addr_list in
                               man.eval list   flow >>$
                                 (fun list flow ->
                                   man.eval (mk_py_call (mk_py_attr index "indices" range) [mk_py_call (mk_py_attr list "__len__" range) [] range] range)   flow >>$
                                     (fun tuple_indices flow ->
                                       let get_nth n =
                                         mk_py_call (mk_py_attr tuple_indices "__getitem__" range) [mk_int ~typ:(T_py None) n range] range in
                                       Cases.bind_list [get_nth 0; get_nth 1; get_nth 2] (man.eval  ) flow |>
                                         Cases.bind_result (fun sss flow ->
                                             let start, stop, step = match List.map Utils.extract_oobject sss with
                                               | [a;b;c] -> a, b, c
                                               | _ -> assert false in
                                             let var_els = var_of_eobj list in
                                             let new_length = mk_var (length_var_of_addr addr_list) range in
                                             flow |>
                                               man.exec (mk_assign (mk_var slicedlist_var range) (mk_var var_els range) range) >>%
                                               switch
                                                 [
                                                   [
                                                     mk_binop ~etyp:T_bool step O_lt (mk_zero ~typ:T_int range) range;
                                                     mk_binop ~etyp:T_bool stop O_lt start range
                                                   ],
                                                   (fun flow -> man.exec
                                                                  (mk_assign new_length (mk_binop ~etyp:T_int
                                                                                           (
                                                                                             mk_binop ~etyp:T_int
                                                                                               (mk_binop ~etyp:T_int
                                                                                                  start
                                                                                                  O_minus
                                                                                                  (mk_binop ~etyp:T_int stop O_plus (mk_one ~typ:T_int range) range)
                                                                                                  range)
                                                                                               O_div
                                                                                               (mk_unop O_minus ~etyp:T_int step range)
                                                                                               range
                                                                                           )
                                                                                           O_plus
                                                                                           (mk_one ~typ:T_int range)
                                                                                           range
                                                                     ) range) flow);

                                                   [
                                                     mk_not (mk_binop ~etyp:T_bool step O_lt (mk_zero ~typ:T_int range) range) range;
                                                     mk_binop ~etyp:T_bool start O_lt stop range
                                                   ],
                                                   (fun flow -> man.exec
                                                                  (mk_assign new_length (mk_binop ~etyp:T_int
                                                                                           (
                                                                                             mk_binop ~etyp:T_int
                                                                                               (mk_binop ~etyp:T_int
                                                                                                  stop
                                                                                                  O_minus
                                                                                                  (mk_binop ~etyp:T_int
                                                                                                     start
                                                                                                     O_plus
                                                                                                     (mk_one ~typ:T_int range)
                                                                                                     range)
                                                                                                  range)
                                                                                               O_div
                                                                                               step
                                                                                               range
                                                                                           )
                                                                                           O_plus
                                                                                           (mk_one ~typ:T_int range)
                                                                                           range)
                                                                     range) flow
                                                   );

                                                   [mk_binop ~etyp:T_bool
                                                      (mk_binop ~etyp:T_bool (mk_binop ~etyp:T_bool step O_lt (mk_zero ~typ:T_int range) range) O_log_and (mk_not (mk_binop ~etyp:T_bool stop O_lt start range) range) range)
                                                      O_log_or
                                                      (mk_binop ~etyp:T_bool (mk_not (mk_binop ~etyp:T_bool step O_lt (mk_zero ~typ:T_int range) range) range) O_log_and (mk_not (mk_binop ~etyp:T_bool start O_lt stop range) range) range)
                                                      range
                                                   ],
                                                   (fun flow -> man.exec
                                                                  (mk_assign new_length (mk_zero ~typ:T_int range) range) flow
                                                   )
                                                 ]
                                                 man
                                             >>$ fun () flow ->
                                                 Eval.singleton (mk_py_object (addr_list, None) range) flow
                                           )
                                     )
                                 )
                             )
                         )
                         ~felse:(fun flow ->
                           man.eval index   flow >>$
                             (fun index flow ->
                               let msg = Format.asprintf "list indices must be integers or slices, not %a" pp_addr_kind (akind @@ fst @@ object_of_expr index) in
                               man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow >>%
                                 Eval.empty
                             )
                         )
                     )
                 )
             )
             ~felse:(fun flow ->
               let msg = Format.asprintf "descriptor '__getitem__' requires a 'list' object but received %a" pp_addr_kind (akind @@ fst @@ object_of_expr list) in
               man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow >>%
                 Eval.empty
             )
         )
       |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__add__" as f, _))}, _)}, args, []) ->
       Utils.bind_list_args man args flow range (fun vargs flow ->
           let evargs = List.map (fun x -> mk_var x range) vargs in
           Utils.check_instances f man flow range evargs
             ["list"; "list"]
             (fun _ flow ->
               (* First, allocate new addr for the list, and new addr for the list elements *)
               (* Then assign the el addr to both addresses above *)
               let addr_list = mk_alloc_addr A_py_list range in
               man.eval   addr_list flow >>$
                 (fun list_addr flow ->
                     let alist_addr = Addr.from_expr list_addr in
                     let els_res_var = var_of_addr alist_addr in
                     let els_res_length = length_var_of_addr alist_addr in
                     Cases.bind_list evargs (man.eval  ) flow |>
                       Cases.bind_result (fun exprs flow ->
                           let listl, listr = match exprs with [l; r] -> l, r | _ -> assert false in
                           let elsl_var = var_of_eobj listl in
                           let elsr_var = var_of_eobj listr in
                           let elsl_length = length_var_of_eobj listl in
                           let elsr_length = length_var_of_eobj listr in

                           let flow = List.fold_left (fun acc el ->
                                          acc >>% man.exec   (mk_assign (mk_var els_res_var range) el range) )
                                        (Post.return flow) [mk_var elsl_var range;
                                                            mk_var elsr_var range] in
                           flow >>%
                           man.exec  (mk_assign (mk_var els_res_length range)
                                                                    (mk_binop (mk_var elsl_length range) O_plus (mk_var elsr_length range) ~etyp:T_int range) range)  >>%
                             Eval.singleton (mk_py_object (alist_addr, None) range)
                         )
                   )
             )
         )
       |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__iadd__", _))}, _)}, args, []) ->
      (* let's rewrite list.__iadd__(s, t) into list.extend(s, t) && return s *)
      man.eval   (mk_py_call (mk_py_object (find_builtin "list.extend") range) args range) flow
      >>$ (fun nonety flow ->
          man.eval   (List.hd args) flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__setitem__" as f, _))}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:2 f man flow range args
        ["list"]
        (fun args flow ->
           let list, index, value = match args with | [l; i; v] -> l, i, v | _ -> assert false in
           assume (mk_py_isinstance_builtin index "int" range) man flow
             ~fthen:(fun flow ->
                 let var_els = var_of_eobj list in
                 let length_els = length_var_of_eobj list in
                 assume
                   (mk_binop ~etyp:T_bool
                      (mk_binop ~etyp:T_bool (Utils.extract_oobject index) O_lt (mk_var length_els range) range)
                      O_log_and
                      (mk_binop ~etyp:T_bool (mk_unop ~etyp:T_int O_minus (mk_var length_els range) range) O_le (Utils.extract_oobject index) range)
                      range
                   )
                    man flow
                   ~fthen:(fun flow ->
                       man.exec   (mk_assign (mk_var var_els range) value range) flow >>% man.eval   (mk_py_none range)
                     )
                   ~felse:(fun flow ->
                       man.exec (Utils.mk_builtin_raise_msg "IndexError" "list assignment index out of range" range) flow >>%
                       Eval.empty
                     )
               )
             ~felse:(fun flow ->
                 assume (mk_py_isinstance_builtin index "slice" range) man flow
                   ~fthen:(fun flow ->
                       man.eval (mk_py_call (mk_py_object (find_builtin "list.extend") range) [list; value] range) flow
                     )
                   ~felse:(fun flow ->
                       let msg = Format.asprintf "list indices must be integers or slices, not %a" pp_addr_kind (akind @@ fst @@ object_of_expr index) in
                       man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow >>%
                       Eval.empty
                     )
               )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin (f, _))}, _)}, args, [])
      when is_compare_op_fun "list" f ->
      Utils.check_instances ~arguments_after_check:1 f man flow range args ["list"]
        (fun eargs flow ->
           let e1, e2 = match args with [l; r] -> l, r | _ -> assert false in
           assume (mk_py_isinstance_builtin e2 "list" range) man flow
             ~fthen:(man.eval (mk_py_top (T_py (Some Bool)) range))
             ~felse:(fun flow ->
                 let expr = mk_constant ~etyp:(T_py (Some NotImplemented)) C_py_not_implemented range in
                 man.eval expr flow)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__mul__" as f, _))}, _)}, args, [])
      | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__rmul__" as f, _))}, _)}, args, []) ->
       Utils.bind_list_args man args flow range (fun vargs flow ->
           let evargs = List.map (fun x -> mk_var x range) vargs in
           Utils.check_instances f man flow range evargs ["list"; "int"]
             (fun _ flow ->
               let addr_list = mk_alloc_addr A_py_list range in
               (* WOOPS: this alloc may trigger an address renaming invalidating els_list and len_list *)
               man.eval   addr_list flow >>$
                 (fun eaddr_list flow ->
                     let addr_list = Addr.from_expr eaddr_list in
                     let els_var = var_of_addr addr_list in
                     let els_len = length_var_of_addr addr_list in
                     Cases.bind_list evargs (man.eval  ) flow |>
                       Cases.bind_result (fun exprs flow ->
                           let list, int = match exprs with [l; r] -> l, r | _ -> assert false in
                           let els_list = var_of_eobj list in
                           let len_list = length_var_of_eobj list in

                           man.exec   (mk_assign (mk_var els_var range) (mk_var els_list range) range) flow >>%
                           man.exec  (mk_assign (mk_var els_len range)
                                                                    (mk_binop (mk_var len_list range) O_mult (Utils.extract_oobject int) ~etyp:T_int range) range) >>%
                             Eval.singleton (mk_py_object (addr_list, None) range)
                         )
                   )
             )
         )
       |> OptionExt.return

    | E_py_object ({addr_kind = A_py_list}, e) ->
      Eval.singleton exp flow |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.append" as f, _))}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 f man flow range args
        ["list"]
        (fun args flow ->
           let list, element = match args with | [l; e] -> l, e | _ -> assert false in
           let var_els = var_of_eobj list in
           let len_els = length_var_of_eobj list in
           man.exec   (mk_assign (mk_var var_els range) element range) flow >>%
           man.exec  (mk_assign (mk_var len_els range)
                                       (mk_binop (mk_var len_els range) O_plus (mk_int 1 range) ~etyp:T_int range) range) >>%
           man.eval (mk_py_none range))
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.insert" as f, _))}, _)}, args, []) ->
      Utils.check_instances f ~arguments_after_check:1 man flow range args
        ["list"; "int"]
        (fun args flow ->
           let list, index, element = match args with | [l; i; e] -> l, i, e | _ -> assert false in
           let var_els = var_of_eobj list in
           let len_els = length_var_of_eobj list in
           flow |>
           man.exec   (mk_assign (mk_var var_els range) element range) >>%
           man.exec  (mk_assign (mk_var len_els range)
                                       (mk_binop (mk_var len_els range) O_plus (mk_int 1 range) ~etyp:T_int range) range) >>%
           man.eval (mk_py_none range))
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__new__", _))}, _)}, cls::args, []) ->
      Utils.new_wrapper man range flow "list" cls
        ~fthennew:(man.eval (mk_expr ~etyp:(T_py None) (E_py_list []) range))

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__init__" as f, _))}, _)}, args, [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.extend" as f, _))}, _)}, args, []) ->
      Utils.check_instances f ~arguments_after_check:1 man flow range args
        ["list"]
        (fun eargs flow ->
           (* FIXME: check manually (with ekind list) that we have a list or list_iterator as we are in the same abstract domain? *)
           let list, other = match eargs with e1::e2::[] -> e1, e2 | _ -> assert false in
           let var_els = var_of_eobj list in
           let len_els = length_var_of_eobj list in
           assume (mk_py_isinstance_builtin other "list" range) man flow
             ~fthen:(fun flow ->
                 let var_sndels = var_of_eobj other in
                 let len_sndels = length_var_of_eobj other in
                 flow |>
                 man.exec   (mk_assign (mk_var var_els range) (mk_var var_sndels range) range) >>%
                 man.exec  (mk_assign (mk_var len_els range)
                             (mk_binop (mk_var len_els range) O_plus (mk_var len_sndels range) ~etyp:T_int range) range) >>%
                 man.eval (mk_py_none range)
               )
             ~felse:(fun flow ->
                 assume (mk_py_isinstance_builtin other "range" range) man flow
                   ~fthen:(fun flow ->
                     (* FIXME: length precision *)
                     (* TODO: more precision on top (for range) *)
                     let ra a = mk_py_attr other a range in
                     let flow =
                       flow |>
                         man.exec   (mk_assign (mk_var var_els range) (mk_py_top T_int range) range) >>%
                         man.exec   (mk_assume (mk_binop ~etyp:(T_py None)
                                                (mk_binop ~etyp:(T_py None) (ra "start") O_le (mk_var var_els range) range)
                                                O_py_and
                                                (mk_binop ~etyp:(T_py None) (mk_var var_els range) O_lt (ra "stop") range) range) range) in
                     flow >>%
                     man.eval
                       (mk_py_call (mk_py_object (find_builtin_function "len") range) [other] range) >>$
                       (fun len flow ->
                           man.exec  (mk_assign (mk_var len_els range) (Utils.extract_oobject len) range) flow >>%
                             man.eval   (mk_py_none range)
                         )
                     )
                   (* TODO: if object has iter field call it and then call next *)
                   ~felse:(fun flow ->
                       assume (mk_py_isinstance_builtin other "list_reverseiterator" range) man flow
                         ~fthen:(fun flow ->
                           man.eval   (mk_var (itseq_of_eobj other) range) flow >>$
                             (fun list_eobj flow ->
                                 let var_sndels = var_of_eobj list_eobj in
                                 let len_sndels = length_var_of_eobj list_eobj in
                                 flow |>
                                   man.exec   (mk_assign (mk_var var_els range) (mk_var var_sndels range) range) >>%
                                   man.exec  (mk_assign (mk_var len_els range)
                                                                            (mk_binop (mk_var len_els range) O_plus (mk_var len_sndels range) ~etyp:T_int range) range) >>%
                                   man.eval (mk_py_none range)
                               )
                           )
                         ~felse:(fun flow ->
                             assume (mk_py_isinstance_builtin other "zip" range) man flow
                               ~fthen:(fun flow ->
                                 man.eval   (mk_var (itseq_of_eobj other) range) flow >>$
 (fun l1_eobj flow ->
                                       man.eval   (mk_var (itseq2_of_eobj other) range) flow >>$
 (fun l2_eobj flow ->
                                             let var_l1 = var_of_eobj l1_eobj in
                                             let len_l1 = length_var_of_eobj l1_eobj in
                                             let var_l2 = var_of_eobj l2_eobj in
                                             let len_l2 = length_var_of_eobj l2_eobj in
                                             flow |>
                                               man.exec (mk_assign (mk_var var_els range) (mk_expr ~etyp:(T_py None) (E_py_tuple [mk_var var_l1 range; mk_var var_l2 range]) range) range) >>%
                                               man.exec (mk_assign (mk_var len_els range)
                                                           (mk_py_call (mk_py_object (find_builtin "min") range) [mk_var len_l1 range; mk_var len_l2 range] range)
                                                           range) >>%
                                               man.eval (mk_py_none range)
                                           )
                                     )
                                 )
                               ~felse:(fun flow ->
                                 let msg = Format.asprintf "%a is not iterable" pp_expr list in
                                   man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow >>% Eval.empty)
                                 )
                     )
               )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.count" as f, _))}, _)}, args, []) ->
      (* TODO: something smarter depending on the occurence of \gamma(element) in \gamma(list elements) ? *)
      Utils.check_instances ~arguments_after_check:1 f man flow range args
        ["list"]
        (fun _ flow -> man.eval (mk_py_top T_int range) flow)
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.clear" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args ["list"]
        (fun args flow ->
           let list = List.hd args in
           let var_els = var_of_eobj list in
           let len_els = length_var_of_eobj list in
           flow |>
           man.exec   (mk_remove_var var_els range) >>%
           man.exec  (mk_assign (mk_var len_els range) (mk_int 0 range) range) >>%
           man.eval (mk_py_none range)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.index" as f, _))}, _)}, args, []) ->
      Utils.check_instances f ~arguments_after_check:1 man flow range args
        ["list"]
        (fun args flow ->
          let msg = Format.asprintf "%a is not in list" pp_expr (List.hd @@ List.tl args) in
           let eval_verror_f = man.exec (Utils.mk_builtin_raise_msg "ValueError" msg range) flow |> post_to_flow man in
           let flow = Flow.copy_ctx eval_verror_f flow in
           let eval_verror = Eval.empty eval_verror_f in
           let eval_res = man.eval (mk_py_top T_int range) flow in
           Eval.join_list ~empty:(fun () -> Eval.empty flow) (eval_res :: eval_verror :: []))
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.pop", _))}, _)} as call, [arg], []) ->
      let args' = arg :: (mk_int ~typ:(T_py (Some Int)) (-1) range) :: [] in
      man.eval {exp with ekind = E_py_call(call, args', [])} flow
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.pop" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["list"; "int"]
        (fun args flow ->
           let list, popindex = match args with l::i::[] -> l, i | _ -> assert false in
           let var_els = var_of_eobj list in
           let len_els = length_var_of_eobj list in
           assume
             (mk_binop ~etyp:T_bool
                (mk_binop ~etyp:T_bool (Utils.extract_oobject popindex) O_lt (mk_var len_els range) range)
                O_log_and
                (mk_binop ~etyp:T_bool (mk_unop ~etyp:T_int O_minus (mk_var len_els range)  range) O_le (Utils.extract_oobject popindex)  range)
                range
             )
              man flow
             ~fthen:(fun flow ->
                 flow |>
                 man.exec  (mk_assign (mk_var len_els range)
                                             (mk_binop  ~etyp:T_int (mk_var len_els range) O_minus (mk_int 1 range)range) range) >>%
                 man.eval (mk_var var_els range)
               )
             ~felse:(fun flow ->
               assume (mk_binop ~etyp:T_bool (mk_var len_els range) O_eq (mk_int 0 range) range)

                   man flow
                   ~fthen:(fun flow ->
                       man.exec (Utils.mk_builtin_raise_msg "IndexError" "pop from empty list" range) flow >>% Eval.empty)
                   ~felse:(fun flow ->
                       man.exec (Utils.mk_builtin_raise_msg "IndexError" "pop index out of range" range) flow >>% Eval.empty)
               )
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.remove" as f, _))}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 f man flow range args
        ["list"]
        (fun args flow ->
           let len_list = mk_var (length_var_of_eobj @@ List.hd args) range in
           let eval_verror_f = man.exec (Utils.mk_builtin_raise_msg "ValueError" "list.remove(x): x not in list" range) flow |> post_to_flow man in
           let eval_verror = Eval.empty eval_verror_f in
           let flow = Flow.copy_ctx eval_verror_f flow in
           let eval_none =
             man.exec (mk_assign len_list (mk_binop ~etyp:T_int len_list O_minus (mk_int ~typ:T_int 1 range) range) range) flow >>%
             man.eval (mk_py_none range) in
           Eval.join_list ~empty:(fun () -> Eval.empty flow) (eval_none :: eval_verror :: [])
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.reverse" as f, _))}, _)}, args, kwargs)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.sort" as f, _))}, _)}, args, kwargs) ->
      if kwargs <> [] then warn_at range "list.sort/reverse kwargs ignored";
      Utils.check_instances f man flow range args
        ["list"]
        (fun _ flow -> man.eval (mk_py_none range) flow)
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__iter__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["list"]
        (fun args flow ->
           let list = match args with | [l] -> l | _ -> assert false in
           let a = mk_alloc_addr (A_py_iterator ("list_iterator", None)) range in
           man.eval   a flow >>$
 (fun eaddr_it flow ->
                 let addr_it = Addr.from_expr eaddr_it in
                 flow |>
                   man.exec   (mk_assign (mk_var (itseq_of_addr addr_it) range) list range) >>%
                   man.exec  (mk_assign (mk_var (itindex_var_of_addr addr_it) range) (mk_int 0 range) range) >>%
                   Eval.singleton (mk_py_object (addr_it, None) range)
             )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list_reverseiterator.__next__", _))}, _)}, [iterator], [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list_iterator.__next__", _))}, _)}, [iterator], []) ->
       (* it_pos in reverse iterator + value analysis? *)
      (* todo: checks ? *)
      man.eval    iterator flow >>$
 (fun iterator flow ->
            man.eval   (mk_var (itseq_of_eobj iterator) range) flow >>$
 (fun list_eobj flow ->
                  let list_addr = match ekind list_eobj with
                    | E_py_object (a, _) -> a
                    | _ -> assert false in
                  let var_els = var_of_addr list_addr in
                  let len_els = length_var_of_addr list_addr in
                  let it_pos = itindex_var_of_eobj iterator in
                  assume (mk_binop ~etyp:T_bool (mk_var it_pos range) O_lt (mk_var len_els range) range)
                     man flow
                    ~fthen:(fun flow ->
                      let els = man.eval (mk_var var_els range) flow in
                      OptionExt.none_to_exn @@ bind_opt (fun case flow ->
                                                   Some begin match case with
                                                     (* FIXME *)
                                                     | Empty ->
                                                        warn_at range "does this still happen in the types?";
                                                        man.exec (Utils.mk_builtin_raise "StopIteration" range) flow >>% Eval.empty

                                                     | Result (e,_,_) ->
                                                        man.exec
                                                          (mk_assign (mk_var it_pos range)
                                                             (mk_binop (mk_var it_pos range) O_plus (mk_int 1 range) ~etyp:T_int range) range) flow >>%
                                                        Eval.singleton e

                                                     | NotHandled -> assert false
                                                     end
                                                 ) els
                    )
                    ~felse:(fun flow ->
                      man.exec (Utils.mk_builtin_raise "StopIteration" range) flow >>% Eval.empty
                    )
                )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list_reverseiterator.__iter__", _))}, _)}, [iterator], [])
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list_iterator.__iter__", _))}, _)}, [iterator], []) ->
      (* todo: checks ? *)
      man.eval   iterator flow |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__reversed__" as f, _))}, _)}, args, []) ->
       (* FIXME: values *)
      Utils.check_instances f man flow range args
        ["list"]
        (fun args flow ->
           let list = match args with | [l] -> l | _ -> assert false in
           let a = mk_alloc_addr (A_py_iterator ("list_reverseiterator", None)) range in
           man.eval   a flow >>$
 (fun eaddr_it flow ->
               (* FIXME list_reverseiterator index *)
               let addr_it = Addr.from_expr eaddr_it in
               flow |>
                 man.exec   (mk_assign (mk_var (itseq_of_addr addr_it) range) list range) >>%
                 man.exec  (mk_assign (mk_var (itindex_var_of_addr addr_it) range) (mk_int 0 range) range) >>%
               Eval.singleton (mk_py_object (addr_it, None) range)
             )
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__len__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["list"]
        (fun args flow ->
          man.eval   (mk_py_top T_int range) flow >>$
 (fun eint flow ->
                match ekind eint with
                | E_py_object (addr, _) ->
                   Eval.singleton {eint with ekind = E_py_object (addr, Some {(mk_var (length_var_of_eobj @@ List.hd args) range) with etyp=T_int})} flow
                | _ -> assert false)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("list.__contains__" as f, _))}, _)}, args, []) ->
       (* /!\ if remove is called, contains should be unprecise
          l = ['a', 'b', 'c']
          l.remove('a')
          assert(not l.contains('a'))
        *)
      Utils.check_instances f ~arguments_after_check:1 man flow range args ["list"]
        (fun args flow ->
          let list, el = match args with a::b::[] -> a, b | _ -> assert false in
          assume
            (mk_binop ~etyp:(T_py None) (mk_var (var_of_eobj list) range) O_eq el range)
            man flow
            ~fthen:(man.eval   (mk_py_top T_bool range))
            ~felse:(man.eval   (mk_py_false range))
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("math.fsum", _))}, _)}, args, []) ->
      bind_list args man.eval flow |>
      bind_result (fun args flow ->
          if List.length args = 1 then
            let in_ty = List.hd args in
            assume (mk_py_isinstance_builtin in_ty "list" range) man flow
              ~fthen:(fun flow ->
                  (* FIXME: we're assuming that we use the list abstraction *)
                  let var_els_in_ty = var_of_eobj in_ty in
                  assume (mk_py_isinstance_builtin (mk_var var_els_in_ty range) "float" range) man flow
                    ~fthen:(man.eval (mk_py_top (T_float F_DOUBLE) range))
                    ~felse:(fun flow ->
                        man.exec (Utils.mk_builtin_raise_msg "TypeError" "must be real number" range) flow >>%
                        Eval.empty
                      )
                )
              ~felse:(fun flow ->
                let msg = Format.asprintf "%a is not iterable" pp_expr in_ty in
                  man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow >>%
                  Eval.empty
                )
          else
            let msg = Format.asprintf "fsum() takes exactly one argument (%d given)" (List.length args) in
            man.exec (Utils.mk_builtin_raise_msg "TypeError" msg range) flow >>%
            Eval.empty
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("enumerate.__new__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        (* FIXME: first argument should be subclass of enumerate *)
        ["type"; "list"]
        (fun args flow ->
           let list = match args with | [_; l] -> l | _ -> assert false in
           let a = mk_alloc_addr (A_py_iterator ("enumerate", None)) range in
           man.eval   a flow >>$
 (fun eaddr_it flow ->
               let addr_it = Addr.from_expr eaddr_it in
               man.exec   (mk_assign (mk_var (itseq_of_addr addr_it) range) list range) flow >>%
               Eval.singleton (mk_py_object (addr_it, None) range)
             )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("enumerate.__iter__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["enumerate"]
        (fun args flow ->
           Eval.singleton (List.hd args) flow)
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("enumerate.__next__", _))}, _)}, [iterator], []) ->
       (* FIXME: new py_iterator + values *)
      man.eval   iterator flow >>$
        (fun iterator flow ->
          man.eval   (mk_var (itseq_of_eobj iterator) range) flow >>$
            (fun list_eobj flow ->
              let var_els = var_of_eobj list_eobj in
              (* FIXME: els is bounded by the size of the list *)
              let els = Utils.mk_positive_integer man flow range
                          (fun posint flow ->
                            man.eval (mk_expr ~etyp:(T_py None)
                                        (E_py_tuple [posint; mk_var var_els range])
                                        range) flow ) in
              let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow >>% Eval.empty in
              Cases.join_list ~empty:(fun () -> assert false) (Cases.copy_ctx stopiteration els :: stopiteration :: [])
            )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("zip.__new__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        (* FIXME: first argument should be subclass of enumerate *)
        ["type"; "list"; "list"]
        (fun args flow ->
           let list1, list2 = match args with | [_; l1; l2] -> l1, l2 | _ -> assert false in
           let a = mk_alloc_addr (A_py_iterator ("zip", None)) range in
           man.eval   a flow >>$
             (fun eaddr_it flow ->
               let addr_it = Addr.from_expr eaddr_it in
               flow |>
                 man.exec   (mk_assign (mk_var (itseq_of_addr  addr_it) range) list1 range) >>%
                 man.exec   (mk_assign (mk_var (itseq2_of_addr addr_it) range) list2 range) >>%
               Eval.singleton (mk_py_object (addr_it, None) range)
             )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("zip.__iter__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["zip"]
        (fun args flow ->
           Eval.singleton (List.hd args) flow)
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("zip.__next__", _))}, _)}, [iterator], []) ->
      man.eval   iterator flow >>$
 (fun iterator flow ->
            Cases.bind_list (List.map (fun x -> mk_var x range) [itseq_of_eobj iterator; itseq2_of_eobj iterator])
              (man.eval  ) flow |>
              Cases.bind_result (fun its flow ->
                  let list1_eobj, list2_eobj = match its with [a; b] -> a, b | _ -> assert false in
                  let var_els1 = var_of_eobj list1_eobj in
                  let var_els2 = var_of_eobj list2_eobj in
                  let els = man.eval (mk_expr ~etyp:(T_py None) (E_py_tuple [mk_var var_els1 range;
                                                           mk_var var_els2 range]) range) flow in
                  let flow = Flow.set_ctx (Cases.get_ctx els) flow in
                  let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow >>% Eval.empty in
                  Eval.join_list ~empty:(fun () -> Eval.empty flow) (Cases.copy_ctx stopiteration els::stopiteration::[])
                )
          )
      |> OptionExt.return

    (* the last case of str.split uses this list abstraction so every case is here... *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.split", _))}, _)} as call, [str], []) ->
      (* rewrite into str.split(str, " ", -1) *)
      let args' = (mk_constant ~etyp:(T_py (Some Str)) (C_string " ") range) :: (mk_int (-1) ~typ:(T_py None) range) :: [] in
      man.eval {exp with ekind = E_py_call(call, str :: args', [])} flow
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.split", _))}, _)} as call , [str; split], []) ->
      (* rewrite into str.split(str, split, -1) *)
      let args' = (mk_int (-1) ~typ:(T_py None) range) :: [] in
      man.eval {exp with ekind = E_py_call(call, str :: split :: args', [])} flow
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.split" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["str"; "str"; "int"]
        (fun eargs flow ->
          let module Powerset = Universal.Strings.Powerset in
          let self, sep, maxsplit = match eargs with
            | a :: b :: c :: [] -> a, b, c
            | _ -> assert false in
          let u_self = man.ask (Powerset.mk_strings_powerset_query (Utils.extract_oobject self)) flow in
          let u_sep = man.ask (Powerset.mk_strings_powerset_query (Utils.extract_oobject sep)) flow in
          let u_maxsplit = Utils.get_eobj_itv man flow maxsplit in
            let maxsplit = match (Bot.bot_to_exn u_maxsplit) with
              | ItvUtils.IntBound.Finite l, _ ->
                 let l = Z.to_int l in
                 if l < 0 then None else Some l
              | _ -> assert false in
          let module StringPower = Powerset.StringPower in
          debug "self = %a, sep = %a, maxsplit = %a"
            (format StringPower.print) u_self
            (format StringPower.print) u_sep
            ItvUtils.IntItv.fprint_bot u_maxsplit;
          (* if the separator is a char and maxsplit is a constant, let's work on u_self *)
          (* additional assumptions on u_self and u_sep could be lifted if needed *)
          if not (StringPower.is_top u_self) && not (StringPower.is_top u_sep) && StringPower.for_all (fun s -> String.length s = 1) u_sep && ItvUtils.IntItv.is_singleton (Bot.bot_to_exn u_maxsplit) && StringPower.cardinal u_self = 1 && StringPower.cardinal u_sep = 1 then
            let splits =
              let sep = String.get (StringPower.choose u_sep) 0 in
              let splitted = String.split_on_char sep (StringPower.choose u_self) in
              let rec adjust_size count l acc =
                if count = 0 then
                  List.rev ((String.concat (String.make 1 sep) l) :: acc)
                else
                  match l with
                  | [] -> List.rev acc
                  | hd :: tl -> adjust_size (count-1) tl (hd::acc)
              in
              adjust_size (match maxsplit with None -> String.length (StringPower.choose u_self) | Some l -> l) splitted [] in
            let () = debug "splits: %a" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") Format.pp_print_string) splits in
            man.eval (mk_expr ~etyp:(T_py None) (E_py_list (List.map (fun s -> mk_string ~etyp:(T_py None) s range) splits)) range) flow
          else
            (* otherwise, let's return a list of T strings, of size <= maxsplit or length u_self *)
            let addr_list = mk_alloc_addr A_py_list range in
            man.eval addr_list flow >>$
              (fun eaddr_list flow ->
                let addr_list = Addr.from_expr eaddr_list in
                let els_var = var_of_addr addr_list in
                let length_var = length_var_of_addr addr_list in
                let length_itv  = match maxsplit with
                  | None ->
                     if StringPower.is_top u_self then
                       (* FIXME: positive range *)
                       mk_top T_int range
                     else
                       mk_int_interval 0 (StringPower.fold (fun string upper ->
                                 max (String.length string) upper) u_self 0) ~typ:T_int range
                  | Some l ->
                     mk_int_interval 0 l ~typ:T_int range in
                Post.return flow >>%
                  man.exec (mk_assign (mk_var els_var range) (mk_py_top T_string range) range) >>%
                  man.exec (mk_assign (mk_var length_var range) length_itv range) >>%
                  Eval.singleton (mk_py_object (addr_list, None) range)
              )
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("bytes.split", _))}, _)} as call, [str], []) ->
      (* rewrite into str.split(str, " ", -1) *)
      let args' = (mk_py_top (T_py (Some Bytes)) range) :: (mk_int (-1) ~typ:(T_py (Some Int)) range) :: [] in
      man.eval {exp with ekind = E_py_call(call, str :: args', [])} flow
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("bytes.split", _))}, _)} as call , [str; split], []) ->
      (* rewrite into str.split(str, split, -1) *)
      let args' = (mk_int (-1) ~typ:(T_py (Some Int)) range) :: [] in
      man.eval {exp with ekind = E_py_call(call, str :: split :: args', [])} flow
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("bytes.split" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["bytes"; "bytes"; "int"]
        (fun eargs flow ->
           (* FIXME: notok, as one strong element. Fixed by adding to tops, but terrible *)
           man.eval (mk_expr ~etyp:(T_py None) (E_py_list [mk_py_top (T_py (Some Bytes)) range; mk_py_top (T_py (Some Bytes)) range]) range) flow
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("str.splitlines" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["str"]
        (fun eargs flow ->
           man.eval (mk_expr ~etyp:(T_py None) (E_py_list [mk_py_top T_string range]) range) flow
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dir" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args []
        (fun _ -> man.eval (mk_expr ~etyp:(T_py None) (E_py_list [mk_py_top T_string range]) range))
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("mopsa.assert_list_of", _))}, _)}, args, []) ->
      bind_list args man.eval flow |>
      bind_result (fun eargs flow ->
          let list, type_v = match eargs with [d;e] -> d,e | _ -> assert false in
          assume (mk_py_isinstance_builtin list "list" range) man flow
            ~fthen:(fun flow ->
                let var = var_of_eobj list in
                Libs.Py_mopsa.check man
                  (mk_py_isinstance (mk_var var range) type_v range)
                  range flow
              )
            ~felse:(Libs.Py_mopsa.check man (mk_py_false range) range)
        )
      |> OptionExt.return

    | E_py_annot {ekind = E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) } when get_orig_vname c.py_cls_a_var = "List" ->
      let addr_list = mk_alloc_addr A_py_list range in
      man.eval   addr_list flow >>$
 (fun eaddr_list flow ->
          let addr_list = Addr.from_expr eaddr_list in
          let els_var = var_of_addr addr_list in
          let len_var = length_var_of_addr addr_list in
          let stmt = mk_stmt (S_py_annot (mk_var els_var range, mk_expr ~etyp:(T_py None) (E_py_annot i) range)) range in
          flow |>
            man.exec   stmt >>%
            man.exec  (mk_assign (mk_var len_var range) (mk_top T_int range) range) >>%
            Eval.singleton (mk_py_object (addr_list, None) range)
        )
      |> OptionExt.return

    | E_py_check_annot (tocheck, {ekind = E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) }) when get_orig_vname c.py_cls_a_var = "List" ->
      debug "s_py_check_annot list";
      assume (mk_py_isinstance_builtin tocheck "list" range) man flow
        ~fthen:(fun flow ->
            man.eval   tocheck flow |>
            bind_result (fun iterator flow ->
                let list_addr = match ekind iterator with
                  | E_py_object ({addr_kind = A_py_list} as a, _) -> a
                  | _ -> Exceptions.panic "should be a list: %a@\nflow = %a@\n" pp_expr iterator (format (Flow.print man.lattice.print)) flow in
                let var_els = var_of_addr list_addr in
                man.eval (mk_expr ~etyp:(T_py None) (E_py_check_annot (mk_var var_els range, i)) range) flow
              )
          )
        ~felse:(fun flow ->
            man.eval   (mk_py_false range) flow
          )
      |> OptionExt.return

    | _ -> None



  let exec stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_remove {ekind = E_addr ({addr_kind = A_py_list} as a, _)} ->
       let va = var_of_addr a in
       let la = length_var_of_addr a in
       flow |>
         man.exec   (mk_remove_var va range) >>%
         man.exec  (mk_remove_var la range) >>%
         Post.return |> OptionExt.return

    | S_remove {ekind = E_addr ({addr_kind = A_py_iterator (kind, _)} as a, _)} ->
       let va = itseq_of_addr a in
       let flow = man.exec   (mk_remove_var va range) flow in
       flow >>%
         (
           if kind = "list_iterator" || kind = "list_reverseiterator" then
             man.exec  (mk_remove_var (itindex_var_of_addr a) range)
           else
             (fun x -> Post.return x)
         )
       |> OptionExt.return

    | S_rename ({ekind = E_addr ({addr_kind = A_py_iterator (kind, _)} as a, _)}, {ekind = E_addr (a', _)}) ->
       let va = itseq_of_addr a in
       let va' = itseq_of_addr a' in
       flow |>
         (
           if kind = "list_iterator" || kind = "list_reverseiterator" then
             man.exec  (mk_rename_var (itindex_var_of_addr a) (itindex_var_of_addr a') range)
           else
             (fun x -> Post.return x)
         ) >>%
         man.exec   (mk_rename_var va va' range) |> OptionExt.return

    | S_invalidate {ekind = E_addr ({addr_kind = A_py_iterator (kind, _)} as a, _)} ->
       let va = itseq_of_addr a in
       flow |>
         man.exec   (mk_remove_var va range) >>%
         (
           if kind = "list_iterator" || kind = "list_reverseiterator" then
             man.exec  (mk_remove_var (itindex_var_of_addr a) range)
           else
             fun x -> Post.return x
         ) |>
         OptionExt.return

    | S_expand ({ekind = E_addr ({addr_kind = A_py_iterator (kind, _)} as a, _)}, addrs)
      | S_fold ({ekind = E_addr ({addr_kind = A_py_iterator (kind, _)} as a, _)}, addrs) ->
       let mk_stmt = match skind stmt with
         | S_expand _ -> mk_expand_var
         | S_fold _  -> mk_fold_var
         | _ -> assert false in
       let va = itseq_of_addr a in
       let vas = List.map (fun ea' ->
                     match ekind ea' with
                     | E_addr ({addr_kind = A_py_iterator (kind', _)} as a', _) when kind = kind' -> itseq_of_addr a'
                     | _ -> assert false) addrs in
       flow |>
         man.exec   (mk_stmt va vas range) >>%
         ( if kind = "list_iterator" || kind = "list_reverseiterator" then
             man.exec
               (mk_stmt (itindex_var_of_addr a)
                  (List.map (fun ea' ->
                     match ekind ea' with
                     | E_addr ({addr_kind = A_py_iterator (kind', _)} as a', _) when kind = kind' -> itindex_var_of_addr a'
                     | _ -> assert false) addrs)
                  range)
           else
             fun x -> Post.return x )
       |> OptionExt.return

    | S_invalidate {ekind = E_addr ({addr_kind = A_py_list} as a, _)} ->
       let va = var_of_addr a in
       let la = length_var_of_addr a in
       flow |>
         man.exec   (mk_remove_var va range) >>%
         man.exec  (mk_remove_var la range) |>
         OptionExt.return


    | S_expand ({ekind = E_addr ({addr_kind = A_py_list} as a, _)}, addrs)
      | S_fold ({ekind = E_addr ({addr_kind = A_py_list} as a, _)}, addrs) ->
       Debug.debug ~channel:"addrenv" "fold py_list";
       let mk_stmt = match skind stmt with
         | S_expand _ -> mk_expand_var
         | S_fold _ -> mk_fold_var
         | _ -> assert false in
       let va = var_of_addr a in
       let la = length_var_of_addr a in
       let vas, las = List.split @@ List.map (fun ea' -> match ekind ea' with
                                      | E_addr ({addr_kind = A_py_list} as a', _) -> var_of_addr a', length_var_of_addr a'
                                      | _ -> assert false) addrs in
       flow |>
         man.exec   (mk_stmt va vas range) >>%
         man.exec  (mk_stmt la las range) |>
         OptionExt.return

    | S_rename ({ekind = E_addr ({addr_kind = A_py_list} as a, _)}, {ekind = E_addr (a', _)}) ->
      (* FIXME: I guess we could just do it for every data_container. Maybe add a data_container domain on top of them performing the renaming?*)
      (* working on lists entails smashed element variable being index by the address, meaning we need to rename them *)
      let va = var_of_addr a in
      let la = length_var_of_addr a in
      let va' = var_of_addr a' in
      let la' = length_var_of_addr a' in
      debug "renaming %a into %a@\n" pp_var va pp_var va';
      flow |>
      man.exec   (mk_rename_var va va' range) >>%
      man.exec  (mk_rename_var la la' range) |>
      OptionExt.return
      (* FIXME: now we need to do the same for iterators based on this address, but it's complicated *)
      (* let to_rename =
       *     man.ask (Universal.Heap.Recency.Q_select_allocated_addresses
       *                            (fun addr -> match akind addr with
       *                               | A_py_iterator (_, l, _) ->
       *                                 List.exists (fun a_l -> compare_addr a_l a = 0) l
       *                               | _ -> false)
       *                         )
       *                 flow
       *     in
       *     List.fold_left (fun flow iterator ->
       *     let new_iterator = match akind iterator with
       *       | A_py_iterator (name, addrs, pos) ->
       *          let new_addrs = List.map (fun addr ->
       *                              if compare_addr addr a = 0 then a' else addr) addrs in
       *          {iterator with addr_kind = A_py_iterator(name, new_addrs, pos);
       *                         addr_mode = a'.addr_mode
       *          }
       *       | _ -> assert false
       *     in
       *     debug "renaming %a into %a" pp_addr iterator pp_addr new_iterator;
       *     man.exec ~zone:Universal.Zone.Z_u_heap (mk_rename (mk_addr iterator range) (mk_addr new_iterator range) range) flow
       *   ) flow to_rename *)

    | _ -> None


  let ask : type r. ('a, r) query -> ('a, unit) man -> 'a flow -> r option =
    fun query man flow ->
    match query with
    | Q_variables_linked_to ({ekind = E_addr ({addr_kind = A_py_list} as addr, _)} as e) ->
       let range = erange e in
       let content_var = var_of_addr addr in
       let length_var = length_var_of_addr addr in
       man.ask (Q_variables_linked_to (mk_var content_var range)) flow |>
         VarSet.add length_var |>
         VarSet.add content_var |>
         OptionExt.return

    | Universal.Ast.Q_debug_addr_value ({addr_kind = A_py_list} as addr) ->
       let open Framework.Engines.Interactive in
       let content_list = man.ask (Q_debug_variable_value (var_of_addr addr)) flow in
       let length_list =
         let itv = man.ask (Universal.Numeric.Common.mk_int_interval_query (mk_var (length_var_of_addr addr) (Location.mk_fresh_range ()))) flow in
         {var_value = Some (Format.asprintf "%a" Universal.Numeric.Common.pp_int_interval itv);
          var_value_type = T_int;
          var_sub_value = None} in
       Some {var_value = None;
             var_value_type = T_any;
             var_sub_value = Some (Named_sub_value
                                     (("list contents", content_list)::
                                     ("list length", length_list)::[]))
         }

    | _ -> None

  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
