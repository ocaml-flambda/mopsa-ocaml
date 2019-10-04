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

(* A general expansion-based abstraction for Python tuples, (hopefully)
   irrelevant of the value/type domain *)

open Mopsa
open Sig.Domain.Stateless
open Ast
open Addr
open Universal.Ast
open Data_container_utils

type addr_kind +=
  | A_py_tuple of Rangeset.t list
  (* variables where the expanded elements are stored *)

let () =
  register_join_akind (fun default ak1 ak2 ->
      match ak1, ak2 with
      | A_py_tuple ts1, A_py_tuple ts2 -> A_py_tuple (List.map2 Rangeset.union ts1 ts2)
      | _ -> default ak1 ak2);
  register_is_data_container (fun default ak -> match ak with
      | A_py_tuple _ -> true
      | _ -> default ak)

let () =
  Format.(register_addr_kind {
      print = (fun default fmt a ->
          match a with
          | A_py_tuple vars -> fprintf fmt "tuple[%a]" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") (fun fmt -> Rangeset.iter (fun ra -> pp_range fmt ra))) vars
          | _ -> default fmt a);
      compare = (fun default a1 a2 ->
          match a1, a2 with
          | A_py_tuple t1, A_py_tuple t2 ->
            Compare.list Rangeset.compare t1 t2
          | _ -> default a1 a2);})


module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.objects.tuple"
    end)

  let interface = {
    iexec = {provides = [Zone.Z_py_obj]; uses = [Zone.Z_py_obj]};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj; Universal.Zone.Z_u_heap, Z_any]}
  }

  let init (prog:program) man flow = flow

  let var_of_addr a = match akind a with
    | A_py_tuple s -> List.mapi (fun i _ -> mk_addr_attr a ("tuple[" ^ string_of_int i ^ "]") T_any) s
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
    | E_py_tuple els ->
      let addr_tuple = mk_alloc_addr (A_py_tuple (List.map (fun _ -> Rangeset.singleton range) els)) range in
      man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_tuple flow |>
      Eval.bind (fun eaddr_tuple flow ->
          let addr_tuple = addr_of_expr eaddr_tuple in
          let els_vars = var_of_addr addr_tuple in
          let flow = List.fold_left2 (fun acc vari eli ->
              man.exec ~zone:Zone.Z_py
                (mk_assign (mk_var ~mode:STRONG vari range) eli range) acc) flow els_vars els in
          Eval.singleton (mk_py_object (addr_tuple, None) range) flow
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("tuple.__contains__" as f))}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 f man flow range args
        ["tuple"]
        (fun eargs flow ->
           let tuple = List.hd eargs in
           let isin = List.hd (List.tl eargs) in
           let tuple_vars = var_of_eobj tuple in
           let mk_comp var = mk_binop (mk_var ~mode:STRONG var range) O_eq isin range in
           if List.length tuple_vars = 0 then
             man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_false range) flow
           else
             let or_expr = List.fold_left (fun acc var ->
                 mk_binop acc O_py_or (mk_comp var) range
               ) (mk_comp (List.hd tuple_vars)) (List.tl tuple_vars) in
           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) or_expr flow
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("tuple.__getitem__" as f))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["tuple"; "int"]
        (fun eargs flow ->
           let tuple = List.hd eargs in
           let pos = match ekind (List.hd (List.tl args)) with
             | E_constant (C_int z) -> Z.to_int z
             | _ -> Exceptions.panic "tuple.__getitem__ over non-constant integer" in
           let tuple_vars = var_of_eobj tuple in
           if 0 <= pos && pos < List.length tuple_vars then
             let () = debug "ok@\n" in
             man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_var ~mode:STRONG (List.nth tuple_vars pos) range) flow
           else
             man.exec ~zone:Zone.Z_py_obj (Utils.mk_builtin_raise_msg "IndexError" "tuple index out of range" range) flow |>
             Eval.empty_singleton
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("tuple.__iter__" as f))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["tuple"]
        (fun args flow ->
           let tuple = List.hd args in
           let tuple_addr = match ekind tuple with
             | E_py_object ({addr_kind = A_py_tuple _} as a, _) -> a
             | _ -> assert false in
           let addr_iterator = mk_alloc_addr (Py_list.A_py_iterator ("tuple_iterator", [tuple_addr], Some 0)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_iterator flow |>
           Eval.bind (fun addr_it flow ->
               let addr_it = match ekind addr_it with
                 | E_addr a -> a
                 | _ -> assert false in
               Eval.singleton (mk_py_object (addr_it, None) range) flow
             )
        )
      |> Option.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "tuple_iterator.__next__")}, _)}, [iterator], []) ->
      (* todo: checks? *)
      (* ugly assign iterator = iterator at pos+1... *)
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) iterator flow |>
      Eval.bind (fun eiterator flow ->
          let tuple_it_addr, tuple_addr, tuple_pos = match ekind eiterator with
            | E_py_object ({addr_kind = Py_list.A_py_iterator (s, [a], d)} as addr, _) when s = "tuple_iterator" -> addr, a, d
            | _ -> assert false in
          let vars_els = var_of_addr tuple_addr in
          match tuple_pos with
          | Some d when d < List.length vars_els ->
            let () = debug "exec incoming@\n" in
            let flow = man.exec
                         (mk_rename (mk_addr tuple_it_addr range)
                            (mk_addr {tuple_it_addr with addr_kind = Py_list.A_py_iterator ("tuple_iterator", [tuple_addr], Some (d+1))} range) range) flow in
            man.eval (mk_var ~mode:STRONG (List.nth vars_els d) range) flow
          | _ ->
            man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton
        )
      |> Option.return



    | _ -> None


  let exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_rename ({ekind = E_addr ({addr_kind = A_py_tuple _} as a)}, {ekind = E_addr a'}) ->
      let vas = var_of_addr a in
      let vas' = var_of_addr a' in
      List.fold_left2 (fun flow v v' ->
          man.exec ~zone:Zone.Z_py (mk_rename_var v v' range) flow)
        flow vas vas'
      |> Post.return |> Option.return
    | _ -> None


  let ask _ _ _ = None
end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
