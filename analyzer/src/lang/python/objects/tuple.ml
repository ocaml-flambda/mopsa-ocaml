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
open Ast
open Addr
open Universal.Ast

type addr_kind +=
  | A_py_tuple of var list (* variable where the smashed elements are stored *)

let () =
  Format.(register_addr {
      print = (fun default fmt a ->
          match a with
          | A_py_tuple vars -> fprintf fmt "tuple[%a]" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_var) vars
          | _ -> default fmt a);
      compare = (fun default a1 a2 ->
          match a1, a2 with
          | A_py_tuple t1, A_py_tuple t2 ->
            Compare.list compare_var t1 t2
          | _ -> default a1 a2);})


module Domain =
struct

  type _ domain += D_python_objects_tuple : unit domain

  let id = D_python_objects_tuple
  let name = "python.objects.tuple"
  let identify : type a. a domain -> (unit, a) eq option = function
    | D_python_objects_tuple -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  module VarInfo = struct type t = var list let compare = Compare.list compare_var let print = Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_var end
  module TupleInfo = struct
    type tuple_size = int
    type t = Callstack.cs * range * tuple_size
    let compare (cs, r, s) (cs', r', s') =
      Compare.compose
        [
          (fun () -> Callstack.compare cs cs');
          (fun () -> compare_range r r');
          (fun () -> Pervasives.compare s s')
        ]
    let print fmt (cs, r, s) =
      Format.fprintf fmt "(%a, %a, %d)"
        Callstack.pp_call_stack cs
        pp_range r
        s
  end

  module Equiv = Equiv.Make(TupleInfo)(VarInfo)

  type ('a, _) Annotation.key +=
    | KTupleInfo : ('a, Equiv.t) Annotation.key

  let () =
    Annotation.(register_stateless_annot {
        eq = (let f: type a b. (a, b) key -> (Equiv.t, b) eq option =
                function
                | KTupleInfo -> Some Eq
                | _ -> None
              in
              f);
        print = (fun fmt m -> Format.fprintf fmt "Tuple annots: @[%a@]" Equiv.print m);
      }) ();
    ()


  let fresh_expanded_vars range d =
    let rec gen_aux pos acc =
      if pos < 0 then acc else
        gen_aux (pos-1) ((mkfresh (fun uid -> Format.asprintf "$t:%a:%d" pp_range range pos) T_any ())::acc)
    in gen_aux (d-1) []

  let get_var_equiv ((cs, range, size) as info: TupleInfo.t) (e: Equiv.t) =
    try
      Equiv.find_l info e, e
    with Not_found ->
      let vars = fresh_expanded_vars range size in
      let new_eq = Equiv.add (info, vars) e in
      vars, new_eq

  let get_var_flow (info: TupleInfo.t) (f: 'a flow) : var list * 'a flow =
    let a = Flow.get_annot KTupleInfo f in
    let var, a = get_var_equiv info a in
    var, Flow.set_annot KTupleInfo a f

  let exec_interface = {export = []; import = [Zone.Z_py_obj]}
  let eval_interface = {export = [Zone.Z_py, Zone.Z_py_obj]; import = [Zone.Z_py, Zone.Z_py_obj; Universal.Zone.Z_u_heap, Z_any]}

  let init (prog:program) man flow =
    Some (
      Flow.set_annot KTupleInfo Equiv.empty flow
    )


  let rec eval zones exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_py_tuple els ->
      Eval.eval_list els man.eval flow |>
      Eval.bind(fun els flow ->
          let els_vars, flow = get_var_flow (Callstack.get flow, range, List.length els) flow in
          let flow = List.fold_left2 (fun acc vari eli ->
              man.exec ~zone:Zone.Z_py
                (mk_assign (mk_var ~mode:WEAK vari range) eli range) acc) flow els_vars els in
          let addr_tuple = mk_alloc_addr (A_py_tuple els_vars) range in
          man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_tuple flow |>
          Eval.bind (fun eaddr_tuple flow ->
              let addr_tuple = match ekind eaddr_tuple with
                | E_addr a -> a
                | _ -> assert false in
              Eval.singleton (mk_py_object (addr_tuple, None) range) flow
            )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "tuple.__contains__")}, _)}, args, []) ->
      Utils.check_instances ~arguments_after_check:1 man flow range args
        ["tuple"]
        (fun eargs flow ->
           let tuple = List.hd eargs in
           let isin = List.hd (List.tl eargs) in
           let tuple_vars = match ekind tuple with
             | E_py_object ({addr_kind = A_py_tuple vars}, _) -> vars
             | _ -> assert false in
           let mk_comp var = mk_binop (mk_var ~mode:WEAK var range) Framework.Ast.O_eq isin range in
           if List.length tuple_vars = 0 then
             man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_py_false range) flow
           else
             let or_expr = List.fold_left (fun acc var ->
                 mk_binop acc O_py_or (mk_comp var) range
               ) (mk_comp (List.hd tuple_vars)) (List.tl tuple_vars) in
           man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) or_expr flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "tuple.__getitem__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["tuple"; "int"]
        (fun eargs flow ->
           let tuple = List.hd eargs in
           let pos = match ekind (List.hd (List.tl args)) with
             | E_constant (C_int z) -> Z.to_int z
             | _ -> Exceptions.panic "tuple.__getitem__ over non-constant integer" in
           let tuple_vars = match ekind tuple with
             | E_py_object ({addr_kind = A_py_tuple vars}, _) -> vars
             | _ -> assert false in
           if 0 <= pos && pos < List.length tuple_vars then
             man.eval (mk_var ~mode:WEAK (List.nth tuple_vars pos) range) flow
           else
             man.exec (Utils.mk_builtin_raise "IndexError" range) flow |>
             Eval.empty_singleton
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "tuple.__iter__")}, _)}, args, []) ->
      Utils.check_instances man flow range args
        ["tuple"]
        (fun args flow ->
           let tuple = List.hd args in
           let tuple_addr = match ekind tuple with
             | E_py_object ({addr_kind = A_py_tuple _} as a, _) -> a
             | _ -> assert false in
           let addr_iterator = mk_alloc_addr (Py_list.A_py_iterator ("tuple_iterator", tuple_addr, Some 0)) range in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) addr_iterator flow |>
           Eval.bind (fun addr_it flow ->
               let addr_it = match ekind addr_it with
                 | E_addr a -> a
                 | _ -> assert false in
               Eval.singleton (mk_py_object (addr_it, None) range) flow
             )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "tuple_iterator.__next__")}, _)}, [iterator], []) ->
      (* todo: checks? *)
      (* ugly assign iterator = iterator at pos+1... *)
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) iterator flow |>
      Eval.bind (fun eiterator flow ->
          let tuple_it_addr, tuple_addr, tuple_pos = match ekind eiterator with
            | E_py_object ({addr_kind = Py_list.A_py_iterator (s, a, d)} as addr, _) when s = "tuple_iterator" -> addr, a, d
            | _ -> assert false in
          let vars_els = match akind tuple_addr with
            | A_py_tuple a -> a
            | _ -> assert false in
          match tuple_pos with
          | Some d when d < List.length vars_els ->
            let () = debug "exec incoming@\n" in
            let flow = man.exec
                (mk_assign iterator
                   (mk_py_object ({tuple_it_addr with addr_kind = Py_list.A_py_iterator ("tuple_iterator", tuple_addr, Some (d+1))}, None) range) range) flow in
            man.eval (mk_var ~mode:WEAK (List.nth vars_els d) range) flow
          | _ ->
            man.exec (Utils.mk_builtin_raise "StopIteration" range) flow |> Eval.empty_singleton
        )
      |> OptionExt.return



    | _ -> None


  let exec zone stmt man flow = None

  let ask _ _ _ = None
end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
