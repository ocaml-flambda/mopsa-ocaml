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

(** Range objects. *)
(* FIXME: range(1, 10, -1) ; range(10, 1, 1) ; range(1, 3, 0) *)

open Mopsa
open Sig.Domain.Stateless
open Ast
open Addr
open Universal.Ast

let name = "python.objects.range"

type addr_kind +=
   | A_py_range
   | A_py_range_iterator

let () =
  Format.(register_addr_kind {
      print = (fun default fmt a ->
          match a with
          | A_py_range -> fprintf fmt "range"
          | A_py_range_iterator -> fprintf fmt "range_iterator"
          | _ -> default fmt a
      );
      compare = (fun default a1 a2 ->
          match a1, a2 with
          | _ -> default a1 a2);})


let () = register_addr_kind_nominal_type (fun default ak ->
             match ak with
             | A_py_range -> "range"
             | A_py_range_iterator -> "range_iterator"
             | _ -> default ak)

let opt_py_range_allocation_policy : string ref = ref "all"
let () = Universal.Heap.Policies.register_option opt_py_range_allocation_policy name "-py-range-alloc-pol" "range objects"
           (fun default ak -> match ak with
                              | A_py_range
                                | A_py_range_iterator ->
                                    (Universal.Heap.Policies.of_string !opt_py_range_allocation_policy) ak
                              | _ -> default ak);


module Domain =
struct

  include GenStatelessDomainId(struct
              let name = name
            end)

  let interface = {
    iexec = {provides = []; uses = []};
    ieval = {provides = [Zone.Z_py, Zone.Z_py_obj]; uses = [Zone.Z_py, Zone.Z_py_obj]}
  }

  let init _ _ flow = flow

  let allocate_builtin ?(mode=STRONG) man range flow bltin oe =
    (* allocate addr, and map this addr to inst bltin *)
    let range = tag_range range "alloc_%s" bltin in
    let cls = fst @@ find_builtin bltin in
    man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) (mk_alloc_addr ~mode:mode (A_py_instance cls) range) flow |>
    Eval.bind (fun eaddr flow ->
        let addr = match ekind eaddr with
          | E_addr a -> a
          | _ -> assert false in
        man.exec ~zone:Zone.Z_py_obj (mk_add eaddr range) flow |>
        Eval.singleton (mk_py_object (addr, oe) range)
      )

  let alarms = []

  let rec eval zs exp man flow =
    let range = exp.erange in
    match ekind exp with
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("slice.__new__" as f, _))}, _)}, cls :: args, []) ->
      Utils.new_wrapper man range flow "slice" cls
        ~fthennew:(fun flow ->
            let intornone = ["int"; "NoneType"] in
            Utils.check_instances_disj f man flow range args
              [intornone; intornone; intornone]
              (fun _ flow -> allocate_builtin man range flow "slice" (Some exp))
          )

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__new__", _))}, _)} as call, cls :: [up], []) ->
      let args' = (mk_constant T_int (C_int (Z.of_int 0)) range)::up::(mk_constant T_int (C_int (Z.of_int 1)) range)::[] in
      man.eval {exp with ekind = E_py_call(call, cls :: args', [])} flow
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__new__", _))}, _)} as call, cls :: [down; up], []) ->
      let args' = down::up::(mk_constant T_int (C_int (Z.of_int 1)) range)::[] in
      man.eval {exp with ekind = E_py_call(call, cls :: args', [])} flow
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__new__" as f, _))}, _)}, cls :: args, []) ->
      Utils.new_wrapper man range flow "range" cls
        ~fthennew:(fun flow ->
            Utils.check_instances f man flow range args
              ["int"; "int"; "int"]
              (fun args flow ->
                 let start, stop, step = match args with a::b::c::[] -> a, b, c | _ -> assert false in
                 let alloc_range = tag_range range "alloc_%s" "range" in
                 man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) (mk_alloc_addr ~mode:STRONG A_py_range alloc_range) flow |>
                 Eval.bind (fun eaddr flow ->
                     let addr = match ekind eaddr with
                       | E_addr a -> a
                       | _ -> assert false in
                     let obj = mk_py_object (addr, None) range in
                     man.exec ~zone:Zone.Z_py_obj (mk_add eaddr range) flow |>
                     man.exec ~zone:Zone.Z_py (mk_assign (mk_py_attr obj "start" range) start range) |>
                     man.exec ~zone:Zone.Z_py (mk_assign (mk_py_attr obj "stop" range) stop range) |>
                     man.exec ~zone:Zone.Z_py (mk_assign (mk_py_attr obj "step" range) step range) |>
                     Eval.singleton obj
                   )
              )
          )

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__contains__", _))}, _)}, args, []) ->
      (* isinstance(arg1, range) && isinstance(arg2, int) ? *)
      Exceptions.panic "todo: %a@\n" pp_expr exp

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__len__", _))}, _)}, [arg], []) ->
      man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) arg flow |>
      Eval.bind (fun arg flow ->
          let ra s = mk_py_attr arg s range in
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)
            (mk_binop
               (mk_binop
                  (ra "stop")
                  O_minus
                  (ra "start")
                  range
               )
               O_py_floor_div (ra "step") range) flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__iter__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["range"]
        (fun r flow ->
           let range_obj = List.hd r in
           let alloc_range = tag_range range "alloc_%s" "range" in
           man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) (mk_alloc_addr ~mode:STRONG A_py_range_iterator alloc_range) flow |>
           Eval.bind (fun eaddr flow ->
               let addr = match ekind eaddr with
                 | E_addr a -> a
                 | _ -> assert false in
               let obj = mk_py_object (addr, None) range in
               (* FIXME: replace stop by length which should be computed, see rangeobject.c:197 *)
               flow |>
               man.exec ~zone:Zone.Z_py_obj (mk_add eaddr range) |>
               man.exec ~zone:Zone.Z_py (mk_assign (mk_py_attr obj "start" range) (mk_py_attr range_obj "start" range) range) |>
               man.exec ~zone:Zone.Z_py (mk_assign (mk_py_attr obj "stop" range) (mk_py_attr range_obj "stop" range) range) |>
               man.exec ~zone:Zone.Z_py (mk_assign (mk_py_attr obj "step" range) (mk_py_attr range_obj "step" range) range) |>
               man.exec ~zone:Zone.Z_py (mk_assign (mk_py_attr obj "index" range) (mk_int 0 ~typ:T_int range) range) |>
               (* FIXME: rangeobject:874: no stop but a len field. These are CPython fields and not attributes too *)
               Eval.singleton obj)
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__reversed__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["range"]
        (fun r flow ->
          let r = List.hd r in
          (* FIXME corner cases, see range_reverse in rangeobject.c *)
          let start = mk_py_attr r "start" range in
          let step = mk_py_attr r "step" range in
          let len = mk_py_call (mk_py_object (find_builtin_function "range.__len__") range) [r] range in
          man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj)
            (mk_py_call (mk_py_object (find_builtin_function "range.__iter__") range)
               [mk_py_call (mk_py_object (find_builtin "range") range)
                  [ mk_binop (mk_binop start O_minus step range) O_plus (mk_binop len O_mult step range) range;
                    mk_binop start O_minus step range;
                    mk_unop O_minus step range
                  ]
                  range]
               range) flow
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range_iterator.__next__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["range_iterator"]
        (fun rangeit flow ->
           let rangeit = List.hd rangeit in
           let start = mk_py_attr rangeit "start" range in
           let step = mk_py_attr rangeit "step" range in
           let index = mk_py_attr rangeit "index" range in
           let stop = mk_py_attr rangeit "stop" range in
           assume
             (mk_binop
               (mk_binop start O_plus (mk_binop index O_mult step range) range)
               O_lt
               stop
               range
             )
             ~zone:Zone.Z_py man flow
             ~fthen:(fun flow ->
                 man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) (mk_binop start O_plus (mk_binop index O_mult step range) range) flow |> Eval.add_cleaners [mk_assign index (mk_binop index O_plus (mk_int 1 ~typ:T_int range) range) range]
               )
             ~felse:(fun flow ->
                   man.exec (Utils.mk_builtin_raise "StopIteration" range) flow
                   |> Eval.empty_singleton
               )
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range_iterator.__iter__", _))}, _)}, [self], []) ->
      man.eval self flow |> OptionExt.return

    | _ -> None

  let exec _ _ _ _ = None
  let ask _ _ _ = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
