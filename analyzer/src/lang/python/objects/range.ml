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
open Sig.Abstraction.Stateless
open Ast
open Addr
open Universal.Ast

let name = "python.objects.range"


let opt_py_range_allocation_policy : string ref = ref "all"
let () = Universal.Heap.Policies.register_option opt_py_range_allocation_policy name "-py-range-alloc-pol" "for range objects"
           (fun default ak -> match ak with
                              | A_py_instance {addr_kind = A_py_class (C_builtin "range", _)}
                                | A_py_instance {addr_kind = A_py_class (C_builtin "range_iterator", _)} ->
                                    (Universal.Heap.Policies.of_string !opt_py_range_allocation_policy) ak
                              | _ -> default ak)

let opt_py_slice_allocation_policy : string ref = ref "all"
let () = Universal.Heap.Policies.register_option opt_py_slice_allocation_policy name "-py-slice-alloc-pol" "for slice objects"
           (fun default ak -> match ak with
                              | A_py_instance {addr_kind = A_py_class (C_builtin "slice", _)} ->
                                    (Universal.Heap.Policies.of_string !opt_py_slice_allocation_policy) ak
                              | _ -> default ak)



module Domain =
struct

  include GenStatelessDomainId(struct
              let name = name
            end)

  let init _ _ flow = flow

  let allocate_builtin ?(mode=STRONG) man range flow bltin oe =
    (* allocate addr, and map this addr to inst bltin *)
    let range = tag_range range "alloc_%s" bltin in
    let cls = fst @@ find_builtin bltin in
    man.eval   (mk_alloc_addr ~mode:mode (A_py_instance cls) range) flow >>$
      (fun eaddr flow ->
        let addr = match ekind eaddr with
          | E_addr a -> a
          | _ -> assert false in
        man.exec   (mk_add eaddr range) flow >>%
        Eval.singleton (mk_py_object (addr, oe) range)
      )

  let alarms = []

  let rec eval exp man flow =
    let range = exp.erange in
    match ekind exp with
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("slice.__new__" as f, _))}, _)}, cls :: args, []) ->
      Utils.new_wrapper man range flow "slice" cls
        ~fthennew:(fun flow ->
            let intornone = ["int"; "NoneType"] in
            Utils.check_instances_disj f man flow range args
              [intornone; intornone; intornone]
              (fun _ flow ->
                let start, stop, step = match args with a::b::c::[] -> a,b,c | _ -> assert false in
                man.eval   (mk_alloc_addr ~mode:STRONG (A_py_instance (fst @@ find_builtin "slice")) (tag_range range "alloc_slice")) flow >>$
                  (fun eaddr flow ->
                    let addr = match ekind eaddr with
                      | E_addr a -> a
                      | _ -> assert false in
                    let obj = mk_py_object (addr, None) range in
                    man.exec   (mk_add eaddr range) flow >>%
                    man.exec   (mk_assign (mk_py_attr obj "start" range) start range) >>%
                    man.exec   (mk_assign (mk_py_attr obj "stop" range) stop range) >>%
                    man.exec   (mk_assign (mk_py_attr obj "step" range) step range) >>%
                    Eval.singleton obj
                  )
              )
        )

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("slice.indices" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["slice"; "int"]
        (fun eargs flow ->
          let slice, length = match eargs with a::b::[] -> a, b | _ -> assert false in
          (* assumes that _PySlice_GetLongIndices and PySlice_Unpack+PySlice_AdjustIndices carry the same meaning in sliceobject.c *)
          let py_ssize_t_max (*FIXME. should be sys.maxsize *) = mk_z (Z.of_string "9223372036854775807") ?typ:(Some T_int) range in
          let py_ssize_t_min = mk_binop (mk_unop O_minus py_ssize_t_max range) O_minus (mk_int 1 ?typ:(Some T_int) range) range in
          (* fixme: potential overflow over int start/stop/step (in new or here?) *)
          let _step = mk_range_attr_var range "step" T_py in
          let _start = mk_range_attr_var range "start" T_py in
          let _stop = mk_range_attr_var range "stop" T_py in

          let step = mk_py_attr slice "step" range in
          let start = mk_py_attr slice "start" range in
          let stop = mk_py_attr slice "stop" range in


          let zero = mk_int 0 range in
          let one = mk_int 1 range in
          let mone = mk_int (-1) range in

          let unpack__step = mk_assign (mk_var _step range) (mk_expr ~etyp:T_py (E_py_if (mk_py_isinstance_builtin step "NoneType" range,
                                                            one,
                                                            step)) range) range in
          let unpack__start = mk_assign (mk_var _start range)
                             (mk_expr ~etyp:T_py (E_py_if (mk_py_isinstance_builtin start "NoneType" range,
                                                mk_expr ~etyp:T_py (E_py_if (
                                                             mk_binop (mk_var _step range) O_lt zero range,
                                                             py_ssize_t_max,
                                                             zero)) range,
                                                start)) range) range in
          let unpack__stop = mk_assign (mk_var _stop range)
                               (mk_expr ~etyp:T_py (E_py_if (mk_py_isinstance_builtin stop "NoneType" range,
                                                  mk_expr ~etyp:T_py (E_py_if (
                                                               mk_binop (mk_var _step range) O_lt zero range,
                                                               py_ssize_t_min,
                                                               py_ssize_t_max
                                                    )) range,
                                                  stop)) range) range in


          let adjust_st st =
            mk_if
              (mk_binop st O_lt zero range)
              (mk_block
                 [mk_assign st (mk_binop st O_plus length range) range;
                  mk_if
                    (mk_binop st O_lt zero range)
                    (mk_assign st (mk_expr ~etyp:T_py (E_py_if (mk_binop (mk_var _step range) O_lt zero range,
                                                     mone,
                                                     zero)) range) range)
                    (mk_nop range)
                    range
                 ]
                 range)
              (mk_if
                 (mk_binop st O_ge length range)
                 (mk_assign st (mk_expr ~etyp:T_py (E_py_if (mk_binop (mk_var _step range) O_lt zero range,
                                                  mk_binop length O_minus one range,
                                                  length)) range) range)
                 (mk_nop range)
                 range) range
          in
          let adjust__start = adjust_st (mk_var _start range) in
          let adjust__stop = adjust_st (mk_var _stop range) in

          man.exec   (mk_block [unpack__step; unpack__start; unpack__stop; adjust__start; adjust__stop] range) flow >>%
            man.eval   (mk_expr ~etyp:T_py (E_py_tuple [mk_var _start range;
                                                                            mk_var _stop range;
                                                                            mk_var _step range]) range) |>
            Cases.add_cleaners [mk_remove_var _step range;
                               mk_remove_var _start range;
                               mk_remove_var _stop range]

        )
      |> OptionExt.return



    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__new__", _))}, _)} as call, cls :: [up], []) ->
      let args' = (mk_constant ~etyp:T_int (C_int (Z.of_int 0)) range)::up::(mk_constant ~etyp:T_int (C_int (Z.of_int 1)) range)::[] in
      man.eval {exp with ekind = E_py_call(call, cls :: args', [])} flow
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__new__", _))}, _)} as call, cls :: [down; up], []) ->
      let args' = down::up::(mk_constant ~etyp:T_int (C_int (Z.of_int 1)) range)::[] in
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
                 man.eval   (mk_alloc_addr ~mode:STRONG (A_py_instance (fst @@ find_builtin "range")) alloc_range) flow >>$
 (fun eaddr flow ->
                     let addr = match ekind eaddr with
                       | E_addr a -> a
                       | _ -> assert false in
                     let obj = mk_py_object (addr, None) range in
                     man.exec    (mk_add eaddr range) flow >>%
                     man.exec   (mk_assign (mk_py_attr obj "start" range) start range) >>%
                     man.exec   (mk_assign (mk_py_attr obj "stop" range) stop range) >>%
                     man.exec   (mk_assign (mk_py_attr obj "step" range) step range) >>%
                     Eval.singleton obj
                   )
              )
          )

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__contains__", _))}, _)}, args, []) ->
      (* isinstance(arg1, range) && isinstance(arg2, int) ? *)
      Exceptions.panic "todo: %a@\n" pp_expr exp

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__len__", _))}, _)}, [arg], []) ->
      man.eval   arg flow >>$
 (fun arg flow ->
          let ra s = mk_py_attr arg s range in
          man.eval  
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
           man.eval   (mk_alloc_addr ~mode:STRONG (A_py_instance (fst @@ find_builtin "range_iterator")) alloc_range) flow >>$
 (fun eaddr flow ->
               let addr = match ekind eaddr with
                 | E_addr a -> a
                 | _ -> assert false in
               let obj = mk_py_object (addr, None) range in
               (* FIXME: replace stop by length which should be computed, see rangeobject.c:197 *)
               flow |>
               man.exec    (mk_add eaddr range) >>%
               man.exec   (mk_assign (mk_py_attr obj "start" range) (mk_py_attr range_obj "start" range) range) >>%
               man.exec   (mk_assign (mk_py_attr obj "stop" range) (mk_py_attr range_obj "stop" range) range) >>%
               man.exec   (mk_assign (mk_py_attr obj "step" range) (mk_py_attr range_obj "step" range) range) >>%
               man.exec   (mk_assign (mk_py_attr obj "index" range) (mk_int 0 ~typ:T_int range) range) >>%
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
          man.eval  
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
               man flow
             ~fthen:(fun flow ->
               man.eval   (mk_binop start O_plus (mk_binop index O_mult step range) range) flow |>
                 (* add_cleaners is ugly, but a bind_some is incorrect
                    (the return of eval will be something like <<int
                    :: start + index * step>>. If we update index
                    afterwards, it will change the value in the return
                    above too... *)
                 Cases.add_cleaners [mk_assign index (mk_binop index O_plus (mk_int 1 ~typ:T_int range) range) range]
               )
             ~felse:(fun flow ->
                   man.exec (Utils.mk_builtin_raise "StopIteration" range) flow >>%
                   Eval.empty_singleton
               )
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range_iterator.__iter__", _))}, _)}, [self], []) ->
      man.eval self flow |> OptionExt.return

    | _ -> None

  let exec stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_py_for (target, ({ekind = E_py_object ({addr_kind = A_py_instance {addr_kind = A_py_class (C_builtin "range", _)}}, _)} as rangeobj), body, {skind = S_block ([], _)}) ->
       (** if ranges are desugared in the generic way of
          desugar/loops.ml, we lose precision in the non-relational
          value analysis. We are unable to relate the target and start
          + index * step (of the range_iterator object). At the end of
          a loop like `for x in range(10)`, we thus have only 1 <= x
          <= 9, but index = 10, which is disappointing. Instead, we
          try to desugar:
          ```python
          for target in range(start, stop, step):
              body```
          as (if s > 0, otherwise you need to invert the sign in the comparison in the loop):
          ```python
          if start < stop:
              target = start
              while target < stop:
                  body
                  target = target + step
              target = target - step```
           (It seems that in the case of the else statement, the usual desugar is sometimes better, so we keep it).
        *)
       let ra s = mk_py_attr rangeobj s (tag_range range "%s" s) in
       Utils.bind_list_args man [ra "start"; ra "stop"; ra "step"] flow range
         (fun vars flow ->
             let start, stop, step = match List.map (fun x -> mk_var x range) vars with
               | [a;b;c] -> a, b, c
               | _ -> assert false in

             let gen_stmt comp_op =
               let assign_target = mk_assign target start range in
               let old_body = match skind body with
                 | S_block (stmts, _) -> stmts
                 | _ -> [body] in
               let targetostep o = mk_binop target o step range in
               let incr_target = mk_assign target (targetostep O_plus) range in
               let decr_target = mk_assign target (targetostep O_minus) range in
               (* let new_else = match skind orelse with
                *   | S_block ([], _) -> orelse
                *   | S_block (t, _) -> mk_block (decr_target :: t) range
                *   | _ -> mk_block (decr_target :: orelse :: []) range in *)
               let while_stmt =
                 mk_while (mk_binop target comp_op stop range)
                   (mk_block (old_body @ [incr_target]) range) range in
               mk_if (mk_binop start comp_op stop range)
                 (mk_block (assign_target :: while_stmt :: decr_target :: []) range)
                 (mk_nop range) range
             in
             assume (mk_binop step O_gt (mk_zero range) range) man flow
               ~fthen:(fun flow -> man.exec (gen_stmt O_lt) flow >>% Post.return)
               ~felse:(fun flow -> man.exec (gen_stmt O_gt) flow >>% Post.return)
         )
       |> OptionExt.return

       | _ -> None

  let ask _ _ _ = None

end

let () =
  register_stateless_domain (module Domain)
