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
        let addr = Addr.from_expr eaddr in
        man.exec   (mk_add eaddr range) flow >>%
        Eval.singleton (mk_py_object (addr, oe) range)
      )

  let checks = []

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
                    let addr = Addr.from_expr eaddr in
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
          let py_ssize_t_max (*FIXME. should be sys.maxsize *) = mk_z (Z.of_string "9223372036854775807") ?typ:(Some (T_py (Some Int))) range in
          let py_ssize_t_min = mk_binop ~etyp:(T_py None) (mk_unop ~etyp:(T_py (Some Int)) O_minus py_ssize_t_max range) O_minus (mk_int 1 ?typ:(Some (T_py None)) range) range in
          (* fixme: potential overflow over int start/stop/step (in new or here?) *)
          let _step = mk_range_attr_var range "step" (T_py None) in
          let _start = mk_range_attr_var range "start" (T_py None) in
          let _stop = mk_range_attr_var range "stop" (T_py None) in

          let step = mk_py_attr slice "step" range in
          let start = mk_py_attr slice "start" range in
          let stop = mk_py_attr slice "stop" range in


          let zero = mk_int ~typ:(T_py None) 0 range in
          let one = mk_int ~typ:(T_py None) 1 range in
          let mone = mk_int ~typ:(T_py None) (-1) range in

          let unpack__step = mk_assign (mk_var _step range) (mk_expr ~etyp:(T_py None) (E_py_if (mk_py_isinstance_builtin step "NoneType" range,
                                                            one,
                                                            step)) range) range in
          let unpack__start = mk_assign (mk_var _start range)
                             (mk_expr ~etyp:(T_py None) (E_py_if (mk_py_isinstance_builtin start "NoneType" range,
                                                mk_expr ~etyp:(T_py None) (E_py_if (
                                                             mk_binop ~etyp:(T_py None) (mk_var _step range) O_lt zero range,
                                                             py_ssize_t_max,
                                                             zero)) range,
                                                start)) range) range in
          let unpack__stop = mk_assign (mk_var _stop range)
                               (mk_expr ~etyp:(T_py None) (E_py_if (mk_py_isinstance_builtin stop "NoneType" range,
                                                  mk_expr ~etyp:(T_py None) (E_py_if (
                                                               mk_binop ~etyp:(T_py None) (mk_var _step range) O_lt zero range,
                                                               py_ssize_t_min,
                                                               py_ssize_t_max
                                                    )) range,
                                                  stop)) range) range in


          let adjust_st st =
            mk_if
              (mk_binop ~etyp:(T_py None) st O_lt zero range)
              (mk_block
                 [mk_assign st (mk_binop ~etyp:(T_py None) st O_plus length range) range;
                  mk_if
                    (mk_binop ~etyp:(T_py None) st O_lt zero range)
                    (mk_assign st (mk_expr ~etyp:(T_py None) (E_py_if (mk_binop ~etyp:(T_py None) (mk_var _step range) O_lt zero range,
                                                     mone,
                                                     zero)) range) range)
                    (mk_nop range)
                    range
                 ]
                 range)
              (mk_if
                 (mk_binop ~etyp:(T_py None) st O_ge length range)
                 (mk_assign st (mk_expr ~etyp:(T_py None) (E_py_if (mk_binop ~etyp:(T_py None) (mk_var _step range) O_lt zero range,
                                                  mk_binop ~etyp:(T_py None) length O_minus one range,
                                                  length)) range) range)
                 (mk_nop range)
                 range) range
          in
          let adjust__start = adjust_st (mk_var _start range) in
          let adjust__stop = adjust_st (mk_var _stop range) in

          man.exec   (mk_block [unpack__step; unpack__start; unpack__stop; adjust__start; adjust__stop] range) flow >>%
            man.eval   (mk_expr ~etyp:(T_py None) (E_py_tuple [mk_var _start range;
                                                                            mk_var _stop range;
                                                                            mk_var _step range]) range) |>
            Cases.add_cleaners [mk_remove_var _step range;
                               mk_remove_var _start range;
                               mk_remove_var _stop range]

        )
      |> OptionExt.return



    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__new__", _))}, _)} as call, cls :: [up], []) ->
       let args' = [mk_int 0 ~typ:(T_py None) range; up; mk_int 1 ~typ:(T_py None) range] in
       man.eval {exp with ekind = E_py_call(call, cls :: args', [])} flow
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__new__", _))}, _)} as call, cls :: [down; up], []) ->
      let args' = [down; up; mk_int 1 ~typ:(T_py None) range] in
      man.eval {exp with ekind = E_py_call(call, cls :: args', [])} flow
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__new__" as f, _))}, _)}, cls :: args, []) ->
      Utils.new_wrapper man range flow "range" cls
        ~fthennew:(fun flow ->
            Utils.check_instances f man flow range args
              ["int"; "int"; "int"]
              (fun args flow ->
                let start, stop, step = match args with a::b::c::[] -> a, b, c | _ -> assert false in
                let easy_constant = match snd @@ object_of_expr step with
                | Some {ekind = E_constant (C_int z)} -> Some (Z.to_int z)
                | _ -> None in
                let do_alloc flow =
                  let flow = Flow.add_safe_check Alarms.CHK_PY_VALUEERROR range flow in
                  let alloc_range = tag_range range "alloc_%s" "range" in
                  man.eval   (mk_alloc_addr ~mode:STRONG (A_py_instance (fst @@ find_builtin "range")) alloc_range) flow >>$
                    (fun eaddr flow ->
                      let addr = Addr.from_expr eaddr in
                      let obj = mk_py_object (addr, None) range in
                      man.exec (mk_add eaddr range) flow >>%
                        man.exec (mk_assign (mk_py_attr obj "start" range) start range) >>%
                        man.exec (mk_assign (mk_py_attr obj "stop" range) stop range) >>%
                          man.exec (mk_assign (mk_py_attr obj "step" range) step range) >>%
                        Eval.singleton obj
                    ) in
                match easy_constant with
                | Some z when z <> 0 ->
                   do_alloc flow
                | Some z -> (* z = 0 *)
                   man.exec (Utils.mk_builtin_raise_msg "ValueError" "range() arg 3 must not be zero" range) flow >>% Eval.empty
                | None ->
                  assume (ne step (mk_zero range ~typ:(T_py None)) ~etyp:(T_py None) range) man flow
                    ~fthen:(do_alloc)
                    ~felse:(fun flow ->
                      man.exec (Utils.mk_builtin_raise_msg "ValueError" "range() arg 3 must not be zero" range) flow >>% Eval.empty)
              )
        )

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__contains__" as f, _))}, _)}, args, []) ->
       Utils.check_instances f man flow range args
         ["range"; "int"]
         (fun r flow ->
           man.eval (mk_py_top T_bool range) flow
         )
       |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__len__", _))}, _)}, [arg], []) ->
      man.eval arg flow >>$? fun arg flow ->
      let ra s = mk_py_attr arg s range in
      Utils.try_eval_expr ~on_empty:(fun _ _ _ _ -> None)
        ~on_result:(Eval.singleton)
        man
        (mk_binop ~etyp:(T_py None)
           (mk_binop ~etyp:(T_py None)
              (ra "stop")
              O_minus
              (ra "start")
              range
           )
           O_py_floor_div (ra "step") range)
        flow

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__iter__" as f, _))}, _)}, args, []) ->
       Utils.check_instances f man flow range args
         ["range"]
         (fun r flow ->
           let range_obj = List.hd r in
           let alloc_range = tag_range range "alloc_%s" "range" in
           man.eval   (mk_alloc_addr ~mode:STRONG (A_py_instance (fst @@ find_builtin "range_iterator")) alloc_range) flow >>$
             (fun eaddr flow ->
               let addr = Addr.from_expr eaddr in
               let obj = mk_py_object (addr, None) range in
               (* FIXME: replace stop by length which should be computed, see rangeobject.c:197 *)
               flow |>
                 man.exec    (mk_add eaddr range) >>%
                 man.exec   (mk_assign (mk_py_attr obj "start" range) (mk_py_attr range_obj "start" range) range) >>%
                 man.exec   (mk_assign (mk_py_attr obj "length" range) (mk_py_call (mk_py_attr range_obj "__len__" range) [] range) range) >>%
                 man.exec   (mk_assign (mk_py_attr obj "step" range) (mk_py_attr range_obj "step" range) range) >>%
                 man.exec   (mk_assign (mk_py_attr obj "index" range) (mk_int 0 ~typ:(T_py None) range) range) >>%
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
          let range = tag_range range "reversed" in
          let py_add s1 s2 = add s1 s2 ~typ:(T_py None) range in
          let py_sub s1 s2 = sub s1 s2 ~typ:(T_py None) range in
          let py_mul s1 s2 = mul s1 s2 ~typ:(T_py None) range in
          man.eval
            (mk_py_call (mk_py_object (find_builtin_function "range.__iter__") range)
               [mk_py_call (mk_py_object (find_builtin "range") range)
                  [ py_add start (py_mul (py_sub len (mk_one ~typ:(T_py None) range)) step);
                    py_sub start step;
                    mk_unop ~etyp:(T_py None) O_minus step range
                  ]
                  range]
               range) flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range.__getitem__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["range"; "int"]
        (fun r flow ->
          let r, pos = match r with [a;b] -> a, b | _ -> assert false in
          (*
             l = len(r)
             i = l+pos if(pos < 0) else pos
             if(i < 0 || i >= l) IndexError "range object index out of range"
             return r.start + i * r.step
           *)
          let l = mk_range_attr_var range "l" (T_py None) in
          let i = mk_range_attr_var range "i" (T_py None) in
          let stmts =
            [
              mk_assign (mk_var l range) (mk_py_call (mk_py_object (find_builtin "len") range) [r] range) range;
              mk_assign (mk_var i range) (mk_expr (
                  E_py_if (lt pos (mk_zero ~typ:(T_py None) range) ~etyp:(T_py None) range,
                           add ~typ:(T_py None) (mk_var l range) pos range,
                           pos)) ~etyp:(T_py None) range) range
            ] in
          man.exec (mk_block stmts range) flow >>% fun flow ->
          assume (py_or ~etyp:(T_py None) (lt ~etyp:(T_py None) (mk_var i range) (mk_zero range ~typ:(T_py None)) range) (ge ~etyp:(T_py None) (mk_var i range) (mk_var l range) range) range) man flow
            ~fthen:(fun flow ->
              man.exec (mk_block [mk_remove_var i range; mk_remove_var l range] range) flow >>% fun flow ->
              man.exec (Utils.mk_builtin_raise_msg "IndexError" "range object index out of range" range) flow >>% Eval.empty
            )
            ~felse:(fun flow ->
              Flow.add_safe_check Alarms.CHK_PY_INDEXERROR range flow |>
              man.eval (add
                          (mk_py_attr r "start" range)
                          (mul (mk_var i range) (mk_py_attr r "step" range) ~typ:(T_py None) range)
                          ~typ:(T_py None) range)
            )
        |> Cases.add_cleaners [mk_remove_var i range; mk_remove_var l range]
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
           let length = mk_py_attr rangeit "length" range in
           assume
             (lt index length ~etyp:(T_py None) range)
               man flow
               ~fthen:(fun flow ->
                 let flow = Flow.add_safe_check Alarms.CHK_PY_STOPITERATION range flow in
                 man.eval   (mk_binop ~etyp:(T_py None) start O_plus (mk_binop ~etyp:(T_py None) index O_mult step range) range) flow |>
                 (* add_cleaners is ugly, but a bind_result is incorrect
                    (the return of eval will be something like <<int
                    :: start + index * step>>. If we update index
                    afterwards, it will change the value in the return
                    above too... *)
                 Cases.add_cleaners [mk_assign index (mk_binop ~etyp:(T_py None) index O_plus (mk_int 1 ~typ:(T_py None) range) range) range]
               )
             ~felse:(fun flow ->
                   man.exec (Utils.mk_builtin_raise "StopIteration" range) flow >>%
                   Eval.empty
               )
        )
      |> OptionExt.return


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("range_iterator.__iter__", _))}, _)}, [self], []) ->
      man.eval self flow |> OptionExt.return


    (* FIXME: that's __new__ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("reversed.__new__", _))}, _)}, cls :: args, []) ->
       Utils.new_wrapper man range flow "reversed" cls
         ~fthennew:(fun flow ->
           let to_rev = List.hd args in
           assume (mk_py_hasattr to_rev "__reversed__" range) man flow
             ~fthen:(fun flow ->
               Flow.add_safe_check Alarms.CHK_PY_TYPEERROR range flow |>
               man.eval (mk_py_call (mk_py_attr (List.hd args) "__reversed__" range) [] range))
             ~felse:(fun flow ->
               assume (py_and (mk_py_hasattr to_rev "__len__" range) (mk_py_hasattr to_rev "__getitem__" range) range) man flow
                 ~fthen:(fun flow ->
                   (* - allocate reversed object,
                      - put _index to be len(to_rev)-1,
                      - _seq to be to_rev *)
                   let flow = Flow.add_safe_check Alarms.CHK_PY_TYPEERROR range flow in
                   allocate_builtin man range flow "reversed" None >>$ fun reversed_obj flow ->
                   man.exec (mk_assign (mk_py_attr reversed_obj "_index" range) (sub (mk_py_call (mk_py_attr to_rev "__len__" range) [] range) (mk_one ~typ:(T_py None) range) ~typ:(T_py None) range) range) flow >>%
                   man.exec (mk_assign (mk_py_attr reversed_obj "_seq" range) to_rev range) >>%
                   Eval.singleton reversed_obj
                 )
                 ~felse:(fun flow -> man.exec (Utils.mk_builtin_raise_msg "TypeError" (Format.asprintf "'%a' object is not reversible" pp_expr to_rev) range) flow >>% Eval.empty)
             )
         )

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("reversed.__iter__", _))}, _)}, [self], []) ->
       (* FIXME: check type *)
       man.eval self flow |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("reversed.__next__" as f, _))}, _)}, args, []) ->
       Utils.check_instances f man flow range args
         ["reversed"]
         (fun eargs flow ->
           let rev = List.hd eargs in
           let index = mk_py_attr rev "_index" range in
           let seq = mk_py_attr rev "_seq" range in
           assume (ge index (mk_zero ~typ:(T_py None) range) ~etyp:(T_py None) range) man flow
             ~fthen:(fun flow ->
               let flow = Flow.add_safe_check Alarms.CHK_PY_STOPITERATION range flow in
               man.eval (mk_py_call (mk_py_attr seq "__getitem__" range) [index] range) flow |>
                 (* add_cleaners is ugly but necessary, see comment for range_iterator.__next__ *)
                 Cases.add_cleaners [mk_assign index (sub index (mk_int 1 ~typ:(T_py None) range) ~typ:(T_py None) range) range]
             )
             ~felse:(fun flow ->
               man.exec (Utils.mk_builtin_raise "StopIteration" range) flow >>%
                 Eval.empty
             )
         )
       |> OptionExt.return


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
       let start = Timing.start () in
       let ra s = mk_py_attr rangeobj s (tag_range rangeobj.erange "%s" s) in
       let res = Utils.bind_list_args man [ra "start"; ra "stop"; ra "step"] flow rangeobj.erange
         (fun vars flow ->
             let start, stop, step = match List.map (fun x -> mk_var x range) vars with
               | [a;b;c] -> a, b, c
               | _ -> assert false in

             let gen_stmt comp_op =
               let assign_target = mk_assign target start rangeobj.erange in
               let old_body = match skind body with
                 | S_block (stmts, _) -> stmts
                 | _ -> [body] in
               let targetostep o = mk_binop ~etyp:(T_py None) target o step rangeobj.erange in
               let incr_target = mk_assign target (targetostep O_plus) rangeobj.erange in
               let decr_target = mk_assign target (targetostep O_minus) rangeobj.erange in
               (* let new_else = match skind orelse with
                *   | S_block ([], _) -> orelse
                *   | S_block (t, _) -> mk_block (decr_target :: t) range
                *   | _ -> mk_block (decr_target :: orelse :: []) range in *)
               let while_stmt =
                 mk_while (mk_binop ~etyp:(T_py None) target comp_op stop range)
                   (mk_block (old_body @ [incr_target]) range) range in
               mk_if (mk_binop ~etyp:(T_py None) start comp_op stop range)
                 (mk_block (assign_target :: while_stmt :: decr_target :: []) range)
                 (mk_nop range) range
             in
             assume (mk_binop ~etyp:(T_py None) step O_gt (mk_zero ~typ:(T_py None) range) range) man flow
               ~fthen:(fun flow ->
                 man.exec (gen_stmt O_lt) flow >>% Post.return
               )
               ~felse:(fun flow -> man.exec (gen_stmt O_gt) flow >>% Post.return)
         )
                 |> OptionExt.return in
       Debug.debug ~channel:"profiling" "for loop at range %a: %.4f" pp_range range (Timing.stop start);
       res


       | _ -> None

  let ask _ _ _ = None

  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
