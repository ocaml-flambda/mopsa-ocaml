(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Handling of and/or operators. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Lattice
open Framework.Flow
open Framework.Eval
open Framework.Exec
open Framework.Ast
open Universal.Ast
open Ast

let name = "python.desugar.bool"
let debug fmt = Debug.debug ~channel:name fmt


module Domain =
struct

  let init _ ctx _ flow = ctx, flow

  let is_bool_function f =
    match ekind f with
    | E_var v -> v.vname = "bool"
    | E_addr a -> compare_addr a (Addr.find_builtin "bool") = 0
    | _ -> false

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    (* E⟦ e1 and e2 ⟧ *)
    | E_binop(O_py_and, e1, e2) ->
      man.eval ctx e1 flow |>
      eval_compose
        (fun e1 flow1 ->
           Universal.Utils.assume_to_eval e1
             (fun true_flow -> Some (man.eval ctx e2 true_flow))
             (fun false_flow -> oeval_singleton (Some e1, false_flow, []))
             man ctx flow ()
        )

    (* E⟦ e1 or e2 ⟧ *)
    | E_binop(O_py_or, e1, e2) ->
      man.eval ctx e1 flow |>
      eval_compose
        (fun e1 flow1 ->
           Universal.Utils.assume_to_eval e1
             (fun true_flow -> oeval_singleton (Some e1, true_flow, []))
             (fun false_flow -> Some (man.eval ctx e2 false_flow))
             man ctx flow ()
        )

    (* E⟦ e1 is e2 ⟧ *)
    | E_binop(O_py_is, e1, e2) ->
      eval_list [e1; e2] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
          let cls1 = Addr.classof @@ addr_of_expr e1 and cls2 = Addr.classof @@ addr_of_expr e2 in
          if compare_addr cls1 cls2 <> 0 then
            oeval_singleton (Some (mk_false range), flow, [])
          else
            match etyp e1, ekind e1, ekind e2 with
            | T_string, _, _ -> oeval_singleton (Some (mk_top T_bool range), flow, [])
            | T_addr, E_addr a1, E_addr a2 ->
              begin
                match Universal.Heap.Recency.is_weak a1, Universal.Heap.Recency.is_weak a2, compare_addr a1 a2 = 0 with
                | false, false, true -> oeval_singleton (Some (mk_true range), flow, [])
                | false, false, false -> oeval_singleton (Some (mk_false range), flow, [])
                | true, true, false -> oeval_singleton (Some (mk_false range), flow, [])
                | _ -> oeval_singleton (Some (mk_top T_bool range), flow, [])
              end

            | _ -> re_eval_singleton (man.eval ctx) (Some (mk_binop e1 O_eq e2 range), flow, [])
        )

    (* E⟦ e1 is not e2 ⟧ *)
    | E_binop(O_py_is_not, e1, e2) ->
      re_eval_singleton (man.eval ctx) (Some (mk_not (mk_binop e1 O_py_is e2 range) range), flow, [])

    (* E⟦ e1 in e2 ⟧ *)
    | E_binop(O_py_in, e1, e2) ->
      eval_list [e1; e2] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
          let cls2 = Addr.classof @@ addr_of_expr e2 in
          Universal.Utils.assume_to_eval
            (Utils.mk_addr_hasattr cls2 "__contains__" range)
            (fun true_flow ->
               let exp' = mk_py_call (mk_py_addr_attr cls2 "__contains__" range) [e2; e1] range in
               re_eval_singleton (man.eval ctx) (Some exp', true_flow, [])
            )
            (fun false_flow ->
               Universal.Utils.assume_to_eval
                 (Utils.mk_addr_hasattr cls2 "__iter__" range)
                 (fun true_flow ->
                    let v = mktmp () in
                    let stmt = mk_stmt (S_py_for (
                        mk_var v range,
                        e2,
                        mk_if
                          (mk_binop (mk_var v range) O_eq e1 range)
                          (mk_stmt S_break range)
                          (mk_block [] range)
                          range,
                        (mk_block [] range)
                      )) range
                    in
                    let flow = man.exec ctx stmt true_flow in
                    re_eval_singleton (man.eval ctx) (Some (mk_var v range), flow, [mk_remove_var v range])
                 )
                 (fun false_flow ->
                    Universal.Utils.assume_to_eval
                      (Utils.mk_addr_hasattr cls2 "__getitem__" range)
                      (fun true_flow ->
                         Framework.Exceptions.panic_at range "evaluating 'in' operator using __getitem__ not supported"
                      )
                      (fun false_flow ->
                         let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) false_flow in
                         oeval_singleton (None, flow, [])
                      )
                      man ctx false_flow ()
                 )
                 man ctx false_flow ()
            ) man ctx flow ()
        )

    (* E⟦ e1 in e2 ⟧ *)
    | E_binop(O_py_not_in, e1, e2) ->
      re_eval_singleton (man.eval ctx) (Some (mk_not (mk_binop e1 O_py_in e2 range) range), flow, [])


    (* E⟦ not e ⟧ *)
    | E_unop(O_py_not, e) ->
      let e' =
        if is_bool_function e then e else Utils.mk_builtin_call "bool" [e] e.erange
      in
      Universal.Utils.assume_to_eval e'
        (fun true_flow -> oeval_singleton (Some (mk_false exp.erange), true_flow, []))
        (fun false_flow -> oeval_singleton (Some (mk_true exp.erange), false_flow, []))
        man ctx flow ()

    (* E⟦ e1 op e2 op e3 ... ⟧ *)
    | E_py_multi_compare(left, ops, rights) ->
      debug "multi compare";
      let range = erange exp in
      man.eval ctx left flow |>
      eval_compose (fun left flow ->
          debug "left evaluated";
          let rec aux left flow = function
            | [] ->
              debug "leaf case -> true";
              oeval_singleton (Some (mk_true range), flow, [])

            | (op, right) :: tl ->
              man.eval ctx right flow |>
              eval_compose (fun right flow ->
                  Universal.Utils.assume_to_eval
                    (mk_binop left op right range)
                    (fun true_flow -> aux right true_flow tl)
                    (fun false_flow -> oeval_singleton (Some (mk_false range), flow, []))
                    man ctx flow ()
                )
          in
          aux left flow (List.combine ops rights)
        )


    | _ -> None


  let rec exec man ctx stmt flow =
    let range = srange stmt in
    match skind stmt with
    (* S⟦ ?e ⟧ *)
    | S_assume(e) when is_py_expr e ->
      debug "%a" Framework.Pp.pp_stmt stmt;
      man.eval ctx e flow |>
      eval_to_exec (fun e flow ->
          man.eval ctx (Utils.mk_builtin_call "bool" [e] range) flow |>
          eval_to_exec (fun b flow ->
              match ekind b with
              | E_py_addr_value(_, {ekind = E_constant (C_true)}) -> flow
              | E_py_addr_value(_, {ekind = E_constant (C_false)}) -> man.flow.set TCur man.env.bottom flow
              | _ -> Framework.Exceptions.fail "call to bool returned a non boolean expression %a" Framework.Pp.pp_expr b
            ) (man.exec ctx) man.flow
        ) (man.exec ctx) man.flow |>
      return

    | _ -> None


  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
