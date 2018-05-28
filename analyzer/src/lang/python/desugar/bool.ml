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
open Framework.Flow
open Framework.Eval
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
    | E_binop(O_py_and, e1, e2) ->
      man.eval ctx e1 flow |>
      eval_compose
        (fun e1 flow1 ->
           Universal.Utils.assume_to_eval e1
             (fun true_flow -> Some (man.eval ctx e2 true_flow))
             (fun false_flow -> oeval_singleton (Some e1, false_flow, []))
             man ctx flow ()
        )

    | E_binop(O_py_is, e1, e2) ->
      eval_list [e1; e2] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
          let cls1 = Addr.classof e1 and cls2 = Addr.classof e2 in
          if compare_addr cls1 cls2 <> 0 then
            oeval_singleton (Some (mk_false range), flow, [])
          else
            match etyp e1, ekind e1, ekind e2 with
            | T_string, _, _ ->
              oeval_join
                (oeval_singleton (Some (mk_false exp.erange), flow, []))
                (oeval_singleton (Some (mk_true exp.erange), flow, []))

            | T_addr, E_addr a1, E_addr a2 ->
              begin
                match Universal.Heap.Recency.is_weak a1, Universal.Heap.Recency.is_weak a2, compare_addr a1 a2 = 0 with
                | false, false, true -> oeval_singleton (Some (mk_true range), flow, [])
                | false, false, false -> oeval_singleton (Some (mk_false range), flow, [])
                | true, true, false -> oeval_singleton (Some (mk_false range), flow, [])
                | _ ->
                  oeval_join
                    (oeval_singleton (Some (mk_false range), flow, []))
                    (oeval_singleton (Some (mk_true range), flow, []))
              end

            | _ -> re_eval_singleton (man.eval ctx) (Some (mk_binop e1 O_eq e2 range), flow, [])
        )

    | E_binop(O_py_is_not, e1, e2) ->
      re_eval_singleton (man.eval ctx) (Some (mk_not (mk_binop e1 O_py_is e2 range) range), flow, [])

    | E_binop(O_py_or, e1, e2) ->
      man.eval ctx e1 flow |>
      eval_compose
        (fun e1 flow1 ->
           Universal.Utils.assume_to_eval e1
             (fun true_flow -> oeval_singleton (Some e1, true_flow, []))
             (fun false_flow -> Some (man.eval ctx e2 false_flow))
             man ctx flow ()
        )

    | E_unop(O_py_not, e) ->
      let e' =
        if is_bool_function e then e else Utils.mk_builtin_call "bool" [e] e.erange
      in
      Universal.Utils.assume_to_eval e'
        (fun true_flow -> oeval_singleton (Some (mk_false exp.erange), true_flow, []))
        (fun false_flow -> oeval_singleton (Some (mk_true exp.erange), false_flow, []))
        man ctx flow ()

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


  let exec _ _ _ _  = None

  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
