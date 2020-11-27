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

(** Intra-procedural iterator for blocks, assignments and tests *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast


module Domain =
struct

  include GenStatelessDomainId(
    struct
      let name = "universal.iterators.intraproc"
    end
    )

  let checks = []

  let init prog man flow = flow

  let rec negate_bool_expr e =
    match ekind e with
    | E_constant (C_bool true) -> mk_false e.erange
    | E_constant (C_bool false) -> mk_true e.erange
    | E_constant (C_top T_bool) -> e
    | E_unop(O_log_not, ee) -> ee
    | E_binop(O_log_and,e1,e2) -> mk_log_or (negate_bool_expr e1) (negate_bool_expr e2) e.erange
    | E_binop(O_log_or,e1,e2) -> mk_log_and (negate_bool_expr e1) (negate_bool_expr e2) e.erange
    | E_binop(op,e1,e2) when is_comparison_op op -> mk_binop e1 (negate_comparison_op op) e2 e.erange ~etyp:T_bool
    | _ -> mk_not e e.erange

  let rec to_bool_expr e =
    match ekind e with
    | E_constant (C_bool _) -> e
    | E_constant (C_top T_bool) -> e
    | E_var _ -> e
    | E_unop(O_log_not,e) -> negate_bool_expr (to_bool_expr e)
    | E_unop(op,_) when is_predicate_op op -> e
    | E_binop(op,_,_) when is_comparison_op op -> e
    | E_binop(op,e1,e2) when is_logic_op op -> mk_binop (to_bool_expr e1) op (to_bool_expr e2) e.erange ~etyp:T_bool
    | _ -> ne e zero e.erange

  let rec eval_bool_expr e ~ftrue ~ffalse ~fboth range man flow =
    let ee =
      match expr_to_const e with
      | Some c -> { e with ekind = E_constant c }
      | None -> e
    in
    match ekind ee with
    | E_constant (C_bool true) -> ftrue flow
    | E_constant (C_bool false) -> ffalse flow
    | E_constant (C_int n) -> if Z.(n <> zero) then ftrue flow else ffalse flow
    | E_constant (C_top T_bool) -> fboth flow
    | E_constant (C_top T_int) -> fboth flow
    | E_unop(O_log_not,ee) -> eval_bool_expr ee ~ftrue:ffalse ~ffalse:ftrue ~fboth range man flow
    | _ ->
      assume (to_bool_expr ee) man flow ~route:(Below name)
        ~fthen:ftrue
        ~felse:ffalse

  let exec stmt man flow =
    match skind stmt with
    | S_expression e ->
      man.eval e flow >>$? fun e flow ->
      Post.return flow |>
      OptionExt.return

    | S_assign(x,e) when is_universal_type (etyp e) ->
      man.eval e flow >>$? fun e flow ->
      man.exec (mk_assign x e stmt.srange) flow ~route:(Below name) |>
      OptionExt.return

    | S_assume{ekind = E_constant (C_bool b)} ->
      Post.return (if b then flow else Flow.remove T_cur flow) |>
      OptionExt.return

    | S_assume{ekind = E_unop(O_log_not, {ekind = E_constant (C_bool b)})} ->
      Post.return (if not b then flow else Flow.remove T_cur flow) |>
      OptionExt.return

    | S_assume{ekind = E_constant (C_int n)} ->
      Post.return (if Z.(n <> zero) then flow else Flow.remove T_cur flow) |>
      OptionExt.return

    | S_assume{ekind = E_unop(O_log_not, {ekind = E_constant (C_int n)})} ->
      Post.return (if Z.(n = zero) then flow else Flow.remove T_cur flow) |>
      OptionExt.return

    | S_assume e when is_universal_type (etyp e) ->
      man.eval e flow >>$? fun e flow ->
      eval_bool_expr e stmt.srange man flow
        ~ftrue:(fun flow -> Post.return flow)
        ~ffalse:(fun flow -> Post.return (Flow.remove T_cur flow))
        ~fboth:(fun flow -> Post.return flow) |>
      OptionExt.return

    | S_block(block,local_vars) ->
      Some (
        let post = List.fold_left (fun acc stmt -> acc >>% man.exec stmt) (Post.return flow) block in
        let post = List.fold_left (fun acc var -> acc >>% man.exec (mk_remove_var var stmt.srange)) post local_vars in
        post
      )

    | S_if(cond, s1, s2) ->
      (* First, evaluate the condition *)
      let evl = man.eval cond flow in
      (* Filter flows that satisfy the condition.
         Note that cleaners returned by the evaluation should be removed just after
         applying the filter and before executing the body of the branch. *)
      let then_post = ( evl >>$ fun cond flow ->
                        man.exec (mk_assume cond cond.erange) flow
                      ) |>
                      (* Execute the cleaners of the evaluation here *)
                      exec_cleaners man >>%
                      man.exec s1
      in
      (* Propagate the flow-insensitive context to the other branch *)
      let then_ctx = Cases.get_ctx then_post in
      let evl' = Cases.set_ctx then_ctx evl in
      let else_post = ( evl' >>$ fun cond flow ->
                        man.exec (mk_assume (mk_not cond cond.erange) cond.erange) flow
                      ) |>
                      (* Execute the cleaners of the evaluation here *)
                      exec_cleaners man >>%
                      man.exec s2
      in
      Post.join then_post else_post |>
      OptionExt.return

    | S_print_state ->
      let printer = empty_printer () in
      Flow.print man.lattice.print printer flow;
      Framework.Output.Factory.print printer (srange stmt);
      Some (Post.return flow)

    | S_print_expr el ->
      let printer = empty_printer () in
      List.iter (man.print_expr flow printer) el;
      Framework.Output.Factory.print printer (srange stmt);
      Some (Post.return flow)


    | _ -> None

  let is_not_universal e = not (is_universal_type e.etyp)

  let eval exp man flow =
    match ekind exp with
    | E_binop (O_log_and, e1, e2)
      when is_universal_type exp.etyp ->
      assume e1 man flow
        ~fthen:(fun flow ->
            (* Since we didn't check the type of the sub-expression [e1], we
               need to translate to Universal (if this isn't the case already).
               That way, we can handle expressions from other semantics, as long
               as they can be translated to Universal.
               Note that we need to do that because we checked that the type of
               the whole expression is Universal. *)
            man.eval e2 flow ~translate:"Universal"
          )
        ~felse:(fun flow -> Eval.singleton (mk_false exp.erange) flow)
      |> OptionExt.return

    | E_binop (O_log_or, e1, e2)
      when is_universal_type exp.etyp ->
      assume e1 man flow
        ~fthen:(fun flow -> Eval.singleton (mk_true exp.erange) flow)
        ~felse:(fun flow -> man.eval e2 flow ~translate:"Universal")
      |> OptionExt.return

    | E_unop (O_log_not, { ekind = E_binop (O_log_and, e1, e2) })
      when is_universal_type exp.etyp ->
      man.eval (mk_log_or (mk_not e1 e1.erange) (mk_not e2 e2.erange) exp.erange) flow |>
      OptionExt.return

    | E_unop (O_log_not, { ekind = E_binop (O_log_or, e1, e2) })
      when is_universal_type exp.etyp ->
      man.eval (mk_log_and (mk_not e1 e1.erange) (mk_not e2 e2.erange) exp.erange) flow |>
      OptionExt.return

    | E_binop(op,e1,e2)
      when is_comparison_op op  &&
           is_universal_type exp.etyp ->
      man.eval exp ~route:(Below name) flow ~translate:"Universal" >>$? fun exp flow ->
      eval_bool_expr exp exp.erange man flow
        ~ftrue:(fun flow -> Eval.singleton (mk_true exp.erange) flow)
        ~ffalse:(fun flow -> Eval.singleton (mk_false exp.erange) flow)
        ~fboth:(fun flow -> Eval.singleton (mk_top T_bool exp.erange) flow) |>
      OptionExt.return

    | E_unop(op,ee) when is_predicate_op op  &&
                         is_universal_type exp.etyp ->
      man.eval exp ~route:(Below name) flow ~translate:"Universal" >>$? fun exp flow ->
      eval_bool_expr exp exp.erange man flow
        ~ftrue:(fun flow -> Eval.singleton (mk_true exp.erange) flow)
        ~ffalse:(fun flow -> Eval.singleton (mk_false exp.erange) flow)
        ~fboth:(fun flow -> Eval.singleton (mk_top T_bool exp.erange) flow) |>
      OptionExt.return

    | _ -> None


  let ask query man flow = None

  let print_expr man flow printer exp = ()

end

let () =
  register_stateless_domain (module Domain)
