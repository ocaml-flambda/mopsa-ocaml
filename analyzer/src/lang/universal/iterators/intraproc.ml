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

  let alarms = []

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
    | _ -> assert false

  let rec to_bool_expr e =
    match ekind e with
    | E_constant (C_bool _) -> e
    | E_constant (C_top T_bool) -> e
    | E_unop(O_log_not,e) -> negate_bool_expr (to_bool_expr e)
    | E_unop(op,e) when is_predicate_op op -> e
    | E_binop(op,_,_) when is_comparison_op op -> e
    | E_binop(op,e1,e2) when is_logic_op op -> mk_binop (to_bool_expr e1) op (to_bool_expr e2) e.erange ~etyp:T_bool
    | _ -> ne e zero e.erange

  let eval_bool_expr e ~ftrue ~ffalse ~fboth range man flow =
    match ekind e with
    | E_constant (C_bool true) -> ftrue flow
    | E_constant (C_bool false) -> ffalse flow
    | E_constant (C_top T_bool) -> fboth flow
    | _ ->
      assume (to_bool_expr e) man flow ~route:Below
        ~fthen:ftrue
        ~felse:ffalse

  let exec stmt man flow =
    match skind stmt with
    | S_expression e ->
      man.eval e flow >>$? fun e flow ->
      Post.return flow |>
      OptionExt.return

    | S_assign(x,e) ->
      man.eval e flow >>$? fun e flow ->
      man.post (mk_assign x e stmt.srange) flow ~route:Below |>
      OptionExt.return

    | S_assume{ekind = E_constant (C_bool true)}
    | S_assume{ekind = E_unop(O_log_not, {ekind = E_constant (C_bool false)})} ->
      Post.return flow |>
      OptionExt.return

    | S_assume{ekind = E_constant (C_bool false)}
    | S_assume{ekind = E_unop(O_log_not, {ekind = E_constant (C_bool true)})} ->
      Post.return (Flow.bottom_from flow) |>
      OptionExt.return

    | S_assume e ->
      man.eval e flow >>$? fun e flow ->
      eval_bool_expr e stmt.srange man flow
        ~ftrue:(fun flow -> Post.return flow)
        ~ffalse:(fun flow -> Post.return (Flow.bottom_from flow))
        ~fboth:(fun flow -> Post.return flow) |>
      OptionExt.return

    | S_block(block,local_vars) ->
      Some (
        let flow = List.fold_left (fun acc stmt -> man.exec stmt acc) flow block in
        let flow = List.fold_left (fun acc var -> man.exec (mk_remove_var var stmt.srange) acc) flow local_vars in
        Post.return flow
      )

    | S_if(cond, s1, s2) ->
      man.eval cond flow >>$? fun cond flow ->
      let then_flow = man.exec (mk_assume cond cond.erange) flow |>
                      man.exec s1
      in
      let else_flow = Flow.copy_ctx then_flow flow |>
                      man.exec (mk_assume (mk_not cond cond.erange) cond.erange) |>
                      man.exec s2
      in
      Flow.join man.lattice then_flow else_flow |>
      Post.return |>
      OptionExt.return

    | S_print ->
      Framework.Output.Factory.print (srange stmt) (Flow.print man.lattice.print) flow;
      Some (Post.return flow)

    | _ -> None


  let eval exp man flow =
    match ekind exp with
    | E_binop (O_log_and, e1, e2) ->
      assume e1 man flow
        ~fthen:(fun flow -> man.eval e2 flow)
        ~felse:(fun flow -> Eval.singleton (mk_false exp.erange) flow)
      |> OptionExt.return

    | E_binop (O_log_or, e1, e2) ->
      assume e1 man flow
        ~fthen:(fun flow -> Eval.singleton (mk_true exp.erange) flow)
        ~felse:(fun flow -> man.eval e2 flow)
      |> OptionExt.return

    | E_unop (O_log_not, { ekind = E_unop (O_log_not, e) }) ->
      man.eval e flow |>
      OptionExt.return

    | E_unop (O_log_not, { ekind = E_binop (O_log_and, e1, e2) }) ->
      man.eval (mk_log_or (mk_not e1 e1.erange) (mk_not e2 e2.erange) exp.erange) flow |>
      OptionExt.return

    | E_unop (O_log_not, { ekind = E_binop (O_log_or, e1, e2) }) ->
      man.eval (mk_log_and (mk_not e1 e1.erange) (mk_not e2 e2.erange) exp.erange) flow |>
      OptionExt.return

    | E_binop(op,e1,e2) when is_comparison_op op ->
      man.eval exp ~route:Below flow >>$? fun exp flow ->
      eval_bool_expr exp exp.erange man flow
        ~ftrue:(fun flow -> Eval.singleton (mk_true exp.erange) flow)
        ~ffalse:(fun flow -> Eval.singleton (mk_false exp.erange) flow)
        ~fboth:(fun flow -> Eval.singleton (mk_top T_bool exp.erange) flow) |>
      OptionExt.return

    | E_unop(op,ee) when is_predicate_op op ->
      man.eval exp ~route:Below flow >>$? fun exp flow ->
      eval_bool_expr exp exp.erange man flow
        ~ftrue:(fun flow -> Eval.singleton (mk_true exp.erange) flow)
        ~ffalse:(fun flow -> Eval.singleton (mk_false exp.erange) flow)
        ~fboth:(fun flow -> Eval.singleton (mk_top T_bool exp.erange) flow) |>
      OptionExt.return

    | _ -> None

  let ask query man flow = None

end

let () =
  register_stateless_domain (module Domain)
