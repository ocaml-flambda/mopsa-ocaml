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
open Numeric.Common

(******************)
(** Trace markers *)
(******************)

type marker += M_if of bool * expr

let () = register_marker {
    marker_print = (fun next fmt -> function
        | M_if(true, cond) ->
          Format.fprintf fmt "if (%a)" pp_expr cond
        | M_if(false, cond) ->
          Format.fprintf fmt "if (!%a)" pp_expr cond
        | m ->
          next fmt m
      );
    marker_compare = (fun next m1 m2 ->
        match m1, m2 with
        | M_if(branch1, cond1), M_if(branch2, cond2) ->
          Compare.pair Bool.compare compare_expr
            (branch1, cond1) (branch2, cond2)
        | _ ->
          next m1 m2
      );
    marker_name = (fun next -> function
        | M_if _ -> "if"
        | m -> next m
      );
  }

(**********************)
(** Domain definition *)
(**********************)

module Domain =
struct

  include GenStatelessDomainId(
    struct
      let name = "universal.iterators.intraproc"
    end
    )

  let checks = []

  let init prog man flow = None

  let rec negate_bool_expr e =
    match ekind e with
    | E_constant (C_bool true) -> mk_false e.erange
    | E_constant (C_bool false) -> mk_true e.erange
    | E_constant (C_top T_bool) -> e
    | E_unop(O_log_not, ee) -> ee
    | E_binop(O_log_and,e1,e2) -> mk_log_or (negate_bool_expr e1) (negate_bool_expr e2) e.erange
    | E_binop(O_log_or,e1,e2) -> mk_log_and (negate_bool_expr e1) (negate_bool_expr e2) e.erange
    | E_binop(O_log_xor, e1, e2) -> mk_log_xor (negate_bool_expr e1) e2 e.erange
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
    | _ -> assert false

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
      assume (to_bool_expr ee) man flow ~route:(Below name) ~translate:"Universal"
        ~fthen:ftrue
        ~felse:ffalse

  let exec stmt man flow =
    match skind stmt with
    | S_expression e ->
      man.eval e flow >>$? fun e flow ->
      Post.return flow |>
      OptionExt.return

    | S_assign(x,e) when is_universal_type (etyp x) ->
      man.eval e flow ~translate:"Universal" >>$? fun e flow ->
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
      man.eval e flow ~translate:"Universal" >>$? fun e' flow ->
      man.exec (mk_assume e' stmt.srange) flow ~route:(Below name) |>
      OptionExt.return

    (* Skip the analysis of the block if there is no indirect flow and the
       current environment is empty *)
    | S_block (b, cleaner)
      when Flow.is_empty flow ||
           (* no indirect flow *)
           ( Flow.is_singleton flow && Flow.mem T_cur flow &&
             (* empty environment *)
             man.lattice.is_bottom (Flow.get T_cur man.lattice flow) ) ->
      Post.return flow |>
      OptionExt.return


    | S_block(block,local_vars) ->
      Some (
        let post = List.fold_left (fun acc stmt -> acc >>% man.exec stmt) (Post.return flow) block in
        let end_range =
          if is_orig_range stmt.srange
          then set_range_start stmt.srange (get_range_end stmt.srange)
          else stmt.srange in
        let post = List.fold_left (fun acc var -> acc >>% man.exec (mk_remove_var var end_range)) post local_vars in
        post
      )

    (* Skip the analysis of if there is no flow *)
    | S_if(cond, s1, s2) when Flow.is_empty flow ->
      Post.return flow |>
      OptionExt.return

    (* Use [assume], that skips the analyis of a branch if its input environment is empty. *)
    (* This is sound if there is no inderct flow, because [assume] will not
       execute the branch if its [cur] environment is empty, while an indirect
       flow may have an empty [cur] environment. *)
    | S_if(cond, s1, s2) when Flow.is_singleton flow && Flow.mem T_cur flow ->
      assume cond man flow
        ~fthen:(fun flow ->
            man.exec (mk_add_marker (M_if(true, cond)) stmt.srange) flow >>%
            man.exec s1)
        ~felse:(fun flow ->
            man.exec (mk_add_marker (M_if(false, cond)) stmt.srange) flow >>%
            man.exec s2)
      |> OptionExt.return

    | S_if(cond, s1, s2) ->
      (* Use this function to execute a branch when the other one is not
         reachable. In addition to the execution of the body of the reachable branch,
         this function executes the unreachable branch with an empty T_cur
         environment. This ensures that indirect flows in the branch are
         executed. *)
      let exec_one_branch stmt other branch flow =
        let post1 =
          man.exec (mk_add_marker (M_if(branch, cond)) stmt.srange) flow >>%
          man.exec stmt
        in
        let ctx1 = Cases.get_ctx post1 in
        let flow2 = Flow.set_ctx ctx1 flow |>
                    Flow.remove T_cur
        in
        let post2 = man.exec other flow2 in
        Post.join post1 post2
      in
      (* Execute both branches and ensure proper propagation of the context *)
      let exec_both_branches flow1 flow2 =
        let post1 =
          man.exec (mk_add_marker (M_if(true, cond)) stmt.srange) flow1 >>%
          man.exec s1
        in
        let ctx1 = Cases.get_ctx post1 in
        let flow2 = Flow.set_ctx ctx1 flow2 in
        let post2 =
          man.exec (mk_add_marker (M_if(false, cond)) stmt.srange) flow2 >>%
          man.exec s2
        in
        Post.join post1 post2
      in
      assume cond man flow
        ~fthen:(exec_one_branch s1 s2 true)
        ~felse:(exec_one_branch s2 s1 false)
        ~fboth:(exec_both_branches)
        (* When both environment are empty, we still need to execute both
           branches because of eventual indirect flows *)
        ~fnone:(exec_both_branches)
      |>
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
      assume_num e1 man flow
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
      assume_num e1 man flow
        ~fthen:(fun flow -> Eval.singleton (mk_true exp.erange) flow)
        ~felse:(fun flow -> man.eval e2 flow ~translate:"Universal")
      |> OptionExt.return

    | E_binop (O_log_xor, e1, e2)
      when is_universal_type exp.etyp ->
      let s1 =
        assume_num e1 man flow
          ~fthen:(fun flow -> man.eval (mk_not e2 exp.erange) ~translate:"Universal" flow)
          ~felse:(fun flow -> man.eval e2 flow ~translate:"Universal")
      in
      let s2 =
        assume_num (mk_not e1 exp.erange) man flow
          ~fthen:(fun flow -> man.eval e2 flow ~translate:"Universal")
          ~felse:(fun flow -> man.eval (mk_not e2 exp.erange) ~translate:"Universal" flow)
      in
      Eval.join s1 s2 |>
      OptionExt.return

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
      eval_bool_expr exp exp.erange man flow
        ~ftrue:(fun flow -> Eval.singleton (mk_true exp.erange) flow)
        ~ffalse:(fun flow -> Eval.singleton (mk_false exp.erange) flow)
        ~fboth:(fun flow -> Eval.singleton (mk_top T_bool exp.erange) flow) |>
      OptionExt.return

    | E_unop(op,ee) when is_predicate_op op  &&
                         is_universal_type exp.etyp ->
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
