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

(** Desugar conditional expressions. *)

open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open Ast



(** {2 Domain definition} *)
(** ===================== *)

module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.iterators.intraproc"
    end)

  let dependencies = []

  let alarms = []


  (** Initialization *)
  (** ============== *)

  let init _ _ flow = flow


  (** Post-condition computation *)
  (** ========================== *)

  let exec_add b range man flow =
    man.exec ~route:Below (mk_add b range) flow

  let exec_remove b range man flow =
    man.exec ~route:Below (mk_remove b range) flow >>%
    man.exec ~route:Below (mk_invalidate b range)

  let exec_rename b1 b2 range man flow =
    man.exec ~route:Below (mk_rename b1 b2 range) flow >>% fun flow ->
    let bb1 = to_c_block_object b1 in
    let bb2 = to_c_block_object b2 in
    man.exec ~route:Below (mk_rename bb1 bb2 range) flow

  let exec_forget b range man flow =
    man.exec ~route:Below (mk_forget b range) flow

  let exec_expand b bl range man flow =
    man.exec ~route:Below (mk_expand b bl range) flow >>% fun flow ->
    let bb = to_c_block_object b in
    let bbl = List.map to_c_block_object bl in
    man.exec ~route:Below (mk_expand bb bbl range) flow


  let exec_fold b bl range man flow =
    man.exec ~route:Below (mk_fold b bl range) flow >>% fun flow ->
    let bb = to_c_block_object b in
    let bbl = List.map to_c_block_object bl in
    man.exec ~route:Below (mk_fold bb bbl range) flow

  
  let exec stmt man flow =
    match skind stmt with
    | S_add b when is_c_type b.etyp ->
      exec_add b stmt.srange man flow |>
      OptionExt.return

    | S_remove b when is_c_type b.etyp ->
      exec_remove b stmt.srange man flow |>
      OptionExt.return

    | S_rename(b1,b2) when is_c_type b1.etyp && is_c_type b2.etyp ->
      exec_rename b1 b2 stmt.srange man flow |>
      OptionExt.return

    | S_forget b when is_c_type b.etyp ->
      exec_forget b stmt.srange man flow |>
      OptionExt.return

    | S_expand(b,bl) when is_c_type b.etyp && List.for_all (fun b -> is_c_type b.etyp) bl  ->
      exec_expand b bl stmt.srange man flow |>
      OptionExt.return

    | S_fold(b,bl) when is_c_type b.etyp && List.for_all (fun b -> is_c_type b.etyp) bl  ->
      exec_fold b bl stmt.srange man flow |>
      OptionExt.return

    | S_assume { ekind = E_binop (O_c_and, e1, e2) } ->
      man.exec (mk_assume e1 stmt.srange) flow >>%? fun flow ->
      man.exec (mk_assume e2 stmt.srange) flow |>
      OptionExt.return

    | S_assume { ekind = E_binop (O_c_or, e1, e2) } ->
      let post1 = man.exec (mk_assume e1 stmt.srange) flow in
      let post2 = man.exec (mk_assume e2 stmt.srange) flow in
      Post.join post1 post2 |>
      OptionExt.return

    | S_assume { ekind = E_unop (O_log_not, { ekind = E_binop (O_c_and, e1, e2); etyp }); erange } ->
      man.exec (mk_assume (mk_binop (mk_not e1 e1.erange) O_c_or (mk_not e2 e2.erange) ~etyp erange) stmt.srange) flow |>
      OptionExt.return

    | S_assume { ekind = E_unop (O_log_not, { ekind = E_binop (O_c_or, e1, e2); etyp }); erange } ->
      man.exec (mk_assume (mk_binop (mk_not e1 e1.erange) O_c_and (mk_not e2 e2.erange) ~etyp erange) stmt.srange) flow |>
      OptionExt.return

    | _ -> None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval exp man flow  =
    match ekind exp with
    | E_c_conditional(cond, e1, e2) ->
      assume cond
        ~fthen:(fun flow ->
            man.eval e1 flow
          )
        ~felse:(fun flow ->
            man.eval e2 flow
          )
        man flow |>
      OptionExt.return

    | E_binop(O_c_and, e1, e2) ->
      assume e1
        ~fthen:(fun flow ->
            assume e2 man flow
              ~fthen:(Eval.singleton (mk_true exp.erange))
              ~felse:(Eval.singleton (mk_false exp.erange))
          )
        ~felse:(fun flow ->
            Eval.singleton (mk_false exp.erange) flow
          )
        man flow |>
      OptionExt.return

    | E_binop(O_c_or, e1, e2) ->
      assume e1
        ~fthen:(fun flow ->
            Eval.singleton (mk_true exp.erange) flow
          )
        ~felse:(fun flow ->
            assume e2 man flow
              ~fthen:(Eval.singleton (mk_true exp.erange))
              ~felse:(Eval.singleton (mk_false exp.erange))
          )
        man flow |>
      OptionExt.return

    | E_c_assign(lval, rval) ->
      man.eval rval flow >>$? fun rval flow ->
      man.exec (mk_assign lval rval exp.erange) flow >>%? fun flow ->
      Eval.singleton rval flow |>
      OptionExt.return

    | E_c_statement {skind = S_block (l,local_vars)} ->
      begin
        match List.rev l with
        | {skind = S_expression e}::q ->
          let q' = List.rev q in
          let stmt' = mk_block q' (erange exp) in
          man.exec stmt' flow >>%? fun flow ->
          man.eval e flow |>
          Cases.add_cleaners (List.map (fun v -> mk_remove_var v exp.erange) local_vars) |>
          OptionExt.return

        | _ -> panic "E_c_statement %a not supported" pp_expr exp
      end

    | E_c_statement {skind = S_expression e} ->
      man.eval e flow |>
      OptionExt.return


    | _ -> None


  (** Query handler *)
  (** ============= *)

  let ask _ _ _  = None

end

let () = register_stateless_domain (module Domain)
