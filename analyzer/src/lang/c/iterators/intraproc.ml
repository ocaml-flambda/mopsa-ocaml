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
open Sig.Domain.Stateless
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

  let exec stmt man flow =
    match skind stmt with
    | S_expression { ekind = E_c_assign(x,e); erange } ->
      man.post (mk_assign x e erange) flow |>
      OptionExt.return

    | S_expression(e) when is_c_type e.etyp ->
      Some (
        man.eval e flow >>$ fun e flow ->
        Post.return flow
      )

    | _ -> None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval exp man flow  =
    match ekind exp with
    | E_c_conditional(cond, e1, e2) ->
      assume cond
        ~fthen:(fun flow ->
            man.eval e1 flow |>
            Rewrite.return_eval
          )
        ~felse:(fun flow ->
            man.eval e2 flow |>
            Rewrite.return_eval
          )
        man flow |>
      OptionExt.return

    | E_binop(O_c_and, e1, e2) ->
      assume e1
        ~fthen:(fun flow ->
            man.eval e2 flow |>
            Rewrite.return_eval
          )
        ~felse:(fun flow ->
            Rewrite.return_singleton (Universal.Ast.mk_zero exp.erange) flow
          )
        man flow |>
      OptionExt.return

    | E_binop(O_c_or, e1, e2) ->
      assume e1
        ~fthen:(fun flow ->
            Rewrite.return_singleton (Universal.Ast.mk_one exp.erange) flow
          )
        ~felse:(fun flow ->
            man.eval e2 flow |>
            Rewrite.return_eval
          )
        man flow |>
      OptionExt.return

    | E_c_assign(lval, rval) ->
      man.eval rval flow >>$? fun rval flow ->
      man.post (mk_assign lval rval exp.erange) flow >>$? fun () flow ->
      Rewrite.return_singleton rval flow |>
      OptionExt.return

    | E_c_statement {skind = S_block (l,local_vars)} ->
      begin
        match List.rev l with
        | {skind = S_expression e}::q ->
          let q' = List.rev q in
          let stmt' = mk_block q' (erange exp) in
          man.post stmt' flow >>$? fun () flow ->
          man.eval e flow |>
          Rewrite.return_eval |>
          Cases.add_cleaners (List.map (fun v -> mk_remove_var v exp.erange) local_vars) |>
          OptionExt.return

        | _ -> panic "E_c_statement %a not supported" pp_expr exp
      end

    | E_c_statement {skind = S_expression e} ->
      man.eval e flow |>
      Rewrite.return_eval |>
      OptionExt.return


    | _ -> None


  (** Query handler *)
  (** ============= *)

  let ask _ _ _  = None

end

let () = register_stateless_domain (module Domain)
