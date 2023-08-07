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

(** Interpreter of for and do-while loops. *)

open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open Ast
open Common.Scope_update


(** {2 Domain definition} *)
(** ===================== *)

module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.iterators.loops"
    end)

  let checks = []

  (** Initialization *)
  (** ============== *)

  let init _ _ flow = flow


  let exec stmt man flow =
    match skind stmt with
    | S_c_for(init, cond, incr, body) ->
      let range = stmt.srange in
      (* If init contains variable declarations, change their scope to the scope of the entire block *)
      let init', vars = match skind init with
        | S_block(sl,vl) -> mk_block sl ~vars:[] init.srange, vl
        | _ -> init,[]
      in
      let stmt = Universal.Ast.(
          mk_block ~vars [
            init';
            mk_stmt (S_while (
                (match cond with None -> mk_one range | Some e -> e),
                (match incr with None -> body | Some e -> mk_block [body; mk_stmt (S_expression e) e.erange] body.srange)
              )) range
          ] range
        )
      in
      man.exec stmt flow |> OptionExt.return

    | S_c_do_while(body, {ekind = E_constant (C_int z)}) when Z.equal z Z.zero ->
       man.exec body flow |> OptionExt.return

    | S_c_do_while(body, cond) ->
      (*
         The basic idea is to rewrite a do while into:
         ```
            body;
            mk_stmt (S_while (cond, body)) range
         ```
         However we have to handle break statements from the unrolled first body...
      *)
      let range = stmt.srange in
      let open Universal_iterators__.Loops in
      (* backup previous break and continue flows *)
      let continue_before, break_before = Flow.get T_continue man.lattice flow, Flow.get T_break man.lattice flow in
      let flow = Flow.remove T_continue flow |> Flow.remove T_break in
      man.exec body flow >>%? fun after_one ->
      (* break at first unrolling: will be added after the loop
         continue at first unrolling: added into the state entering the loop *)
      let break, others = Flow.partition (fun tk _ -> tk = T_break) after_one in
      let others = Flow.rename T_continue T_cur man.lattice others in
      Post.join
        (Post.return (Flow.rename T_break T_cur man.lattice break))
        (man.exec (mk_stmt (S_while (cond, body)) range) others) >>%? fun flow ->
      Flow.set T_break break_before man.lattice flow  |>
      Flow.set T_continue continue_before man.lattice |>
      Post.return
      |> OptionExt.return

    | S_c_break upd ->
      update_scope upd stmt.srange man flow >>%? fun flow' ->
      let stmt' = { stmt with skind = S_break } in
      man.exec stmt' flow' |>
      OptionExt.return

    | S_c_continue upd ->
      update_scope upd stmt.srange man flow >>%? fun flow' ->
      let stmt' = { stmt with skind = S_continue } in
      man.exec stmt' flow' |>
      OptionExt.return

    | _ -> None

  let eval _ _ _  = None

  let ask _ _ _  = None

  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
