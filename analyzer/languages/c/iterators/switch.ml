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

(** Control flow abstraction for switch statements. *)

open Mopsa
open Sig.Abstraction.Stateless
open Universal.Iterators.Loops
open Ast


module Domain =
struct


  (** {2 Domain header} *)
  (** ***************** *)

  include GenStatelessDomainId(struct
      let name = "c.iterators.switch"
    end)


  let checks = []

  (** {2 Initialization} *)
  (** ****************** *)

  let init _ _ flow =  flow


  (** {2 Token for cases flows} *)
  (** ************************* *)

  type token += T_c_switch_case of expr * range
  type token += T_c_switch_default of range

  let () =
    register_token {
      print = (fun next fmt -> function
          | T_c_switch_case (e,range) -> Format.fprintf fmt "switch-case(%a,%a)" pp_expr e pp_range range
          | T_c_switch_default range -> Format.fprintf fmt "switch-default(%a)" pp_range range
          | t -> next fmt t
        );
      compare = (fun next t1 t2 ->
          match t1,t2 with
          | T_c_switch_case (e1,r1), T_c_switch_case (e2,r2) -> Compare.pair compare_expr compare_range (e1,r1) (e2,r2)
          | T_c_switch_default r1, T_c_switch_default r2 -> compare_range r1 r2
          | _ -> next t1 t2
        );
    }


  (** Get the locations and expressions of cases in a switch
      statements. Cases are ordered similarly to their occurance
      locations in the source code.
  *)
  let get_cases body =
    (* The visitor preserves the order of occurance in source code *)
    let cases, default = Visitor.fold_stmt
        (fun acc e -> Keep acc )
        (fun (cases,default) s ->
           match skind s with
           | S_c_switch _ ->
             (* Do not go inside nested switches *)
             Keep (cases,default)

           | S_c_switch_case (e,_) ->
             (* Note: cases are added in reverse order here. They need
                to be reversed when returned.  *)
             Keep ((e,s.srange) :: cases,default)

           | S_c_switch_default _ ->
             Keep (cases,Some s.srange)

           | _ -> VisitParts (cases,default)
        )
        ([],None) body
    in
    List.rev cases, default


  (** Computation of post-conditions *)
  (** ============================== *)

  (** 𝕊⟦ switch (e) body ⟧ *)
  let exec_switch e body guard_cleaner range man flow =
    (* Save initial state before removing the break flows *)
    let flow0 = flow in
    let flow = Flow.remove T_break flow in

    (* Get the ranges of cases *)
    let cases,default = get_cases body in

    (* Iterate over cases, filter the input environments with the case
       condition and jump to case body *)
    let switch_range = tag_range range "switch" in
    let rec iter cases flow =
      match cases with
      | [] -> man.exec guard_cleaner flow
      | (e',r) :: tl ->
        (* Filter the environments *)
        let cond = mk_binop e O_eq e' ~etyp:u8 switch_range in
        assume cond
          ~fthen:(fun flow ->
              man.exec guard_cleaner flow >>% fun flow ->
              (* Case reachable, so save cur in the flow of the case before removing cur *)
              let cur = Flow.get T_cur man.lattice flow in
              Flow.set (T_c_switch_case (e',r)) cur man.lattice flow |>
              Flow.remove T_cur |>
              Post.return
            )
          ~felse:(fun flow ->
              (* Case unreachable, so check the next cases *)
              iter tl flow
            )
          man flow |>
        Post.remove_duplicates man.lattice
    in

    iter cases flow |>
    (* Merge all cases in one, so that we will execute the body only once *)
    Post.remove_duplicates man.lattice >>% fun flow -> 
    (* Put the remaining cur environments in the flow of the default case. If
       no default case is present, save cur in no_default. *)
    let flow, no_default =
      let cur = Flow.get T_cur man.lattice flow in
      match default with
      | None ->
        let flow = Flow.remove T_cur flow in
        flow, cur
      | Some r ->
        let flow = Flow.set (T_c_switch_default r) cur man.lattice flow |>
                   Flow.remove T_cur
        in
        flow, man.lattice.bottom
    in

    (* Execute the body of the switch statement *)
    man.exec body flow >>% fun flow ->

    (* Merge cur, break and no_default environments *)
    let flow = Flow.add T_cur no_default man.lattice flow |>
               Flow.add T_cur (Flow.get T_break man.lattice flow) man.lattice
    in

    (* Restore the previous break environments *)
    let flow = Flow.set T_break (Flow.get T_break man.lattice flow0) man.lattice flow in

    Post.return flow


  (* 𝕊⟦ case e: ⟧ *)
  let exec_case upd e range man flow =
    (* Get the case environments and update their scope *)
    let env = Flow.get (T_c_switch_case (e,range)) man.lattice flow in
    Flow.add T_cur env man.lattice flow |>
    Flow.remove (T_c_switch_case (e,range)) |>
    Common.Scope_update.update_scope upd range man

  (* 𝕊⟦ default: ⟧ *)
  let exec_default upd range man flow =
    let env = Flow.get (T_c_switch_default range) man.lattice flow in
    Flow.add T_cur env man.lattice flow |>
    Flow.remove (T_c_switch_default range) |>
    Common.Scope_update.update_scope upd range man

  let exec stmt man flow =
    match skind stmt with
    | S_c_switch(e, body) ->
       (
         (* Evaluate e once in case of side-effects *)
         man.eval e flow |>
         bind (fun case flow ->
             match case with
             | Empty -> Cases.empty flow
             | NotHandled -> Cases.not_handled flow
             | Result(e, _, cleaners) ->
               exec_switch e body (Universal.Ast.mk_block (StmtSet.elements cleaners) (erange e)) stmt.srange man flow)
       ) |>
      OptionExt.return

    | S_c_switch_case(e,upd) ->
      exec_case upd e stmt.srange man flow |>
      OptionExt.return

    | S_c_switch_default upd ->
      exec_default upd stmt.srange man flow |>
      OptionExt.return

    | _ -> None

  (** Evaluation of expressions *)
  (** ========================= *)

  let eval exp man flow = None

  (** Answer to queries *)
  (** ================= *)

  let ask _ _ _ = None

  (** Pretty printer *)
  (** ============== *)

  let print_expr _ _ _ _ = ()

end

let () =
    register_stateless_domain (module Domain)
