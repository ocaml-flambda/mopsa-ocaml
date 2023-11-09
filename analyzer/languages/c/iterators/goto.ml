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

(** Control flow abstraction for Goto statements.

  Due to its design, MOPSA drives the analysis by iterating inductively on
  the syntax of the AST. This means that in order to analyze the sequencing
  of multiple statements, MOPSA analyzes each of them in order,
  using as precondition to one statement the postcondition of the
  preceding one.

  For example, given a precondition `pre`, MOPSA computes the postcondition
  of the following program

  ```
    S_1;
    S_2;
    S_3;
  ```

  as `exec S_1 pre |> exec S_2 |> exec S_3`.

  On the other hand, goto statements can be used to arbitrarily redirect
  the control flow to other program points, thus they do not fit well in
  this picture since when a label is reached arbitrary flows may be converging
  to it. For example in the following program

  ```
    S_1;
    if (rand) then goto label;
    S_2;
  label:
    S_3;
  ```

  the precondition for the statment S_3 is not just the one coming naturally
  from the syntax (i.e., the postcondition of S_2), but also the one
  coming from the goto statement.

  To address this issue, MOPSA allows to store additional flows than just the
  natural one in the Flow structure: when a goto statement is encountered,
  the natural flow is saved in a new token corresponding to the goto's target
  and it is reset to ⊥. Later when the target label is reached,
  all the incoming goto flows are joined with the natural one. In the previous example,
  the execution of the goto stores in a new token the natural flow and sets
  it to ⊥. Then when `label` is reached, the natural flow (from S_2) is
  joined with the flow stored in the goto token. In addition, the goto flow
  is removed to save memory.

  This approach works as long as the control flow graph contains only gotos
  targeting labels that occur later in the syntax. To see why this is the
  case, consider the following example:

  ```
    S_1;
  hdr:
    if (b) then goto exit;
    S_body;
    goto hdr;
  exit:
    S_2;
  ```

  In this case, the label `hdr` is executed before the goto, therefore the
  flow coming from the latter is not available when the former is executed.
  Notice also that the postconditon of the program contains not only the
  natural flow, but also a goto flow for the `hdr` label. This is the case
  as the goto flows are removed when the corresponding label are executed,
  but this happens before the goto.

  To ensure the soundness of the analysis, we need to compute an
  over-approximation of the flows reaching `hdr`. This is done, by repeating
  the execution of the program using always the same preconditon, but, crucially,
  by joining the goto flows with the ones obtained in the previous iterations.
  This means that in the previous example, the second iteration would reuse
  the precondition of the first one, but in addition the goto flows from the
  postconditon of the first iteration are added. This procedure is iterated,
  possibly with widening, until the goto flows converge.

*)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Common.Scope_update

let name = "c.iterators.goto"

(*==========================================================================*)
(**                       {2 Command line options}                          *)
(*==========================================================================*)

let opt_goto_down : bool ref = ref false
(** Enable down iterations for goto *)

let () =
  register_domain_option name {
    key = "-goto-down";
    category = "Goto";
    doc = " perform a down iteration after goto stabilization";
    spec = ArgExt.Set opt_goto_down;
    default = "false";
  };



(*==========================================================================*)
(**                            {2 Flow tokens}                              *)
(*==========================================================================*)

type token +=
  | T_goto of string
  (** Goto environments *)


let () =
  register_token
    {
      print = (fun next fmt -> function
          | T_goto str -> Format.fprintf fmt "goto %s" str
          | tk -> next fmt tk
        );
      compare = (fun next a b ->
          match a,b with
          | T_goto x, T_goto y -> compare x y
          | _ -> next a b
        )
    }

(*==========================================================================*)
(**                        {2 Abstract domain}                              *)
(*==========================================================================*)


module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.iterators.goto"
    end)

  let checks = []

  (** Initialization *)
  (** ============== *)

  let init _ _ flow =  flow

  let exec stmt man flow =
    match skind stmt with
    | S_c_goto (s,upd) ->
      (* Save T_cur env in T_goto s token, then set T_cur to bottom. *)
      update_scope upd stmt.srange man flow >>%? fun flow ->
      let cur = Flow.get T_cur man.lattice flow in
      let flow0 = Flow.add (T_goto s) cur man.lattice flow |>
                  Flow.remove T_cur
      in
      Post.return flow0 |>
      OptionExt.return

    | S_c_label s ->
      (* Moves flow in goto label inside current *)
      let fromlbl = Flow.get (T_goto s) man.lattice flow in
      let flow0 = Flow.add T_cur fromlbl man.lattice flow |>
                  Flow.remove (T_goto s)
      in
      Post.return flow0 |>
      OptionExt.return

    | S_c_goto_stab stmt' ->
      (* Compute a postcondition of a block possibly containing
         backward gotos. This is sound as long as no goto statements
         outside this block refer a label in the block
       *)
      let bottom = Flow.bottom_from flow in
      (* Ignore goto flows from gotos and label outside this block,
         they will be added later in the postcondition
       *)
      let nogotos, gotos = Flow.fold (fun (nogotos, gotos) k v ->
          match k with
          | T_goto _ -> (nogotos, Flow.add k v man.lattice gotos)
          | _       -> (Flow.add k v man.lattice nogotos, gotos)
        ) (bottom, bottom) flow in
      let init_report = Flow.get_report flow in
      (* retain only goto tokens *)
      let get_gotos f = Flow.filter
          (fun t e -> match t with | T_goto _ -> true | _ -> false)
          f
      in
      (* drop all goto tokens *)
      let drop_gotos f = Flow.filter
          (fun t e -> match t with | T_goto s -> false | _ -> true)
          f
      in
      let rec next f i wid_limit =
        man.exec stmt' f >>% fun f' ->
        let f1, f1' = get_gotos f, get_gotos f' in
        if Flow.subset man.lattice f1' f1 then
          (* goto flows are stable *)
          if !opt_goto_down then
            down (drop_gotos f |> Flow.join man.lattice f1' |> Flow.set_report init_report)
          else
            Post.return f'
        else
          (* for the next iteration, we have to reuse the
             same precondition, only with the new goto flows *)
          let f2' = Flow.join man.lattice f f1' in
          if i >= wid_limit
          then next (Flow.widen man.lattice f f2') (i+1) wid_limit
          else next f2' (i+1) wid_limit
      and down f =
        man.exec stmt' f >>% fun f' ->
        Post.return f'
      in
      next nogotos 0 1 >>%? fun flow1 ->
      let flow1_minus_gotos = Flow.filter (fun k v ->
          match k with
          | T_goto s -> false | _ -> true
        ) flow1
      in
      let flow2 = Flow.join man.lattice gotos flow1_minus_gotos in
      Post.return flow2 |>
      OptionExt.return

    | _ -> None

  let eval man exp flow = None

  let ask _ _ _  = None

  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
