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

(** Desugaring loops into a usual case *)

open Mopsa
open Sig.Abstraction.Stateless
open Addr
open Universal.Ast
open Ast

let opt_desugar_for_range = ref true
let opt_desugar_for_tuple = ref true

let () =
  register_language_option "python" {
      key = "-py-disable-desugar-for-range";
      category = "Loops";
      doc = " disable the special desugaring on for-range-based loops (desugaring not worth it when no numerical domains are available)";
      spec = ArgExt.Clear opt_desugar_for_range;
      default = "enabled";
    };
  register_language_option "python" {
      key = "-py-disable-desugar-for-tuple";
      category = "Loops";
      doc = " disable the special desugaring on for ... in (...) loops";
      spec = ArgExt.Clear opt_desugar_for_tuple;
      default = "enabled";
    }

module Domain =
  struct

    include GenStatelessDomainId(struct
        let name = "python.desugar.loops"
      end)

    let checks = []

    let init _ _ flow = flow
    let eval _ _ _ = None


    let exec stmt man flow =
      let range = srange stmt in
      match skind stmt with
      | S_py_for(target, iterable, body, {skind = S_block ([], _)}) ->
        (* Here, we will just focus on the case where there is no else statement to the for loop *)
        (* Goal: Rewrite for target in iterable: body into

           it = iterable
           while(1):
             try:
               target = next(it)
             except StopIteration:
               break
             body

           You will probably need:
           - Utils.mk_builtin_call (in `../lang/`)
           - Utils.mk_try_stopiteration
           - mk_while (in `languages/universal/lang/ast`)
           - man.exec
        *)
        panic_at range "for loop to implement in one of the tracks of this tutorial!"

      | S_py_while (test, body, {skind = S_block ([], _)}) ->
        man.exec
          (mk_while
             (Utils.mk_builtin_call "bool" [test] range)
             body
             range
          ) flow
        |> OptionExt.return

      | S_py_while (test, body, orelse) ->
        (* the else clause is supposed to be applied when the loop exits, and when this is not due to a break *)
        warn_at range "else/while statement not precise";
        Some
          (man.exec
             (mk_while
                (Utils.mk_builtin_call "bool" [test] range)
                body
                range
             ) flow >>% fun flow ->
           man.exec orelse flow >>% fun else_flow ->
           Post.return (Flow.join man.lattice flow else_flow)
          )

      | _ -> None

    let ask _ _ _ = None

    let print_expr _ _ _ _ = ()

  end

let () =
  register_stateless_domain (module Domain)
