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
      | S_py_while (test, body, {skind = S_block ([], _)}) ->
        let start = Timing.start () in
        let res = man.exec
            (mk_while
               (Utils.mk_builtin_call "bool" [test] range)
               body
               range
            ) flow
                  >>% Post.return
                  |> OptionExt.return in
        Debug.debug ~channel:"profiling" "while loop at range %a: %.4f" pp_range range (Timing.stop start);
        res

      | S_py_while (test, body, orelse) ->
        (* the else clause is supposed to be applied when the loop exits, and when this is not due to a break *)
        warn_at range "else/while statement not precise";
        let start = Timing.start () in
        let res = man.exec
            (mk_while
               (Utils.mk_builtin_call "bool" [test] range)
               body
               range
            ) flow |> post_to_flow man in
        let res = Flow.join man.lattice res (man.exec orelse res |> post_to_flow man) in
        Debug.debug ~channel:"profiling" "while loop at range %a: %.4f" pp_range range (Timing.stop start);
        res |> Post.return |> OptionExt.return

      | S_py_for(target, ({ekind = E_py_object (addr, oe)} as iterable), body, orelse) when match akind addr, skind orelse with
                                                                                            | A_py_instance {addr_kind =  A_py_class (C_builtin "range", _)}, S_block ([], _) -> false
                                                                                            | _ -> true ->
         (* iter is better than iterable.__iter__, as the error
           created is generated by iter() (TypeError: '...' object is
           not iterable), and is not an AttributeError stating that
           __iter__ does not exist *)
         (* same for next *)
         let start = Timing.start () in
         let res =
           Utils.bind_list_args man [Utils.mk_builtin_call "iter" [iterable] iterable.erange] flow range
             (fun vars flow ->
               let tmp = mk_var (List.hd vars) range in
               let l_else =
                 match skind orelse with
                 | S_block ([],_) -> [mk_stmt S_break range]
                 | _ -> [orelse; mk_stmt S_break range] in
               let inner_block  =
                 begin match skind body with
                 | S_block (l,_) ->
                    (mk_block
                       ((Utils.mk_try_stopiteration
                           (mk_assign
                              target
                              (Utils.mk_builtin_call "next" [tmp] range)
                              range
                           )
                           (mk_block l_else range)
                           range) :: l) range)
                 | _ ->
                    (mk_block
                       [Utils.mk_try_stopiteration
                          (mk_assign
                             target
                             (Utils.mk_builtin_call "next" [tmp] range)
                             range
                          )
                          (mk_block l_else range) range
                       ; body] range)
                 end in
               let stmt =
                 mk_while
                   (mk_py_true range)
                   inner_block
                   range
               in
               man.exec stmt flow >>%
                 Post.return
             )
         in
         Debug.debug ~channel:"profiling" "for loop at range %a: %.4f" pp_range range (Timing.stop start);
         res |> OptionExt.return

      | S_py_for(target, iterable, body, orelse) when
             match ekind iterable with
             | E_py_object _ -> false
             | _ -> true ->
                man.eval iterable flow
                |> bind_result
                     (fun iterable flow -> man.exec {stmt with skind = S_py_for(target, iterable, body, orelse)} flow >>% Post.return)
                |> OptionExt.return

      | _ -> None

    let ask _ _ _ = None

  end

let () =
  register_stateless_domain (module Domain)
