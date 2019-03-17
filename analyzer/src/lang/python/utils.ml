(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2019 The MOPSA Project.                                    *)
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

open Framework.Ast
open Ast

let debug fmt = Debug.debug ~channel:"python.utils" fmt

let rec partition_list_by_length n l =
  if n = 0 then [], l
  else
    match l with
    | hd :: tl ->
      let lhd, ltl = partition_list_by_length (n-1) tl in
      hd :: lhd, ltl
    | _ -> assert false

let mk_builtin_raise exn range =
  mk_stmt (S_py_raise (Some (mk_py_object (Addr.find_builtin exn) range))) range

let mk_builtin_call f params range =
  mk_py_call (mk_py_object (Addr.find_builtin f) range) params range

let mk_hasattr obj attr range =
  mk_builtin_call "hasattr" [obj; Universal.Ast.mk_string attr range] range

let mk_object_hasattr obj attr range =
  mk_hasattr (mk_py_object obj range) attr range

let mk_addr_hasattr obj attr range =
  mk_hasattr (Universal.Ast.mk_addr obj range) attr range


let mk_try_stopiteration body except range =
  mk_try
    body
    [mk_except
       (Some (mk_py_object (Addr.find_builtin "StopIteration") range)) (* (mk_addr (fst @@ Addr.find_builtin "StopIteration") range)) *)
       None
       except
    ]
    (Universal.Ast.mk_block [] range)
    (Universal.Ast.mk_block [] range)
    range

let check_instances ?(arguments_after_check=0) man flow range exprs instances processing =
  let open Mopsa in
  let tyerror = fun flow -> man.exec (mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
  let rec aux iexprs lexprs linstances flow =
    match lexprs, linstances with
    | _, [] ->
      if arguments_after_check = List.length lexprs then
        processing iexprs flow
      else
        tyerror flow
    | e::es, i::is ->
      Eval.assume (Addr.mk_py_isinstance_builtin e i range) man flow
        ~fthen:(aux iexprs es is)
        ~felse:tyerror
    | [], _ -> tyerror flow in
  Eval.eval_list exprs man.eval flow |>
  Eval.bind (fun exprs flow -> aux exprs exprs instances flow)

let check_instances_disj ?(arguments_after_check=0) man flow range exprs instances processing =
  let open Mopsa in
  let tyerror = fun flow -> man.exec (mk_builtin_raise "TypeError" range) flow |> Eval.empty_singleton in
  let rec aux iexprs lexprs linstances flow =
    match lexprs, linstances with
    | _, [] ->
      if arguments_after_check = List.length lexprs then
        processing iexprs flow
      else
        tyerror flow
    | e::es, i::is ->
      (*   let rec aux2 instances flow =
       *     match instances with
       *     | [] -> tyerror flow
       *     | inst::instl ->
       *       Eval.assume (Addr.mk_py_isinstance_builtin e inst range) man flow
       *         ~fthen:(aux iexprs es is)
       *         ~felse:(aux2 instl) in
       *   aux2 i flow
       * | [], _ -> assert false in *)
      let mk_onecond = fun i -> Addr.mk_py_isinstance_builtin e i range in
      let cond = List.fold_left (fun acc el ->
          mk_binop acc O_py_or (mk_onecond el) range)
          (mk_onecond @@ List.hd i) (List.tl i) in
      Eval.assume cond man flow
        ~fthen:(aux iexprs es is)
        ~felse:tyerror
    | _ -> tyerror flow
  in
  Eval.eval_list exprs man.eval flow |>
  Eval.bind (fun exprs flow -> aux exprs exprs instances flow)
