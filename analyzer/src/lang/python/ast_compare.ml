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

(** Pretty printer of the Python extension to the AST. *)

open Mopsa
open Ast
open Universal.Ast
open Addr
open Format


let () =
  register_expr_compare (fun default e1 e2 ->
      match ekind e1, ekind e2 with
      | E_py_undefined b1, E_py_undefined b2 -> Pervasives.compare b1 b2
      | E_py_object (a1, oe1), E_py_object (a2, oe2) ->
        Compare.compose
          [ (fun () -> compare_addr a1 a2);
            (fun () -> Compare.option compare_expr oe1 oe2); ]

      | E_py_tuple l1, E_py_tuple l2
      | E_py_list l1, E_py_list l2
      | E_py_set l1, E_py_set l2 ->
        Compare.list compare_expr l1 l2

      | E_py_index_subscript (o1, i1), E_py_index_subscript (o2, i2) ->
        Compare.compose
          [ (fun () -> compare_expr o1 o2);
            (fun () -> compare_expr i1 i2);
          ]
      | E_py_slice_subscript (o1, s1, e1, st1), E_py_slice_subscript (o2, s2, e2, st2) ->
        Compare.compose
          (List.fold_left2 (fun acc el1 el2 -> (fun () -> compare_expr el1 el2)::acc) [] [o1;s1;e1;st1] [o2;s2;e2;st2])

      | E_py_attribute (e1, s1), E_py_attribute (e2, s2) ->
        Compare.compose
          [ (fun () -> compare_expr e1 e2);
            (fun () -> Pervasives.compare s1 s2);
          ]

      | E_py_dict (k1, v1), E_py_dict (k2, v2) ->
        Compare.compose
          [ (fun () -> Compare.list compare_expr k1 k2);
            (fun () -> Compare.list compare_expr v1 v2);
          ]

      | E_py_generator_comprehension _, E_py_generator_comprehension _
      | E_py_list_comprehension _, E_py_list_comprehension _
      | E_py_set_comprehension _, E_py_set_comprehension _
      | E_py_dict_comprehension _, E_py_dict_comprehension _ -> Exceptions.panic "compare comprehensions"

      | E_py_call (f1, args1, kwargs1), E_py_call (f2, args2, kwargs2) ->
        Compare.compose
          [ (fun () -> compare_expr f1 f2);
            (fun () -> Compare.list compare_expr args1 args2);
            (fun () -> Compare.list (fun (so1, e1) (so2, e2) ->
                 Compare.compose [ (fun () -> Compare.option Pervasives.compare so1 so2);
                                   (fun () -> compare_expr e1 e2) ]) kwargs1 kwargs2);
          ]

      | E_py_yield e1, E_py_yield e2 ->
        compare_expr e1 e2

      | E_py_if (t1, b1, e1), E_py_if (t2, b2, e2) ->
        Compare.compose
          [ (fun () -> compare_expr t1 t2);
            (fun () -> compare_expr b1 b2);
            (fun () -> compare_expr e1 e2); ]


      | E_py_bytes b1, E_py_bytes b2 ->
        Pervasives.compare b1 b2

      | E_py_lambda _, E_py_lambda _
      | E_py_multi_compare _, E_py_multi_compare _ -> Exceptions.panic "compare py"

      | _ -> default e1 e2
    )
