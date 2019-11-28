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
  register_constant_compare (fun default c1 c2 ->
      match c1, c2 with
      | C_py_imag f1, C_py_imag f2 -> Pervasives.compare f1 f2
      | C_py_none, C_py_none
      | C_py_ellipsis, C_py_ellipsis
      | C_py_not_implemented, C_py_not_implemented
      | C_py_empty, C_py_empty -> 0
      | _ -> default c1 c2);
  register_expr_compare (fun default e1 e2 ->
      match ekind e1, ekind e2 with
      | E_py_ll_getattr (e11, e12), E_py_ll_getattr (e21, e22)
      | E_py_ll_hasattr (e11, e12), E_py_ll_hasattr (e21, e22) ->
        Compare.compose
          [ (fun () -> compare_expr e11 e21);
            (fun () -> compare_expr e21 e22); ]
      | E_py_ll_setattr (e11, e12, oe13), E_py_ll_setattr (e21, e22, oe23) ->
        Compare.compose
          [ (fun () -> compare_expr e11 e21);
            (fun () -> compare_expr e21 e22);
            (fun () -> Option.compare compare_expr oe13 oe23); ]
      | E_py_annot e1, E_py_annot e2 -> compare_expr e1 e2
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

      | E_py_check_annot (e11, e12), E_py_check_annot (e21, e22) ->
        Compare.compose
          [ (fun () -> compare_expr e11 e12);
            (fun () -> compare_expr e12 e22);
          ]


      | _ -> default e1 e2
    );
  register_stmt_compare (fun default s1 s2 ->
      match skind s1, skind s2 with
      | S_py_class c1, S_py_class c2 ->
        Compare.compose
          [ (fun () -> compare_var c1.py_cls_var c2.py_cls_var);
            (fun () -> compare_stmt c1.py_cls_body c2.py_cls_body);
            (fun () -> Compare.list compare_var c1.py_cls_static_attributes c2.py_cls_static_attributes);
            (fun () -> Compare.list compare_expr c1.py_cls_bases c2.py_cls_bases);
            (fun () -> Compare.list compare_expr c1.py_cls_decors c2.py_cls_decors);
            (fun () -> Compare.list (Compare.pair (Compare.option Pervasives.compare) compare_expr) c1.py_cls_keywords c2.py_cls_keywords);
            (fun () -> compare_range c1.py_cls_range c2.py_cls_range);
          ]

      | S_py_function f1, S_py_function f2 ->
        Compare.compose
          [
            (fun () -> compare_var f1.py_func_var f2.py_func_var);
            (fun () -> Compare.list compare_var f1.py_func_parameters f2.py_func_parameters);
            (fun () -> Compare.list (Compare.option compare_expr) f1.py_func_defaults f2.py_func_defaults);
            (fun () -> compare_stmt f1.py_func_body f2.py_func_body);
            (fun () -> Pervasives.compare f1.py_func_is_generator f2.py_func_is_generator);
            (fun () -> Compare.list compare_expr f1.py_func_decors f2.py_func_decors);
            (fun () -> compare_range f1.py_func_range f2.py_func_range);
          ]

      | S_py_try (b1, e1, l1, f1), S_py_try (b2, e2, l2, f2) ->
        Compare.compose
          [
            (fun () -> compare_stmt b1 b2);
            (fun () ->
               Compare.list (fun e1 e2 ->
                   Compare.compose
                     [
                       (fun () -> Compare.option compare_expr e1.py_excpt_type e2.py_excpt_type);
                       (fun () -> Compare.option compare_var e1.py_excpt_name e2.py_excpt_name);
                       (fun () -> compare_stmt e1.py_excpt_body e2.py_excpt_body);
                     ]) e1 e2);
            (fun () -> compare_stmt l1 l2);
            (fun () -> compare_stmt f1 f2);
          ]

      | S_py_raise o1, S_py_raise o2 ->
        Compare.option compare_expr o1 o2

      | S_py_if (c1, t1, e1), S_py_if (c2, t2, e2)
      | S_py_while (c1, t1, e1), S_py_while (c2, t2, e2) ->
        Compare.compose
          [
            (fun () -> compare_expr c1 c2);
            (fun () -> compare_stmt t1 t2);
            (fun () -> compare_stmt e1 e2);
          ]

      | S_py_multi_assign (ls1, r1), S_py_multi_assign (ls2, r2) ->
        Compare.compose
          [
            (fun () -> Compare.list compare_expr ls1 ls2);
            (fun () -> compare_expr r1 r2);
          ]

      | S_py_aug_assign (l1, o1, r1), S_py_aug_assign (l2, o2, r2) ->
        Compare.compose
          [ (fun () -> compare_expr l1 l2);
            (fun () -> compare_operator o1 o2);
            (fun () -> compare_expr r1 r2); ]

      | S_py_annot (v1, ty1), S_py_annot (v2, ty2) ->
        Compare.compose
          [ (fun () -> compare_expr v1 v2);
            (fun () -> compare_expr ty1 ty2);
          ]

      | S_py_for (t1, i1, b1, e1), S_py_for (t2, i2, b2, e2) ->
        Compare.compose
          [ (fun () -> compare_expr t1 t2);
            (fun () -> compare_expr i1 i2);
            (fun () -> compare_stmt b1 b2);
            (fun () -> compare_stmt e1 e2); ]

      | S_py_import (m1, a1, r1), S_py_import (m2, a2, r2) ->
        Compare.compose
          [ (fun () -> Pervasives.compare m1 m2);
            (fun () -> Compare.option compare_var a1 a2);
            (fun () -> compare_var r1 r2); ]

      | S_py_import_from (m1, n1, r1, v1), S_py_import_from (m2, n2, r2, v2) ->
        Compare.compose
          [ (fun () -> Pervasives.compare m1 m2);
            (fun () -> Pervasives.compare n1 n2);
            (fun () -> compare_var r1 r2);
            (fun () -> compare_var v1 v2); ]

      | S_py_delete e1, S_py_delete e2 ->
        compare_expr e1 e2

      | S_py_assert (e1, o1), S_py_assert (e2, o2) ->
        Compare.compose
          [ (fun () -> compare_expr e1 e2);
            (fun () -> Compare.option compare_expr o1 o2); ]

      | S_py_with (c1, a1, b1), S_py_with (c2, a2, b2) ->
        Compare.compose
          [ (fun () -> compare_expr c1 c2);
            (fun () -> Compare.option compare_expr a1 a2);
            (fun () -> compare_stmt b1 b2);]

      | _ -> default s1 s2
    )
