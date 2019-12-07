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
open Format


let pp_except fmt e =
  fprintf fmt "except %a%a:@\n@[<h 2>  %a@]"
    (fun fmt e -> match e with
       | None -> ()
       | Some e -> pp_expr fmt e
    ) e.py_excpt_type
    (fun fmt v -> match v with
       | None -> ()
       | Some v -> fprintf fmt " as %a" pp_var v
    ) e.py_excpt_name
    pp_stmt e.py_excpt_body

let pp_py_object fmt (obj: py_object) =
  match obj with
  | (addr, None) -> fprintf fmt "⟪%a⟫" Universal.Ast.pp_addr addr
  | (addr, Some e) -> fprintf fmt "⟪%a :: %a⟫" Universal.Ast.pp_addr addr pp_expr e

let () =
  register_program_pp (fun default fmt prog ->
      match prog.prog_kind with
      | Py_program(globals, body) -> pp_stmt fmt body
      | _ -> default fmt prog
    );
  register_typ_pp (fun default fmt typ ->
    match typ with
    | T_py_not_implemented -> pp_print_string fmt "notimplemented"
    | T_py_none -> pp_print_string fmt "none"
    | T_py_complex -> pp_print_string fmt "complex"
    | T_py_bytes -> pp_print_string fmt "bytes"
    | _ -> default fmt typ
    );
  register_constant_pp (fun default fmt -> function
      | C_py_ellipsis -> pp_print_string fmt "C_py_ellipsis"
      | C_py_none -> pp_print_string fmt "C_py_None"
      | C_py_not_implemented -> pp_print_string fmt "NotImplemented"
      | C_py_imag j -> fprintf fmt "%aj" pp_print_float j
      | c -> default fmt c
    );
  register_operator_pp (fun default fmt -> function
      | O_py_and -> pp_print_string fmt "and"
      | O_py_or -> pp_print_string fmt "or"
      | O_py_floor_div -> pp_print_string fmt "//"
      | O_py_is -> pp_print_string fmt "is"
      | O_py_is_not -> pp_print_string fmt "is not"
      | O_py_in -> pp_print_string fmt "in"
      | O_py_not_in -> pp_print_string fmt "not in"
      | O_py_mat_mult -> pp_print_string fmt "@"
      | O_py_not -> pp_print_string fmt "py_not"
      | op -> default fmt op
    );
  register_expr_pp (fun default fmt exp ->
      match ekind exp with
      | E_py_ll_hasattr (e, attr) -> Format.fprintf fmt "E_py_ll_hasattr(%a, %a)" pp_expr e pp_expr attr
      | E_py_ll_getattr (e, attr) -> Format.fprintf fmt "E_py_ll_getattr(%a, %a)" pp_expr e pp_expr attr
      | E_py_ll_setattr (e, attr, ovalu) -> Format.fprintf fmt "E_py_ll_setattr(%a, %a, %a)" pp_expr e pp_expr attr (Option.print pp_expr) ovalu
      | E_py_annot e -> fprintf fmt "(annot) %a" pp_expr e
      | E_py_undefined true -> fprintf fmt "global undef"
      | E_py_undefined false -> fprintf fmt "local undef"
      | E_py_object obj -> pp_py_object fmt obj
      | E_py_attribute(obj, attr) ->
        fprintf fmt "%a.%s" pp_expr obj attr
      | E_py_list(elts) ->
        fprintf fmt "[%a]"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_expr) elts
      | E_py_tuple(elts) ->
        if List.length elts = 1 then
          fprintf fmt "(%a,)" pp_expr (List.hd elts)
        else
          fprintf fmt "(%a)"
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_expr) elts
      | E_py_set(elts) ->
        fprintf fmt "{%a}"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_expr) elts
      | E_py_dict(keys, values) ->
        fprintf fmt "{%a}"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")
             (fun fmt (k, v) -> fprintf fmt "%a: %a" pp_expr k pp_expr v)
          ) (List.combine keys values)

      | E_py_index_subscript(obj, index) ->
        fprintf fmt "%a[%a]" pp_expr obj pp_expr index
      | E_py_slice_subscript(obj, a, b, s) ->
        fprintf fmt "%a[%a : %a : %a]" pp_expr obj pp_expr a pp_expr b pp_expr s
      | E_py_call(f, args, keywords) ->
        fprintf fmt "%a(%a%a%a)"
          pp_expr f
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_expr) args
          (fun fmt -> function [] -> () | _ -> pp_print_string fmt ", ") keywords
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")
             (fun fmt -> function
                | (None, kwd) -> fprintf fmt "**%a" pp_expr kwd
                | (Some k, v) -> fprintf fmt "%s = %a" k pp_expr v
             )
          ) keywords
      | E_py_yield(e) ->
        fprintf fmt "yield %a" pp_expr e
      | E_py_if (test, body, orelse) ->
        fprintf fmt
          "%a if %a else %a"
          pp_expr body
          pp_expr test
          pp_expr orelse
      | E_py_list_comprehension(e, comprhs) ->
        fprintf fmt
          "[%a %a]"
          pp_expr e
          (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt " ") (fun fmt (target, iter, conds) ->
               fprintf fmt
                 "for %a in %a%a"
                 pp_expr target
                 pp_expr iter
                 (pp_print_list (fun fmt cond -> fprintf fmt " if %a" pp_expr cond)) conds
             )
          ) comprhs
      | E_py_set_comprehension(e, comprhs) ->
        fprintf fmt
          "{%a %a}"
          pp_expr e
          (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt " ") (fun fmt (target, iter, conds) ->
               fprintf fmt
                 "for %a in %a%a"
                 pp_expr target
                 pp_expr iter
                 (pp_print_list (fun fmt cond -> fprintf fmt " if %a" pp_expr cond)) conds
             )
          ) comprhs
      | E_py_generator_comprehension(e, comprhs) ->
        fprintf fmt
          "(%a %a)"
          pp_expr e
          (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt " ") (fun fmt (target, iter, conds) ->
               fprintf fmt
                 "for %a in %a%a"
                 pp_expr target
                 pp_expr iter
                 (pp_print_list (fun fmt cond -> fprintf fmt " if %a" pp_expr cond)) conds
             )
          ) comprhs
      | E_py_dict_comprehension(k, v, comprhs) ->
        fprintf fmt
          "{%a: %a %a}"
          pp_expr k
          pp_expr v
          (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt " ") (fun fmt (target, iter, conds) ->
               fprintf fmt
                 "for %a in %a%a"
                 pp_expr target
                 pp_expr iter
                 (pp_print_list (fun fmt cond -> fprintf fmt " if %a" pp_expr cond)) conds
             )
          ) comprhs

      | E_py_lambda l ->
        fprintf fmt "lambda %a: %a"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_var) l.py_lambda_parameters
          pp_expr l.py_lambda_body
      | E_py_multi_compare(left, ops, rights) ->
        let l = List.combine ops rights in
        fprintf fmt "%a %a"
          pp_expr left
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ")
             (fun fmt (op, right) -> fprintf fmt "%a %a" pp_operator op pp_expr right)
          ) l

      | E_py_bytes(s) ->
        fprintf fmt "b\"%s\"" s

      | E_py_check_annot (e1, e2) -> fprintf fmt "check_annot(%a, %a)" pp_expr e1 pp_expr e2

      | _ -> default fmt exp
    );

  register_stmt_pp (fun default fmt stmt ->
      match skind stmt with
      | S_py_class(cls) ->
        fprintf fmt "class %a(%a):@\n@[<h 2>  %a@]" pp_var cls.py_cls_var
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt ", ")
             pp_expr) cls.py_cls_bases
          pp_stmt cls.py_cls_body

      | S_py_function(func) ->
        fprintf fmt "%a@\ndef %a(%a)%a:@\n@[<h 2>  %a@]"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") (fun fmt d -> fprintf fmt "@@%a" pp_expr d)) func.py_func_decors
          pp_var func.py_func_var
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") (fun fmt (var, oty) ->
               match oty with
               | None -> pp_var fmt var
               | Some ty -> fprintf fmt "%a: %a" pp_var var pp_expr ty
             )) (List.map2 (fun  x y -> (x, y)) func.py_func_parameters func.py_func_types_in)
          (fun fmt oty -> match oty with
             | None -> ()
             | Some ty -> fprintf fmt " -> %a" pp_expr ty) func.py_func_type_out
          pp_stmt func.py_func_body

      | S_py_try(body, excepts, orelse, final) ->
        fprintf fmt "try:@\n@[<h 2>  %a@]@\n%a@\nelse:@[<h 2>  %a@]@\nfinally:@[<h 2>  %a@]"
          pp_stmt body
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") pp_except) excepts
          pp_stmt orelse
          pp_stmt final

      | S_py_raise e ->
        fprintf fmt "raise %a" (fun fmt -> function None -> () | Some e -> pp_expr fmt e) e

      | S_py_while(test, body, orelse) ->
        fprintf fmt "while %a:@\n@[<h 2>  %a@]@\nelse:@\n@[<h 2>  %a@]"
          pp_expr test
          pp_stmt body
          pp_stmt orelse

      | S_py_if(test, sthen, selse) ->
          fprintf fmt "@[<v 4>if (%a) {@,%a@]@,@[<v 4>} else {@,%a@]@,}" pp_expr test pp_stmt sthen pp_stmt selse

      | S_py_multi_assign(targets, e) ->
        fprintf fmt "%a %a"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") (fun fmt x -> fprintf fmt "%a =" pp_expr x)) targets
          pp_expr e

      | S_py_aug_assign(x, op, e) ->
        fprintf fmt "%a %a= %a"
          pp_expr x
          pp_operator op
          pp_expr e

      | S_py_annot(x, typ) ->
        fprintf fmt "%a: %a"
          pp_expr x
          pp_expr typ

      | S_py_for(target, iter, body, orelse) ->
        fprintf fmt "for %a in %a:@\n@[<h 2>  %a@]@\nelse:@\n@[<h 2>  %a@]"
          pp_expr target
          pp_expr iter
          pp_stmt body
          pp_stmt orelse

      | S_py_import(mdl, asname, vroot) ->
        fprintf fmt "import %s%a"
          mdl
          (fun fmt -> function None -> () | Some name -> fprintf fmt " as %a" pp_var name) asname


      | S_py_import_from(mdl, name, vroot, asname) ->
        fprintf fmt "from %s import %s as %a"
          mdl
          name
          pp_var asname

      | S_py_delete e -> fprintf fmt "del %a" pp_expr e

      | S_py_assert(e, None) -> fprintf fmt "assert %a" pp_expr e
      | S_py_assert(e, Some msg) -> fprintf fmt "assert %a, %a" pp_expr e pp_expr msg

      | S_py_with(ctx, asname, body) ->
        fprintf fmt "with %a%a:@\n@[<h 2>  %a@]"
          pp_expr ctx
          (fun fmt -> function None -> () | Some name -> fprintf fmt " as %a" pp_expr name) asname
          pp_stmt body

      | _ -> default fmt stmt
    );
  ()
