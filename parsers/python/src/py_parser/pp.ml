(**

  Copyright (c) 2017-2019 Aymeric Fromherz and The MOPSA Project

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 *)

(**
   Pp - Pretty printer of the AST.
*)

open Cst
open Ast
open Format

let str = pp_print_string

let rec print_var fmt v =
  fprintf fmt "%s@@%d" v.name v.uid

and print_program fmt p = print_stmt fmt p.prog_body

and print_stmt fmt (stmt: Ast.stmt) =
  match stmt.skind with
  | S_assign (v, e) -> fprintf fmt "%a = %a" print_exp v print_exp e
  | S_type_annot (v, e) -> fprintf fmt "%a: %a" print_exp v print_exp e
  | S_expression e -> print_exp fmt e
  | S_while (test, body, orelse) ->
    fprintf fmt
      "while (%a):@\n@[<h 2>  %a@]%a"
      print_exp test
      print_stmt body
      print_orelse orelse
  | S_break -> str fmt "break"
  | S_continue -> str fmt "continue"
  | S_block sl ->
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") print_stmt fmt sl
  | S_aug_assign (v, op, e) -> fprintf fmt "%a %a= %a" print_exp v print_binop op print_exp e
  | S_if (test, body, orelse) ->
    fprintf fmt
      "if %a:@\n@[<h 2>  %a@]%a"
      print_exp test
      print_stmt body
      print_orelse orelse

  | S_function(func) ->
    fprintf fmt "def %a(%a):@\n@[<h 2>  %a@]"
      print_var func.func_var
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") print_var) func.func_parameters
      print_stmt func.func_body
  | S_class(cls) ->
    fprintf fmt "class %a(%a):@\n@[<h 2>  %a@]"
      print_var cls.cls_var
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") print_exp) cls.cls_bases
      print_stmt cls.cls_body
  | S_for (target, iter, body, orelse) ->
    fprintf fmt
      "for %a in %a:@\n@[<h 2>  %a@]%a"
      print_exp target
      print_exp iter
      print_stmt body
      print_orelse orelse
  | S_return e -> fprintf fmt "return %a" print_exp e
  | S_raise(e, c) ->
    fprintf fmt "raise %a%a"
      (fun fmt e -> match e with None -> () | Some e -> print_exp fmt e) e
      (fun fmt c -> match e with None -> () | Some c -> fprintf fmt " from %a" print_exp c) c
  | S_try (body, excepts, orelse, finally) ->
    fprintf fmt "try:@\n@[<h 2>  %a@]@\n%a%a%a"
      print_stmt body
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") print_except) excepts
      print_orelse orelse
      print_finally finally

  | S_import(mdl, asname, vroot) ->
    fprintf fmt "import %s<%a>%a"
      mdl
      print_var vroot
      (fun fmt -> function None -> () | Some name -> fprintf fmt " as %a" print_var name) asname

  | S_import_from(mdl, name, _, asname) ->
    fprintf fmt "from %s import %s as %a"
      mdl
      name
      print_var asname

  | S_delete e -> fprintf fmt "del %a" print_exp e

  | S_assert(e, None) -> fprintf fmt "assert %a" print_exp e
  | S_assert(e, Some msg) -> fprintf fmt "assert %a, %a" print_exp e print_exp msg

  | S_with(ctx, asname, body) ->
    fprintf fmt "with %a%a:@\n@[<h 2>  %a@]"
      print_exp ctx
      (fun fmt -> function None -> () | Some name -> fprintf fmt " as %a" print_exp name) asname
      print_stmt body

  | S_pass -> str fmt "pass"

and print_except fmt (exc: except) =
  let e, v, body = exc in
  fprintf fmt "except %a%a:@\n@[<h 2>  %a@]"
    (fun fmt e -> match e with
       | None -> ()
       | Some e -> print_exp fmt e
    ) e
    (fun fmt v -> match v with
       | None -> ()
       | Some v -> fprintf fmt " as %a" print_var v
    ) v
    print_stmt body

and print_number fmt (n: Cst.number) =
  match n with
  | Cst.Int i -> Z.pp_print fmt i
  | Cst.Float f -> pp_print_float fmt f
  | Cst.Imag s -> pp_print_string fmt s

and print_exp fmt exp =
  match exp.ekind with
  | E_ellipsis -> str fmt "..."
  | E_true -> str fmt "True"
  | E_false -> str fmt "False"
  | E_none -> str fmt "Py_None"
  | E_notimplemented -> str fmt "NotImplemented"
  | E_num(n) -> print_number fmt n
  | E_str(s)-> fprintf fmt "\"%s\"" s
  | E_bytes(s)-> fprintf fmt "b\"%s\"" s
  | E_attr(obj, attr) -> fprintf fmt "%a.%s" print_exp obj attr
  | E_id(v) -> print_var fmt v
  | E_call(f, args, keywords) ->
    fprintf fmt "%a(%a%a%a)"
      print_exp f
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") print_exp) args
      (fun fmt -> function [] -> () | _ -> pp_print_string fmt ", ") keywords
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")
         (fun fmt -> function
            | (None, kwd) -> fprintf fmt "**%a" print_exp kwd
            | (Some k, v) -> fprintf fmt "%s = %a" k print_exp v
         )
      ) keywords
  | E_generator_comp(e, comps) ->
    fprintf fmt "(%a %a)"
      print_exp e
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ ") print_comp) comps
  | E_list_comp(e, comps) ->
    fprintf fmt "[%a %a]"
      print_exp e
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ ") print_comp) comps
  | E_set_comp(e, comps) ->
    fprintf fmt "{%a %a}"
      print_exp e
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ ") print_comp) comps
  | E_dict_comp(k, v, comps) ->
    fprintf fmt "{%a: %a %a}"
      print_exp k
      print_exp v
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ ") print_comp) comps
  | E_list(el) ->
    fprintf fmt "[%a]"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") print_exp) el
  | E_index_subscript(obj, index) ->
    fprintf fmt "%a[%a]"
      print_exp obj
      print_exp index
  | E_slice_subscript(obj, lower, upper, step) ->
    fprintf fmt "%a[%a:%a:%a]"
      print_exp obj
      print_exp lower
      print_exp upper
      print_exp step
  | E_tuple(el) ->
    fprintf fmt "(%a)"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") print_exp) el
  | E_set(el) ->
    fprintf fmt "set(%a)"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") print_exp) el
  | E_dict(keys, values) ->
    fprintf fmt "{%a}"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
         (fun fmt (k, v) -> fprintf fmt "%a: %a" print_exp k print_exp v)
      ) (List.combine keys values)
  | E_if (test, body, orelse) ->
    fprintf fmt
      "%a if %a else %a"
      print_exp body
      print_exp test
      print_exp orelse
  | E_yield(e) ->
    fprintf fmt "yield %a" print_exp e
  | E_binop(e1, op, e2) ->
    fprintf fmt "( %a ) %a ( %a )"
      print_exp e1
      print_binop op
      print_exp e2
  | E_unop(op, e) ->
    fprintf fmt "%a ( %a )"
      print_unop op
      print_exp e
  | E_lambda l ->
    fprintf fmt "lambda %a: %a"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") print_var) l.lambda_parameters
      print_exp l.lambda_body
  | E_multi_compare(left, ops, rights) ->
    let l = List.combine ops rights in
    fprintf fmt "%a %a"
      print_exp left
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ")
         (fun fmt (op, right) -> fprintf fmt "%a %a" print_binop op print_exp right)
      ) l

and print_comp fmt comp =
  let (target, iter, conds) = comp in
  fprintf fmt "for %a in %a %a"
    print_exp target
    print_exp iter
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ ") (fun fmt e -> fprintf fmt "if %a" print_exp e)) conds

and print_binop fmt = function
  | O_arithmetic op -> print_arithop fmt op
  | O_comparison op -> print_cmpop fmt op
  | O_bool op -> print_boolop fmt op

and print_arithop fmt = function
  | Add -> str fmt "+"
  | Sub -> str fmt "-"
  | Mult -> str fmt "*"
  | MatMult -> str fmt "@"
  | Div -> str fmt "/"
  | Mod -> str fmt "%"
  | Pow -> str fmt "**"
  | LShift -> str fmt "<<"
  | RShift -> str fmt ">>"
  | BitOr -> str fmt "|"
  | BitXor -> str fmt "^"
  | BitAnd -> str fmt "&"
  | FloorDiv -> str fmt "//"

and print_boolop fmt = function
  | And -> str fmt "and"
  | Or -> str fmt "or"

and print_unop fmt = function
   | Invert -> str fmt "~"
   | Not -> str fmt "not"
   | UAdd -> str fmt "+"
   | USub -> str fmt "-"

and print_cmpop fmt = function
  | Eq -> str fmt "=="
  | NotEq -> str fmt "!="
  | Lt -> str fmt "<"
  | LtE -> str fmt "<="
  | Gt -> str fmt ">"
  | GtE -> str fmt ">="
  | Is -> str fmt "is"
  | IsNot -> str fmt "is not"
  | In -> str fmt "in"
  | NotIn -> str fmt "not in"

and print_orelse fmt = function
    | None -> ()
    | Some s ->
      fprintf fmt "@\nelse:@\n@[<h 2>  %a@]"
        print_stmt s

and print_finally fmt = function
    | None -> ()
    | Some s ->
      fprintf fmt "@\nfinally:@\n@[<h 2>  %a@]"
        print_stmt s
