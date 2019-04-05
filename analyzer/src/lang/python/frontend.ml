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

(**
   Python frontend translates the parser's AST into Framework's AST.
*)
open Mopsa
open Lexing
open Ast
open Utils

let debug fmt = Debug.debug ~channel:"python.frontend" fmt

(** Entry point of the frontend *)
let rec parse_program (files: string list) : program =
  match files with
  | [filename] ->
    let ast, counter = Py_parser.Main.parse_file ~counter:(Framework.Ast.Var.get_vcounter_val ()) filename in
    Framework.Ast.Var.start_vcounter_at counter;
    {
      prog_kind = from_program filename ast;
      prog_range = mk_program_range [filename];
    }

  | _ -> assert false

and parse_file (filename: string) =
  let ast, counter = Py_parser.Main.parse_file ~counter:(Framework.Ast.Var.get_vcounter_val ()) filename in
  Framework.Ast.Var.start_vcounter_at counter;
  from_stmt ast.prog_body

(** Create a Universal.var variable from Py_parser.Ast.var *)
and from_var (v:Py_parser.Ast.var) =
  let open Framework.Ast in
  (* let () = if Hashtbl.mem tmp v.uid && Hashtbl.find tmp v.uid <> v.name then
   *     Exceptions.panic "%d is already %s, conflict with current %s@\n" v.uid (Hashtbl.find tmp v.uid) v.name
   *   else
   *     Hashtbl.add tmp v.uid v.name in *)
  mkv v.name (v.name ^ ":" ^ (string_of_int v.uid)) v.uid T_any

(** Translate a Python program into a stmt *)
and from_program filename (p: Py_parser.Ast.program) : prog_kind =
  let body = from_stmt p.prog_body in
  let globals = List.map from_var p.prog_globals in
  Ast.Py_program (globals, body)


(** Translation of a Python statement *)
and from_stmt (stmt: Py_parser.Ast.stmt) : stmt =
  let srange' = stmt.srange in
  let skind' =
    match stmt.skind with
    | S_assign (x, e) ->
      S_assign (from_exp x, from_exp e)

    | S_expression e ->
      Universal.Ast.S_expression (from_exp e)

    | S_while (test, body, None) ->
      Universal.Ast.S_while (
        from_exp test,
        from_stmt body
      )

    | S_while (test, body, Some orelse) ->
      S_py_while(
        from_exp test,
        from_stmt body,
        from_stmt orelse
      )

    | S_break ->
      Universal.Ast.S_break

    | S_continue ->
      Universal.Ast.S_continue

    | S_block sl ->
      Universal.Ast.S_block (List.map from_stmt sl)

    | S_aug_assign (x, op, e) ->
      S_py_aug_assign(from_exp x, from_binop op, from_exp e)

    | S_if (test, body, orelse) ->
      S_py_if (
        from_exp test,
        from_stmt body,
        from_stmt_option (tag_range srange' "empty if else") orelse
      )

    | S_function f ->
      Ast.S_py_function {
        py_func_var = from_var f.func_var;
        py_func_parameters = List.map from_var f.func_parameters;
        py_func_defaults = List.map from_exp_option f.func_defaults;
        py_func_locals = List.map from_var f.func_locals;
        py_func_body = from_stmt f.func_body;
        py_func_is_generator = f.func_is_generator;
        py_func_decors = List.map from_exp f.func_decors;
        py_func_range = f.func_range;
      }

    | S_class cls ->
      S_py_class {
        py_cls_var = from_var cls.cls_var;
        py_cls_body = from_stmt cls.cls_body;
        py_cls_bases = List.map from_exp cls.cls_bases;
        py_cls_static_attributes = List.map from_var cls.cls_static_attributes;
        py_cls_keywords = List.map (fun (k, v) -> (k, from_exp v)) cls.cls_keywords;
        py_cls_decors = List.map from_exp cls.cls_decors;
        py_cls_range = cls.cls_range;
      }

    | S_for (target,iter,body,orelse) ->
      S_py_for(
        from_exp target,
        from_exp iter,
        from_stmt body,
        from_stmt_option (tag_range srange' "empty for else") orelse
      )

    | S_return e ->
      Universal.Ast.S_return (Some (from_exp e))

    | S_raise(e, c)->
      S_py_raise (match e with None -> None | Some e -> Some (from_exp e))

    | S_try (body, excepts, orelse, finally) ->
      S_py_try (
        from_stmt body,
        excepts |> List.map (fun (typ, name, body) ->
            {
              py_excpt_type = (match typ with None -> None | Some e -> Some (from_exp e));
              py_excpt_name = (match name with None -> None | Some v -> Some (from_var v));
              py_excpt_body = from_stmt body;
            }),
        from_stmt_option (tag_range srange' "empty try else") orelse,
        from_stmt_option (tag_range srange' "empty try finally")finally
      )

    | S_import(modul, None, vroot) -> S_py_import(modul, None, from_var vroot)
    | S_import(modul, Some vasname, vroot) -> S_py_import(modul, Some (from_var vasname), from_var vroot)

    | S_import_from(modul, name, vroot, vname) -> S_py_import_from(modul, name, from_var vroot, from_var vname)

    | S_with(ctx, target, body) ->
      S_py_with(
        from_exp ctx,
        from_exp_option target,
        from_stmt body
      )

    | S_pass -> Universal.Ast.S_block []

    | S_delete e -> S_py_delete (from_exp e)

    | S_assert(e, msg) -> S_py_assert(from_exp e, from_exp_option msg)


  in
  {skind = skind'; srange = srange'}

(** Translate an optional statement into en eventual empty one *)
and from_stmt_option : Location.range -> Py_parser.Ast.stmt option -> stmt
  = fun none_case_range -> function
    | None -> {skind = Universal.Ast.S_block []; srange = none_case_range}
    | Some s -> from_stmt s

and from_exp_option : Py_parser.Ast.expr option -> expr option
  = function
    | None -> None
    | Some e -> Some (from_exp e)


(** Translation of expressions *)
and from_exp exp =
  let ekind, etyp = match exp.ekind with
    | E_true ->
      E_constant (Universal.Ast.C_bool true),
      Universal.Ast.T_bool

    | E_false ->
      E_constant (Universal.Ast.C_bool false),
      Universal.Ast.T_bool

    | E_none ->
      E_constant (C_py_none),
      T_py_none

    | E_notimplemented ->
      E_constant (C_py_not_implemented),
      T_py_not_implemented

    | E_num (Py_parser.Cst.Int i) ->
      E_constant (Universal.Ast.C_int i),
      Universal.Ast.T_int

    | E_num (Py_parser.Cst.Float f) ->
      E_constant (Universal.Ast.C_float f),
      Universal.Ast.T_float Universal.Ast.F_DOUBLE

    | E_num (Py_parser.Cst.Imag j) ->
      ignore (Str.string_match (Str.regexp "\\(.*\\)j") j 0);
      let j = Str.matched_group 1 j in
      let j = float_of_string j in
      E_constant (Ast.C_py_imag j),
      T_py_complex

    | E_str s ->
      E_constant (Universal.Ast.C_string s),
      Universal.Ast.T_string

    | E_attr (obj, attr) ->
      E_py_attribute (from_exp obj, attr),
      T_any

    | E_id v ->
      E_var (from_var v, STRONG),
      T_any

    | E_binop (left, op, right) ->
      E_binop (
        from_binop op,
        from_exp left,
        from_exp right
      ),
      T_any

    | E_unop (op, operand) ->
      E_unop (
        from_unop op,
        from_exp operand
      ),
      T_any

    | E_call (f, args, keywords) ->
      E_py_call (
        from_exp f,
        List.map from_exp args,
        List.map (fun (k, v) -> (k, from_exp v)) keywords
      ),
      T_any

    | E_list elts ->
      E_py_list (
        List.map from_exp elts
      ),
      T_any

    | E_index_subscript (obj, index) ->
      E_py_index_subscript (from_exp obj, from_exp index),
      T_any

    | E_slice_subscript (obj,a,b,s) ->
      E_py_slice_subscript(from_exp obj, from_exp a, from_exp b, from_exp s),
      T_any

    | E_yield e ->
      E_py_yield(from_exp e),
      T_any

    | E_if(test, body, orelse) ->
      E_py_if(
        from_exp test,
        from_exp body,
        from_exp orelse
      ),
      T_any

    | E_tuple el ->
      E_py_tuple(List.map from_exp el),
      T_any

    | E_list_comp (e, comprhs) ->
      Ast.E_py_list_comprehension (
        from_exp e,
        comprhs |> List.map (fun (target, iter, conds) ->
            (from_exp target, from_exp iter, List.map from_exp conds)
          )
      ),
      T_any

    | E_dict (keys, values) ->
      Ast.E_py_dict(
        List.map from_exp keys,
        List.map from_exp values
      ),
      T_any

    | E_lambda l ->
      E_py_lambda {
        py_lambda_body = from_exp l.lambda_body;
        py_lambda_parameters = List.map from_var l.lambda_parameters;
        py_lambda_defaults = List.map from_exp_option l.lambda_defaults;
      },
      T_any

    | E_bytes s ->
      E_py_bytes s, Universal.Ast.T_string

    | E_set el ->
      E_py_set(List.map from_exp el), T_any

    | E_generator_comp (e,comprhs) ->
      E_py_generator_comprehension (
        from_exp e,
        comprhs |> List.map (fun (target, iter, conds) ->
            (from_exp target, from_exp iter, List.map from_exp conds)
          )
      ),
      T_any

    | E_set_comp (e,comprhs) ->
      E_py_set_comprehension (
        from_exp e,
        comprhs |> List.map (fun (target, iter, conds) ->
            (from_exp target, from_exp iter, List.map from_exp conds)
          )
      ),
      T_any

    | E_dict_comp (k,v,comprhs) ->
      E_py_dict_comprehension (
        from_exp k,
        from_exp v,
        comprhs |> List.map (fun (target, iter, conds) ->
            (from_exp target, from_exp iter, List.map from_exp conds)
          )
      ),
      T_any

    | E_multi_compare(left, ops, rights) ->
      E_py_multi_compare (
        from_exp left,
        List.map from_binop ops,
        List.map from_exp rights
      ),
      T_any


  in
  {ekind; etyp; erange = exp.erange}


and from_binop : Py_parser.Ast.binop -> operator = function
  | O_arithmetic op -> from_arithmetic_op op
  | O_comparison op -> from_comparison_op op
  | O_bool op -> from_bool_op op

and from_arithmetic_op = function
  | Add -> Universal.Ast.O_plus
  | Sub -> Universal.Ast.O_minus
  | Mult -> Universal.Ast.O_mult
  | Div -> Universal.Ast.O_div
  | FloorDiv -> O_py_floor_div
  | Mod -> Universal.Ast.O_mod
  | Pow -> Universal.Ast.O_pow
  | BitOr -> Universal.Ast.O_bit_or
  | BitXor -> Universal.Ast.O_bit_xor
  | BitAnd -> Universal.Ast.O_bit_and
  | MatMult -> O_py_mat_mult
  | RShift -> Universal.Ast.O_bit_rshift
  | LShift -> Universal.Ast.O_bit_lshift

and from_bool_op = function
  | And -> O_py_and
  | Or -> O_py_or

and from_comparison_op : Py_parser.Cst.cmpop -> operator = function
  | Eq -> O_eq
  | NotEq -> O_ne
  | Lt -> O_lt
  | LtE -> O_le
  | Gt -> O_gt
  | GtE -> O_ge
  | Is -> O_py_is
  | IsNot -> O_py_is_not
  | In -> O_py_in
  | NotIn -> O_py_not_in

and from_unop = function
  | Not -> O_log_not
  | USub -> Universal.Ast.O_minus
  | UAdd -> Universal.Ast.O_plus
  | Invert -> Universal.Ast.O_bit_invert
