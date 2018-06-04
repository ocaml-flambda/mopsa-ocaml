(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Python frontend translates the parser's AST into Framework's AST.
*)
open Framework.Ast
open Lexing
open Py_CST
open Py_AST
open Ast

let debug fmt = Debug.debug ~channel:"python.frontend" fmt

let build_ast_from_file filename =
  let f = open_in filename in
  let buf = Lexing.from_channel f in
  buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = filename };

  let ast =
    try
      (* Parse the program source *)
      let cst = Py_parser.file_input Py_lexer.next_token buf in
      close_in f;

      (* Simplify the CST into an AST *)
      Py_CST_to_AST.translate_program cst |>

      (* Resolve scopes and generate unique IDs for variables *)
      Py_UID.translate_program

    with
    | Py_lexer.LexingError e ->
      let pos = Lexing.lexeme_start_p buf in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum-pos.pos_bol + 1 in
      Debug.fail "Lexer error \"%s\" in file %s, line %d, characters %d-%d:\n" e filename line (col-1) col
    | Py_parser.Error ->
      let pos = Lexing.lexeme_start_p buf in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum-pos.pos_bol + 1 in
      Debug.fail "Parser error in file %s, line %d, characters %d-%d:\n" filename line (col-1) col
  in
  debug "ast =@\n  @[%a@]" Py_pp.print_program ast;
  ast

(** Entry point of the frontend *)
let rec parse_program (files: string list) : Framework.Ast.program =
  match files with
  | [filename] ->
    let ast = build_ast_from_file filename in
    (* Start the translation of the AST *)
    from_program filename ast

  | _ -> assert false

and parse_file (filename: string) =
  let ast = build_ast_from_file filename in
  from_stmt ast.prog_body

(** Create a Universal.var variable from Py_AST.var *)
and from_var v =
  let open Framework.Ast in
  {
    vname = v.name;
    vuid = v.uid;
    vtyp = T_any;
    vkind = V_orig;
  }

(** Translate a Python program into a Framework.Ast.stmt *)
and from_program filename (p: Py_AST.program) : Framework.Ast.program =
  let body = from_stmt p.prog_body in
  let globals = List.map from_var p.prog_globals in
  {prog_kind = Ast.Py_program (globals, body); prog_file = filename}


(** Translation of a Python statement *)
and from_stmt (stmt: Py_AST.stmt) : Framework.Ast.stmt =
  let srange' = from_range stmt.srange in
  let skind' =
    match stmt.skind with
    | S_assign (x, e) ->
      Universal.Ast.S_assign (from_exp x, from_exp e, Universal.Ast.STRONG)

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
      Universal.Ast.S_if (
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
      }

    | S_class cls ->
      S_py_class {
        py_cls_var = from_var cls.cls_var;
        py_cls_body = from_stmt cls.cls_body;
        py_cls_bases = List.map from_exp cls.cls_bases;
        py_cls_static_attributes = List.map from_var cls.cls_static_attributes;
        py_cls_keywords = List.map (fun (k, v) -> (k, from_exp v)) cls.cls_keywords;
        py_cls_decors = List.map from_exp cls.cls_decors;
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
and from_stmt_option : Framework.Ast.range -> Py_AST.stmt option -> Framework.Ast.stmt
  = fun none_case_range -> function
    | None -> {skind = Universal.Ast.S_block []; srange = none_case_range}
    | Some s -> from_stmt s

and from_exp_option : Py_AST.expr option -> Framework.Ast.expr option
  = function
    | None -> None
    | Some e -> Some (from_exp e)


(** Translation of expressions *)
and from_exp exp =
  let ekind, etyp = match exp.ekind with
    | E_true ->
      E_constant (Universal.Ast.C_true),
      Universal.Ast.T_bool

    | E_false ->
      E_constant (Universal.Ast.C_false),
      Universal.Ast.T_bool

    | E_none ->
      E_constant (C_py_none),
      T_py_none

    | E_notimplemented ->
      E_constant (C_py_not_implemented),
      T_py_not_implemented

    | E_num (Py_CST.Int i) ->
      E_constant (Universal.Ast.C_int i),
      Universal.Ast.T_int

    | E_num (Py_CST.Float f) ->
      E_constant (Universal.Ast.C_float f),
      Universal.Ast.T_float

    | E_num (Py_CST.Imag j) ->
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
      E_var (from_var v),
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
  {ekind; etyp; erange = from_range exp.erange}


and from_location loc : Framework.Ast.loc =
  {
    loc_file = loc.file;
    loc_line = loc.line;
    loc_column = loc.column;
  }

and from_range range =
  Range_origin {
    range_begin = from_location range.rbegin;
    range_end = from_location range.rend;
  }

and from_binop : Py_AST.binop -> Framework.Ast.operator = function
  | O_arithmetic op -> from_arithmetic_op op
  | O_comparison op -> from_comparison_op op
  | O_bool op -> from_bool_op op

and from_arithmetic_op = function
  | Add -> Universal.Ast.O_plus T_any
  | Sub -> Universal.Ast.O_minus T_any
  | Mult -> Universal.Ast.O_mult T_any
  | Div -> Universal.Ast.O_div T_any
  | FloorDiv -> O_py_floor_div
  | Mod -> Universal.Ast.O_mod T_any
  | Pow -> Universal.Ast.O_pow
  | BitOr -> Universal.Ast.O_bit_or
  | BitXor -> Universal.Ast.O_bit_xor
  | BitAnd -> Universal.Ast.O_bit_and
  | MatMult -> O_py_mat_mult
  | RShift -> Universal.Ast.O_bit_rshift
  | LShift -> Universal.Ast.O_bit_lshift

and from_bool_op = function
  | And -> Universal.Ast.O_log_and
  | Or -> Universal.Ast.O_log_or

and from_comparison_op : Py_CST.cmpop -> Framework.Ast.operator = function
  | Eq -> Universal.Ast.O_eq
  | NotEq -> Universal.Ast.O_ne
  | Lt -> Universal.Ast.O_lt
  | LtE -> Universal.Ast.O_le
  | Gt -> Universal.Ast.O_gt
  | GtE -> Universal.Ast.O_ge
  | Is -> O_py_is
  | IsNot -> O_py_is_not
  | In -> O_py_in
  | NotIn -> O_py_not_in

and from_unop = function
  | Not -> Ast.O_py_not
  | USub -> Universal.Ast.O_minus T_any
  | UAdd -> Universal.Ast.O_plus T_any
  | Invert -> Universal.Ast.O_bit_invert


(* (\** Add a sequence of initializations before a body to assign undefined values to a list of variables *\)
 * and add_undefined_init_values vars body =
 *   let stmtl =  List.fold_right (fun v acc ->
 *         let stmt = Universal.Ast.S_assign (
 *             {ekind = Universal.Ast.E_var (from_var v); etyp = T_any; erange = unknown_range},
 *             {ekind = Universal.Ast.E_constant C_py_undefined; etyp = T_py_undefined; erange = unknown_range}
 *           )
 *         in
 *         {Framework.Ast.skind = stmt; srange = unknown_range} :: acc
 *     ) vars []
 *   in
 *   match body.skind with
 *   | Universal.Ast.S_block sl ->
 *     {body with skind = Universal.Ast.S_block (stmtl @ sl)}
 *   | _ ->
 *     {body with skind = Universal.Ast.S_block (stmtl @ [body])}
 *
 * and initialize_special_vars filepath body =
 *   let stmt1 = {
 *     Framework.Ast.skind = Universal.Ast.S_assign(
 *         {ekind = Universal.Ast.E_var (from_var {name = "__name__"; uid = -1}); etyp = T_any; erange = unknown_range},
 *         {ekind = Universal.Ast.E_constant (Universal.Ast.C_string "__main__"); etyp = Universal.Ast.T_string; erange = unknown_range}
 *       );
 *     srange = unknown_range
 *   }
 *   in
 *   let stmt2 = {
 *     Framework.Ast.skind = Universal.Ast.S_assign(
 *         {ekind = Universal.Ast.E_var (from_var {name = "__file__"; uid = -1}); etyp = T_any; erange = unknown_range},
 *         {ekind = Universal.Ast.E_constant (Universal.Ast.C_string filepath); etyp = Universal.Ast.T_string; erange = unknown_range}
 *       );
 *     srange = unknown_range
 *   }
 *   in
 *   match body.skind with
 *   | Universal.Ast.S_block sl ->
 *     {body with skind = Universal.Ast.S_block (stmt1 :: stmt2 :: sl)}
 *   | _ ->
 *     {body with skind = Universal.Ast.S_block ([stmt1; stmt2; body])}
 *
 * (\** Initialize __name__ and __file__ variables *\) *)
