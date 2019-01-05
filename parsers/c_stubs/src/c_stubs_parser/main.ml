(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Main entry point of the stub parser *)

open Location

let debug fmt = Debug.debug ~channel:"c_stubs_parser.main" fmt

(** Parse the stub specification from comments of a function *)
let parse_function_comment
    (func:C_AST.func)
    (prj:C_AST.project)
    (preds: Cst.predicate with_range list)
  : Ast.stub option
  =
  match func.func_com with
  | [] -> None

  | _ :: _ :: _ ->
    Exceptions.warn "c_stubs.main: %s: functions with several comments not supported" func.func_org_name;
    None

  | [com] ->
    let comment = com.com_text in
    let file = com.com_range.range_begin.loc_file in
    let line = com.com_range.range_begin.loc_line in
    let col = com.com_range.range_begin.loc_column in

    let comment' = Passes.Macro_expansion.doit comment prj in

    (* Create the lexing buffer *)
    let buf = Lexing.from_string comment' in
    buf.lex_curr_p <- {
      pos_fname = file;
      pos_lnum = line;
      pos_bol = 0;
      pos_cnum = col;
    };

    (* Parse the comment *)
    try
      let cst = Parser.stub Lexer.read buf in
      match cst with
      | None -> None
      | Some cst ->
        (* Remove predicates *)
        let cst1 = Passes.Predicate_expansion.doit cst preds in

        (* Resolve scoping of variables *)
        let cst2 = Passes.Scoping.doit cst1 in

        (* Translate CST into AST *)
        let ast = Passes.Cst_to_ast.doit prj func cst2 in
        Some ast
    with
    | Lexer.SyntaxError s ->
      let range = Location.from_lexing_range (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf) in
      Exceptions.syntax_error range "%s" s

    | Parser.Error ->
      let range = Location.from_lexing_range (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf) in
      Exceptions.unnamed_syntax_error range


(** Parse the stub specification from comments of a variable *)
let parse_var_comment
  (var:C_AST.variable)
  (prj:C_AST.project)
  (preds:Cst.predicate with_range list)
  : Ast.stub option
  =
  (* Create a dummy init function *)
  let func = C_AST.{
    func_uid = 0;
    func_org_name = "$init";
    func_unique_name = "$init";
    func_is_static = false;
    func_return = var.var_type;
    func_parameters = [||];
    func_body = None;
    func_static_vars = [];
    func_local_vars = [];
    func_variadic = false;
    func_range = var.var_range;
    func_com = var.var_com;
  }
  in
  parse_function_comment func prj preds


(** Check whether a comment is a global predicate *)
let is_global_predicate com =
  match com with
  | [com] ->    
    let comment = com.Clang_AST.com_text |>
                  String.trim
    in
    let lexeme = "/*$$" in
    let start = String.sub comment 0 (String.length lexeme) in
    start = lexeme

  | _ -> false

(** Parse comment specifying a global predicate *)
let parse_global_predicate_comment com =
  match com with
  | [] -> []
  | _ :: _ :: _ -> []
  | [com] ->
    let comment = com.Clang_AST.com_text in
    let file = com.com_range.range_begin.loc_file in
    let line = com.com_range.range_begin.loc_line in
    let col = com.com_range.range_begin.loc_column in

    (* Create the lexing buffer *)
    let buf = Lexing.from_string comment in
    buf.lex_curr_p <- {
      pos_fname = file;
      pos_lnum = line;
      pos_bol = 0;
      pos_cnum = col;
    };

    (* Parse the comment *)
    try
      let cst = Parser.stub Lexer.read buf in
      OptionExt.option_dfl1 [] (fun cst ->
          List.fold_left (fun acc section ->
              match section with
              | Cst.S_predicate pred -> pred :: acc
              | _ -> acc
            ) [] cst.content
        ) cst
    with
    | Lexer.SyntaxError s ->
      let range = Location.from_lexing_range (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf) in
      Exceptions.syntax_error range "%s" s

    | Parser.Error ->
      let range = Location.from_lexing_range (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf) in
      Exceptions.unnamed_syntax_error range
