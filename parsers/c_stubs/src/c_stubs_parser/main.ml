(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Main entry point of the stub parser *)

open Location

(** Parse the stub specification from comments of a function *)
let parse_function_comment
    (func:C_AST.func)
    (prj:C_AST.project)
  : Ast.stub with_range option
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
      match cst with
      | None -> None
      | Some cst ->
        (* Resolve scoping of variables *)
        let cst1 = Passes.Scoping.doit cst in

        (* Remove predicates *)
        let cst2 = Passes.Predicate_expansion.doit cst1 in

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
