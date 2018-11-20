(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Main entry point of the stub parser *)

open Location

let parse
    (comment:string)
    (file:string)
    (line:int)
    (col:int)
    (prj:C_AST.project)
  : Ast.stub with_range option
  =
  (* Create the lexing buffer *)
  let buf = Lexing.from_string comment in
  buf.lex_curr_p <- {
    pos_fname = file;
    pos_lnum = line;
    pos_bol = 0;
    pos_cnum = col;
  };
  (* Parse the comment as a CST *)
  try
    let cst = Parser.stub Lexer.read buf in
    match cst with
    | None -> None
    | Some cst ->
      (* Remove predicates *)
      let cst' = Passes.Predicate_inlining.doit cst in
      (* Translate CST into AST *)
      let ast = Passes.Cst_to_ast.doit prj cst' in
      Some ast
  with
  | Lexer.SyntaxError s ->
    let range = Location.from_lexing_range (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf) in
    Exceptions.syntax_error range "%s" s

  | Parser.Error ->
    let range = Location.from_lexing_range (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf) in
    Exceptions.unnamed_syntax_error range
