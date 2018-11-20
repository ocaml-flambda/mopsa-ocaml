(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Main entry point of the parser *)

let parse_file (filename:string) : Ast.program =
  let f = open_in filename in
  let buf = Lexing.from_channel f in
  buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = filename };

  try
    (* Parse the program source *)
    let cst = Parser.file_input Lexer.next_token buf in
    close_in f;

    (* Simplify the CST into an AST *)
    Cst_to_ast.translate_program cst |>

    (* Resolve scopes and generate unique IDs for variables *)
    Scoping.translate_program

  with
  | Lexer.LexingError e ->
    let range = Location.from_lexing_range (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf) in
    Exceptions.syntax_error range "%s" e

  | Parser.Error ->
    let range = Location.from_lexing_range (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf) in
    Exceptions.unnamed_syntax_error range
