(*
  Opens and parses a file given as argument.
*)

open U_ast
open U_ast_printer
open Lexing

(* parsing, with nice error messages *)

let parse_from_string s =
  let lex = from_string s in
  try
    lex.lex_curr_p <- {lex.lex_curr_p with pos_cnum = 0};
    U_parser.file U_lexer.token lex
  with
  | Failure s ->
    let range = Location.from_lexing_range (Lexing.lexeme_start_p lex) (Lexing.lexeme_end_p lex) in
    Exceptions.syntax_error range "%s" s

let parse_file (filename:string) : prog =
  let f = open_in filename in
  let lex = from_channel f in
  try
    lex.lex_curr_p <- { lex.lex_curr_p with pos_fname = filename; };
    U_parser.file U_lexer.token lex
  with
  | U_parser.Error ->
    let range = Location.from_lexing_range (Lexing.lexeme_start_p lex) (Lexing.lexeme_end_p lex) in
    Exceptions.unnamed_syntax_error range

  | Failure x ->
    if x = "lexing: empty token" then (
      let range = Location.from_lexing_range (Lexing.lexeme_start_p lex) (Lexing.lexeme_end_p lex) in
      Exceptions.unnamed_syntax_error range
    )
    else
      raise (Exceptions.panic "%s" x)
