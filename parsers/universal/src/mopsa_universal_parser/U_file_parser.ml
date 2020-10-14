(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2019 The MOPSA Project.                                    *)
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
  Opens and parses a file given as argument.
 *)

open Mopsa_utils
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
