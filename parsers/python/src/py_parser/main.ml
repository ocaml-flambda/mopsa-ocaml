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
