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
      Printf.eprintf "Error near %s\n\n"
        (string_of_position lex.lex_start_p);
      failwith s

let parse_file (filename:string) : prog =
  let f = open_in filename in
  let lex = from_channel f in
  try
    lex.lex_curr_p <- { lex.lex_curr_p with pos_fname = filename; };
    U_parser.file U_lexer.token lex
  with
  | U_parser.Error ->
      Printf.eprintf "Parse error (invalid syntax) near %s\n"
        (string_of_position lex.lex_start_p);
      failwith "Parse error"
  | Failure x ->
     if x = "lexing: empty token" then (
       Printf.eprintf "Parse error (invalid token) near %s\n"
                      (string_of_position lex.lex_start_p);
       failwith "Parse error"
     )
     else raise (Failure x)
