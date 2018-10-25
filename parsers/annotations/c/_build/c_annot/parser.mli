
(* The type of tokens. *)

type token = 
  | TRUE
  | STRING of (string)
  | RIGHT_BRACK
  | RIGHT_BRACE
  | NULL
  | LEFT_BRACK
  | LEFT_BRACE
  | INT of (int)
  | ID of (string)
  | FLOAT of (float)
  | FALSE
  | EOF
  | COMMA
  | COLON

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.annot)
