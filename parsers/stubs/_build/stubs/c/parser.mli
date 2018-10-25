
(* The type of tokens. *)

type token = 
  | TRUE
  | SUB
  | STRING of (string)
  | SEMICOL
  | RSHIFT
  | RPAR
  | RETURN
  | REQUIRES
  | RBRACK
  | RBRACE
  | OR
  | NEW
  | NEQ
  | MUL
  | MOD
  | LT
  | LSHIFT
  | LPAR
  | LOGOR
  | LOGAND
  | LOCAL
  | LE
  | LBRACK
  | LBRACE
  | INT of (Z.t)
  | IN
  | IMPLIES
  | IDENT of (string)
  | GT
  | GE
  | FREE
  | FORALL
  | FLOAT of (float)
  | FALSE
  | EXISTS
  | EQUAL
  | EQ
  | EOF
  | ENSURES
  | DOT
  | DIV
  | COMMA
  | COLON
  | CASE
  | BITXOR
  | BITOR
  | BITNOT
  | BITAND
  | ASSUMES
  | ASSIGNS
  | AND
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val stub: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.stub Ast.with_range)
