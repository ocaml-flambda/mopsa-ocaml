
(* The type of tokens. *)

type token = 
  | TOK_string of (string)
  | TOK_real of (string)
  | TOK_int of (string)
  | TOK_id of (string)
  | TOK_char of (char)
  | TOK_WHILE
  | TOK_TRUE
  | TOK_STRING
  | TOK_STAR
  | TOK_SEMICOLON
  | TOK_RPAREN
  | TOK_RETURN
  | TOK_REAL
  | TOK_RCURLY
  | TOK_RBRACKET
  | TOK_RAND
  | TOK_PRINT_ALL
  | TOK_PRINT
  | TOK_PLUS
  | TOK_NOT_EQUAL
  | TOK_MINUS
  | TOK_LPAREN
  | TOK_LESS_EQUAL
  | TOK_LESS
  | TOK_LCURLY
  | TOK_LBRACKET
  | TOK_INT
  | TOK_IF
  | TOK_GREATER_EQUAL
  | TOK_GREATER
  | TOK_FOR
  | TOK_FALSE
  | TOK_EXCLAIM
  | TOK_EQUAL_EQUAL
  | TOK_EQUAL
  | TOK_EOF
  | TOK_ELSE
  | TOK_DIVIDE
  | TOK_CONCAT
  | TOK_COMMA
  | TOK_CIRC
  | TOK_CHAR
  | TOK_BAR_BAR
  | TOK_BAR
  | TOK_ASSERT
  | TOK_ARRAY
  | TOK_AND_AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (U_ast.prog)
