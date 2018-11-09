
(* The type of tokens. *)

type token = 
  | UNSIGNED
  | UNION
  | TRUE
  | TLONG
  | TINT
  | TFLOAT
  | TDOUBLE
  | TCHAR
  | STRUCT
  | STRING of (string)
  | STAR
  | SIZE
  | SIGNED
  | SEMICOL
  | RSHIFT
  | RPAR
  | RETURN
  | REQUIRES
  | RBRACK
  | PREDICATE
  | PLUS
  | OR
  | OLD
  | OFFSET
  | NOT
  | NEW
  | NEQ
  | MOD
  | MINUS
  | LT
  | LSHIFT
  | LPAR
  | LOR
  | LOCAL
  | LNOT
  | LE
  | LBRACK
  | LAND
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
  | EQ
  | EOF
  | ENSURES
  | DOT
  | DIV
  | CONST
  | COLON
  | CHAR of (char)
  | CASE
  | BXOR
  | BOR
  | BNOT
  | BASE
  | BAND
  | ASSUMES
  | ASSIGNS
  | ASSIGN
  | ARROW
  | AND
  | ADDROF

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val stub: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.stub)
