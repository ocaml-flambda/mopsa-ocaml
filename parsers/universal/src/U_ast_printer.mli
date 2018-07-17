(*
  Pretty-printer for abstract syntax trees.
*)

open Format
open U_ast

(* locations *)
val string_of_position: position -> string
val string_of_extent: extent -> string

(* printers *)
val print_unary_op: formatter -> unary_op -> unit
val print_binary_op: formatter -> binary_op -> unit
val print_typ: formatter -> typ -> unit
val print_var: formatter -> var -> unit
val print_expr: formatter -> expr -> unit
val print_stat: formatter -> stat -> unit
val print_prog: formatter -> prog -> unit
