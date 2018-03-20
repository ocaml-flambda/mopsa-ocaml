(**
  C_print - Printing, converts C AST to valid C code.


  Copyright (C) 2017 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.
  @author Antoine Miné
*)

open C_AST

val integer_suffix : integer_type -> string
val float_suffix : float_type -> string
(** Suffix for litterals. *)

val string_of_signedness : signedness -> string
val string_of_integer_type : integer_type -> string
val string_of_float_type : float_type -> string
val string_of_record_kind : record_kind -> string
val string_of_qualifier : qualifier -> string
val string_of_binary_arithmetic : binary_arithmetic -> string
val string_of_binary_logical : binary_logical -> string
val string_of_binary_operator : binary_operator -> string
val string_of_unary_operator : unary_operator -> string
val string_of_inc_direction : inc_direction -> string
val string_of_var_decl : variable -> string
val string_of_var_advance_decl : variable -> string
val string_of_func_decl : func -> string
val string_of_func_proto : func -> string
val string_of_expr : expr -> string
val string_of_type : typ -> string
val string_of_type_qual : type_qual -> string
val string_of_string_literal : string -> string
val string_of_enum_decl : enum_type -> string
val string_of_record_decl : record_type -> string
val string_of_typedef : typedef -> string
(** Convert SAST types to string. *)

val print_project : out_channel -> project -> unit
(** Print a whole project as a valid C source. *)
