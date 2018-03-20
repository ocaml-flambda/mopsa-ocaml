(**
  Clang_utils - Utilities for the Clang AST

 
  Copyright (C) 2017 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.

  @author Antoine Miné
 *)


open Clang_AST


(** {1 Locations} *)

val empty_loc : loc
val empty_range : range
val loc_is_empty : loc -> bool
val range_is_empty : range -> bool


(** {1 Types} *)

val target_unsigned_int :
  target_int_type -> target_int_type

val int_type_align : target_info -> target_int_type -> int

val real_type_align :
  target_info -> target_real_type -> int

val int_type_width : target_info -> target_int_type -> int

val real_type_width :
  target_info -> target_real_type -> int

                         
(** {1 Debugging utilities} *)

external dump_block: recursive:bool -> 'a -> unit = "mlclang_dump_block"
