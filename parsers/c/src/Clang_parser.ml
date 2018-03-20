(**
  Clang_parser - Extracting Clang AST to OCaml.


  Copyright (C) 2017 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.

  @author Antoine Miné
 *)

open Clang_AST
open Clang_utils


(** {1 Version} *)


let version = "0.1"



(** {1 Target information} *)

external get_default_target_options: unit -> target_options = "mlclang_get_default_target_options"
(** Returns the default target, which corresponds to the host. *)

external get_target_info: target_options -> target_info = "mlclang_get_target_info"
(** Gets the target informations (type width and alignment, ...) for the given target. *)


(** {1 Parsing} *)


external parse: target:target_options -> filename:string -> args:string array -> decl * diagnostic list = "mlclang_parse"
(** Parse the source file for the specified target, given the the specified compile-time options. Also returns a list of errors. *)
