(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Translate a CST into a AST:
    - The input CST should not contain any predicate.
    - The local variables of the AST are given uids depending on their scope.
    - User-defined C types (struct, union, enum and typedef) are resolved
    using the project declarations.
    - Implicit casts are added to C expressions.
 *)

open Location

let rec doit
    (prj:C_AST.project)
    (cst:Cst.stub with_range)
  : Ast.stub with_range
  =
  assert false
