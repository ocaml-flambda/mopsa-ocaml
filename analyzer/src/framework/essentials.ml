(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Essential modules. *)

(** AST *)
include Ast
module Visitor = Visitor

include Manager

module Flow = Flow

module Context = Context

module Post = Post

module Eval = Eval

include Composers

include Utils.Location

let return x = Some x

module Var =
struct
  type t = var
  let compare = compare_var
  let print = pp_var
end
