(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Essential modules. *)

include Ast

module Visitor = Visitor

include Manager

module Flow = Flow

module Annotation = Annotation
type 'a annot = 'a Annotation.annot

module Channel = Channel
type 'a with_channel = 'a Channel.with_channel

module Post = Post
type 'a post = 'a Post.post

module Eval = Eval

include Location

include Domain

include Options

include Exceptions

module Var =
struct
  type t = var
  let compare = compare_var
  let print = pp_var
end

type expr_info = {
  compare : (Ast.expr -> Ast.expr -> int) -> Ast.expr -> Ast.expr -> int;
  print : (Format.formatter -> Ast.expr -> unit) -> Format.formatter -> Ast.expr -> unit;
  visit : (Ast.expr -> Ast.expr Visitor.structure) -> Ast.expr -> Ast.expr Visitor.structure;
}

let register_expr info =
  Ast.register_expr_compare info.compare;
  Ast.register_pp_expr info.print;
  Visitor.register_expr_visitor info.visit;
  ()
