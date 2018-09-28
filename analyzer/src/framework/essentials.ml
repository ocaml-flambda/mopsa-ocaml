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

include Zone

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

include Alarm

module Var =
struct
  type t = var
  let compare = compare_var
  let print = pp_var
  let var_uniq_name v =
    let () = Format.fprintf Format.str_formatter "%a" pp_var v in
    Format.flush_str_formatter ();
end

type expr_info = {
  compare : (Ast.expr -> Ast.expr -> int) -> Ast.expr -> Ast.expr -> int;
  print : (Format.formatter -> Ast.expr -> unit) -> Format.formatter -> Ast.expr -> unit;
  visit : (Ast.expr -> Ast.expr Visitor.structure) -> Ast.expr -> Ast.expr Visitor.structure;
}

type stmt_info = {
  compare : (Ast.stmt -> Ast.stmt -> int) -> Ast.stmt -> Ast.stmt -> int;
  print : (Format.formatter -> Ast.stmt -> unit) -> Format.formatter -> Ast.stmt -> unit;
  visit : (Ast.stmt -> Ast.stmt Visitor.structure) -> Ast.stmt -> Ast.stmt Visitor.structure;
}


let register_expr (info: expr_info) : unit =
  Ast.register_expr_compare info.compare;
  Ast.register_pp_expr info.print;
  Visitor.register_expr_visitor info.visit;
  ()

let register_stmt (info: stmt_info) : unit =
  Ast.register_stmt_compare info.compare;
  Ast.register_pp_stmt info.print;
  Visitor.register_stmt_visitor info.visit;
  ()
