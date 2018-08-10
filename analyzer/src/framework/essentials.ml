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

type zone = Zone.zone

module Var =
struct
  type t = var
  let compare = compare_var
  let print = pp_var
end

