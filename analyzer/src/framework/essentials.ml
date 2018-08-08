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

module Post = Post
type 'a post = 'a Post.post

module Eval = Eval

include Location

include Domain

include Options

include Exceptions

include Zone

module Var =
struct
  type t = var
  let compare = compare_var
  let print = pp_var
end

(** {2 Utility functions} *)

(** [set_local tk a man flow] overwrites the local part of a domain in the
   abstract element bound to token [tk] in [flow] *)
let set_local (tk: token) (a:'t) (man:('a, 't) man) (flow:'a flow) : 'a flow =
  Flow.set tk (man.set a (Flow.get tk man flow)) man flow

(** [get_local tk man flow] retrieves the local part of a domain in the
   abstract element bound to token [tk] in [flow] *)
let get_local (tk:token) (man:('a, 't) man) (flow:'a flow) : 't =
  man.get (Flow.get tk man flow)

(** [map_local tk f man flow] is equivalent to [set_local tk (f (get_local tk man flow)) man flow] *)
let map_local (tk:token) (f:'t -> 't) (man:('a, 't) man) (flow:'a flow) : 'a flow =
  set_local tk (f (get_local tk man flow)) man flow
