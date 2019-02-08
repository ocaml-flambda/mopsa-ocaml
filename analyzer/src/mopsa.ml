(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Essential modules. *)

include Framework.Ast

module Logging = Framework.Logging

module Setup = Framework.Setup

module Visitor = Framework.Visitor

include Framework.Zone

include Framework.Manager

module Flow = Framework.Flow

module Annotation = Framework.Annotation
type 'a annot = 'a Annotation.annot

module Channel = Framework.Channel
type 'a with_channel = 'a Channel.with_channel

module Post = Framework.Post
type 'a post = 'a Post.post

module Eval = Framework.Eval

include Location

include Framework.Domain

include Framework.Options

include Exceptions

module Callstack = Framework.Callstack

include Framework.Alarm
