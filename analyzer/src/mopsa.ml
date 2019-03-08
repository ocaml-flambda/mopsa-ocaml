(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
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

module Query = Framework.Query
type 'a query = 'a Query.query

include Location

include Framework.Domain

include Framework.Options

include Exceptions

module Callstack = Framework.Callstack

include Framework.Alarm
