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

include Framework.Core.Ast

module Engine = Framework.Engine

module Logging = Framework.Engine.Logging

module Setup = Framework.Config.Setup

module Visitor = Framework.Core.Visitor

include Framework.Core.Zone

include Framework.Core.Manager

module Flow = Framework.Core.Flow

module Annotation = Framework.Core.Annotation
type 'a annot = 'a Annotation.annot

module Channel = Framework.Core.Channel

module Post = Framework.Core.Post
type 'a post = 'a Post.post

module Eval = Framework.Core.Eval

module Query = Framework.Core.Query

include Location

include Framework.Core.Domain
include Framework.Core.Value

include Framework.Config.Options

include Exceptions

module Callstack = Framework.Core.Callstack

include Framework.Core.Alarm

include Framework.Core.Eq

module Config = Framework.Config
module Abstraction = Framework.Config.Abstraction
module Options = Framework.Config.Options

module Export = Framework.Export
