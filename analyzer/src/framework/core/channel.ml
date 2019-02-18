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

(** Utility functions for decorating values with reduction channels. *)

open Manager

(** Decorator type of values with a list of reduction channels *)
type 'a with_channel = {
  value : 'a;
  channels : Post.channel list;
}

(** Decorate a value with a list of channels *)
let return ?(channels = []) a = {
  value = a;
  channels;
}

(** Get a value by removing the channel decoration *)
let without_channel (a: 'a with_channel) : 'a =
  a.value

(** Apply a decorated function on a value *)
let bind (f: 'a -> 'b with_channel) (a: 'a with_channel) : 'b with_channel =
  let b = f a.value in
  {b with channels = a.channels @ b.channels}

(** Apply a decorated function on a token flow and return the modified flow and the resulting channels *)
let map_domain_env (tk:token) (f:'t -> 't with_channel) (man:('a, 't) man) (flow:'a flow) : 'a flow * Post.channel list=
  let a = f (Flow.get_domain_env tk man flow) in
  Flow.set_domain_env tk a.value man flow, a.channels
