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

(** Signature of interactive engine interfaces *)

open Core.All
open Mopsa_utils
open Location
open Callstack
open Breakpoint
open Toplevel
open Action
open Envdb
open Trace

type command =
  | Continue
  | Next
  | Step
  | Finish
  | NextI
  | StepI
  | Backward

type state = {
  mutable depth: int;
  (** Current depth of the interpretation tree *)

  mutable command: command;
  (** Last entered command *)

  mutable command_depth: int;
  (** Depth of the interpretation tree when the command was issued *)

  mutable command_callstack : callstack;
  (** Callstack when the command was issued *)

  mutable callstack : callstack;
  (** Current call-stack *)

  mutable loc : range option;
  (** Last analyzed line of code *)

  mutable locstack : range option list;
  (** Stack of lines of codes *)

  mutable trace: trace;
  (** Analysis trace *)

  mutable call_preamble : bool;
  (** Flag set when calling a function and reset when reaching its first loc *)

  mutable alarms : AlarmSet.t;
  (** Set of discovered alarms *)
}

(* Initialize a state *)
let init_state s =
  s.command <- StepI;
  s.depth <- 0;
  s.command_depth <- 0;
  s.command_callstack <- empty_callstack;
  s.callstack <- empty_callstack;
  s.loc <- None;
  s.locstack <- [];
  s.trace <- empty_trace;
  s.call_preamble <- false;
  s.alarms <- AlarmSet.empty

(* Copy a state *)
let copy_state s =
  { command = s.command;
    depth = s.depth;
    command_depth = s.command_depth;
    command_callstack = s.command_callstack;
    callstack = s.callstack;
    loc = s.loc;
    locstack = s.locstack;
    trace = s.trace;
    call_preamble = s.call_preamble;
    alarms = s.alarms; }

(** Global state *)
let state = {
  command = StepI;
  depth = 0;
  command_depth = 0;
  command_callstack = empty_callstack;
  callstack = empty_callstack;
  loc = None;
  locstack = [];
  trace = empty_trace;
  call_preamble = false;
  alarms = AlarmSet.empty;
}

module type INTERFACE = functor(Toplevel : TOPLEVEL) ->
sig
  val init : unit -> unit
  val reach : action -> (Toplevel.t, Toplevel.t) man -> Toplevel.t flow -> unit
  val alarm : alarm list -> action -> (Toplevel.t, Toplevel.t) man -> Toplevel.t flow -> unit
  val read_command : action -> Toplevel.t envdb -> (Toplevel.t, Toplevel.t) man -> Toplevel.t flow -> command
  val finish : (Toplevel.t, Toplevel.t) man -> Toplevel.t flow -> unit
  val error : exn -> unit
end
