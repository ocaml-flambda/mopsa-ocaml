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

(** Callstack - representation of the call stack of a program execution *)

open Location


(** {1 Call sites} *)
(** ************** *)

type callsite = {
  call_fun_orig_name:  string; (** Original name of the called function *)
  call_fun_uniq_name:  string; (** Unique name of the called function *)
  call_range: range;           (** Call location *)
}
(** Call site is the location of a call in the program *)

val pp_callsite : Format.formatter -> callsite -> unit
(** Print a call site *)

val compare_callsite : callsite -> callsite -> int
(** Compare two call sites *)


(** {2 Call stacks} *)
(** *************** *)

type callstack = callsite list
(** A call stack *)

val pp_callstack : Format.formatter -> callstack -> unit
(** Print a call stack *)

val pp_callstack_short : Format.formatter -> callstack -> unit
(** Print a call stack in a short style *)

val compare_callstack : callstack -> callstack -> int
(** Compare two call stacks *)

val empty_callstack : callstack
(** Empty call stack *)

val is_empty_callstack : callstack -> bool
(** Check that a call stack is empty *)

val callstack_length : callstack -> int
(** Return the length of a call stack *)

val push_callstack : string -> ?uniq:string -> range -> callstack -> callstack
(** [push_callstack orig ~uniq range cs] adds the call to function
    [orig] at location [range] at the top of the call stack [cs].
    The default unique function name of the function is its original name. *)

exception Empty_callstack
(** Exception raised when a call stack is empty *)

val pop_callstack : callstack -> callsite * callstack
(** [pop_callstack cs] returns the last call site in [cs] and the remaining call stack.
    Raises [Empty_callstack] if the call stack is empty. *)

val callstack_top : callstack -> callsite
(** [callstack_top cs] returns the last call site in [cs].
    Raises [Empty_callstack] if the call stack is empty. *)

val callstack_begins_with : callstack -> callstack -> bool
(** [callstack_begins_with ext cs] checks that [ext] is an extension of [cs], i.e. ext = x @ cs *)
