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

(** Definitions common to output engines *)

open Core.All


(* Signature of an output engine *)
(* ----------------------------- *)

module type OUTPUT =
sig
  val report : ('a,'b) man -> 'a flow -> time:float -> files:string list -> out:string option -> unit
  val panic : exn -> btrace:string -> time:float -> files:string list -> out:string option -> unit
  val help : ArgExt.arg list -> out:string option -> unit
  val dump : ('a,'b) man -> 'a flow -> range:Location.range -> out:string option -> unit
  val print  : printer -> range:Location.range -> out:string option -> unit
  val list_domains : string list -> out:string option -> unit
  val list_hooks : string list -> out:string option -> unit
  val list_checks : check list -> out:string option -> unit
end


(* Output formats *)
(* -------------- *)

type format =
  | F_text (* Textual output *)
  | F_json (* Formatted output in JSON *)


(* Command line option *)
(* ------------------- *)

let opt_format = ref F_text
let opt_file : string option ref = ref None
let opt_display_lastflow = ref false
let opt_silent = ref false

