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

(** Common exceptions and warnings *)


(** {2 Warnings} *)
(** =-=-=-=-=-=- *)

let warn fmt = Debug.warn fmt

let warn_at range fmt =
  Format.kasprintf (fun str ->
      Debug.warn "%a: %s" Location.pp_range range str
    ) fmt

(** {2 Panic exceptions} *)
(** =-=-=-=-=-=-=-=-=-=- *)

exception Panic of string (** message *) * string (** OCaml line of code *)
exception PanicAt of Location.range * string (** message *) * string (** OCaml line of code *)

(** Raise a panic exception using a formatted string *)
let panic ?(loc="") fmt =
  Format.kasprintf (fun str ->
      if loc = "" then warn "panic: %s" str
      else warn "panic raised in %s: %s" loc str;
      raise (Panic (str, loc))
    ) fmt

let panic_at ?(loc="") range fmt =
  Format.kasprintf (fun str ->
      if loc = "" then warn_at range "panic: %s" str
      else warn_at range "panic raised in %s: %s" loc str;
      raise (PanicAt (range, str, loc))
    ) fmt


(** {2 Syntax-related exceptions *)
(** =-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

exception SyntaxError of Location.range * string
exception SyntaxErrorList of (Location.range * string) list

exception UnnamedSyntaxError of Location.range
exception UnnamedSyntaxErrorList of Location.range list

let syntax_error range fmt =
    Format.kasprintf (fun str ->
      raise (SyntaxError (range, str))
    ) fmt

let syntax_errors l =
  raise (SyntaxErrorList l)

let unnamed_syntax_error range =
    raise (UnnamedSyntaxError range)

let unnamed_syntax_errors ranges =
    raise (UnnamedSyntaxErrorList ranges)
