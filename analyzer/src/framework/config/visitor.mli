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

(** Visitor of configuration files *)


open Yojson.Basic
open Yojson.Basic.Util


type 'a visitor = {
  leaf : string -> 'a;
  seq : Yojson.Basic.t list -> 'a;
  compose : Yojson.Basic.t list -> 'a;
  union : Yojson.Basic.t list -> 'a;
  apply : Yojson.Basic.t -> Yojson.Basic.t -> 'a;
  nonrel : Yojson.Basic.t -> 'a;
  product : Yojson.Basic.t list -> string list -> 'a;
}

val visit : 'a visitor -> Yojson.Basic.t -> 'a
