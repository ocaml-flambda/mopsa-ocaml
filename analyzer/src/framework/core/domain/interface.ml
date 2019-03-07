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

(** Zoning interface of abstract domains *)

open Zone

(** Generic zone interface *)
type 'a interface = {
  provides : 'a list;
  uses :     'a list;
}

let concat (i: 'a interface) (j: 'a interface) = {
  provides = i.provides @ j.provides;
  uses     = i.uses @ j.uses;
}

let sat_exec (zone:zone) (exec:zone interface) =
  List.exists (Zone.sat_zone zone) exec.provides

let sat_eval (zone:zone*zone) (eval:(zone*zone) interface) =
  List.exists (Zone.sat_zone2 zone) eval.provides
