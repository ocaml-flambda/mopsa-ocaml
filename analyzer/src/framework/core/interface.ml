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

(** Interfaces are declarations of zones on which a domain operates. *)

open Zone

(** Interface of a transfer function (exec, eval) *)
type 'a function_interface = {
  provides : 'a list; (** List of provides zones *)
  uses     : 'a list; (** List of uses zones *)
}

(** Interface of a domain *)
type interface = {
  exec : zone function_interface; (** Interface of exec transfer function *)
  eval : (zone*zone) function_interface; (** Interface of eval transfer function *)
}

(** Concatenate two interfaces *)
let concat (i: interface) (j: interface) =
  {
    exec = {
      provides = i.exec.provides @ j.exec.provides;
      uses     = i.exec.uses @ j.exec.uses;
    };
    eval = {
      provides = i.eval.provides @ j.eval.provides;
      uses     = i.eval.uses @ j.eval.uses;
    };
}

(** Check if an interface satisfies a zone required by an exec *)
let sat_exec (zone:zone) (i:interface) =
  List.exists (Zone.sat_zone zone) i.exec.provides

(** Check if an interface satisfies a zone required by an eval *)
let sat_eval (zone:zone*zone) (i:interface) =
  List.exists (Zone.sat_zone2 zone) i.eval.provides
