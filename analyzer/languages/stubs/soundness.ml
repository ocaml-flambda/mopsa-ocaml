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

(** Soundness assumptions *)

open Mopsa

type assumption_kind += A_stub_soundness_message of string

let () =
  register_assumption {
    print = (fun next fmt -> function
        | A_stub_soundness_message msg -> Format.pp_print_string fmt msg
        | a -> next fmt a
      );
    compare = (fun next a1 a2 ->
        match a1,a2 with
        | A_stub_soundness_message msg1, A_stub_soundness_message msg2 ->
          String.compare msg1 msg2
        | _ -> next a1 a2
      );
  }
