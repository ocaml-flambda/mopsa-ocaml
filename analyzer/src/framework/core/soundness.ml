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

(** Soundness warnings *)

open Location

type warning = {
  warn_range   : range option;
  warn_message : string;
}

let pp_warning fmt w =
  match w.warn_range with
  | None   -> Format.fprintf fmt "%s" w.warn_message
  | Some r -> Format.fprintf fmt "%a: %s" pp_range r w.warn_message 


let compare_warning w1 w2 =
  Compare.compose [
    (fun () -> OptionExt.compare compare_range w1.warn_range w2.warn_range);
    (fun () -> compare w1.warn_message w2.warn_message);
  ]

module Warnings = Set.Make(struct
    type t = warning
    let compare = compare_warning
  end)

let warnings = ref Warnings.empty

let is_sound () = Warnings.is_empty !warnings

let get_warnings () = Warnings.elements !warnings

let warn fmt =
  Format.kasprintf (fun msg ->
      Exceptions.warn "%s" msg;
      let w = {
        warn_range = None;
        warn_message = msg;
      }
      in
      warnings := Warnings.add w !warnings
    ) fmt

let warn_at range fmt =
  Format.kasprintf (fun msg ->
      Exceptions.warn_at range "%s" msg;
      let w = {
        warn_range = Some range;
        warn_message = msg;
      }
      in
      warnings := Warnings.add w !warnings
    ) fmt
