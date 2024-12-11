(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2021 The MOPSA Project.                               *)
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

(** Trace partitioning keeping the k last of markers *)

open Mopsa
open Sig.Abstraction.Partitioning

module Domain =
struct

  type t = marker list

  let name = "universal.partitioning.tail-markers"

  let checks = []

  let opt_max_length = ref 1

  let () = register_domain_option name {
      key = "-tail-markers";
      doc = "threshold of the number of last markers to consider when partitioning traces";
      spec = ArgExt.Set_int opt_max_length;
      category = "Partitioning";
      default = string_of_int !opt_max_length;
    }

  let print fmt = function
    | [] ->
      Format.pp_print_string fmt "ε"
    | t ->
      Format.fprintf fmt "%a"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " ▶ ")
           pp_marker
        ) (List.rev t)

  let compare = List.compare compare_marker

  let init = []

  let add marker p =
    if !opt_max_length = 0 then [] else
    if List.length p < !opt_max_length then
      marker :: p
    else
      marker :: (List.rev p |> List.tl |> List.rev)

  let exec stmt man flow = None
  let eval expr man flow = None
  let ask query man flow = None

end

let () = register_partitioning (module Domain)
