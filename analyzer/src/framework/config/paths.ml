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

(** Information about system setup of the analyzer *)

(** Path to share directory *)
let opt_share_dir = ref ""

let set_share_dir dir =
  if not (Sys.is_directory dir) then Exceptions.panic "%s is not a directory" dir;
  opt_share_dir := dir

(* Return the path to the configurations directory *)
let get_configs_dir () =
  Filename.concat !opt_share_dir "configs"

(* Return the path to the stubs directory *)
let get_stubs_dir () =
  Filename.concat !opt_share_dir "stubs"

(* Return the path to the stubs directory of a language *)
let get_lang_stubs_dir lang () =
  Filename.concat (get_stubs_dir ()) lang

let resolve_stub lang stub =
  Filename.concat (get_lang_stubs_dir lang ()) stub


(** Return the path of the configuration file *)
let resolve_config_file config =
  let config =
    try Sys.getenv "MOPSACONFIG"
    with Not_found -> config
  in
  if Sys.file_exists config && not (Sys.is_directory config) then config
  else
    let file = Filename.concat (get_configs_dir ()) config in
    if Sys.file_exists file && not (Sys.is_directory file) then file
    else Exceptions.panic "unable to find configuration file %s" config

