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

(** Render the output of an analysis depending on the selected engine. *)

open Core.All
open Common
open Mopsa_utils


(* Output engines *)
(* -------------- *)

let engines = Hashtbl.create 16

let () = Hashtbl.add engines F_text (module Text : OUTPUT)
let () = Hashtbl.add engines F_json (module Json : OUTPUT)

let get_output_engine () : (module OUTPUT) = Hashtbl.find engines !opt_format


(* Result rendering *)
(* ---------------- *)

(* Print collected alarms in the desired output format *)
let report man flow ~time ~files =
  let report = Core.Flow.get_report flow in
  let return_v = if !opt_silent || is_safe_report report then 0 else 1 in
  let module E = (val (get_output_engine ())) in
  if !opt_no_report then () else E.report man flow ~time ~files ~out:!opt_file;
  return_v

let panic ~btrace exn ~time ~files =
  let module E = (val (get_output_engine ())) in
  E.panic exn ~btrace ~time ~files ~out:!opt_file;
  2

let help (args:ArgExt.arg list) =
  let module E = (val (get_output_engine ())) in
  E.help args ~out:!opt_file

let list_domains (domains:string list) =
  let module E = (val (get_output_engine ())) in
  E.list_domains domains ~out:!opt_file

let list_checks (checks:check list) =
  let module E = (val (get_output_engine ())) in
  E.list_checks checks ~out:!opt_file

let list_hooks (hooks:string list) =
  let module E = (val (get_output_engine ())) in
  E.list_hooks hooks ~out:!opt_file

let print printer range =
  let module E = (val (get_output_engine ())) in
  E.print printer ~range ~out:!opt_file
