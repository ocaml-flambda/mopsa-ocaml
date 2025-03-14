(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2023 The MOPSA Project.                               *)
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

open Core.All
open Mopsa_utils
open Callstack
open Action

module FileMap = MapExt.StringMap
module LineMap = MapExt.IntMap
module CallstackMap = MapExt.Make
    (struct
      type t = callstack
      let compare = compare_callstack
    end)

type 'a envdb = (action * 'a CallstackMap.t) LineMap.t FileMap.t

let empty_envdb = FileMap.empty

let add_envdb action ctx (env:'a) man (db:'a envdb) =
  let range = action_range action in
  let file = Location.get_range_file range |> Params.Paths.absolute_path in
  let line = Location.get_range_line range in
  let cs = find_ctx callstack_ctx_key ctx in
  match FileMap.find_opt file db with
  | None ->
    FileMap.add file (LineMap.singleton line (action, CallstackMap.singleton cs env)) db
  | Some lmap ->
    match LineMap.find_opt line lmap with
    | None ->
      FileMap.add file (LineMap.add line (action, CallstackMap.singleton cs env) lmap) db
    | Some (action', cmap) ->
      match CallstackMap.find_opt cs cmap with
      | None ->
        FileMap.add file (LineMap.add line (action, CallstackMap.add cs env cmap) lmap) db
      | Some env' ->
        let env'' = man.lattice.join ctx env env' in
        FileMap.add file (LineMap.add line (action, CallstackMap.add cs env'' cmap) lmap) db

let find_envdb file line (db: 'a envdb) =
  FileMap.find file db |>
  LineMap.find line

let find_envdb_opt file line db =
  try Some(find_envdb file line db)
  with Not_found -> None

