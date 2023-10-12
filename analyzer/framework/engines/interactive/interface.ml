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

(** Signature of interactive engine interfaces *)

open Core.All
open Mopsa_utils
open Callstack
open Breakpoint
open Toplevel

type action =
  | Exec of stmt * route
  | Eval of expr * route * semantic

let action_range = function
  | Exec(stmt,_)   -> stmt.srange
  | Eval(expr,_,_) -> expr.erange

let action_vars = function
  | Exec(stmt,_)   -> stmt_vars stmt
  | Eval(expr,_,_) -> expr_vars expr

let action_line_opt action =
  let range = action_range action in
  if not (Location.is_orig_range range) then
    None
  else
    let file = Location.get_range_file range in
    let line = Location.get_range_line range in
    Some(file, line)

(** Get the variables appearing in the line of an action *)
let action_line_vars action =
  match action_line_opt action with
  | None -> []
  | Some(_, line) ->
    let visit_expr acc e =
      if not (Location.is_orig_range e.erange) then VisitParts acc
      else
        let line' = Location.get_range_line e.erange in
        if line = line' then Keep (acc@expr_vars e) else VisitParts acc
    in
    let visit_stmt acc s = VisitParts acc in
    match action with
    | Exec (stmt, _) ->
      fold_stmt visit_expr visit_stmt [] stmt

    | Eval (expr, _, _) ->
      fold_expr visit_expr visit_stmt [] expr

type command =
  | Continue
  | Next
  | Step
  | Finish
  | NextI
  | StepI

module FileMap = MapExt.StringMap
module LineMap = MapExt.IntMap
module CallstackMap = MapExt.Make
    (struct
      type t = callstack
      let compare = compare_callstack
    end)

type 'a envdb = (action * 'a CallstackMap.t) LineMap.t FileMap.t

let empty_envdb = FileMap.empty

let add_envdb action cs (env:'a) man (db:'a envdb) =
  let range = action_range action in
  let file = Location.get_range_file range |> Params.Paths.absolute_path in
  let line = Location.get_range_line range in
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
        let env'' = man.lattice.join empty_ctx env env' in
        FileMap.add file (LineMap.add line (action, CallstackMap.add cs env'' cmap) lmap) db

let find_envdb file line (db: 'a envdb) =
  FileMap.find file db |>
  LineMap.find line

let find_envdb_opt file line db =
  try Some(find_envdb file line db)
  with Not_found -> None

module type INTERFACE = functor(Toplevel : TOPLEVEL) ->
sig
  val init : unit -> unit
  val reach : action -> (Toplevel.t, Toplevel.t) man -> Toplevel.t flow -> unit
  val read_command : action -> Toplevel.t envdb -> (Toplevel.t, Toplevel.t) man -> Toplevel.t flow -> command
  val finish : (Toplevel.t, Toplevel.t) man -> Toplevel.t flow -> unit
  val error : exn -> unit
end
