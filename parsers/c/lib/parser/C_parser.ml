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

(**
  Main entry point of the C parser
 *)
open Mopsa_utils
module StringSet = SetExt.StringSet

let debug fmt = Debug.debug ~channel:"c.parser" fmt

(* if only_parse is true, only parses the file without translating it to
   C AST nor adding the result to the context
 *)
let parse_file
    (command:string)
    (file:string)
    ?(target_options=Clang_parser.get_default_target_options ())
    (opts:string list)
    (warn_all:bool)
    (enable_cache:bool)
    (keep_static:bool)
    (only_parse:bool)
    (ctx:Clang_to_C.context)
  =
  (* remove some options that are in the way *)
  let filtered_opts =
    List.filter (fun o -> not (List.mem o ["-MF"])) opts
  in
  let opts = "-fparse-all-comments":: (* needed to get all comments *)
               filtered_opts
  in

  debug "Parsing %s, command '%s', target '%s', argument list %a" file command
    target_options.target_triple (ListExt.fprint ListExt.printer_list (fun ch s -> Format.fprintf ch "'%s'" s)) opts;

  let r = Clang_parser_cache.parse command target_options enable_cache file (Array.of_list opts)
  in

  let is_error =
    List.exists
      (function Clang_AST.({diag_level = Level_Fatal}) -> true | _ -> false)
       r.parse_diag
  in

  if not is_error then (
    List.iter (fun d ->
        if d.Clang_AST.diag_level = Level_Error ||
           d.Clang_AST.diag_level = Level_Warning && warn_all
        then
          let pos = Location.mk_pos
                      d.diag_loc.loc_file
                      d.diag_loc.loc_line
                      d.diag_loc.loc_column
          in
          let range = Location.mk_orig_range pos pos in
          Exceptions.warn_at range "%s" d.diag_message
      ) r.parse_diag;
    if only_parse then ()
    else
      Clang_to_C.add_translation_unit
        ctx (Filename.basename file)
        r.parse_decl r.parse_files r.parse_comments r.parse_macros
        keep_static
  )
  else
    let errors =
      List.map
        (fun diag ->
           let open Clang_AST in
           let pos = Location.mk_pos
               diag.diag_loc.loc_file
               diag.diag_loc.loc_line
               diag.diag_loc.loc_column
           in
           let range = Location.mk_orig_range pos pos in
           (range, diag.diag_message)
        )
        r.parse_diag
    in
    Exceptions.syntax_errors errors
