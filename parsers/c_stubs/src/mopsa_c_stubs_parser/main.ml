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

(** Main entry point of the stub parser *)

open Mopsa_utils
open Mopsa_c_parser
open Clang_AST
open C_AST
open Location


let debug fmt = Debug.debug ~channel:"c_stubs_parser.main" fmt

let starts_with text prefix =
  (String.length text >= String.length prefix) &&
  (String.sub text 0 (String.length prefix) = prefix)

let is_stub_comment (com,_) =
  let comment = com.Clang_AST.com_text |>
                String.trim
  in
  starts_with comment "/*$"

let is_predicates_comment (com,_) =
  let comment = com.Clang_AST.com_text |>
                String.trim
  in
  starts_with comment "/*$="

let is_directive_comment (com,_) =
  let comment = com.Clang_AST.com_text |>
                String.trim
  in
  starts_with comment "/*$!"


exception StubNotFound


(* Parse function's comment into a stub CST *)
let rec parse_cst func ?(selector=is_stub_comment) prj enums predicates cache =
  match Hashtbl.find_opt cache func.func_org_name with
  | Some cst -> cst
  | None ->
    (* Find the stub of the function *)
    match List.find_opt selector func.func_com with
    | None -> raise StubNotFound
    | Some (com,macros) ->
      (* Create the lexing buffer *)
      let comment = com.com_text in
      let file = com.com_range.range_begin.loc_file in
      let line = com.com_range.range_begin.loc_line in
      let col = com.com_range.range_begin.loc_column in
      let buf = Lexing.from_string comment in
      buf.lex_curr_p <- {
        pos_fname = file;
        pos_lnum = line;
        pos_bol = 0;
        pos_cnum = col;
      };
      (* Parse the comment *)
      try
        let cst = Parser.parse_stub (Passes.Preprocessor.read predicates macros enums Lexer.read) buf in
        (* Resolve scoping of variables *)
        let cst' = Passes.Scoping.doit cst in
        (* Save the stub in the cache, so it can be used later when resolving
           aliases *)
        Hashtbl.add cache func.func_org_name cst';
        cst'
      with
      | Passes.Preprocessor.AliasFound alias ->
        (* Find the alias function *)
        begin match StringMap.find_opt alias prj.proj_funcs with
          | None -> raise StubNotFound
          | Some f ->
            parse_cst f prj enums predicates cache
        end

    | Lexer.SyntaxError s ->
      let range = Location.from_lexing_range (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf) in
      Exceptions.syntax_error range "%s" s

    | Parser.Error ->
      let range = Location.from_lexing_range (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf) in
      Exceptions.unnamed_syntax_error range



(** Parse the stub specification from comments of a function *)
let rec parse_function_comment
    (func:C_AST.func)
    ?(selector=is_stub_comment)
    (prj:C_AST.project)
    (enums:Z.t C_AST.StringMap.t)
    (predicates:Passes.Preprocessor.predicate C_AST.StringMap.t)
    (cache:(string,Cst.stub) Hashtbl.t)
  : Ast.stub
  =
  let cst = parse_cst func ~selector prj enums predicates cache in
  debug "stub of function %s:@\n  @[%a]" func.func_org_name Cst.pp_stub cst;
  (* Translate CST into AST *)
  Passes.Cst_to_ast.doit prj func cst

(** Parse comment of a stub directive *)
let parse_directive_comment
    (com:(Clang_AST.comment * C_AST.macro C_AST.StringMap.t) list)
    (range:Clang_AST.range)
    (prj:C_AST.project)
    (enums:Z.t C_AST.StringMap.t)
    (predicates:Passes.Preprocessor.predicate C_AST.StringMap.t)
    (stubs:(string,Cst.stub) Hashtbl.t)
  : Ast.stub
  =
  (* Create a dummy init function *)
  let func = {
      func_uid = 0;
      func_org_name = "$directive:" ^ (Clang_dump.string_of_range range);
      func_unique_name = "$directive:" ^ (Clang_dump.string_of_range range);
      func_is_static = false;
      func_return = C_AST.T_void, C_AST.no_qual;
      func_parameters = [||];
      func_body = None;
      func_static_vars = [];
      func_local_vars = [];
      func_variadic = false;
      func_range = range;
      func_name_range = range;
      func_com = com;
    }
  in
  parse_function_comment func ~selector:is_directive_comment prj enums predicates stubs


(** Parse a comment of predicates declarations *)
let parse_predicates_comment (coms:(Clang_AST.comment * Clang_AST.macro StringMap.t) list) : Passes.Preprocessor.predicate list =
  coms |> List.fold_left (fun acc (com,macros) ->
      if is_predicates_comment (com,macros) then
        (* Create the lexing buffer *)
        let comment = com.com_text in
        let file = com.com_range.range_begin.loc_file in
        let line = com.com_range.range_begin.loc_line in
        let col = com.com_range.range_begin.loc_column in
        let buf = Lexing.from_string comment in
        buf.lex_curr_p <- {
          pos_fname = file;
          pos_lnum = line;
          pos_bol = 0;
          pos_cnum = col;
        };
        (* Parse the comment *)
        try
          Passes.Preprocessor.parse_predicates Lexer.read buf @ acc
        with
        | Lexer.SyntaxError s ->
          let range = Location.from_lexing_range (Lexing.lexeme_start_p buf) (Lexing.lexeme_end_p buf) in
          Exceptions.syntax_error range "%s" s
      else
        acc
    ) []
