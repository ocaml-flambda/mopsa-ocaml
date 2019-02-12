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
  Clang_to_C - Translates Clang AST to C AST and link C AST
 *)

type context
(** Abstract structure used internally during project parsing & linking. *)
    
val create_context: string -> Clang_AST.target_info -> context
(** [create_context project_name target] creates a new project (i.e., a program
    made of several translation units accumulated) with the given name and  
    target.
    Returns a context to manipulate the project.
 *)
                                                                          
val add_translation_unit: context -> string -> Clang_AST.decl -> Clang_AST.comment list -> Clang_AST.macro list -> unit
(** [add_translation_unit context name decl coms macros] converts a Clang definition
    of a translation unit with the given name (generally, the source C file)
    into a cAST and accumulates the definition to the projet.
 *)

val link_project: context -> C_AST.project
(** [link_project context] links all the translation units accumulated in the
    project and returns the consolidated definitions.
 *)
                                  

val dump_decls: bool ref
(** dump each C declarations found, for debugging *)

val log_rename: bool ref
(** log when renaming (or assign a name to an anonymous) *)

val log_merge: bool ref
(* log when merging declarations *)

val dump_dir: string ref
(** Log destination directory. *)

val simplify: bool ref
(* Whether to apply simplification *)                   

val new_uid: context -> C_AST.uid
                        
                     
