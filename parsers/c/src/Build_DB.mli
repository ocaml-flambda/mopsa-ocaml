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
  Build_DB - Build a database to manage the analysis of multi-file projects

  To help with the analysis of large projects that may have multiple
  source files, specific libraries, headers, preprocessing and compilation
  options, we create a database of these information.
  The database is typically extracted from an actual build, using wrappers
  for the build tools - see BuildWrapper.

  Note: the database is not very efficient as it is completely loaded and
  recreated at each file operation.
  Also, it requires a file lock in case of a concurrent build (e.g.: make -j).
  It should suffice for small project though and is probably not a bottleneck.
 *)


(** {1 DB representation} *)


type source_kind = SOURCE_C | SOURCE_CXX | SOURCE_ASM | SOURCE_UNKNOWN
(** Source languages. *)

type source = {
    source_path: string; (** absolute path of source file *)
    source_kind: source_kind;
    source_opts: string list; (** compilation options *)
    source_cwd: string; (** directory from where the compilation was launched *)
  }
(** Represents a compiled source. *)

val source_unknown : string -> source
(** Helper to create an unknown source with the given name. *)

type library_kind = LIBRARY_STATIC | LIBRARY_DYNAMIC
(** Kinds of lirbaries. *)

module StringMap : Map.S
(** Map with string key *)


type file_kind =
  | Object of source
  | Library of library_kind * file StringMap.t (** contents, indexed by file name *)
  | Executable of file list
  | Unknown of string (** absolute path *)

and file = string (** absolute path *) * file_kind

type db = file StringMap.t
(** Represents a full compilation database. Files are indexed by absolute paths. *)

val empty_db: db
(** The empty database. *)


(** {1 Printing} *)

val source_kind_name : source_kind -> string

val print_file : string -> file -> unit
val print_db : db -> unit


(** {1 Apply file operations to DB} *)

val db_remove : bool -> db -> string -> db
(** [db_remove recurse db file] deletes a file or directory, possibly recursively. *)

val db_copymove : bool -> bool -> db -> string -> string -> db
(** [db_copymove move rcur db org_file dest_file] copies or moves a file or directory. *)

val db_add_archive : db -> string -> library_kind -> string list -> db
(** [db_add_archive db archive kind files] creates or add files to a static or dynamic library. *)

val db_remove_archive : db -> string -> string list -> db
(** [db_remove_archive db archive files] removes some files from a static or dynamic library. *)

val db_extract_archive : db -> string -> string list -> db
(** [db_extract_archive db archive files] extracts a specified set of files from a library. *)

val db_extract_archive_all : db -> string -> db
(** [db_extract_archive_all db archive] extracts a the files in a library. *)

val db_compile : db -> source_kind -> string -> string -> string list -> file StringMap.t
(** [db_compile db language source object args] compliles the specified source file in the specified language into the specified object file, with the specified compilation command-line arguments. *)

val db_link : db -> string -> string list -> db
(** [db_link db executable sources] links the specified list of files into the specified executable file. *)


(** {1 DB loading, saving, locking} *)

val load_db : string -> db
val save_db : string -> db -> unit
(** [load_db db_filename] and [save_db db_filename db] load and save a database to file. *)

val lock_db : string -> unit
val unlock_db : string -> unit
(** [lock_db db_filename] and [unlock_db db_filename] use file-locking operations to ensure atomicity of database operations. *)


(** {1 DB extraction for analysis driver} *)

val get_executables : db -> string list
(** [get_executables db] returns the list of all executable compiled in this database. Full path-names are returned. *)

val get_executable_file_sources : db -> string -> source list
(** [get_executable_file_sources db executable_path] returns the list of sources that compose the given executable in the database (includes recursively the contents of libraries linked to the executable, when known). *)

val get_executable_sources : db -> string -> source list
(** [get_executable_sources db executable] behaves as [get_executable_file_sources] but uses the short executable name instead of the full path-name. *)



(** {1 Exported utilities} *)

val log : bool ref
val logfile : out_channel ref
val starts_with : string -> string -> bool
val absolute_path : string -> string
