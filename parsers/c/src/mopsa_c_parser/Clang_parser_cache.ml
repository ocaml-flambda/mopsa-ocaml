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
  Clang_parser_cache - Cache parsed AST to improve efficiency.

  AST are cached in marshalized files.
  We store the list of files used during parsing and check that they
  have not been modified before using the cache.
 *)



open Clang_AST
open Clang_parser


let debug fmt = Mopsa_utils.Debug.debug ~channel:"c.parser_cache" fmt

(** Version number. 
    This is checked when using the cache, and should be changed when
    the signature or the AST type change to invalidate the cache.
*)
let version = "Mopsa.C.AST/1"

       
(** Source file identification. *)
type file_signature =
    string   (* absolute filename *)
    * float  (* last modification time *)
    * int    (* length *)

        
(** Parse identification. *)
type signature =
    string                  (* parser command *)
    * target_options        (* target *)
    * string array          (* parser arguments *)
    * file_signature list   (* file names and timestamp *)


                     
(** Make filename absolute. *)
let file_abs f =
(*  if Filename.is_relative f then Filename.concat (Sys.getcwd ()) f
  else*) f

         
let get_file_signature (f:string) : file_signature =
  let f = file_abs f in
  let s = Unix.stat f in
  f, s.Unix.st_mtime, s.Unix.st_size

                        
let get_signature cmd tgt opts files : signature =
  cmd, tgt, opts, List.map get_file_signature files

                           
(** Checks that the signature is valid. *)    
let check_signature cmd tgt opts signature : bool =
  let cmd', tgt', opts', files' = signature in
  cmd = cmd' && tgt = tgt' && opts = opts' &&
  (List.for_all (fun s -> let f,_,_ = s in get_file_signature f = s) files')

    
(** File name of cache for a given source file name. *)
let file_cache_name file =
   file ^ ".mopsa_ast" 

    
(** Drop-in replacement to [Clang_parser.cache], but uses a cache on disk. *)
let parse cmd tgt file opts : parse_result =

  debug "Clang_parser_cache: parsing %s" file;
      
  (* try to read cache *)
  let file_cache = file_cache_name file in
  debug "Clang_parser_cache: looking for cache file %s" file_cache;
  let from_cache : parse_result option =
    try
      (* try cache file *)
      let f = Unix.openfile file_cache [Unix.O_RDWR] 0o666 in
      Unix.lockf f F_LOCK 0;
      let cache = Unix.in_channel_of_descr f in
      let v = Marshal.from_channel cache in
      let r = 
        if v <> version then (
          debug "Clang_parser_cache: %s incompatible version" file_cache;
          None
        )
        else
          let signature : signature = Marshal.from_channel cache in
          let check =
            try check_signature cmd tgt opts signature with _ -> false
          in
          if check then  (
            (* correct signature -> use cache *)
            debug "Clang_parser_cache: %s found" file_cache;
            Some (Marshal.from_channel cache)
          )
          else (
            (* incorrect signature *)
            debug "Clang_parser_cache: %s incompatible signature" file_cache;
            None
          )
      in
      ignore (Unix.lseek f 0 SEEK_SET);
      Unix.lockf f F_ULOCK 0;
      close_in cache;
      r
    with _ ->
      (* cache file not available *)
      debug "Clang_parser_cache: %s cache file not found" file_cache;
      None 
  in
  
  match from_cache with
  | Some c -> c
  | None ->
     (* parse *)
     let r = Clang_parser.parse cmd tgt file opts in
     let files = List.sort compare r.parse_files in
     let files = List.filter (fun x -> x <> "<built-in>") files in
     let c = get_signature cmd tgt opts files in
     (* store signature & parse result *)
     debug "Clang_parser_cache: storing cache to %s" file_cache;

     let f = Unix.openfile file_cache [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_TRUNC] 0o666 in
     let cache = Unix.out_channel_of_descr f in
     Unix.lockf f F_LOCK 0;
     Marshal.to_channel cache version [];
     Marshal.to_channel cache c [];
     Marshal.to_channel cache r [];
     flush cache;
     ignore (Unix.lseek f 0 SEEK_SET);
     Unix.lockf f F_ULOCK 0;
     close_out cache;
     r

let parse cmd tgt enable_cache file opts =
  if enable_cache then parse cmd tgt file opts
  else Clang_parser.parse cmd tgt file opts
   
