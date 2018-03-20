(**
  Build_DB - Build database to manage the analysis of large projects


  Copyright (C) 2017 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.

  @author Antoine Miné
 *)


let log = ref false
let logfile = ref (open_out "/dev/null")
                        

(** {1 DB representation} *)


module StringMap = Map.Make(String)
(** Map with string key *)

type source_kind = SOURCE_C | SOURCE_CXX | SOURCE_ASM | SOURCE_UNKNOWN

let source_kind_name = function
  | SOURCE_C -> "C"
  | SOURCE_CXX -> "C++"
  | SOURCE_ASM -> "assembly"
  | SOURCE_UNKNOWN -> "unknown"

type source = {
    source_path: string; (** absolute path of source file *)
    source_kind: source_kind;
    source_opts: string list; (** compilation options *)
    source_cwd: string; (** directory from where the compilation was launched *)
  }
(** a compiled source *)

let source_unknown (path:string) = {
    source_path = path;
    source_kind = SOURCE_UNKNOWN;
    source_opts = [];
    source_cwd = Sys.getcwd();

  }

module SourceSet = Set.Make(struct type t=source let compare=compare end)
(* sets of sources with physical equality, to easily remove dupplicates *)
                           
                
type library_kind = LIBRARY_STATIC | LIBRARY_DYNAMIC

let library_kind_name = function
  | LIBRARY_STATIC -> "static"
  | LIBRARY_DYNAMIC -> "dynamic"

                         
type file_kind =
  | Object of source
  | Library of library_kind * file StringMap.t (** contents, indexed by file name *)
  | Executable of file list
  | Unknown of string (** absolute path *)
                       
and file = string (** absolute path *) * file_kind

type db = file StringMap.t (** indexed by absolute path *)

let empty_db = StringMap.empty


(** {1 Printing} *)

               
let rec print_file indent (name,kind) =
  match kind with
  | Object s ->
     Printf.printf
       "%sOBJECT %s source=%s args=" indent
       (source_kind_name s.source_kind) s.source_path;
     List.iter (fun l -> Printf.printf "%s;" l) s.source_opts;
     Printf.printf " path=%s\n" s.source_cwd
  | Library (k,contents) ->
     Printf.printf
       "%sLIBRARY %s %s\n" indent
       (library_kind_name k) name;
     StringMap.iter
       (fun tag file ->
         Printf.printf "%s%s\n" indent tag;
         print_file (indent^"    ") file
       ) contents
  | Executable contents ->
     Printf.printf "%sEXECUTABLE %s\n" indent name;
     List.iter (fun file -> print_file (indent^"  ") file) contents
  | Unknown _ ->
     Printf.printf "%sUNKNOWN %s\n" indent name

    
let print_db (db:db) =
  StringMap.iter (fun _ -> print_file "") db

                 
               
(** {1 Utilities} *)

               
(* whether string s starts with pref *)
let starts_with pref s =
  let pl,sl = String.length pref, String.length s in
  (sl >= pl) && (String.sub s 0 pl = pref)



(* ensures that a filename has an absolute path
   and is normalized (no . nor ..)
 *)
let absolute_path name =
  (* add currenct directory *)
  let name =
    if Filename.is_relative name
    then Filename.concat (Sys.getcwd()) name
    else name
  in
  (* remove . and .. *)
  let rec normalize path =
    let path, name = Filename.dirname path, Filename.basename path in
    if name = path then path else
      let path = normalize path in
      if name = Filename.current_dir_name then path
      else if name = Filename.parent_dir_name then Filename.dirname path
      else Filename.concat path name
  in
  normalize name


         
(** {1 Apply file operations to DB} *)


(** recurse in directory *)
let get_files (db:db) (file:string) (recur:bool) : string list =
  let file = absolute_path file in
  if recur && Sys.file_exists file && Sys.is_directory file then
    (* ensures file ends with a directory separator *)
    let file = Filename.concat file "" in 
    StringMap.fold
      (fun k _ acc -> if starts_with file k then k::acc else acc)
      db []
  else
    (* non-directory *)
    if StringMap.mem file db then [file]
    else []
  

(** delete a file or directory *)
let db_remove (recur:bool) (db:db) (file:string) : db =
  let files = get_files db file recur in
  List.fold_left
    (fun db k ->
      if !log then Printf.fprintf !logfile "DB: remove %s\n%!" k;
      StringMap.remove k db
    ) db files

                 
(** copy or move a file or directory *)
let db_copymove (move:bool) (recur:bool) (db:db) (org:string) (dest:string) : db =
  let files = get_files db org recur in
  let org = absolute_path org
  and dest = absolute_path dest in
  let move = Sys.file_exists dest && Sys.is_directory dest in (* move instead of rename *)
  let prefix = if move then Filename.dirname org else org in (* prefix to remove *)
  List.fold_left
    (fun db korg ->
      (* replace prefix with dest in filename to get copied filename *)
      let kdest = dest^(String.sub korg 0 (String.length prefix)) in
      if !log then Printf.fprintf !logfile "DB: %s %s to %s\n%!" (if move then "move" else "copy") korg kdest;
      let _,f = StringMap.find korg db in
      let db = if move then StringMap.remove korg db else db in
      StringMap.add kdest (kdest,f) db
    ) db files


(** create or add files to an archive *)    
let db_add_archive (db:db) (archive:string) (kind:library_kind) (files: string list) : db =
  let archive = absolute_path archive in
  let contents =
    try
      match StringMap.find archive db
      with _, Library (_,c) -> c | _ -> StringMap.empty
    with Not_found -> StringMap.empty
  in
  let contents =
    List.fold_left
      (fun contents file ->
        let key = Filename.basename file in
        let file = absolute_path file in
        let c =
          try StringMap.find (absolute_path file) db
          with Not_found -> file, Unknown file (* keep track of unknown files in archives *)
        in
        if !log then Printf.fprintf !logfile "DB: add %s to archive %s as %s\n%!" file archive key;
        StringMap.add key c contents
      )
      contents files
  in
  StringMap.add archive (archive, Library (kind,contents)) db

                
(** remove files from an archive *)
let db_remove_archive (db:db) (archive:string) (files: string list) : db =
  let archive = absolute_path archive in
  if StringMap.mem archive db then
    match StringMap.find archive db with
    | _, Library (kind, r) ->
       let r =
         List.fold_left
           (fun r file ->
             let key = Filename.basename file in
             if !log then Printf.fprintf !logfile "DB: remove %s from archive %s\n%!" key archive;
             StringMap.remove key r
           )
           r files in
       StringMap.add archive (archive, Library (kind, r)) db
    | _ -> db (* not an archive: do nothing *)
  else db (* unknow archive: do nothing *)

         
(** extract some files from an archive *)         
let db_extract_archive (db:db) (archive:string) (files: string list) : db =
  let archive = absolute_path archive in
  if StringMap.mem archive db then
    match StringMap.find archive db with
    | _, Library (kind, contents) ->
       List.fold_left
         (fun db f ->
           let src = Filename.basename f
           and dest = absolute_path f in
           try
             let _,v = StringMap.find src contents in
             if !log then Printf.fprintf !logfile "DB: extract %s from archive %s as %s\n%!" src archive dest;
             StringMap.add dest (dest,v) db
           with Not_found -> db
         )
         db files
    | _ -> db (* not an archive: do nothing *)
  else db (* unknow archive: do nothing *)
                

(** extract all files from an archive *)         
let db_extract_archive_all (db:db) (archive:string) : db =
  let archive = absolute_path archive in
  if StringMap.mem archive db then
    match StringMap.find archive db with
    | _, Library (kind, contents) ->
       StringMap.fold
         (fun tag (_,v) db ->
           let dest = absolute_path tag in
           if !log then Printf.fprintf !logfile "DB: extract %s from archive %s as %s\n%!" tag archive dest;
           StringMap.add dest (dest,v) db         
         )
         contents db
    | _ -> db (* not an archive: do nothing *)
  else db (* unknow archive: do nothing *)
                

(** compile to object *)         
let db_compile (db:db) (kind:source_kind) (src:string) (obj:string) (args: string list) =
  let src = absolute_path src
  and obj = absolute_path obj in
  if !log then Printf.fprintf !logfile "DB: compile %s to %s\n%!" src obj;
  let s =
    { source_kind = kind;
      source_path = src;
      source_opts = args;
      source_cwd = Sys.getcwd ();
    }
  in
  StringMap.add obj (obj, Object s) db


(** link to executable *)
let db_link (db:db) (out:string) (files: string list) =
  if files = [] then db
  else
    let out = absolute_path out in
    let files = List.map absolute_path files in
    let contents =
      List.map
        (fun file ->
          try StringMap.find file db
          with Not_found -> file, Unknown file (* keep track of unknown files in exe *)
        )
        files
    in
    if !log then (
      Printf.fprintf !logfile "DB: link executable %s\n%!" out;
      List.iter (fun x -> Printf.fprintf !logfile "BD:   adding %s\n%!" x) files
    );
    StringMap.add out (out, Executable contents) db

      
               
(** {1 DB loading, saving, locking} *)

                
let load_db (dbfile:string) : db =
  let f = open_in dbfile in
  let db:db =
    if in_channel_length f = 0 then StringMap.empty
    else Marshal.from_channel f
  in
  close_in f;
  db

let save_db (dbfile:string) (db:db) =
  let f = open_out dbfile in
  Marshal.to_channel f db [];
  close_out f

let lockf_db op dbfile =
  let f = Unix.openfile dbfile [Unix.O_RDWR;Unix.O_CREAT] 0o640 in
  Unix.lockf f op 0;
  Unix.close f
             
let lock_db (dbfile:string) = lockf_db Unix.F_LOCK dbfile
let unlock_db (dbfile:string) = lockf_db Unix.F_ULOCK dbfile


                                         
(** {1 DB extraction for analysis driver} *)

                                         
(** extract executables from DB *)
let get_executables (db:db) : string list =
  let r = 
    StringMap.fold
      (fun n (_,k) acc ->
        match k with
        | Executable _ -> n::acc
        | _ -> acc
      ) db []
  in
  List.rev r
           
           
(** get all the sources making an executable (including library contents) *)
let get_executable_file_sources (db:db) (exe:string) : source list =
  let rec doit acc = function
    | (_,Object src)::rest ->
       doit (SourceSet.add src acc) rest
    | (_,Library (_,m))::rest ->
       doit (StringMap.fold (fun _ f acc -> doit acc [f]) m acc) rest
    | (_,Unknown src)::rest ->
       doit (SourceSet.add (source_unknown src) acc) rest
    | _::rest ->
       doit acc rest
    | [] -> acc
  in
  match StringMap.find exe db with
  | _, Executable l -> SourceSet.elements (doit SourceSet.empty l)
  | _ -> raise Not_found

               
(** as get_executable_file_sources, but use the executable name instead of absolute file path *)
let get_executable_sources (db:db) (exe:string) : source list =
  let exe = Filename.basename exe in
  let m = StringMap.filter (fun k _ -> Filename.basename k = exe) db in
  if StringMap.is_empty m then raise Not_found
  else get_executable_file_sources db (fst (StringMap.min_binding m))
