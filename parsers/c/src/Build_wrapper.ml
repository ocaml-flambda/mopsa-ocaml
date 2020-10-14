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
  Build_wrapper - Build tool wrappers to help analyzing multi-file projects

  A wrapper for common tools used in an build: compilers, preprocessors,
  linkers, archiving, and file operations.
  This allows extracting from an actual build a database of the sources
  we need to consider to analyze a specific binary or library.

  Note: we generate a single binary to wrap a bunch of tools.
  The name used to invoke the tool, argv.(0), tells which tool to
  emulate.
 *)


open Mopsa_build_db



(** {1 Configuration} *)



let obj_extension = ".o"
let exe_extension = ""
let exe_default = "a.out"
let lib_extension = ".a"
let dll_extension = ".so"
let default_lib_path = ["/lib";"/usr/lib"]

(* used to distinguish the wrapper directory from other directories in $PATH *)
let wrapper_name = "mopsa-wrapper"

(* main environment variable to configure DB and log location *)
let mopsa_env = "MOPSADB"
let mopsa_log = "MOPSADBLOG"


(** {1 Utilities} *)


module StringSet = Set.Make(String)

(** get the directories in $PATH *)
let get_PATH () =
  try Str.split (Str.regexp ":") (Unix.getenv "PATH")
  with Not_found -> ["/usr/bin";"/bin"]

let get_log () =
  try Unix.getenv mopsa_log = "1"
  with Not_found -> false


(* find the location of the wrapper binary (or defaults to .) *)
let find_wrapper_dir () =
  let rec doit = function
    | [] -> "." (* default *)
    | p::rest ->
       if Sys.file_exists (Filename.concat p wrapper_name) then p (* found *)
       else doit rest
  in
  doit (get_PATH ())


(* find the location of a file in PATH, except the one containing wrapper_name *)
let find_unwrapped_exe exe =
  let rec doit = function
    | [] -> None
    | p::rest ->
       let pexe = Filename.concat p (Filename.basename exe)
       and pwrap = Filename.concat p wrapper_name
       in
       if Sys.file_exists pexe && not (Sys.file_exists pwrap) then Some pexe (* found *)
       else doit rest
  in
  doit (get_PATH ())


(* executes the original version of the tool, not the wrapper *)
let exec_unwrapped argv =
  match find_unwrapped_exe argv.(0) with
  | Some tool ->
     if !log then Printf.fprintf !logfile "DB: chaining to %s\n%!" tool;
     argv.(0) <- tool;
     Unix.execv tool argv
  | None ->
     if !log then Printf.fprintf !logfile "DB: no chaining\n%!"



(* utilitiy to split argument lists into files and options;
   glued shorts options are unglued: "-rf" becomes "-r", "-f"
*)
let split_file_options args =
  let files = ref [] and opts = ref [] in
  let rec doit = function
    | [] -> List.rev !files, List.rev !opts
    | "--"::rest -> List.rev (List.rev_append rest !files), List.rev !opts
    | a::rest ->
       if starts_with "--" a then
         (* long option *)
         opts := a::(!opts)
       else if starts_with "-" a && a <> "-" then
         (* short options: generate one entry per letter *)
         for i=1 to String.length a - 1 do
           opts := (Printf.sprintf "-%c" a.[i])::(!opts)
         done
       else
         (* not an option *)
         files := a::(!files);
       doit rest
  in
  doit args


(* whether l contains at least one element in k *)
let contains l k =
  List.exists (fun a -> List.mem a k) l

(* split a list at the end, extracting the last element *)
let cut_list l =
  let rec doit acc = function
    | [] -> [],"" (* special case for empty list *)
    | [a] -> List.rev acc, a
    | a::b -> doit (a::acc) b
  in
  doit [] l


let print_list sep f ch l =
  let first = ref true in
  List.iter
    (fun a ->
      Printf.fprintf ch "%s%a" (if !first then "" else sep) f a;
      first := false
    ) l


(** \@file: loads contents and splits at space *)
let at_file file =
  let file = String.sub file 1 (String.length file-1) in
  let c = open_in file in
  let len = in_channel_length c in
  let buf = Bytes.create len in
  really_input c buf 0 len;
  close_in c;
  (* todo: handle quotes and backslashes *)
  Str.split (Str.regexp "[ \t\r\n]+") (Bytes.to_string buf)


(** replace every @ argument with its contents *)
let expand_at_file args =
  let rec doit acc = function
    | [] -> List.rev acc
    | a::rest ->
       let acc =
         if starts_with "@" a
         then List.rev_append (at_file a) acc
         else a::acc
       in
       doit acc rest
  in
  doit [] args


(** {1 Wrapper for basic file operations (cp, mv, etc.)} *)


let rm db args =
  let files,opts = split_file_options args in
  let recur = contains opts ["-r";"-R";"--recursive"] in
  if !log then Printf.fprintf !logfile "DB: rm recur=%B files=%a\n%!" recur (print_list "," output_string) files;
  List.fold_left (db_remove recur) db files

let mv db args =
  let files,_ = split_file_options args in
  let srcs,dst = cut_list files in
  if !log then Printf.fprintf !logfile "DB: mv srcs=%a dst=%s\n%!" (print_list "," output_string) srcs dst;
  List.fold_left (fun src -> db_copymove true true src dst) db srcs


let cp db args =
  let files,opts = split_file_options args in
  let recur = contains opts ["-r";"-R";"-a";"--archive"] in
  let srcs,dst = cut_list files in
  if !log then Printf.fprintf !logfile "DB: cp recur=%B srcs=%a dst=%s\n%!" recur (print_list "," output_string) srcs dst;
  List.fold_left (fun src -> db_copymove false recur src dst) db srcs


let ln db args =
  (* handle link as copy *)
  let files,_ = split_file_options args in
  let srcs,dst = cut_list files in
  if !log then Printf.fprintf !logfile "DB: ln srcs=%a dst=%s\n%!" (print_list "," output_string) srcs dst;
  List.fold_left (fun src -> db_copymove false true src dst) db srcs



(** {1 Compiler and linker wrapper} *)


type cc_mode =
  | CC_COMPILE | CC_LINK | CC_MAKELIB | CC_NOTHING


type file_kind = C | CXX | ASM | OBJ | LIB | UNKNOWN

let identify_file ckind file =
  match String.lowercase_ascii (Filename.extension file) with
  | ".c" -> ckind
  | ".c++" | ".cxx" | ".cc" | ".cpp" -> CXX
  | ".s" | ".asm" -> ASM
  | ".o" | ".obj" -> OBJ
  | ".a" | ".so" | ".dll" | ".lib" -> LIB
  | _ -> UNKNOWN


let mode_name = function
  | CC_COMPILE -> "compile"
  | CC_LINK -> "link"
  | CC_MAKELIB -> "makelib"
  | CC_NOTHING -> "nothing"

let file_kind_name = function
  | C -> "C"
  | CXX -> "C++"
  | ASM -> "assembly"
  | OBJ -> "obj"
  | LIB -> "lib"
  | UNKNOWN -> "unknown"


(* try to find the library in the library search paths *)
let find_lib lib paths =
  let liba = "lib"^lib^lib_extension
  and libso = "lib"^lib^dll_extension
  in
  let rec doit = function
    | [] -> liba (* not found -> unchanged name *)
    | path::rest ->
       let t1 = Filename.concat path liba
       and t2 = Filename.concat path libso in
       if Sys.file_exists t1 then t1
       else if Sys.file_exists t2 then t2
       else doit rest
  in
  doit (paths@default_lib_path)



let compile ckind db args =
  let args = expand_at_file args in
  let mode = ref CC_LINK in
  let opts = ref [] in
  let out = ref "" in
  let libdirs = ref [] in
  (* libraries are often specified several times on a command line;
     we use a set to avoid considering it several times
   *)
  let libs = ref StringSet.empty in
  let srcs = ref [] in

  let rec doit = function
    | [] -> ()

    (* mode change *)
    | ("-E" | "-fsyntax-only" | "-S")::rest ->
       mode := CC_NOTHING;
       doit rest
    | "-shared"::rest ->
       mode := CC_MAKELIB;
       doit rest
    | "-c"::rest ->
       mode := CC_COMPILE;
       doit rest
    | ("-M"|"-MM"|"-MP")::rest ->
       mode := CC_NOTHING;
       doit rest

    | "-o"::o::rest ->
       (* out file *)
       out := o;
       doit rest

    | (("-I" | "-F" | "-idirafter" | "-iprefix" | "-isystem" | "-imultilib" | "-isysroot" | "-iquote") as o)::v::rest ->
       (* convert argument to absolute path name and keep it *)
       opts := (!opts)@[o;absolute_path v];
       doit rest

    | (("-D" | "-U" | "-include" | "-imacros" | "-iwithprefix" | "-iwithprefixbefore") as a)::v::rest ->
       (* keep an option without changing its argument *)
       opts := (!opts)@[a;v];
       doit rest

    | "-L"::v::rest ->
       (* add library path *)
       libdirs := (!libdirs)@[v];
       doit rest

    | "-l"::v::rest ->
       libs := StringSet.add (find_lib v !libdirs) (!libs);
       doit rest

    | "-z"::_:: rest ->
       (* ld option ignored *)
       doit rest

    | "--"::rest ->
       (* finish with files *)
       srcs := (!srcs)@(List.map absolute_path rest)

    | "-plugin"::v::rest when ends_with "liblto_plugin.so" v ->
       (* FIXME: this is a temporary fix for ignoring link time
          optimization.  In some systems, ld is called after creating
          an executable for performing link time optimization. Ignoring
          this step *should* keep the DB correct, but a better test is
          necessary to be sure about that.
       *)
       mode := CC_NOTHING;
       ()

    | x::rest ->
       if starts_with "-Wl," x then
         (* handle linker options *)
         let opts = List.tl (Str.split (Str.regexp ",") x) in
         doit (opts@rest)
       else if x.[0] = '-' && String.length x > 1 then
         match x.[1] with
         | 'I' | 'D' | 'U' | 'l' | 'L' ->
            (* revert to case with split option and argument *)
            doit ((String.sub x 0 2)::(String.sub x 2 (String.length x-2))::rest)
         | _ ->
            (* keep an option without argument *)
            opts := !(opts)@[x];
            doit rest
       else (
         (* source file *)
         srcs := (!srcs)@[absolute_path x];
         doit rest
       )

  in
  doit args;
  if !log then
    Printf.fprintf !logfile
      "DB: %s out=%s srcs=%a libs=%a libdirs=%a ops=%a\n%!"
      (mode_name !mode) !out
      (print_list "," output_string) !srcs
      (print_list "," output_string) (StringSet.elements !libs)
      (print_list "," output_string) !libdirs
      (print_list "," output_string) !opts;

  (* compile sources into objects, if needed *)
  let db, objs =
    List.fold_left
      (fun (db,objs) src ->
        match identify_file ckind src with
        | (C | CXX | ASM) as id when !mode <> CC_NOTHING ->
           let obj =
             if !mode = CC_COMPILE && !out <> "" then !out (* user-specified name *)
             else (Filename.remove_extension src)^obj_extension (* default name *)
           in
           let kind = match id with
             | C -> SOURCE_C
             | CXX -> SOURCE_CXX
             | ASM -> SOURCE_ASM
             | _ -> SOURCE_UNKNOWN
           in
           db_compile db kind src obj !opts,
           obj::objs
        | _ ->
           db, src::objs (* no compilation to do *)
      )
      (db,[]) !srcs
  in
  (* link objects and libraries, if neede *)
  match !mode with
  | CC_COMPILE | CC_NOTHING -> db
  | CC_LINK -> db_link db (if !out="" then exe_default else !out) (objs@(StringSet.elements !libs))
  | CC_MAKELIB -> db_add_archive db (if !out="" then exe_default else !out) LIBRARY_DYNAMIC objs



let link = compile C



(** {1 Archiver (static libraries) wrapper} *)


type ar_mode = AR_ADD | AR_REMOVE | AR_EXTRACT | AR_NOTHING

let ar db args =
  let args = expand_at_file args in
  let mode, args = match args with
    | mode::rest -> mode, rest
    | [] -> "", []
  in
  (* ignore option-like arguments *)
  let args = List.filter (fun s -> not (starts_with "-" s)) args in
  let mode, archive, files = match args with
    | archive::files ->
       (if String.contains mode 'q' then AR_ADD
        else if String.contains mode 'r' then AR_ADD
        else if String.contains mode 'd' then AR_REMOVE
        else if String.contains mode 'x' then AR_EXTRACT
        else AR_NOTHING),
       archive, files
    | _ -> AR_NOTHING, "", []
  in
  match mode, files with
  | AR_ADD,_ -> db_add_archive db archive LIBRARY_STATIC files
  | AR_REMOVE,_ ->  db_remove_archive db archive files
  | AR_EXTRACT, [] -> db_extract_archive_all db archive
  | AR_EXTRACT,_ -> db_extract_archive db archive files
  | AR_NOTHING,_ -> db



(** {1 Printing} *)

                  
let print dbfile args =
  (* db loading *)
  let db = try load_db dbfile with Unix.Unix_error _ -> empty_db in

  (* argument parsing *)
  let tool = Filename.basename (Sys.argv.(0)) in
  let verbose = ref false
  and json = ref false
  and files = ref [] in
  Arg.parse
    ["-v", Arg.Set verbose, "textual dump of all targets";
     "-json", Arg.Set json, "JSON dump of all targets"
    ]
    (fun x -> files := x::(!files))
    (tool^" [-v | -json | <target list>]");

  (* printing *)

  if !json then (
    Printf.printf "{\n  \"dbfile\": \"%s\",\n  \"contents\":\n" (String.escaped dbfile);
    print_db_json db;
    Printf.printf "\n}\n"
  )

  else (
    Printf.printf "DB file is %s\n" dbfile;
    if !verbose then print_db db
    else if !files = [] then (
      Printf.printf "List of executables:\n";
      List.iter (fun s -> Printf.printf "%s\n" s) (get_executables db)
    )
    else
      List.iter
        (fun exe ->
          try
            let srcs = get_executable_sources db exe in
            Printf.printf "Executable %s\n" exe;
            List.iter
              (fun src ->
                Printf.printf
                  "%s %s, in %s, %a\n"
                  src.source_path
                  (source_kind_name src.source_kind)
                  src.source_cwd
                  (print_list " " output_string) src.source_opts
              ) srcs
          with Not_found ->
            Printf.printf "%s not found\n" exe
        ) (List.rev !files);
  )


(** {1 Entry point} *)

let get_db_file () =
  try
    let path = Unix.getenv "MOPSADB" in
    if Sys.file_exists path && Sys.is_directory path
    then Filename.concat path "mopsa.db"
    else path
  with Not_found -> Filename.concat (find_wrapper_dir ()) "mopsa.db"



let main () =
  let dbfile = get_db_file () in
  log := get_log ();
  let logname = Filename.concat (Filename.dirname dbfile) "mopsa.log" in
  if !log then logfile := open_out_gen [Open_wronly;Open_creat;Open_append] 0o644 logname;

  let tool = Filename.basename (Sys.argv.(0))
  and args = List.tl (Array.to_list Sys.argv) in

  if !log then Printf.fprintf !logfile "DB: db file is %s\n%!" dbfile;
  if !log then Printf.fprintf !logfile "DB: got %s %a\n%!" tool (print_list " " output_string) args;

  (* cut suffix after - *)
  let tool_normalized = List.hd (Str.split (Str.regexp "-") tool) in

  (* executes action f on database *)
  let apply f =
    let d = open_db ~create:true dbfile in
    let db = read_db d in
    let db = f db args in
    write_db d db;
    close_db d;
  in
  
  (* action to database *)
  (match tool_normalized with
    | "cc" | "clang" | "gcc" -> apply (compile C)
    | "c++" | "clang++" | "g++" -> apply (compile CXX)
    | "ld" -> apply link
    | "ar" -> apply ar
    | "rm" -> apply rm
    | "mv" -> apply mv
    | "cp" -> apply cp
    | "ln" -> apply ln
    | "mopsa" -> print dbfile args; exit 0
    | _ -> () (* unknown -> nothing to do! *)
  );

  (* now execute the original command *)
  exec_unwrapped Sys.argv (* note: this does not return *)



let _ = main ()
