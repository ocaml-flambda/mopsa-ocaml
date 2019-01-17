(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Entry point of the analyzer. *)

open Framework


(** Initialize options from environment variables *)
let init_from_env () =
  (* Initialize debug channels from the environment variable MOPSADEBUG. *)
  (try Debug.parse (Unix.getenv "MOPSADEBUG")
   with Not_found -> ());

  (* Get the path of configuration file from variable MOPSACONFIG *)
  (try Options.(common_options.config <- Unix.getenv "MOPSACONFIG")
   with Not_found ->());
  ()


(** Return the path of the configuration file *)
let get_config_path () =
  let config = Framework.Options.(common_options.config) in
  if Sys.file_exists config && not (Sys.is_directory config) then config
  else
    let config' = "configs/" ^ config in
    if Sys.file_exists config' && not (Sys.is_directory config') then config'
    else
      let config'' = "analyzer/" ^ config' in
      if Sys.file_exists config'' && not (Sys.is_directory config'') then config''
      else Exceptions.panic "Unable to find configuration file %s" config



(** Call the appropriate frontend to parse the input sources *)
let parse_program files =
  Framework.Analyzer.progress "parsing";
  match Options.(common_options.lang) with
  | "universal" -> Lang.Universal.Frontend.parse_program files
  | "c" -> Lang.C.Frontend.parse_program files
  | "python" -> Lang.Python.Frontend.parse_program files
  | _ -> Exceptions.panic "Unknown language"



(** Parse command line arguments and apply [f] on the list of target
   source files *)
let iter_sources f () =
  init_from_env ();
  let files = ref [] in
  let n = Array.length Sys.argv in
  let return_value = ref 0 in
  Arg.parse !Options.spec (fun filename ->
      (* NOTE: filename could be a class name, not a file... *)
      (*
      if not (Sys.file_exists filename) then
        Debug.fail "File %s does not exist" filename;
        *)
      files := filename :: !files;
      if !Arg.current = n - 1 then
        return_value := !return_value * 10 + (f !files)
    ) "Modular Open Platform for Static Analysis";
  !return_value

(** Main entry point *)
let () =
  exit @@ iter_sources (fun files ->
      try
        let prog = parse_program files in
        let config = get_config_path () in

        let domain = Config.parse config in

        (* Top layer analyzer *)
        let module Domain = (val domain) in
        let module Analyzer = Analyzer.Make(Domain) in

        let t = Timing.start () in

        Framework.Analyzer.progress "computing initial environments";
        let flow = Analyzer.init prog in
        let stmt =
          Ast.mk_stmt (Ast.S_program prog) prog.prog_range
        in

        Framework.Analyzer.progress "starting the analysis";

        let res = Analyzer.exec stmt flow in
        let t = Timing.stop t in

        let () = Analyzer.output_actions () in
        Output.Factory.render Analyzer.man res t files

      with
        e -> Output.Factory.panic ~btrace:(Printexc.get_backtrace()) e files; 2
    ) ()
