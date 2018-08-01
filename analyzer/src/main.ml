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
  (try
     let debug = Unix.getenv "MOPSADEBUG" in
     Debug.parse debug
   with Not_found ->
     ()
  );
  (try
     Framework.Options.(common_options.config <- Unix.getenv "MOPSACONFIG");
   with Not_found ->
     ()
  );
  ()


(** Start the analysis of [prog] using [domain] as the global abstraction. *)
let perform_analysis (domain: (module Domain.DOMAIN)) (prog : Ast.program) =
  (* Top layer analyzer *)
  let module Domain = (val domain) in
  let module Analyzer = Analyzer.Make(Domain) in

  let t = Timing.start () in

  Debug.info "Computing initial environments ...";
  let flow = Analyzer.init prog in
  Debug.info "Initial environments:@\n%a" (Flow.print Analyzer.man) flow;
  let stmt =
    Ast.mk_stmt (Ast.S_program prog) Framework.Location.(mk_file_range prog.Framework.Ast.prog_file)
  in

  Debug.info "Starting the analysis ...";

  let res = Analyzer.exec stmt flow in
  let t = Timing.stop t in
  Debug.debug ~channel:"result" "Result:@\n@[<h 2>  %a@]" (Flow.print Analyzer.man) res;

  Debug.info "Collecting alarms ...";
  let alarms = try Analyzer.ask Alarm.Q_alarms res with Not_found -> [] in
  t, alarms

type analysis_results =
  | ExcPanic of string
  | ExcPanicAt of Location.range * string
  | ExcUncaught of string * string
  | Success of float * Framework.Alarm.alarm list


let print_results analysis_res =
  match analysis_res with
  | Success(t, []) ->
    Format.printf "Analysis terminated in %.6fs@\n%a No alarm@\n" t
      ((Debug.color "green") Format.pp_print_string) "✔"
  | Success(t, alarms) ->
    Format.printf "Analysis terminated in %.6fs@\n%d alarm%a detected:@\n@[<hov4>    %a@]@\n"
      t
      (List.length alarms)
      Debug.plurial_list alarms
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n-------------@\n")
         Framework.Alarm.print
      ) alarms
  | ExcPanic s
  | ExcPanicAt(_, s) -> (* FIXME: process this case separately *)
    Debug.fail "Panic: %s" s
  | ExcUncaught(name, backtrace)  ->
    Debug.fail "Uncaught analyzer exception in %s@\n%s"
      name
      backtrace



(** Return the path of the configuration file *)
let get_config_path () =
  let config = Framework.Options.(common_options.config) in
  if Sys.file_exists config then config
  else
    let config' = "configs/" ^ config in
    if Sys.file_exists config' then config'
    else
      let config'' = "analyzer/" ^ config' in
      if Sys.file_exists config'' then config''
      else Framework.Exceptions.fail "Unable to find configuration file %s" config



(** Call the appropriate frontend to parse the input sources *)
let parse_program files =
  match Options.(common_options.lang) with
  | "universal" -> Lang.Universal.Frontend.parse_program files
  | _ -> Framework.Exceptions.panic "Unknown language"



(** Parse command line arguments and get all target source files *)
let get_sources f () =
  init_from_env ();
  let files = ref [] in
  let n = Array.length Sys.argv in
  Arg.parse !Options.spec (fun filename ->
      files := filename :: !files;
      if !Arg.current = n - 1 then
        f !files
    ) "Modular Open Platform for Static Analysis"



(** Main entry point *)
let () =
  get_sources (fun files ->
      let result = try
          let prog = parse_program files in
          let config = get_config_path () in
          let domain = Config.parse config in
          
          (* Start the analysis *)
          let () = Debug.debug ~channel:("main") "%a" Framework.Ast.pp_program prog in
          let t, alarms = perform_analysis domain prog in
          Success(t, alarms)
        with
        | Framework.Exceptions.Panic msg -> ExcPanic  msg
        | Framework.Exceptions.PanicAt (range, msg) -> ExcPanicAt (range,  msg)
        | e -> ExcUncaught(Printexc.to_string e, Printexc.get_backtrace ())
      in
      print_results result
    ) ()
