(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Entry point of the analyzer. *)

open Framework
open Framework.Flow


(** Initialize analyzer using environment variables. *)
let init_from_env () =
  try
    let debug = Unix.getenv "MOPSADEBUG" in
    Debug.parse debug
  with Not_found ->
    ()


(** Initialize analyzer components and domains. *)
let init () =
  Options.setup ();
  init_from_env ();
  Lang.Universal.Setup.init ();
  Lang.C.Setup.init ();
  Lang.Python.Setup.init ();
  ()

(** Start the analysis of [prog] using [domain] as the global abstraction. *)
let perform_analysis (domain: (module Domains.Stateful.DOMAIN)) (prog : Ast.program) =
  (* Top layer analyzer *)
  let module Domain = (val domain) in
  let module Analyzer = Analyzer.Make(Domain) in

  let t = Timing.start () in

  Debug.info "Computing initial environments ...";
  let ctx, abs = Analyzer.init prog in
  let stmt =
    Ast.mk_stmt (Ast.S_program prog) Framework.Ast.(mk_file_range prog.prog_file)
  in

  Debug.info "Starting the analysis ...";

  let res = Analyzer.exec ctx stmt abs in
  let t = Timing.stop t in
  Debug.info "Result:@\n@[<h 2>  %a@]" Analyzer.flow_manager.print res;

  Debug.info "Collecting alarms ...";
  let alarms = Analyzer.ask ctx Alarm.QGetAlarms res in
  t, alarms

type analysis_results =
  | ExcPanic of string
  | ExcUncaught of string * string
  | Success of float * Framework.Alarm.alarm list option

let bench_printing analysis_res =
  match analysis_res with
  | Success(t, None) | Success(t, Some []) ->
    Format.printf "{\"time\": %.6f, \"alarms\": []}" t
  | Success(t, Some alarms) ->
    Format.printf "{\"time\": %.6f, \"alarms\": [%a]}"
      t
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         Framework.Alarm.pp_alarm_bench
      ) alarms
  | ExcPanic s ->
    Format.printf "{\"exc\": {\"etype\": \"Panic\", \"info\" : \"%s\"}}" s
  | ExcUncaught(s,s') ->
    Format.printf "{\"exc\": {\"etype\": \"Uncaught\", \"info\" : \"%s in %s\"}}" s s'

let verbose_printing analysis_res =
  match analysis_res with
  | Success(t, None) | Success(t, Some []) ->
    Format.printf "Analysis terminated in %.6fs@\n%a No alarm@\n" t
      ((Debug.color "green") Format.pp_print_string) "✔"
  | Success(t, Some alarms) ->
    Format.printf "Analysis terminated in %.6fs@\n%d alarm%a detected:@\n@[<hov4>    %a@]@\n"
      t
      (List.length alarms)
      Debug.plurial_list alarms
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n-------------@\n")
         Framework.Alarm.pp_alarm
      ) alarms
  | ExcPanic s ->
    Debug.fail "Panic: %s" s
  | ExcUncaught(name, backtrace)  ->
    Debug.fail "Uncaught analyzer exception in %s@\n%s"
      name
      backtrace

(** Start the analysis of [prog] using [domain] as the global abstraction. *)
let start (domain: (module Domains.Stateful.DOMAIN)) (prog : Ast.program) =
  (* Top layer analyzer *)

  let t, alarms = perform_analysis domain prog in
  Success(t, alarms)

(** Return the path of the configuration file. 
    First, check the existence of environment variable MOPSACONFIG.
    Otherwise, use the provided command line option -config.
*)
let get_config_path () =
  let config =
    try Unix.getenv "MOPSACONFIG" 
    with Not_found -> Options.(common_options.config)
  in
  if Sys.file_exists config then config
  else
    let config' = "configs/" ^ config in
    if Sys.file_exists config' then config'
    else
      let config'' = "analyzer/" ^ config' in
      if Sys.file_exists config'' then config''
      else Framework.Exceptions.fail "Unable to find configuration file %s" config


let () =
  init ();
  let files = ref [] in
  let n = Array.length Sys.argv in
  Arg.parse !Options.spec (fun filename ->
      files := filename :: !files;
      if !Arg.current = n - 1 then
        let result =
          try
            let prog =
              match Options.(common_options.lang) with
              | "c" ->
                Lang.C.Setup.start ();
                Lang.C.Frontend.parse_program !files
              | "python" ->
                Lang.Python.Setup.start ();
                Lang.Python.Frontend.parse_program !files
              | _ ->
                Framework.Exceptions.panic "Unknown language"
            in
            Debug.info "Parsing configuration file ...";
            let config = get_config_path () in
            let domain = Config.parse config in
            (* Start the analysis *)
            let t, alarms = perform_analysis domain prog in
            Success(t, alarms)
          with
          | Framework.Exceptions.Panic msg -> ExcPanic (String.escaped msg)
          | e -> ExcUncaught(String.escaped (Printexc.to_string e), Printexc.get_backtrace ())
        in
        match Options.(common_options.output_mode) with
        | "bench" ->
          bench_printing result
        | _ ->
          verbose_printing result
    ) "Modular Open Platform for Static Analysis"
