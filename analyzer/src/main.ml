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
let start (domain: (module Domains.Global.DOMAIN)) (prog : Ast.program) =
  (* Top layer analyzer *)
  let module Domain = (val domain) in
  let module Analyzer = Analyzer.Make(Domain) in

  let t = Timing.start () in

  try
    Debug.info "Computing initial environments ...";
    let ctx, abs = Analyzer.init prog in
    let stmt =
      Ast.mk_stmt (Ast.S_program prog) Framework.Ast.(mk_file_range prog.prog_file)
    in

    Debug.info "Starting the analysis ...";

    let res = Analyzer.exec stmt ctx abs in
    let t = Timing.stop t in
    Debug.info "Result:@\n@[<h 2>  %a@]" Analyzer.flow_manager.print res;

    Debug.info "Collecting alarms ...";
    let alarms = Analyzer.ask Alarm.QGetAlarms ctx res in

    (
      match alarms with
      | None
      | Some [] ->
        Format.printf "Analysis terminated in %.6fs@\n%a No alarm@\n" t ((Debug.color "green") Format.pp_print_string) "✔"
      | Some alarms ->
        Format.printf "Analysis terminated in %.6fs@\n%d alarm%a detected:@\n@[<hov4>    %a@]@\n"
          t
          (List.length alarms)
          Debug.plurial_list alarms
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n-------------@\n")
             Framework.Alarm.pp_alarm
          ) alarms
    );

  with
  | Framework.Exceptions.Panic msg -> Debug.fail "Panic: %s" msg

  | e ->
    Debug.fail "Uncaught analyzer exception in %s@\n%s"
      (Printexc.to_string e)
      (Printexc.get_backtrace ())

      ()

let () =
  init ();
  let files = ref [] in
  let n = Array.length Sys.argv in
  Arg.parse !Options.spec (fun filename ->
      files := filename :: !files;
      if !Arg.current = n - 1 then
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
          let domain = Config.parse Options.(common_options.config) in
          
          (* Start the analysis *)
          start domain prog
        with Framework.Exceptions.Panic msg ->
          Debug.fail "Panic: @[%s@]" msg
    ) "Modular Open Platform for Static Analysis"
