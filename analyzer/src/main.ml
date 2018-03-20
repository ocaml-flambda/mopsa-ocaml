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

let unit_test_mode = ref false

let setup () =
  Options.register (
    "-config",
    Arg.String(fun f -> Config.config_file := f),
    " path to the domain configuration file"
  );
  Options.register (
    "-test",
    Arg.Set unit_test_mode,
    " unit test mode"
  );
  Options.register (
    "-debug",
    Arg.String(fun f ->
        Str.split (Str.regexp ",") f |>
        List.iter Debug.add_channel
      ),
    " debug channels"
  );
  Options.register (
    "-color",
    Arg.Bool(fun f -> Debug.print_color := f),
    " print debug messages in color"
  );
  Lang.Universal.Setup.all ();
  (* Lang.C.Setup.all (); *)
  Lang.Python.Setup.all ();
  ()



(** Start the analysis using the user-provided domain *)
let start (domain: (module Domains.Global.DOMAIN)) (prog : Ast.program) =
  (* Top layer analyzer *)
  let module Domain = (val domain) in
  let module Analyzer = Analyzer.Make(Domain) in

  let t = Timing.start () in

  try
    Debug.info "Computing initial environments ...";
    let abs = Analyzer.init prog in
    let stmt =
      Ast.mk_stmt
        (if !unit_test_mode then Ast.S_unit_test prog else Ast.S_program prog)
        Framework.Ast.(mk_file_range prog.prog_file)
    in
    let ctx = Framework.Context.empty in

    Debug.info "Starting the analysis ...";

    let res = Analyzer.exec stmt ctx abs in
    let t = Timing.stop t in
    Debug.info "Result:@\n@[<h 2>  %a@]" Analyzer.flow_manager.print res;

    (* Get alarms from all the domains *)
    let alarms = Analyzer.ask Alarm.QGetAlarms ctx res in

    (
      match alarms with
      | None
      | Some [] ->
        Format.printf "No alarm@\n"
      | Some alarms ->
        Format.printf "Alarms:@\n@[<hov4>    %a@]@\n"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n-------------@\n")
             Framework.Alarm.pp_alarm
          ) alarms
    );

    Debug.info "Analysis terminated in %.6fs" t

  with
  | Framework.Manager.StmtPanic stmt ->
    Debug.fail "Unable to analyze statement in %a:@\n @[%a@]"
      Framework.Pp.pp_range_verbose stmt.Framework.Ast.srange
      Framework.Pp.pp_stmt stmt

  | Framework.Manager.ExprPanic exp ->
    Debug.fail "Unable to evaluate expression in %a:@\n @[%a@]"
      Framework.Pp.pp_range_verbose exp.Framework.Ast.erange
      Framework.Pp.pp_expr exp

  | e ->
    Debug.fail "Uncaught analyzer exception in %s@\n%s"
      (Printexc.to_string e)
      (Printexc.get_backtrace ())

      ()


let () =
  setup ();

  Arg.parse !Options.spec (fun filename ->
      Debug.info "Parsing the program ...";
      let prog =
        match Filename.extension filename with
        (* | ".c" ->
         *    Lang.C.Frontend.parse_program filename *)
        | ".py" ->
          Lang.Python.Frontend.parse_program filename
        | _ ->
          failwith "Unknown program extension"
      in

      Debug.info "Parsing configuration file ...";
      let domain = Config.parse !Config.config_file in

      (* Start the analysis *)
      start domain prog
    ) "Modular Open Platform for Static Analysis"
