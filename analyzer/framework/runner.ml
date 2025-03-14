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

(** Runner - main entry point of the analysis *)

open Mopsa_utils
open Core.All
open Params.Options

let () = Sys.catch_break true

 (** {2 Command-line options} *)
(** ************************ *)

let opt_interactive = ref "automatic"

let () =
  register_builtin_option {
    key = "-engine";
    category = "Debugging";
    doc = "selects analysis mode";
    spec =  Symbol (
         ["automatic"; "interactive"; "dap"],
        (fun s ->
           match s with
           | "automatic" -> opt_interactive := "automatic"
           | "interactive" -> opt_interactive := "interactive"
           | "dap" -> opt_interactive := "dap"
           | _ -> ()
        )
      );
    default = "automatic";
  }

(** Parse command line arguments and apply [f] on the list of target
   source files *)
let parse_options passed_args f () =
  let files = ref [] in
  let args  = ref None in
  try
    Arg.parse_argv passed_args
    (ArgExt.argext_to_arg_list (Params.Options.get_options ()))
               (fun filename -> files := filename :: !files)
               "Modular Open Platform for Static Analysis";
    f !files !args
  with Stdlib.Arg.Bad f ->
    Params.Options.help (); 2


(** {2 Parsing} *)
(** *********** *)

(** Call the appropriate frontend to parse the input sources *)
let parse_program lang files =
  let front =
    try find_language_frontend lang
    with Not_found -> Exceptions.panic "No front-end found for language %s" lang
  in
  (* NOTE: we use the exit code 3 for parsing errors *)
  match front.parse files with
  | exception (Failure s) ->
    (* Typically caused by raising failure from the C++ bindings to Clang *)
    Format.eprintf "Parsing error: %s\n" s;
    exit 3
  | exception (Exceptions.Panic (s, _)) ->
    (* Typically caused by the MOPSA conversion from Clang AST to C AST *)
    Format.eprintf "%s\n" s;
    exit 3
  | exception e ->
    Format.eprintf "Unknown Exception %s" (Printexc.to_string e);
    exit 3
  | prg -> prg



(** {2 Entry points} *)
(** **************** *)

let analyze_files (files:string list) (args:string list option) : int =
  let t = Timing.start () in
  try
    let config = Params.Paths.resolve_config_file !Params.Config.Parser.opt_config in
    let abstraction = Params.Config.Parser.parse config in
    let domain = Params.Config.Builder.from_json abstraction.domain in

    let prog = parse_program abstraction.language files in

    (* Top layer analyzer *)
    let module Domain = (val domain) in
    let module Toplevel = Toplevel.Make(Domain) in
    let module Engine =
      (val
        match !opt_interactive with
        | "interactive"   ->
           let module E = Engines.Interactive.Engine.Make(Toplevel)(Engines.Interactive.Terminal.Make) in
           (module E)
        | "dap"   ->
           let module E = Engines.Interactive.Engine.Make(Toplevel)(Engines.Interactive.Dap.Make) in
           (module E)
        | "automatic" ->
          let module E = Engines.Automatic.Make(Toplevel) in
          (module E)
        | x -> Exceptions.panic "unknown engine '%s'" x
        : Engines.Engine_sig.ENGINE with type t = Domain.t
      )
    in

    begin try
      let flow = Engine.init prog in
      let stmt = mk_stmt (S_program (prog, args)) prog.prog_range in
      let res =
        try Engine.analyze stmt flow
        with Toplevel.SysBreak flow ->
          let () = Debug.warn "Early termination, hooks will yield partial information only" in
          let () = Hook.on_finish Engine.man flow in
          raise (Toplevel.SysBreak flow)
      in
      let t = Timing.stop t in
      Hook.on_finish Engine.man res;
      Output.Factory.report Engine.man res ~time:t ~files
    with Exit -> exit 1
       | e ->
         let t = try Timing.stop t with _ -> 0. in
         Output.Factory.panic ~btrace:(Printexc.get_backtrace()) e ~time:t ~files (fun () ->
             let front = find_language_frontend abstraction.language in 
             front.on_panic e files t)
  end
  with
  | Exit ->
    exit 1
  | e ->
    let t = try Timing.stop t with _ -> 0. in
    Output.Factory.panic ~btrace:(Printexc.get_backtrace()) e ~time:t ~files (fun () -> ())


let run () =
  exit @@ parse_options Sys.argv analyze_files ()
