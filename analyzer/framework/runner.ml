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


 (** {2 Command-line options} *)
(** ************************ *)

let opt_interactive = ref "automatic"

let () =
  register_builtin_option {
    key = "-engine";
    category = "Debugging";
    doc = "selects analysis mode";
    spec =  ArgExt.Symbol (
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
let parse_options f () =
  let files = ref [] in
  let args  = ref None in
  ArgExt.parse (Params.Options.get_options ())
               (fun filename -> files := filename :: !files)
               (fun rest -> args := Some rest)
               "Modular Open Platform for Static Analysis"
               Params.Options.help;
  f !files !args


(** {2 Parsing} *)
(** *********** *)

(** Call the appropriate frontend to parse the input sources *)
let parse_program lang files =
  let front =
    try find_language_frontend lang
    with Not_found -> Exceptions.panic "No front-end found for language %s" lang
  in
  match front.parse files with
  | exception (Failure s) ->
    Format.eprintf "Parsing error: %s\n" s;
    exit 3
  | exception (Exceptions.Panic (s, _)) ->
    Format.eprintf "%s\n" s;
    exit 4
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
           let module E = Engines.Interactive.Engine.Make(Toplevel) in
           (module E)
        | "dap"   ->
           let module E = Engines.Dap.Make(Toplevel) in
           (module E)
        | "automatic" ->
          let module E = Engines.Automatic.Make(Toplevel) in
          (module E)
        | x -> Exceptions.panic "unknown engine '%s'" x
        : Engines.Engine_sig.ENGINE with type t = Domain.t
      )
    in

    let flow = Engine.init prog in
    let stmt = mk_stmt (S_program (prog, args)) prog.prog_range in
    let res = Engine.exec stmt flow |> post_to_flow Engine.man in
    let t = Timing.stop t in
    Hook.on_finish Engine.man res;
    Output.Factory.report Engine.man res ~time:t ~files
  with e ->
    let t = try Timing.stop t with Not_found -> 0. in
    Output.Factory.panic ~btrace:(Printexc.get_backtrace()) e ~time:t ~files


let run () =
  exit @@ parse_options analyze_files ()
