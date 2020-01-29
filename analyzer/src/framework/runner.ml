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

open Ast.All
open Core
open Config.Options

(** {2 Command-line options} *)
(** ************************ *)

let opt_interactive = ref false

let () =
  register_builtin_option {
    key = "-interactive";
    category = "Debugging";
    doc = " start the analysis in interactive mode";
    spec = ArgExt.Set opt_interactive;
    default = "false";
  }


(** Parse command line arguments and apply [f] on the list of target
   source files *)
let parse_options f () =
  let files = ref [] in
  let args  = ref None in
  ArgExt.parse (Config.Options.to_arg ())
               (fun filename -> files := filename :: !files)
               (fun rest -> args := Some rest)
               "Modular Open Platform for Static Analysis"
               Config.Options.help;
  f !files !args


(** {2 Parsing} *)
(** *********** *)

(** Call the appropriate frontend to parse the input sources *)
let parse_program lang files =
  try
    let front = find_language_frontend lang in
    front.parse files
  with Not_found ->
    Exceptions.panic "No front-end found for language %s" lang



(** {2 Entry points} *)
(** **************** *)

let analyze_files (files:string list) (args:string list option) : int =
  let t = Timing.start () in
  try
    let lang, domain = Config.Parser.parse !Config.Parser.opt_config in

    let prog = parse_program lang files in

    (* Top layer analyzer *)
    let module Domain = (val domain) in
    let module Abstraction = Abstraction.Make(Domain) in
    let module Engine =
      (val
        if !opt_interactive
        then
          let module E = Engines.Interactive.Make(Abstraction) in
          (module E)
        else
          let module E = Engines.Automatic.Make(Abstraction) in
          (module E)
        : Engines.Engine.ENGINE with type t = Domain.t
      )
    in

    let flow = Engine.init prog in
    let stmt = mk_stmt (S_program (prog, args)) prog.prog_range in
    let res = Engine.exec stmt flow in
    let t = Timing.stop t in
    Hook.on_finish Engine.man res;
    Output.Factory.report Engine.man res t files
  with e ->
    let t = try Timing.stop t with Not_found -> 0. in
    Output.Factory.panic ~btrace:(Printexc.get_backtrace()) e t files


let run () =
  exit @@ parse_options analyze_files ()
