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

(** Entry point of the analyzer. *)

open Framework
open Framework.Ast.All
open Framework.Core
open Framework.Config.Options

(** {2 Command-line options} *)
(** ************************ *)

let opt_interactive = ref false

let () =
  register_builtin_option {
    key = "-interactive";
    doc = " start the analysis in interactive mode";
    spec = Arg.Set opt_interactive;
    default = "false";
  }


(** Parse command line arguments and apply [f] on the list of target
   source files *)
let parse_options f () =
  let files = ref [] in
  let n = Array.length Sys.argv in
  let return_value = ref 0 in
  Arg.parse (Config.Options.to_arg ()) (fun filename ->
      files := filename :: !files;
      if !Arg.current = n - 1 then
        return_value := !return_value * 10 + (f !files)
    ) "Modular Open Platform for Static Analysis";
  !return_value


(** {2 Parsing} *)
(** *********** *)

(** Call the appropriate frontend to parse the input sources *)
let parse_program lang files =
  Framework.Core.Debug_tree.phase "parsing";
  match lang with
  | "universal" -> Lang.Universal.Frontend.parse_program files
  | "c" -> Lang.C.Frontend.parse_program files
  | "python" -> Lang.Python.Frontend.parse_program files
  | _ -> Exceptions.panic "Unknown language"


(** {2 Entry point} *)
(** *************** *)

let () =
  exit @@ parse_options (fun files ->
      try
        let lang, domain = Config.Parser.parse () in

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
            : Framework.Engines.Engine.ENGINE with type t = Domain.t
          )
        in

        let t = Timing.start () in

        Debug_tree.phase "computing initial environments";
        let flow = Engine.init prog in

        Debug_tree.phase "starting the analysis";
        let stmt = mk_stmt (S_program prog) prog.prog_range in
        let res = Engine.exec stmt flow in
        let t = Timing.stop t in

        Output.Factory.render Engine.man res t files

      with
        e -> Output.Factory.panic ~btrace:(Printexc.get_backtrace()) e files; 2
    ) ()
