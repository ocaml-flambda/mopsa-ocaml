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

(** Management of command-line options *)

(** Command-line option *)
type opt =
  | O_builtin of desc
  (** Built-in option *)

  | O_language of string * desc
  (** Language option *)

  | O_domain of string * desc
  (** Domain option *)

  | O_standalone of string * desc
  (** Standalone options. Several domains can share a same standalone option
      by importing it. *)

(** Descriptor of a command-line option *)
and desc = {
  key: string;
  doc: string;
  spec: Arg.spec;
  default: string;
}

(** {2 Registration} *)
(** **************** *)

(** List of registered options *)
let options : opt list ref = ref []

(** Map giving the standalone options imported by a domain *)
let imports : (string, string list) Hashtbl.t = Hashtbl.create 16

(** Register a built-in option *)
let register_builtin_option (desc:desc) =
  options := (O_builtin desc) :: !options

(** Register a language option. *)
let register_language_option (lang:string) (desc:desc) =
  options := (O_language (lang, desc)) :: !options

(** Register a domain option. *)
let register_domain_option (dom:string) (desc:desc) =
  options := (O_domain (dom, desc)) :: !options

(** Register a group option *)
let register_standalone_option (name:string) (desc:desc) =
  options := (O_standalone (name, desc)) :: !options

(** Import a standalone option into a domain *)
let import_standalone_option ~(into:string) (name:string) =
  let old = try Hashtbl.find imports into with Not_found -> [] in
  Hashtbl.replace imports into (name::old)

(** Get the imported options of a domain. *)
let find_domain_imports (dom:string) =
  try Hashtbl.find imports dom with Not_found -> []


(** {2 Filters} *)
(** *********** *)

(** Return the list of built-in options *)
let get_builtin_options () =
  List.filter (fun opt ->
      match opt with
      | O_builtin _ -> true
      | _ -> false
    ) !options

(** Return the list of registered options of a language *)
let get_language_options (lang:string) =
  List.filter (fun opt ->
      match opt with
      | O_language (l, desc) -> l = lang
      | _ -> false
    ) !options

(** Find a standalone option *)
let find_standalone_option (name:string) =
  List.find (fun opt ->
      match opt with
      | O_standalone (n, _) -> n = name
      | _ -> false
    ) !options

(** Return the list of registered options of a domain *)
let get_domain_options (dom:string) =
  (* Options registered by the domain *)
  let opt1 = List.filter (fun opt ->
      match opt with
      | O_domain (d, desc) -> d = dom
      | _ -> false
    ) !options
  in
  (* Options registered by the groups of the domain *)
  let opt2 =
    find_domain_imports dom |>
    List.map find_standalone_option
  in
  opt1 @ opt2

(** {2 Interface with Arg and Output} *)
(** ********************************* *)

let opt_to_arg opt =
  match opt with
  | O_builtin d | O_language (_, d) | O_domain (_, d) | O_standalone (_, d) ->
    d.key, d.spec, d.doc

let opt_to_output opt =
  match opt with
  | O_builtin d | O_language (_, d) | O_domain (_, d) | O_standalone (_, d) ->
    d.key, d.spec, d.doc, d.default

let to_arg () =
  List.map opt_to_arg !options


(** {2 Built-in options} *)
(** ******************** *)

(** Path to share directory *)
let () =
  register_builtin_option {
    key = "-share-dir";
    doc = " path to the share directory";
    spec = Arg.Set_string Paths.opt_share_dir;
    default = "";
  }


(** Analysis configuration *)
let () =
  register_builtin_option {
    key = "-config";
    doc = " path to the configuration file to use for the analysis";
    spec = Arg.Set_string Abstraction.opt_config;
    default = "";
  }

(** Debug channels *)
let () =
  register_builtin_option {
    key = "-debug";
    doc = " select active debug channels. (syntax: <c1>,<c2>,...,<cn> and '_' can be used as a wildcard)";
    spec = Arg.String (fun s -> Debug.parse s);
    default = "";
  };
  register_builtin_option {
    key = "-no-color";
    doc = " deactivate colors in debug messages.";
    spec = Arg.Clear Debug.print_color;
    default = "";
  }

(** List of available domains *)
let () =
  register_builtin_option {
    key = "-list";
    doc = " list available domains; if a configuration is specified, only used domains are listed";
    spec = Arg.Unit (fun () ->
        let domains = Abstraction.domains () in
        Output.Factory.list_domains domains
      );
    default = "";
  }

(** Output format *)
let () =
  register_builtin_option {
    key = "-format";
    doc = " selects the output format.";
    spec = Arg.Symbol (
        ["text"; "json"],
        (fun s ->
           match s with
           | "text" -> Output.Factory.(opt_format := F_text)
           | "json" -> Output.Factory.(opt_format := F_json)
           | _ -> assert false
        )
      );
    default = "text";
  }

(** Output stream *)
let () =
  register_builtin_option {
    key = "-output";
    doc = " redirect output to a file";
    spec = Arg.String (fun s -> Output.Factory.opt_file := Some s);
    default = "";
  }

(** Logs activation *)
let () =
  register_builtin_option {
    key = "-log";
    doc = " activate logs";
    spec = Arg.Set Engines.Logging.opt_log;
    default = "false";
  }

(** Short logs *)
let () =
  register_builtin_option {
    key = "-short-log";
    doc = " display logs without abstract states";
    spec = Arg.Set Engines.Logging.opt_short_log;
    default = "false";
  }


(** Help message *)
let help () =
  let options =
    if !Abstraction.opt_config = "" then !options
    else
      (* Get the language and domains of selected configuration *)
      let lang = Abstraction.language () in
      let domains = Abstraction.domains () in

      (* Get the options *)
      (get_builtin_options ())    @
      (get_language_options lang) @
      (List.map get_domain_options domains |> List.flatten)
  in
  let args = List.map opt_to_output options in
  Output.Factory.help args;
  exit 0

let () =
  register_builtin_option {
    key  = "-help";
    doc  = " display the list of options";
    spec = Arg.Unit help;
    default = "";
  };
  register_builtin_option {
    key  = "--help";
    doc  = " display the list of options";
    spec = Arg.Unit help;
    default = "";
  };
  register_builtin_option {
    key  = "-h";
    doc  = " display the list of options";
    spec = Arg.Unit help;
    default = "";
  }
