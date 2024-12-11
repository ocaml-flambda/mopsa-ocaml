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

open Mopsa_utils
open ArgExt
module StringSet = SetExt.StringSet

 (** Command-line option *)
type opt =
  | O_builtin of arg
  (** Built-in option *)

  | O_language of string * arg
  (** Language option *)

  | O_domain of string * arg
  (** Domain option *)

  | O_shared of string * arg
  (** Shared options that can be imported by several domains. *)

(** {2 Registration} *)
(** **************** *)

(** List of registered options *)
let options : opt list ref = ref []

(** Map giving the shared options imported by a domain *)
let imports : (string (* domain *), StringSet.t (* imported options *)) Hashtbl.t = Hashtbl.create 16

(** Register a built-in option *)
let register_builtin_option (arg:arg) =
  options := (O_builtin arg) :: !options

(** Register a language option. *)
let register_language_option (lang:string) (arg:arg) =
  options := (O_language (lang, arg)) :: !options

(** Register a domain option. *)
let register_domain_option (dom:string) (arg:arg) =
  options := (O_domain (dom, arg)) :: !options

(** Register a shared option *)
let register_shared_option (name:string) (arg:arg) =
  options := (O_shared (name, arg)) :: !options

(** Import a shared option into a domain *)
let import_shared_option (name:string) (domain:string) =
  let old = try Hashtbl.find imports domain with Not_found -> StringSet.empty in
  Hashtbl.replace imports domain (StringSet.add name old)

(** Get the imported options of a domain. *)
let find_domain_imports (dom:string) =
  try Hashtbl.find imports dom with Not_found -> StringSet.empty


(** {2 Interface with Arg and Output} *)
(** ********************************* *)

let opt_to_arg opt =
  match opt with
  | O_builtin d | O_language (_, d) | O_domain (_, d) | O_shared (_, d) ->
    d

(** {2 Filters} *)
(** *********** *)

(** Return the list of options *)
let get_options () =
  List.map opt_to_arg !options

 (** Return the list of built-in options *)
let get_builtin_options () =
  List.filter (fun opt ->
      match opt with
      | O_builtin _ -> true
      | _ -> false
    ) !options |>
  List.map opt_to_arg

(** Return the list of registered options of a language *)
let get_language_options (lang:string) =
  List.filter (fun opt ->
      match opt with
      | O_language (l, arg) -> l = lang
      | _ -> false
    ) !options |>
  List.map opt_to_arg

(** Find a standalone option *)
let find_shared_option (name:string) =
  List.find (fun opt ->
      match opt with
      | O_shared (n, _) -> n = name
      | _ -> false
    ) !options

(** Return the list of registered options of a domain *)
let get_domain_options (dom:string) =
  (* Options registered by the domain *)
  let opt1 = List.filter (fun opt ->
      match opt with
      | O_domain (d, arg) -> d = dom
      | _ -> false
    ) !options
  in
  (* Options registered by the groups of the domain *)
  let opt2 =
    find_domain_imports dom |>
    StringSet.elements |>
    List.map find_shared_option
  in
  List.map opt_to_arg (opt1 @ opt2)



(** {2 Built-in options} *)
(** ******************** *)


(** Path to share directory *)
let () =
  register_builtin_option {
    key = "-share-dir";
    category = "Configuration";
    doc = " path to the share directory";
    spec = ArgExt.Set_string Paths.opt_share_dir;
    default = "";
  }


(** Analysis configuration *)
let () =
  register_builtin_option {
    key = "-config";
    category = "Configuration";
    doc = " path to the configuration file to use for the analysis";
    spec = ArgExt.Set_string Config.Parser.opt_config;
    default = "";
  }

(** Warnings *)
let () =
  register_builtin_option {
    key = "-no-warning";
    category = "Debugging";
    doc = " deactivate warning messages";
    spec = ArgExt.Clear Debug.print_warnings;
    default = "";
  }

(** Active hooks *)
let () =
  register_builtin_option {
    key = "-hook";
    category = "Configuration";
    doc = " activate a hook";
    spec = ArgExt.String (fun s ->
        try Core.Hook.activate_hook s
        with Not_found -> Exceptions.panic "hook %s not found" s
      );
    default = "";
  }


(** Size of the cache *)
let () =
  register_builtin_option {
    key = "-cache";
    category = "Configuration";
    doc = " size of the analysis cache";
    spec = ArgExt.Set_int Core.Cache.opt_cache;
    default = "5";
  }


(** Debug channels *)

(* Activate "print" channel by default *)
let () = Debug.parse "print"
let () =
  register_builtin_option {
    key = "-debug";
    category = "Debugging";
    doc = " select active debug channels. (syntax: <c1>,<c2>,...,<cn> and '_' can be used as a wildcard)";
    spec = ArgExt.String (fun s ->
        (* Always keep "print" channel *)
        Debug.parse ("print," ^ s)
      );
    default = "print";
  };
  register_builtin_option {
    key = "-no-color";
    category = "Debugging";
    doc = " deactivate colors in debug messages.";
    spec = ArgExt.Clear Debug.print_color;
    default = "";
  }

(** List of available domains *)
let () =
  register_builtin_option {
    key = "-list";
    category = "Help";
    doc = " list available domains/checks/hooks; if a configuration is specified, only used domains are listed";
    spec = ArgExt.Symbol_exit (
        ["domains"; "checks"; "hooks"; "reductions"],
        (fun selection ->
           match selection with
           | "domains" ->
             let domains =
               if !Config.Parser.opt_config = "" then
                 Config.Parser.all_domains ()
               else
                 Paths.resolve_config_file !Config.Parser.opt_config |>
                 Config.Parser.domains
             in
             List.sort_uniq compare domains |>
             Output.Factory.list_domains

           | "reductions" ->
             let reductions = Config.Parser.all_reductions () in 
             List.sort_uniq compare reductions |>
             Output.Factory.list_reductions

           | "checks" ->
             let checks =
               if !Config.Parser.opt_config = "" then
                 (* List checks of all registered domains *)
                 let domains = Config.Parser.all_domains () in
                 List.fold_left
                   (fun acc domain ->
                      try
                        let module D = (val Sig.Abstraction.Stacked.find_stacked_domain domain) in
                        D.checks @ acc
                      with Not_found ->
                      try
                        let module D = (val Sig.Abstraction.Domain.find_standard_domain domain) in
                        D.checks @ acc
                      with Not_found ->
                      try
                        let module D = (val Sig.Abstraction.Stateless.find_stateless_domain domain) in
                        D.checks @ acc
                      with Not_found -> acc
                   ) [] domains
               else
                 let abstraction = Config.Parser.(parse @@ Paths.resolve_config_file !opt_config) in
                 let domain = Config.Builder.from_json abstraction.domain in
                 let module Domain = (val domain) in
                 Domain.checks
             in
             List.sort_uniq compare checks |>
             Output.Factory.list_checks

           | "hooks" ->
             let d =
               List.map
                 (fun (h:(module Core.Hook.HOOK)) ->
                    let module H = (val h) in
                    H.name
                 ) (Core.Hook.list_hooks ())
             in
             List.sort_uniq compare d |>
             Output.Factory.list_hooks

           | _ -> assert false
        ));
    default = "";
  }

(** Output format *)
let () =
  register_builtin_option {
    key = "-format";
    category = "Output";
    doc = " selects the output format.";
    spec = ArgExt.Symbol (
        ["text"; "json"],
        (fun s ->
           match s with
           | "text" -> Output.Common.(opt_format := F_text)
           | "json" ->
              Output.Common.(opt_format := F_json);
              Debug.print_color := false
           | _ -> assert false
        )
      );
    default = "text";
  }

(** Output last flow *)
let () =
  register_builtin_option {
    key = "-lflow";
    category = "Output";
    doc = " display the last output";
    spec = ArgExt.Set Output.Common.opt_display_lastflow;
    default = "false";
  }


(** Ignore alarms when returning a value to the shell *)
let () =
  register_builtin_option {
    key = "-silent";
    category = "Output";
    doc = " do not return a non-zero value when detecting alarms";
    spec = ArgExt.Set Output.Common.opt_silent;
    default = "unset";
  }


(** Output stream *)
let () =
  register_builtin_option {
    key = "-output";
    category = "Output";
    doc = " redirect output to a file";
    spec = ArgExt.String (fun s -> Output.Common.opt_file := Some s);
    default = "";
  }


let () =
  register_builtin_option {
    key = "-show-callstacks";
    category = "Alarms";
    doc = " display the call stacks when reporting alarms in text format";
    spec = ArgExt.Set Output.Text.opt_show_callstacks;
    default = "false";
  }

let () =
  register_builtin_option {
    key = "-tw";
    category = "Output";
    doc = " set the tab width";
    spec = ArgExt.Set_int Output.Text.opt_tw;
    default = "4";
  }


let () =
  register_builtin_option {
    key = "-show-safe-checks";
    category = "Alarms";
    doc = " show safe checks when reporting alarms in text format";
    spec = ArgExt.Set Output.Common.opt_show_safe_checks;
    default = "false";
  }

(** Apply cleaners on T_cur only *)
let () =
  register_builtin_option {
    key = "-clean-cur-only";
    category = "Configuration";
    doc = " flag to apply cleaners on the current environment only";
    spec = ArgExt.Set Core.Cases.opt_clean_cur_only;
    default = "";
  }

let () = register_builtin_option {
    key = "-hash-heap-address";
    category = "Heap";
    doc = "  format heap addresses with their hash";
    spec = ArgExt.Bool (fun b -> Core.Ast.Addr.opt_hash_addr := b);
    default = "false";
  }

let () = register_builtin_option {
    key = "-working-dir";
    category = "Configuration";
    doc = " set the working directory, used when resolving relative paths";
    spec = ArgExt.String (fun s ->
        if Sys.file_exists s
        then Sys.chdir s
        else Exceptions.panic "'%s' does not exist" s
      );
    default = "";
  }

let () =
  register_builtin_option {
    key = "-marker";
    category = "Partitioning";
    doc = " enable a marker for trace partitioning";
    spec = ArgExt.String Core.Marker.enable_marker;
    default = "";
  }

(** Help message *)
let help () =
  let options =
    if !Config.Parser.opt_config = "" then
      List.map opt_to_arg !options
    else
      (* Get the language and domains of selected configuration *)
      let config = Paths.resolve_config_file !Config.Parser.opt_config in
      let lang = Config.Parser.(language config) in
      let domains = Config.Parser.(domains config) in

      (* Get the options *)
      (get_builtin_options ())    @
      (get_language_options lang) @
      (List.map get_domain_options domains |> List.flatten)
  in
  Output.Factory.help options

let () =
  register_builtin_option {
    key  = "-help";
    category = "Help";
    doc  = " display the list of options";
    spec = ArgExt.Unit_exit help;
    default = "";
  };
  register_builtin_option {
    key  = "--help";
    category = "Help";
    doc  = " display the list of options";
    spec = ArgExt.Unit_exit help;
    default = "";
  };
  register_builtin_option {
    key  = "-h";
    category = "Help";
    doc  = " display the list of options";
    spec = ArgExt.Unit_exit help;
    default = "";
  }


(** Version *)

let print_version () =
  Printf.printf "%s (%s)\n" Version.version Version.dev_version

let () =
  register_builtin_option {
    key = "-v";
    category = "Configuration";
    doc = " Mopsa version";
    spec = ArgExt.Unit_exit print_version;
    default = "";
  }
