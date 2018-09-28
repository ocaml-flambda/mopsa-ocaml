(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Command-line options. *)

(** List of registered command-line options. *)
let spec : (Arg.key * Arg.spec * Arg.doc) list ref = ref []

(** Register a command-line option. *)
let register_option (opt) =
  spec := opt :: !spec


type common_options = {
  mutable lang : string;
  mutable config : string;
  mutable stubs : string list;
  mutable cache : int;
}

let common_options = {
  lang = "";
  config = "";
  stubs = [];
  cache = 20;
}

let () =
  register_option (
    "-lang",
    Arg.String(fun f -> common_options.lang <- f),
    " target language"
  );
  register_option (
    "-config",
    Arg.String(fun f -> common_options.config <- f),
    " path to the domain configuration file"
  );
  register_option (
    "-debug",
    Arg.String(fun f ->
        Debug.parse f
      ),
    " debug channels"
  );
  register_option (
    "-color",
    Arg.Bool(fun f -> Debug.print_color := f),
    " print debug messages in color (default: true)"
  );
  register_option (
    "-stub",
    Arg.String(fun f -> common_options.stubs <- f :: common_options.stubs),
    " path to a stub directory"
  );
  register_option (
    "-cache",
    Arg.Int (fun i -> common_options.cache <- i),
    " size of the cache for exec/eval"
  );
  ()
