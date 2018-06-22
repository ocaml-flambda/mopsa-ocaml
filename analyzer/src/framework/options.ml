(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Command-line options. *)

(**
  List of command-line options.
  It can be extended by modules to add their own options.
*)
let spec : (Arg.key * Arg.spec * Arg.doc) list ref = ref []

(** Register a command-line option. *)
let register (opt) =
  spec := opt :: !spec


type common_options = {
  mutable lang : string;
  mutable config : string;
  mutable unit_test_mode : bool;
  mutable stubs : string list;
  mutable reduce_iter : int;
  mutable output_mode : string;
  mutable cache_size : int;
}

let common_options = {
  lang = "";
  config = "";
  unit_test_mode = false;
  stubs = [];
  reduce_iter = 1;
  output_mode = "verbose";
  cache_size = 10;
}

let setup () =
  register (
    "-output-mode",
    Arg.String(fun s -> common_options.output_mode <- s),
    " output mode (default: verbose)"
  );
  register (
    "-lang",
    Arg.String(fun f -> common_options.lang <- f),
    " target language"
  );
  register (
    "-config",
    Arg.String(fun f -> common_options.config <- f),
    " path to the domain configuration file"
  );
  register (
    "-test",
    Arg.Bool (fun m -> common_options.unit_test_mode <- m),
    " unit test mode (default: false)"
  );
  register (
    "-debug",
    Arg.String(fun f ->
        Debug.parse f
      ),
    " debug channels"
  );
  register (
    "-color",
    Arg.Bool(fun f -> Debug.print_color := f),
    " print debug messages in color (default: true)"
  );
  register (
    "-stub",
    Arg.String(fun f -> common_options.stubs <- f :: common_options.stubs),
    " path to a stub directory"
  );
  register (
    "-reduce-iter",
    Arg.Int(fun n -> common_options.reduce_iter <- n),
    " maximal number of iterations of post-condition reductions"
  );
  register (
    "-cache-size",
    Arg.Int(fun n -> common_options.cache_size <- n),
    " size of exec/eval cache"
  );

  ()
