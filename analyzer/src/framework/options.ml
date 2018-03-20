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
