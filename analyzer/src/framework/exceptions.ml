(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Different kinds of exceptions raised by domains and processed by
    the analyzer. *)

(*==========================================================================*)
                        (** {2 Panic} *)
(*==========================================================================*)


(** Panic exception is raised by domains when they encounter an unsupported
    language construct.
*)
exception Panic of string

let panic fmt =
  Format.kasprintf (fun str ->
      raise (Panic str)
    ) fmt


let fail fmt = Debug.fail fmt
