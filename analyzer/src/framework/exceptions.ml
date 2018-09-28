(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Common exceptions raised by domains. *)

(** Panic exceptions are raised by domains when they encounter an unsupported
    language construct.
*)
exception Panic of string

(** Panic exception with location information *)
exception PanicAt of Location.range * string


(** Raise a panic exception using a formatted string *)
let panic fmt =
  Format.kasprintf (fun str ->
      raise (Panic str)
    ) fmt

let panic_at range fmt =
  Format.kasprintf (fun str ->
      raise (PanicAt (range, str))
    ) fmt

(** Warning message *)
let warn = Debug.warn

(** Failure exception *)
let fail fmt = Debug.fail fmt
