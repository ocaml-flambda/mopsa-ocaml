(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** {2 Panic exceptions} *)
(** =-=-=-=-=-=-=-=-=-=- *)

exception Panic of string
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


(** {2 Syntax-related exceptions *)
(** =-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

exception SyntaxError of Location.range * string
exception SyntaxErrorList of (Location.range * string) list

exception UnnamedSyntaxError of Location.range
exception UnnamedSyntaxErrorList of Location.range list

let syntax_error range fmt =
    Format.kasprintf (fun str ->
      raise (SyntaxError (range, str))
    ) fmt

let syntax_errors l =
  raise (SyntaxErrorList l)

let unnamed_syntax_error range =
    raise (UnnamedSyntaxError range)

let unnamed_syntax_errors ranges =
    raise (UnnamedSyntaxErrorList ranges)


(** {2 Warnings} *)
(** =-=-=-=-=-=- *)

let warn fmt = Debug.warn fmt

let warn_at range fmt =
  Format.kasprintf (fun str ->
      Debug.warn "in %a: %s" Location.pp_range range str
    ) fmt
