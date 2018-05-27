(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Common exceptions raised by domains and processed by the
   analyzer. *)

(*==========================================================================*)
(**                              {2 Panic}                                  *)
(*==========================================================================*)


(** Panic exception is raised by domains when they encounter an unsupported
    language construct.
*)
exception Panic of string

exception PanicAt of Ast.range * string

let panic fmt =
  Format.kasprintf (fun str ->
      raise (Panic str)
    ) fmt

let panic_at range fmt =
  Format.kasprintf (fun str ->
      raise (PanicAt (range, str))
    ) fmt


let fail fmt = Debug.fail fmt
