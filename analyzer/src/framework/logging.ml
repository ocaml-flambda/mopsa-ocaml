(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Log executions of transfer functions *)

let phase fmt = Debug.debug ~channel:"framework.logging.phase" fmt
let parse fmt = Debug.debug ~channel:"framework.logging.parse" fmt
let reach fmt = Debug.debug ~channel:"framework.logging.reach" fmt
let exec fmt = Debug.debug ~channel:"framework.logging.exec" fmt
let pre_state fmt = Debug.debug ~channel:"framework.logging.pre" fmt
let post_state fmt = Debug.debug ~channel:"framework.logging.post" fmt
