(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Utility functions for [exec] transfer functions. *)

let return x = Some x

let fail = None

let extract none = function
  | Some x -> x
  | None -> none

let map f = function
  | None -> None
  | Some x -> Some (f x)

let merge f1 f2 f12 none a1 a2 =
  match a1, a2 with
  | Some x1, None -> f1 x1
  | None, Some x2 -> f2 x2
  | Some x1, Some x2 -> f12 x1 x2
  | None, None -> none
