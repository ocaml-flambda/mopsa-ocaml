(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Inline predicates similarly to macro expansion. At the end,
    formula in the CST should not contain any predicate. 
*)

open Location
open Cst


(** {2 Inlining context} *)
(** -------------------- *)

(** Context maps giving for every predicate its inlined body formula *)
type ctx = formula MapExt.StringMap.t

let empty_context : ctx = MapExt.StringMap.empty

let add_to_context (pred:string) body ctx = MapExt.StringMap.add pred body ctx

let doit (stub:stub with_range) : stub with_range =
  bind_range stub @@ fun stub ->
  assert false
