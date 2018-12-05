(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Base storage of program variables, distinguishing between stack and heap
    storage.
*)

open Framework.Ast
open Universal.Ast
open Framework.Essentials
open Ast

(** lv base *)
type base =
  | V of var
  | A of Universal.Ast.addr
  | S of string

let pp_base fmt = function
  | V v -> pp_var fmt v
  | A a -> pp_addr fmt a
  | S s -> Format.fprintf fmt "\"%s\"" s

let compare_base b b' = match b, b' with
  | V v, V v' -> compare_var v v'
  | A a, A a' -> Universal.Ast.compare_addr a a'
  | S s, S s' -> compare s s'
  | _ -> compare b b'

let base_uid = function
  | V v -> v.vuid
  | A a -> a.addr_uid
  | S _ -> 0 (* FIXME: generate unique identifiers for strings *)

let base_size =
  function
  | V v -> sizeof_type v.vtyp
  | A {addr_kind = Libs.C_stdlib.A_c_static_malloc s} -> s
  | S s -> Z.of_int @@ String.length s
  | b -> Exceptions.panic "[base_size]: unknown base %a" pp_base b
