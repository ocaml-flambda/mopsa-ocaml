(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Base storage of scalar values. *)

open Framework.Ast
open Universal.Ast
open Mopsa
open Ast
open Zone

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
  | S s -> Z.of_int @@ String.length s
  | b -> Exceptions.panic "[base_size]: unknown base %a" pp_base b

(** Evaluate the size of a base *)
let eval_base_size base ?(via=Z_any) range man flow =
  match base with
  | V var -> Eval.singleton (mk_z (sizeof_type var.vtyp) range ~typ:ul) flow
  | S str -> Eval.singleton (mk_int (String.length str + 1) range ~typ:ul) flow
  | A addr ->
    let mk_addr_size addr range =
      mk_expr (Stubs.Ast.E_stub_builtin_call (SIZE, mk_addr addr range)) range ~etyp:ul
    in
    man.eval ~zone:(Z_c, Z_c_scalar) ~via (mk_addr_size addr range) flow
