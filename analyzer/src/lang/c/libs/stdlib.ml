(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of standard C library *)

open Framework.Flow
open Framework.Domains
open Framework.Manager
open Framework.Lattice
open Framework.Domains.Stateless
open Framework.Eval
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.libs.stdlib"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let is_builtin_function = function
    | "malloc" -> true
    | _ -> false

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init man ctx prog flow = ctx, flow

  let exec man ctx stmt flow = None

  let eval man ctx exp flow =
    match ekind exp with
    | E_c_function(f) when is_builtin_function f.c_func_var.vname ->
      debug "builtin function";
      let exp' = mk_expr (E_c_builtin_function f.c_func_var.vname) ~etyp:T_c_builtin_fn exp.erange in
      oeval_singleton (Some exp', flow, [])

    | E_c_call({ekind = E_c_builtin_function "malloc"}, [size]) ->
      Framework.Exceptions.panic "malloc not supported"

    | _ -> None

  let ask _ _ _ _  = None

  end

let setup () =
  Stateless.register_domain name (module Domain)
