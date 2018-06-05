(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Methods of class str. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Ast
open Universal.Ast
open Ast
open Addr

let name = "python.objects.strings"

module Domain = struct

  let eval man ctx exp flow =
    let range = exp.erange in
    match ekind exp with
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.__new__")}, _)}, cls :: args, []) ->
      Framework.Exceptions.panic_at range "str.__new__ not implemented"
      
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.__len__")}, _)}, [self], []) ->
      Framework.Exceptions.panic_at range "str.__len__ not implemented"

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.__eq__")}, _)}, [self; other], []) ->
      Framework.Exceptions.panic_at range "str.__eq__ not implemented"

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.__ne__")}, _)}, [self; other], []) ->
      Framework.Exceptions.panic_at range "str.__eq__ not implemented"

    | _ -> None


  let init _ ctx _ flow = ctx, flow

  let exec man ctx stmt flow = None

  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
