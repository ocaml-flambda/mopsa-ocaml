(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of generators *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Lattice
open Framework.Flow
open Framework.Ast
open Framework.Pp
open Framework.Eval
open Framework.Exec
open Framework.Alarm
open Universal.Ast
open Ast
open Addr

let name = "python.flows.generators"
let debug fmt = Debug.debug ~channel:name fmt


module Domain = struct

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    (* Creation of a generator *)
    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_user func)})}, args, [])
      when func.py_func_is_generator = true
      ->
      assert false

    (* generator.__iter__(self) simply returns self if it is a generator *)
    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "generator.__iter__")})}, [self], []) ->
      begin
        match ekind self with
        | E_addr {addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "generator", _)}, _)} ->
          oeval_singleton (Some self, flow, [])

        | _ ->
          let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
        oeval_singleton (None, flow, [])
      end

    (* Retrieve the next value of a generator *)
    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "generator.__next__")})},
        [{ekind = E_addr ({addr_kind = A_py_instance(_, Some (Generator func))} as addr)}],
        []
      ) ->
      assert false

    (* Yield a value *)
    | E_py_yield e ->
      assert false

    | E_py_generator_comprehension _ ->
      Framework.Exceptions.panic "Generator comprehension not supported"

    | _ -> None

  let init man ctx prog flow = ctx, flow
  let exec man ctx stmt flow = None
  let ask man ctx query flow = None

end

let setup () =
  register_domain name (module Domain)
