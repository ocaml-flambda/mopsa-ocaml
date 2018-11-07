(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Methods of class str. *)

open Framework.Essentials
open Universal.Ast
open Ast
open Addr
open Addr_env

module Domain = struct
  type _ domain += D_python_memory_string : unit domain

  let id = D_python_memory_string
  let name = "python.memory.string"
  let identify : type a. a domain -> (unit, a) eq option = function
    | D_python_memory_string -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  let exec_interface = {export = []; import = []}
  let eval_interface = {export = [Zone.Z_py_value, any_zone]; import = []}

  let init _ _ flow = Some flow

  let mk_py_string s range =
    let mk_py_string_expr e range =
      let addr = {addr_kind = A_py_instance (find_builtin "str", None);
                  addr_uid = Universal.Heap.Pool.get_fresh ()} in
      debug "addr is %a (uid is %d)@\n" pp_addr addr addr.addr_uid;
      mk_py_object (addr, e) range
    in
    mk_py_string_expr (Universal.Ast.mk_string s range) range

  let eval zs exp man flow =
    let range = exp.erange in
    match ekind exp with
    (* 𝔼⟦ str.__new__(cls, arg) ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.__new__")}, _)}, cls :: args, []) ->
      Framework.Exceptions.panic_at range "str.__new__ not implemented"

    (* 𝔼⟦ s | s ∈ String ⟧ *)
    | E_constant (C_string s) ->
       Eval.singleton (mk_py_string s range) flow
       |> OptionExt.return

    (* 𝔼⟦ str.__len__(self) ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.__len__")}, _)}, [self], []) ->
      Framework.Exceptions.panic_at range "str.__len__ not implemented"

    (* 𝔼⟦ str.__eq__(self, other) ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.__eq__")}, _)}, [self; other], []) ->
      Framework.Exceptions.panic_at range "str.__eq__ not implemented"

    (* 𝔼⟦ str.__ne__(self, other) ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "str.__ne__")}, _)}, [self; other], []) ->
      Framework.Exceptions.panic_at range "str.__eq__ not implemented"

    | _ -> None


  let exec _ _ _ _ = None

  let ask _ _ _ = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
