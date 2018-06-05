(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Non-relational abstract environment of Python values. *)

open Framework.Domains
open Framework.Domains.Reduce.Domain
open Framework.Flow
open Framework.Manager
open Framework.Query
open Framework.Eval
open Framework.Exec
open Framework.Ast
open Framework.Utils
open Universal.Ast
open Ast
open Addr
open Addr_env

let name = "python.memory.value_env"
let debug fmt = Debug.debug ~channel:name fmt


module Domain =
struct

  include Universal.Nonrel.Domain.Make(Value)

  let print fmt m =
    Format.fprintf fmt "values: @[%a@]@\n" print m

  let init man ctx prog flow = ctx, set_domain_cur top man flow

  let exec man ctx stmt flow =
    match skind stmt with
    (* S⟦ v = e ⟧ *)
    | S_assign({ekind = E_var {vkind = V_py_value_var _}}, e, mode) ->
      debug "assign value";
      exec man ctx stmt flow

    | S_assume(e) when not (is_py_expr e) ->
      exec man ctx stmt flow

    | _ -> None


end

let setup () =
  register_domain name (module Domain);
  ()
