(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Math Python library. *)

open Framework.Essentials
open Framework.Domains.Stateless
open Universal.Ast
open Ast
open Addr

let name = "python.libs.math"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                               {2 Domain }                               *)
(*==========================================================================*)


module Domain =
struct


  (*==========================================================================*)
  (**                       {2 Transfer functions }                           *)
  (*==========================================================================*)

  let init prog man ctx flow = None

  let exec_interface = Framework.Domain.{
      import = [];
      export = [];
    }

  let eval_interface = Framework.Domain.{
      import = [Zone.Z_py, Zone.Z_py_object];
      export = [Zone.Z_py, Zone.Z_py_object];
    }

  let exec zone stmt man ctx flow = None

  let eval zpath exp man ctx flow =
    let range = erange exp in
    match ekind exp with
    | E_py_call ({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "math.sqrt")}, _)}, [e], []) ->
      man.eval e ~zpath: (Zone.Z_py, Zone.Z_py_object) ctx flow |>
      Eval.bind @@ fun e flow ->
      let ev = object_of_expr e |> value_of_object in
      let exp' = mk_py_float_expr (mk_unop O_sqrt ev range) range in
      Eval.singleton (Some exp') flow

    | _ ->
      None

  let ask _ _ _ _ = None

end




(*==========================================================================*)
(**                             {2 Setup }                                  *)
(*==========================================================================*)


let setup () =
  register_domain name (module Domain)
