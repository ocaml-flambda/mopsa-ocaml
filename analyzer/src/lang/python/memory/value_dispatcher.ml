(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

open Framework.Essentials
open Universal.Ast
open Ast
open Addr
open Addr_env

module Domain = struct

  type _ domain += D_python_memory_vdispatcher : unit domain

  let id = D_python_memory_vdispatcher
  let name = "python.memory.vdispatcher"
  let identify : type a. a domain -> (unit, a) eq option = function
    | D_python_memory_vdispatcher -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  let exec_interface = {export = [Zone.Z_py_value]; import = [Universal.Zone.Z_u_string; Universal.Zone.Z_u_num]}
  let eval_interface = {export = [any_zone, any_zone]; import = []}

  let init _ _ flow = Some flow

  let eval zs exp man flow = None

  let exec z stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_assign({ekind = E_var (v, mode)}, e) ->
       let t = v.vtyp in
       begin match t with
       | T_string -> man.exec ~zone:Universal.Zone.Z_u_string stmt flow |> Post.of_flow
       | T_int -> man.exec ~zone:Universal.Zone.Z_u_num stmt flow |> Post.of_flow
       | _ -> Debug.fail "autre@\n"
       end
       |> OptionExt.return
    | _ ->  None


  let ask _ _ _ = None

end

let () = Framework.Domains.Stateless.register_domain (module Domain)
