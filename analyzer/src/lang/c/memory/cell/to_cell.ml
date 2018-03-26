(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Var-to-cell functor *)

open Framework.Flow
open Framework.Domains
open Framework.Domains.Global
open Framework.Manager
open Framework.Ast
open Universal.Ast
open Ast
open Typ

let name = "c.memory.cell.to_cell"
let debug fmt = Debug.debug ~channel:name fmt

module Make(Sub: Global.DOMAIN) =
struct

  (*==========================================================================*)
                        (** {2 Lattice structure} *)
  (*==========================================================================*)

  include Sub

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let cellify_expr e =
    match ekind e with
    | E_var v when is_c_type v.vtyp ->
      {e with ekind = E_c_cell( {v = v; o = 0; t = e |> etyp} )}
    | _ -> e

  let exec stmt man ctx flow =
    let stmt' = Framework.Visitor.map_stmt
        (fun e -> cellify_expr e)
        (fun s -> s)
        stmt
    in
    Sub.exec stmt' man ctx flow

  let eval exp man ctx flow  =
    let exp' = Framework.Visitor.map_expr
        (fun e -> cellify_expr e)
        (fun s -> s)
        exp
    in
    Sub.eval exp' man ctx flow

  let ask query man ctx flow =
    Sub.ask query man ctx flow

  end

let setup () =
  register_functor name (module Make)
