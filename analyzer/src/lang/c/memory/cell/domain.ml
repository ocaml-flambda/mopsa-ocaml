(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Cell abstraction of low-level C memory operations. *)

open Framework.Flow
open Framework.Domains
open Framework.Domains.Global
open Framework.Manager
open Framework.Ast
open Universal.Ast
open Ast
open Typ

let name = "c.memory.cell"
let debug fmt = Debug.debug ~channel:name fmt

module Make(SubNum: Global.DOMAIN) =
struct

  module Numeric = Numeric.Make(SubNum)

  module Composer = Composers.Iter.Make(Pointer)(Numeric)

  include Composer

  let cellify_expr num e =
    match ekind e with
    | E_var v when is_c_int_type v.vtyp ->
      let c = Numeric.var_to_cell v num in
      {e with ekind = E_c_cell( c )}

    | E_var v when is_c_pointer v.vtyp ->
      {e with ekind = E_c_cell( {v = v; o = 0; t = e |> etyp} )}

    | _ -> e

  let exec stmt man ctx flow =
    let num = get_domain_cur (Composer.tail_man man) flow in
    let stmt' = Framework.Visitor.map_stmt (cellify_expr num) (fun s -> s) stmt in
    Composer.exec stmt' man ctx flow

  let eval exp man ctx flow  =
    let num = get_domain_cur (Composer.tail_man man) flow in
    let exp' = Framework.Visitor.map_expr (cellify_expr num) (fun s -> s) exp in
    Composer.eval exp' man ctx flow

  end

let setup () =
  register_functor name (module Make)
