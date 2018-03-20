(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Intra-procedural control flow abstraction *)

open Framework.Flow
open Framework.Domains
open Framework.Domains.Stateless
open Framework.Domains.Manager
open Framework.Ast
open Ast

let name = "C.cell.toCell"

let debug fmt = Debug.debug ~channel:name fmt

module Make(Sub: Global.DOMAIN) =
struct

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init prg man subman flow = flow      
  
  let exec stmt man subman ctx gabs =
    let open Universal.Ast in
    let s' =
      Framework.Visitor.map_stmt
        (fun e ->
           match ekind e with
           | E_var v ->
             let open Typ.Cell in
             {e with ekind = CellAst.Cell( {v = v; o = 0; t = e |> etyp} )}
           | _ -> e
        )
        (fun s -> s)
        stmt
    in
    Sub.exec s' subman ctx gabs

  let eval _ _ _ _ _  = None

  let ask _ _ _ _ _ = None

  end

let setup () =
  register_stack_domain name (module Make)
