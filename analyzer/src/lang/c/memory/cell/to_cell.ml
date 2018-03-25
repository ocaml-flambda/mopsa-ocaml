(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Var-to-Cell functor *)

open Framework.Flow
open Framework.Domains
open Framework.Domains.Global
open Framework.Manager
open Framework.Ast
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
  
  let exec stmt man ctx gabs =
    let open Universal.Ast in
    let s' =
      Framework.Visitor.map_stmt
        (fun e ->
           match ekind e with
           | E_var v ->
             {e with ekind = E_c_cell( {v = v; o = 0; t = e |> etyp} )}
           | _ -> e
        )
        (fun s -> s)
        stmt
    in
    Sub.exec s' man ctx gabs

  let eval _ _ _ _  = assert false

  let ask _ _ _ _ = assert false

  end

let setup () =
  register_functor name (module Make)
