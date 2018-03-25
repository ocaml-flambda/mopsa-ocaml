(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of C intra-procedural control flow *)

open Framework.Flow
open Framework.Domains
open Framework.Manager
open Framework.Domains.Stateless
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.flows.intraproc"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init prg man fa = fa

  let exec stmt man ctx flow =
    match skind stmt with
    | S_c_local_declaration(v, init) ->
      let flow =
        match init with
        | None -> flow
        | Some (C_init_expr e) -> man.exec (mk_assign (mk_var v stmt.srange) e stmt.srange) ctx flow
        | Some (Ast.C_init_list (_,_)) -> assert false
        | Some (Ast.C_init_implicit _) -> assert false
      in
      Exec.return flow

    | S_c_switch _ -> assert false

    | S_c_switch_case _ -> assert false

    | S_c_switch_default _ -> assert false

    | S_c_goto _ -> assert false
  
    | _ -> None


  let eval exp man ctx flow =
    match ekind exp with
    | E_c_assign(lval, rval) ->
      Eval.compose_eval
        rval
        (fun rval flow ->
           let flow = man.exec (Universal.Ast.mk_assign lval rval exp.erange) ctx flow in
           Eval.singleton (Some rval, flow, [])
        )
        (fun flow -> Eval.singleton (None, flow, []))
        man ctx flow
    | _ -> None

  let ask _ _ _ _  = None

  end

let setup () =
  Stateless.register_domain name (module Domain)
