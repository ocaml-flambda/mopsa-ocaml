(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Decomposition of C arithmetic expressions *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Exec
open Framework.Lattice
open Framework.Ast
open Universal.Ast
open Universal.Utils
open Ast

let name = "c.memory.decomposer"
let debug fmt = Debug.debug ~channel:name fmt
(** Abstract domain. *)

module Domain =
struct

  (*==========================================================================*)
  (**                        {2 Transfer functions}                           *)
  (*==========================================================================*)

  let init man ctx prog flow = ctx, flow


  let rec eval man ctx exp flow =
    match ekind exp with
    | E_binop(op, e, e') ->
      eval_list [e; e'] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          match el with
          | [e; e'] ->
            oeval_singleton
              (Some {exp with ekind = E_binop(op, e, e')},
               flow,
               []
              )
          | _ -> assert false
        )

    | E_unop(op, e) ->
      eval_list [e] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          match el with
          | [e] ->
            oeval_singleton
              (Some {exp with ekind = E_unop(op, e)},
               flow,
               []
              )
          | _ -> assert false
        )
    | E_c_cast(e, m) ->
      eval_list [e] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          match el with
          | [e] ->
            oeval_singleton
              (Some {exp with ekind = E_c_cast(e, m)},
               flow,
               []
              )
          | _ -> assert false
        )
    | _ -> None

  let exec man ctx stmt flow =
    None

  let ask _ _ _ _ =
    None

end


(*==========================================================================*)
(**                            {2 Setup}                                    *)
(*==========================================================================*)

let setup () =
  register_domain name (module Domain)
