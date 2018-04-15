(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Interpreter of && and || boolean operators. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Utils
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.desugar.andor"
let debug fmt = Debug.debug ~channel:name fmt


(** Abstract domain. *)
module Domain =
struct

  (*==========================================================================*)
  (**                        {2 Transfer functions}                           *)
  (*==========================================================================*)
  
  let init prog man ctx flow = ctx, flow

  let eval exp man ctx flow =
    match ekind exp with
    | E_binop(O_log_and, e1, e2) ->
      man.eval e1 ctx flow |>
      eval_compose
        (fun e1 flow1 ->
           Universal.Utils.assume_to_eval e1
             (fun true_flow -> Some (man.eval e2 ctx true_flow))
             (fun false_flow -> oeval_singleton (Some (mk_zero exp.erange), false_flow, []))
             man ctx flow ()
        )


    | E_binop(O_log_or, e1, e2) ->
      man.eval e1 ctx flow |>
      eval_compose
        (fun e1 flow1 ->
           Universal.Utils.assume_to_eval e1
             (fun true_flow -> oeval_singleton (Some (mk_one exp.erange), true_flow, []))
             (fun false_flow -> Some (man.eval e2 ctx false_flow))
             man ctx flow ()
        )

    | _ -> None

  let exec stmt man ctx flow = None

  let ask _ _ _ _ = None

end


(*==========================================================================*)
(**                            {2 Setup}                                    *)
(*==========================================================================*)

let setup () =
  register_domain name (module Domain)
