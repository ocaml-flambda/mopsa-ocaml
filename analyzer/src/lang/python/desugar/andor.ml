(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Handling of and/or operators. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Ast
open Universal.Ast
open Ast

let name = "python.desugar.andor"
let debug fmt = Debug.debug ~channel:name fmt


module Domain =
struct

  let init _ ctx _ flow = ctx, flow

  let eval man ctx exp flow =
    match ekind exp with
    | E_binop(O_py_and, e1, e2) ->
      man.eval ctx e1 flow |>
      eval_compose
        (fun e1 flow1 ->
           Universal.Utils.assume_to_eval e1
             (fun true_flow -> Some (man.eval ctx e2 true_flow))
             (fun false_flow -> oeval_singleton (Some e1, false_flow, []))
             man ctx flow ()
        )



    | E_binop(O_py_or, e1, e2) ->
      man.eval ctx e1 flow |>
      eval_compose
        (fun e1 flow1 ->
           Universal.Utils.assume_to_eval e1
             (fun true_flow -> oeval_singleton (Some e1, true_flow, []))
             (fun false_flow -> Some (man.eval ctx e2 false_flow))
             man ctx flow ()
        )

    | _ -> None


  let exec _ _ _ _  = None

  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
