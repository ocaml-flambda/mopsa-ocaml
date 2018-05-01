(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Interpreter of assignment expressions. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.desugar.cast"
let debug fmt = Debug.debug ~channel:name fmt


(** Abstract domain. *)
module Domain =
struct

  (*==========================================================================*)
  (**                        {2 Transfer functions}                           *)
  (*==========================================================================*)

  let init man ctx prog flow = ctx, flow

  let eval man ctx exp flow =
    match ekind exp with
    | E_c_cast(e, _) when
        not (exp |> etyp |> is_c_int_type) &&
        (not (exp |> etyp |> is_c_pointer_type) || e |> etyp |> is_c_function_type)
      ->
      man.eval ctx e flow |>
      eval_compose (fun e flow ->
          oeval_singleton (Some ({exp with ekind = ekind e}), flow, []) 
        )

    | _ -> None

  let exec man ctx stmt flow = None

  let ask _ _ _ _ = None

end


(*==========================================================================*)
(**                            {2 Setup}                                    *)
(*==========================================================================*)

let setup () =
  register_domain name (module Domain)
