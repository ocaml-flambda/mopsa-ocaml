(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Desugarisation of casts. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Ast
open Framework.Pp
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
    | E_c_cast(e, _) when (exp |> etyp |> is_c_record_type) ->
      let t' = etyp exp in
      re_eval_singleton (man.eval ctx) (Some ({e with etyp = t'}), flow, [])

    | _ -> None

  let exec man ctx stmt flow = None

  let ask _ _ _ _ = None

end


(*==========================================================================*)
(**                            {2 Setup}                                    *)
(*==========================================================================*)

let setup () =
  register_domain name (module Domain)
