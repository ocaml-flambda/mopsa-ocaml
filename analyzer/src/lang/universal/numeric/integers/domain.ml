(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Non-relational abstract domain of integer variables. *)

open Framework.Query
open Framework.Ast
open Framework.Manager
open Framework.Domains.Stateful
open Framework.Utils
open Ast
open Bot

let name = "universal.numeric.integers"
let debug fmt = Debug.debug ~channel:name fmt


module Domain =
struct
  include Nonrel.Domain.Make(Value)

  let print fmt a =
    Format.fprintf fmt "int: @[%a@]@\n" print a

  let init prog man ctx flow =
    ctx, set_domain_cur top man flow

  let eval exp man ctx flow =
    match ekind exp with    
    | E_binop((O_plus | O_minus | O_mult | O_div | O_mod |
               O_eq | O_ne | O_lt | O_le | O_gt | O_ge |
               O_log_and | O_log_or | O_bit_and | O_bit_or |
               O_bit_xor | O_bit_lshift | O_bit_rshift as op), e1, e2) ->
      man_eval_list [e1; e2] man ctx flow |>
      oeval_compose
        (fun el flow ->
           let e1, e2 = match el with [e1; e2] -> e1, e2 | _ -> assert false in
           let exp' = {exp with ekind = E_binop(op, e1, e2)} in
           oeval_singleton (Some exp', flow, [])
        )

    | E_unop((O_minus | O_plus | O_log_not | O_bit_invert | O_sqrt as op), e) ->
      man.eval e ctx flow |>
      eval_compose
        (fun e flow ->
           let exp' = {exp with ekind = E_unop(op, e)} in
           oeval_singleton (Some exp', flow, [])
        )

    | _ -> None
      


end

let setup () =
  register_domain name (module Domain)
