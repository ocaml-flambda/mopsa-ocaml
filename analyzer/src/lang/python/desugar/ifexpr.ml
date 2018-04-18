(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Transformation of conditional expressions. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Ast
open Framework.Eval
open Universal.Ast
open Ast

let name = "python.desugar.ifexpr"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    | E_py_if(test, body, orelse) ->
      let tmp = mktmp () in
      let flow = man.exec ctx
          (mk_if
             test
             (mk_assign (mk_var tmp (tag_range range "true branch lval")) body (tag_range range "true branch assign"))
             (mk_assign (mk_var tmp (tag_range range "false branch lval")) orelse (tag_range range "false branch assign"))
             range
          ) flow
      in
      let exp' = {exp with ekind = E_var tmp} in
      re_eval_singleton (man.eval ctx) (Some exp', flow, [mk_remove_var tmp (tag_range range "cleaner")])

    | _ -> None

  let init _ ctx _ flow = ctx, flow
  let exec _ _ _ _ = None
  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
