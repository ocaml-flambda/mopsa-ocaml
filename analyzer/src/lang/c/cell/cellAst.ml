(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

open Framework.Ast
open Framework.Visitor

(** Ast expression extensions *)
type expr_kind +=
  | Cell of Typ.Cell.t
  | Pointer of Typ.PSL.t * expr

open Format
(** Installation procedure *)
let setup () =
  (** Pretty-printer *)
  Framework.Pp.register_pp_expr (fun default fmt expr ->
      match ekind expr with
      | Cell c -> Typ.Cell.print fmt c
      | Pointer(v, e) ->
        Format.fprintf fmt "(%a,%a)" Typ.PSL.print v Framework.Pp.pp_expr e
      | _ -> default fmt expr
    );
  (** Visitors *)
  Framework.Visitor.register_expr_visitor ( fun default exp ->
      match ekind exp with
      | Cell c -> leaf exp
      | Pointer(v,e) ->
        {exprs = [e] ; stmts = []},
        (fun parts -> {exp with ekind = Pointer(v,List.hd parts.exprs)})
      | _ -> default exp
    )
