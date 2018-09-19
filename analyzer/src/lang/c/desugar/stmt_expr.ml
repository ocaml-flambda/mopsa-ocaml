(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Interpreter of expressions containing statements. *)

open Framework.Essentials
open Universal.Ast
open Ast

module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_desugar_stmt_expr : unit domain
  let id = D_c_desugar_stmt_expr
  let name = "c.desugar.stmt_expr"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_desugar_stmt_expr -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {
    export = [];
    import = [Zone.Z_c]
  }

  let eval_interface = {
    export = [Zone.Z_c, Zone.Z_c_scalar];
    import = [Zone.Z_c, Zone.Z_c_scalar]
  }

  (** Initialization *)
  (** ============== *)

  let init _ _ _ = None


  (** Evaluations *)
  (** *********** *)

  let eval zone exp man flow =
    match ekind exp with
    | E_c_assign(lval, rval) ->
      begin
        man.eval rval ~zone:(Zone.Z_c, Zone.Z_c_scalar) flow |>
        Eval.bind @@ fun rval flow ->
        let flow = man.exec (mk_assign lval rval exp.erange) flow in
        Eval.singleton rval flow
      end
      |> Option.return

    | E_c_statement {skind = S_block l} ->
      begin
        match List.rev l with
        | {skind = S_expression e}::q ->
          let q' = List.rev q in
          let stmt' = mk_block q' (tag_range (erange exp) "block'") in
          let flow' = man.exec stmt' flow in
          man.eval ~zone:(Zone.Z_c, Zone.Z_c_scalar) e flow' |>
          Option.return

        | _ ->
          Debug.fail "E_c_statement %a" pp_expr exp
      end

    | E_c_statement {skind = S_expression e} ->
      man.eval ~zone:(Zone.Z_c, Zone.Z_c_scalar) e flow |>
      Option.return

    | _ -> None


  (** Post-conditions *)
  (** *************** *)

  let exec zone stmt man flow = None


  (** Query handler *)
  (** ************* *)

  let ask _ _ _ = None

end


(*==========================================================================*)
(**                            {2 Setup}                                    *)
(*==========================================================================*)

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
