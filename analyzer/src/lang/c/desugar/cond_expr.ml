(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Desugar conditional expressions `cond?e1:e2`. *)

open Mopsa
open Ast
open Zone

(** {2 Domain definition} *)
(** ===================== *)

module Domain : Framework.Domains.Stateless.S =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_desugar_cond_expr : unit domain
  let id = D_c_desugar_cond_expr
  let name = "c.desugar.cond_expr"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_desugar_cond_expr -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {export = []; import = []}
  let eval_interface = {export = [Z_c, Z_c_low_level]; import = [Z_c, Z_c_low_level]}


  (** Initialization *)
  (** ============== *)

  let init _ _ _ =
    None


  (** Post-condition computation *)
  (** ========================== *)

  let exec zone stmt man flow = None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow  =
    match ekind exp with
    | E_c_conditional(cond, e1, e2) ->
      Eval.assume cond
        ~fthen:(fun flow ->
            man.eval ~zone:(Z_c, Z_c_low_level) e1 flow
          )
        ~felse:(fun flow ->
            man.eval ~zone:(Z_c, Z_c_low_level) e2 flow
          )
        man flow |>
      Eval.return

    | _ -> None


  (** Query handler *)
  (** ============= *)

  let ask _ _ _  = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
