(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


open Framework.Essentials
open Universal.Ast
open Ast

(** {2 Domain definition} *)
(** ===================== *)

module Domain : Framework.Domains.Stateless.S =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_desugar_addre_deref : unit domain
  let id = D_c_desugar_addre_deref
  let name = "c.desugar.addr_deref"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_desugar_addre_deref -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {
    export = [];
    import = []
  }

  let eval_interface = {
    export = [Zone.Z_c, Zone.Z_c_scalar];
    import = [Zone.Z_c, Zone.Z_c_scalar]
  }

  (** Initialization *)
  (** ============== *)

  let init _ _ _ =
    None

  let exec _ _ _ _ = None

  let rec eval zone exp man flow =
    match ekind exp with
    (* 𝔼⟦ &*x ⟧ = x *)
    | E_c_address_of { ekind = E_c_deref x } ->
      man.eval ~zone x flow |>
      Eval.return

    (* 𝔼⟦ *&x ⟧ = x *)
    | E_c_deref { ekind = E_c_address_of x } ->
      man.eval ~zone x flow |>
      Eval.return

    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
