(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Initialize variable with stubs. *)

open Mopsa
open Ast
open Zone

(** {2 Domain definition} *)
(** ===================== *)

module Domain : Framework.Domains.Stateless.S =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_desugar_stub_init : unit domain
  let id = D_c_desugar_stub_init
  let name = "c.desugar.stub_init"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_desugar_stub_init -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {export = [Z_c]; import = []}
  let eval_interface = {export = []; import = []}

  (** Initialization *)
  (** ============== *)

  let init _ _ _ =
    None


  let exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration ({ vkind = V_c {var_init = Some (C_init_stub stub); var_range} } as v)->
      let stmt' = Stubs.Ast.mk_stub_init v stub var_range in
      man.exec stmt' flow |>
      Post.return

    | _ -> None

  let eval _ _ _ _  = None

  let ask _ _ _  = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
