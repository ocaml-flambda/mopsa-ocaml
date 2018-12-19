(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Evaluation of resource attributes *)

open Mopsa
open Universal.Ast
open Stubs.Ast
open Mm.Common.Points_to
open Ast
open Zone


module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_resources_attribute : unit domain

  let id = D_c_resources_attribute
  let name = "c.resources.attribute"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_resources_attribute -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {export = []; import = []}

  let eval_interface = {
    export = [Z_c, Z_c_low_level];
    import = [Z_c, Z_c_points_to]
  }


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow = None

  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow  = None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow =
    match ekind exp with
    | E_stub_attribute({ ekind = E_addr _ }, _) ->
      None

    | E_stub_attribute(p, attr) ->
      man.eval ~zone:(Z_c, Z_c_points_to) p flow |>
      Eval.bind_return @@ fun pt flow ->

      begin match ekind pt with
        | E_c_points_to (P_block (A addr, _)) ->
          man.eval { exp with ekind = E_stub_attribute(mk_addr addr exp.erange, attr) } flow

        | E_c_points_to P_top ->
          (* When the resource is not assigned yet, can we just return an interval ? *)
          let l, u = rangeof exp.etyp in
          Eval.singleton (mk_z_interval l u ~typ:exp.etyp exp.erange) flow

        | _ -> assert false
      end


    | _ -> None

  let ask _ _ _ = None

end

let () =
    Framework.Domains.Stateless.register_domain (module Domain)
