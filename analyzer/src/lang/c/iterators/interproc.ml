(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of C function calls *)

open Framework.Essentials
open Memory.Common.Points_to
open Ast
open Zone


module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_interproc : unit domain

  let id = D_c_interproc
  let name = "c.iterators.interproc"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_interproc -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {export = []; import = []}

  let eval_interface = {
    export = [Z_c, Z_c_low_level];
    import = [
      Z_c, Z_c_points_to;
      Universal.Zone.Z_u, any_zone;
      Stubs.Zone.Z_stubs, Z_any
    ]
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
    | E_c_call(f, args) ->
      begin
        man.eval ~zone:(Zone.Z_c, Z_c_points_to) f flow |>
        Eval.bind @@ fun f flow ->

        match ekind f with
        | E_c_points_to (P_fun f) when Libs.Mopsa.is_builtin_function f.c_func_var.vname ->
          let exp' = {exp with ekind = E_c_builtin_call(f.c_func_var.vname, args)} in
          man.eval ~zone:(Zone.Z_c, Zone.Z_c_low_level) exp' flow

        | E_c_points_to (P_fun ({c_func_body = Some body; c_func_stub = None} as fundec)) ->
          let open Universal.Ast in
          let ret_var = mk_tmp ~vtyp:fundec.c_func_return () in
          let fundec' = {
            fun_name = get_var_uniq_name fundec.c_func_var;
            fun_parameters = fundec.c_func_parameters;
            fun_locvars = List.map (fun (v, _, _) -> v) fundec.c_func_local_vars;
            fun_body = {skind = S_c_goto_stab (body); srange = srange body};
            fun_return_type = Some fundec.c_func_return;
            fun_return_var = ret_var;
            fun_range = fundec.c_func_range;
          }
          in
          let exp' = mk_call fundec' args exp.erange in
          (* Universal will evaluate the call into a temporary variable containing the returned value *)
          man.eval ~zone:(Universal.Zone.Z_u, any_zone) exp' flow

        | E_c_points_to (P_fun {c_func_stub = Some stub}) ->
          let exp' = Stubs.Ast.mk_stub_call stub.content args exp.erange in
          man.eval ~zone:(Stubs.Zone.Z_stubs, any_zone) exp' flow

        | E_c_points_to (P_fun {c_func_body = None; c_func_var}) ->
          panic_at (erange exp) "no implementation found for function %a" pp_var c_func_var

        | _ -> assert false
      end |>
      OptionExt.return


    | _ -> None



  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
