(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of C function calls *)

open Framework.Essentials
open Ast


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
    export = [Zone.Z_c, Zone.Z_c_scalar];
    import = [
      Zone.Z_c, Zone.Z_c_points_to_fun;
      Universal.Zone.Z_u, any_zone
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
        man.eval ~zone:(Zone.Z_c, Zone.Z_c_points_to_fun) f flow |> Eval.bind @@ fun f flow ->
        match ekind f with
        | E_c_builtin_function(name) ->
          let () = debug "builtin : %a" pp_expr f in
          let exp' = {exp with ekind = E_c_builtin_call(name, args)} in
          man.eval ~zone:(Zone.Z_c, Zone.Z_c_scalar) exp' flow

        | E_c_function fundec ->
          let body = get_c_fun_body_panic fundec in
          debug "call to %a, body @[%a@]" pp_var fundec.c_func_var pp_stmt body;
          let open Universal.Ast in
          let fundec' = {
            fun_name = get_var_uniq_name fundec.c_func_var;
            fun_parameters = fundec.c_func_parameters;
            fun_locvars = List.map (fun (v, _, _) -> v) fundec.c_func_local_vars;
            fun_body = {skind = S_c_goto_stab (body); srange = srange body};
            fun_return_type = Some fundec.c_func_return;
          }
          in
          let exp' = mk_call fundec' args exp.erange in
          (* Universal will evaluate the call into a temporary variable containing the returned value *)
          man.eval ~zone:(Universal.Zone.Z_u, any_zone) exp' flow

        | _ -> assert false
      end |>
      OptionExt.return

    | E_c_cast(e, _)
      when (exp |> etyp |> is_c_pointer_type)
        && (exp |> etyp |> under_pointer_type |> is_c_function_type)
      ->
      let t' = exp |> etyp |> under_pointer_type in
      man.eval ~zone:(Zone.Z_c, Zone.Z_c_scalar) {e with etyp = t'} flow |>
      OptionExt.return

    | _ -> None



  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
