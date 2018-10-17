(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Python data model for callables. *)

open Framework.Essentials
open Universal.Ast
open Universal.Ast
open Ast
open Addr

module Domain =
  struct

    type  _ domain += D_python_data_model_callable : unit domain

    let id = D_python_data_model_callable
    let name = "python.data_model.callable"
    let identify : type a. a domain -> (unit, a) eq option = function
      | D_python_data_model_callable -> Some Eq
      | _ -> None

    let debug fmt = Debug.debug ~channel:name fmt

    let exec_interface = {export = []; import = []}
    let eval_interface = {export = [any_zone, any_zone]; import = []}

    let init _ _ flow = Some flow

    let exec _ _ _ _ = None

    let eval zs exp man flow =
      let range = erange exp in
      match ekind exp with
      (* FIXME *)
      | E_py_call({ekind = E_py_object _}, _, _) -> None
      (*   (\* Calls to addresses should be captured by other domains. If we
       *      are here, then we are missing an implementation of the
       *      function *\)
       *   Framework.Exceptions.panic_at range "call %a can not be resolved" pp_expr exp *)

      | E_py_call(f, args, _) ->
         debug "Calling %a from %a" pp_expr exp pp_range_verbose exp.erange;
         man.eval f flow |>
           Eval.bind
             (fun f flow ->
               debug "f is now %a" pp_expr f;
               match ekind f with
               (* Calls on non-object variables and constants is not allowed *)
               | E_var _ | E_constant _ ->
                  let stmt = Utils.mk_builtin_raise "TypeError" range in
                  let flow = man.exec stmt flow in
                  Eval.empty_singleton flow

               (* (\* Calls on instances is OK if __call__ is defined *\)
                * | E_py_object ({addr_kind = A_py_instance(cls, None)}, _) ->
                *    assert false *)

               (* Calls on other kinds of addresses is handled by other domains *)
               | E_py_object _ ->
                  Eval.eval_list args man.eval flow |>
                    (* FIXME FIXME FIXME/danger: args'/arg *)
                    Eval.bind (fun args' flow ->
                        let exp = {exp with ekind = E_py_call(f, args, [])} in
                        man.eval exp flow
                      )

               (* | E_get_type_partition _ ->
                *    Eval.eval_list args man.eval flow |>
                *      (\* FIXME FIXME FIXME/danger: args'/arg *\)
                *      Eval.bind (fun args' flow ->
                *          let exp = {exp with ekind = E_py_call(f, args, [])} in
                *          man.eval exp flow
                *        (\*                        Eval.singleton exp flow *\)
                *        ) *)

               | _ -> debug "callable/E_py_call, on %a@\n" pp_expr f; assert false
             )
         |> Option.return

      | E_py_call(f, args, _) ->
         Framework.Exceptions.panic_at range "calls with keyword arguments not supported"

      | _ -> None


    let ask _ _ _ = None

  end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
