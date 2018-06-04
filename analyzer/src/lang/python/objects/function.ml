(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Definition of python functions and evaluation of their calls. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Exec
open Framework.Ast
open Universal.Ast
open Framework.Utils
open Ast
open Addr

let name = "python.objects.function"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  let exec man ctx stmt flow =
    let range = srange stmt in
    match skind stmt with
    (* Function definition *)
    | S_py_function(func) ->
      debug "creating function object";
      (* Allocate an object for the function and assign it to the variable
         representing the name of the function *)
      let kind =
        if Libs.Mopsa.is_unsupported_fundec func then F_unsupported func.py_func_var.vname else
        if Libs.Mopsa.is_builtin_fundec func then
          let name = Libs.Mopsa.builtin_fundec_name func in
          F_builtin name
        else F_user func
      in
      Addr.eval_alloc man ctx (A_py_function kind) stmt.srange flow |>
      oeval_to_oexec (fun addr flow ->
          man.exec ctx
            (mk_assign
               (mk_var func.py_func_var range)
               (mk_addr addr range)
               range
            ) flow |>
          return
        ) (man.exec ctx) man.flow
    | _ ->
      None

  let init _ ctx _ flow = ctx, flow

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    (* Calls to user-defined functions are translated to {!Universal.Ast.E_call}
       in order to be handled by other domains *)
    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function(F_user pyfundec)}},
        args,
        []
      ) ->
      (* First check the correct number of arguments *)
      let default_args, nondefault_args = List.partition (function None -> false | _ -> true) pyfundec.py_func_defaults in

      if List.length pyfundec.py_func_parameters < List.length nondefault_args then
        let flow =
          man.exec ctx (Utils.mk_builtin_raise "TypeError" exp.erange) flow
        in
        oeval_singleton (None, flow, [])

      else
      if List.length args > (List.length pyfundec.py_func_parameters) then
        let flow =
          man.exec ctx (Utils.mk_builtin_raise "TypeError" exp.erange) flow
        in
        oeval_singleton (None, flow, [])
      else
        let args =
          if List.length args = (List.length pyfundec.py_func_parameters) then
            args
          else
            (* Fill missing args with default parameters *)
            let default_args = List.map (function Some e -> e | None -> assert false) default_args in
            let rec fill_with_default dfs ndfs args =
              match args with
              | [] -> dfs
              | arg :: args' ->
                match ndfs with
                | [] ->
                  (* let dfs' = List.tl dfs in *)
                  arg :: (fill_with_default dfs [] args')
                | _ :: ndfs' ->
                  arg :: (fill_with_default dfs ndfs' args')
            in

            debug "|params| = %d" (List.length pyfundec.py_func_parameters);
            debug "|args| = %d" (List.length args);
            debug "|default| = %d" (List.length default_args);
            debug "|non-default| = %d" (List.length nondefault_args);
            let args = fill_with_default default_args nondefault_args args in

            debug "|args'| = %d" (List.length args);
            args
        in
        if List.length args <> (List.length pyfundec.py_func_parameters) then
          let flow =
            man.exec ctx (Utils.mk_builtin_raise "TypeError" exp.erange) flow
          in
          oeval_singleton (None, flow, [])
        else
          (* Initialize local variables to undefined value and give the call to {!Universal} *)
          let flow = man.exec ctx
              (mk_block (List.mapi (fun i v ->
                   mk_assign (mk_var v range) (mk_expr (E_py_undefined false) range) range
                 ) pyfundec.py_func_locals) range)
              flow
          in
          let tmp = mktmp () in
          let fundec = {
            fun_name = var_uniq_name (pyfundec.py_func_var);
            fun_parameters = pyfundec.py_func_parameters;
            fun_locvars = pyfundec.py_func_locals;
            fun_body = pyfundec.py_func_body;
            fun_return_type = T_any;
          } in


          let flow =
            man.exec ctx
              (mk_assign
                 (mk_var tmp exp.erange)
                 (mk_call fundec args exp.erange)
                 exp.erange
              )
              flow
          in
          let evl = (Some {exp with ekind = E_var tmp}, flow, [mk_remove_var tmp exp.erange]) in
          re_eval_singleton (man.eval ctx) evl

    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_method(f, obj)}},
        args,
        []
      ) ->
      let exp' = mk_py_call (mk_addr f range) ((mk_addr obj range) :: args) range in
      re_eval_singleton (man.eval ctx) (Some exp', flow, [])

    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_method_atomic(f, obj)}},
        args,
        []
      ) ->
      let exp' = mk_py_call (mk_addr f range) (obj :: args) range in
      re_eval_singleton (man.eval ctx) (Some exp', flow, [])


    | _ -> None

  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
