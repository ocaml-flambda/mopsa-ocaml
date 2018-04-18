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
      Universal.Utils.compose_alloc_exec
        (fun addr flow ->
           man.exec ctx
             (mk_assign
                (mk_var func.py_func_var (tag_range range "func var"))
                (mk_addr addr (tag_range range "func addr"))
                (tag_range range "func addr assign")
             ) flow |>
           return
        )
        (Addr.mk_function_addr func) range man ctx flow
    | _ ->
      None

  let init _ ctx _ flow = ctx, flow

  let eval man ctx exp flow =
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
          man.exec ctx (Builtins.mk_builtin_raise "TypeError" (tag_range exp.erange "error1")) flow
        in
        oeval_singleton (None, flow, [])

      else
        (* Fill missing args with default parameters *)
        let default_args = List.map (function Some e -> e | None -> assert false) default_args in
        let rec fill_with_default dfs ndfs args =
          match args with
          | [] -> dfs
          | arg :: args' ->
            match ndfs with
            | [] ->
              let dfs' = List.tl dfs in
              arg :: (fill_with_default dfs' [] args')
            | _ :: ndfs' ->
              arg :: (fill_with_default dfs ndfs' args')
        in
        let args = fill_with_default default_args nondefault_args args in
        if List.length args < List.length nondefault_args then
          let flow =
            man.exec ctx (Builtins.mk_builtin_raise "TypeError" (tag_range exp.erange "error2")) flow
          in
          oeval_singleton (None, flow, [])
        else
          (* Give the call to {!Universal} *)
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
                 (mk_var tmp (tag_range exp.erange "tmp"))
                 (mk_call fundec args (tag_range exp.erange "call"))
                 (tag_range exp.erange "call assign")
              )
              flow
          in
          let evl = (Some {exp with ekind = E_var tmp}, flow, [mk_remove_var tmp (tag_range exp.erange "cleaner")]) in
          re_eval_singleton (man.eval ctx) evl

    | _ -> None

  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
