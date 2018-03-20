(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Inter-procedural iterator with call inlining.  *)

open Framework.Lattice
open Framework.Flow
open Framework.Domains
open Framework.Manager
open Framework.Domains.Global
open Framework.Domains.Stateless
open Framework.Ast
open Ast

let name = "universal.flows.interproc"
let debug fmt = Debug.debug ~channel:name fmt

(*==========================================================================*)
(**                       {2 Loops flow token}                              *)
(*==========================================================================*)

type token +=
  | TReturn of range * expr option
  (** Control flows reaching a return statement at a given location range
      and returning an optional expression. *)


let () =
  register_pp_token (fun next fmt -> function
      | TReturn(r, _) -> Format.fprintf fmt "ret@%a" Framework.Pp.pp_range r
      | tk -> next fmt tk
    )


(*==========================================================================*)
(**                            {2 Domain}                                   *)
(*==========================================================================*)

module Domain : Stateless.DOMAIN =
struct

  (*==========================================================================*)
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)


  let init prg man fa = fa

  let exec stmt manager ctx flow =
    match skind stmt with
    | S_return (eo) ->
      let cur = manager.flow.get TCur flow in
      manager.flow.add (TReturn(stmt.srange, eo)) cur flow |>
      manager.flow.remove TCur |>
      Exec.return

    | _ -> None

  let eval exp manager ctx flow  =
    let range = erange exp in
    match ekind exp with
    | E_call({ekind = E_function f}, args) ->

      (* Clear all return flows *)
      let flow0 = manager.flow.filter (fun _ -> function
          | TReturn _ -> false
          | _ -> true
        ) flow in


      (* Assign arguments to parameters *)
      let parameters_assign = List.mapi (fun i (param, arg) ->
          mk_assign
            (mk_var param (tag_range range "assign param %d lval" i))
            arg
            (tag_range range "assign param %d" i)
        ) (List.combine f.fun_parameters args) in

      let init_block =
        mk_block parameters_assign
          (tag_range range "assign param block")
      in

      (* Execute body *)
      let flow1 = manager.exec init_block ctx flow0 |>
                  manager.exec f.fun_body ctx
      in

      (* Temporary variable to store return expressions *)
      let tmpv = mktmp () in
      let tmp = mk_var tmpv (tag_range range "return tmp") in

      (* Iterate over encountered return flows and assign the returned value to tmp *)
      let flow2 =
        manager.flow.fold (fun acc aenv -> function
            | TReturn(_, None) ->
              manager.flow.add TCur aenv acc

            | TReturn(_, Some e) ->
              manager.flow.set TCur aenv manager.flow.bottom |>
              manager.exec (mk_assign tmp e (tag_range range "return assign")) ctx |>
              manager.flow.join acc

            | tk ->
              manager.flow.add tk aenv acc
          )
          (manager.flow.set TCur manager.env.bottom flow)
          flow1
      in

      (* Remove parameters and local variables from the environment *)
      let parameters_ignore = List.mapi (fun i param ->
          mk_remove_var param (tag_range range "remove param %d" i)
        ) f.fun_parameters  in

      let locvar_ignore =
        List.mapi (fun i param ->
            mk_remove_var param (tag_range range "remove locvar %d" i)
          ) f.fun_locvars
      in
      let ignore_block =
        mk_block (parameters_ignore @ locvar_ignore)
          (tag_range range "ignore block")
      in

      let flow3 = manager.exec ignore_block ctx flow2 in

      (* Re-evaluate the expression [tmp] from the top-level *)
      (Some tmp, flow3, [mk_remove_var tmpv (tag_range range "remove tmp")]) |>
      Eval.re_eval_singleton manager ctx

    | _ -> None

  let ask _ _ _ _ = None

end


let setup () =
  register_domain name (module Domain);
  register_token_compare (fun next tk1 tk2 ->
      match tk1, tk2 with
      | TReturn(r1, _), TReturn(r2, _) ->
        compare_range r1 r2
      | _ -> next tk1 tk2
    )
