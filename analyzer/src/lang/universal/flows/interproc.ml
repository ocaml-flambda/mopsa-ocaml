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
open Framework.Domains.Stateful
open Framework.Domains.Stateless
open Framework.Ast
open Framework.Eval
open Ast

let name = "universal.flows.interproc"
let debug fmt = Debug.debug ~channel:name fmt

(*==========================================================================*)
(**                       {2 Loops flow token}                              *)
(*==========================================================================*)

let should_memo s =
  false

type token +=
  | TReturn of range * expr option
  (** Control flows reaching a return statement at a given location range
      and returning an optional expression. *)


(*==========================================================================*)
(**                            {2 Domain}                                   *)
(*==========================================================================*)

module Domain : Stateless.DOMAIN =
struct

  (*==========================================================================*)
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)


  let init man ctx prg fa = ctx, fa

  let exec manager ctx stmt flow =
    match skind stmt with
    | S_return (eo) ->
      let cur = manager.flow.get TCur flow in
      manager.flow.add (TReturn(stmt.srange, eo)) cur flow |>
      manager.flow.remove TCur |>
      return

    | _ -> None

  let eval manager ctx exp flow  =
    let range = erange exp in
    match ekind exp with
    | E_call({ekind = E_function f}, args) ->
      let () = debug "called %a" Framework.Pp.pp_expr exp in

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

      let body =
        if should_memo f.fun_name then
          mk_stmt
            (Memoisation.S_memoisation f.fun_body)
            (tag_range (erange exp) "memo")
        else
          f.fun_body
      in

      (* Execute body *)
      let flow1 = manager.exec ctx init_block flow0 |>
                  manager.exec ctx body
      in

      (* Temporary variable to store return expressions *)
      let tmpv = mktmp ~vtyp:f.fun_return_type () in
      let tmp = mk_var tmpv (tag_range range "return tmp") in

      (* Iterate over encountered return flows and assign the returned value to tmp *)
      let flow2 =
        manager.flow.fold (fun acc aenv -> function
            | TReturn(_, None) ->
              manager.flow.add TCur aenv acc

            | TReturn(_, Some e) ->
              debug "assign return expression";
              manager.flow.set TCur aenv manager.flow.bottom |>
              manager.exec ctx (mk_assign tmp e (tag_range range "return assign")) |>
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

      let flow3 = manager.exec ctx ignore_block flow2 in

      (* Re-evaluate the expression [tmp] from the top-level *)
      re_eval_singleton (manager.eval ctx) (Some tmp, flow3, [mk_remove_var tmpv (tag_range range "remove tmp")])

    | _ -> None

  let ask _ _ _ _ = None

end


let setup () =
  register_domain name (module Domain);
  register_token_compare (fun next tk1 tk2 ->
      match tk1, tk2 with
      | TReturn(r1, _), TReturn(r2, _) -> compare_range r1 r2
      | _ -> next tk1 tk2
    );
  register_pp_token (fun next fmt -> function
      | TReturn(r, _) -> Format.fprintf fmt "ret@%a" Framework.Pp.pp_range r
      | tk -> next fmt tk
    )
