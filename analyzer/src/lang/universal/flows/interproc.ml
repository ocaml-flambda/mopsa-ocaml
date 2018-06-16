(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Inter-procedural iterator with call inlining.  *)

open Framework.Essentials
open Ast

let name = "universal.flows.interproc"
let debug fmt = Debug.debug ~channel:name fmt

(*==========================================================================*)
(**                       {2 Loops flow token}                              *)
(*==========================================================================*)

let should_memo s =
  false

type Flow.token +=
  | TReturn of Framework.Utils.Location.range * expr option
  (** Control flows reaching a return statement at a given location range
      and returning an optional expression. *)


(*==========================================================================*)
(**                            {2 Domain}                                   *)
(*==========================================================================*)

module Domain : Framework.Domains.Stateless.DOMAIN =
struct

  (*==========================================================================*)
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)


  let init prog man ctx flow = None

  let import_exec = []
  let export_exec = [Framework.Zone.top]

  let exec zone stmt man ctx flow =
    match skind stmt with
    | S_return (eo) ->
      let cur = man.flow.get Flow.TCur flow in
      man.flow.add (TReturn(stmt.srange, eo)) cur flow |>
      man.flow.remove Flow.TCur |>
      Post.of_flow |>
      return

    | _ -> None


  let import_eval = []
  let export_eval = [Framework.Zone.path_top]

  let eval zpath exp man ctx flow  =
    let range = erange exp in
    match ekind exp with
    | E_call({ekind = E_function f}, args) ->
      let () = debug "called %a" pp_expr exp in

      (* Clear all return flows *)
      let flow0 = man.flow.filter (fun tk _ ->
          match tk with
          | TReturn _ -> false
          | _ -> true
        ) flow in


      (* Assign arguments to parameters *)
      let parameters_assign = List.mapi (fun i (param, arg) ->
          mk_assign (mk_var param range) arg range
        ) (List.combine f.fun_parameters args) in

      let init_block =
        mk_block parameters_assign range
      in

      let body =
        (* if should_memo f.fun_name then
         *   mk_stmt
         *     (Memoisation.S_memoisation f.fun_body)
         *     (tag_range (erange exp) "memo")
         * else *)
          f.fun_body
      in

      (* Execute body *)
      let flow1 = man.exec init_block ctx flow0 |>
                  man.exec body ctx
      in

      (* Temporary variable to store return expressions *)
      let tmpv = mktmp ~vtyp:f.fun_return_type () in
      let tmp = mk_var tmpv range in

      (* Iterate over encountered return flows and assign the returned value to tmp *)
      let flow2 =
        man.flow.fold (fun tk aenv acc ->
            match tk with
            | TReturn(_, None) ->
              man.flow.add Flow.TCur aenv acc

            | TReturn(_, Some e) ->
              debug "assign return expression";
              man.flow.set Flow.TCur aenv man.flow.bottom |>
              man.exec (mk_assign tmp e range) ctx |>
              man.flow.join acc

            | tk ->
              man.flow.add tk aenv acc
          )
          flow1
          (man.flow.set Flow.TCur man.env.bottom flow)
      in

      (* Remove parameters and local variables from the environment *)
      let parameters_ignore = List.mapi (fun i param ->
          mk_remove_var param range
        ) f.fun_parameters  in

      let locvar_ignore =
        List.mapi (fun i param ->
            mk_remove_var param range
          ) f.fun_locvars
      in
      let ignore_block =
        mk_block (parameters_ignore @ locvar_ignore) range
      in

      let flow3 = man.exec ignore_block ctx flow2 in

      (* Re-evaluate the expression [tmp] from the top-level *)
      man.eval tmp ctx flow3 |>
      Eval.add_cleaners [mk_remove_var tmpv range] |>
      return

    | _ -> None

  let ask _ _ _ _ = None

end


let setup () =
  Framework.Domains.Stateless.register_domain name (module Domain);
  Flow.register_token_compare (fun next tk1 tk2 ->
      match tk1, tk2 with
      | TReturn(r1, _), TReturn(r2, _) -> Framework.Utils.Location.compare_range r1 r2
      | _ -> next tk1 tk2
    );
  Flow.register_pp_token (fun next fmt -> function
      | TReturn(r, _) -> Format.fprintf fmt "ret@%a" Framework.Utils.Location.pp_range r
      | tk -> next fmt tk
    )
