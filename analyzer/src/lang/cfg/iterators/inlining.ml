(****************************************************************************)
(*                   Copyright (C) 2018 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Interprocedural iterator for CFG based on inlining. *)

open Mopsa
open Universal.Ast
open Universal.Zone
open Universal.Iterators.Interproc.Inlining
open Ast


(*==========================================================================*)
                       (** {2 Iterator} *)
(*==========================================================================*)


module Domain : Framework.Domains.Stateless.S =
struct

  type _ domain += D_cfg_inlining : unit domain

  let name = "cfg.iterators.inlining"

  let id = D_cfg_inlining

  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_cfg_inlining -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  let exec_interface = {export = []; import = []}
  let eval_interface = {export = [Z_u, Z_any]; import = []}

  let init prog man (flow: 'a flow) =
    Some (
      Flow.set_annot Callstack.A_call_stack [] flow
    )

    
  let eval zone exp man flow =
    let range = erange exp in
    match ekind exp with
      
    | E_call({ekind = E_function (User_defined f)}, args) ->

       (* TODO: adapt to CFG *)

       
      (* Clear all return flows *)
      let flow0 = Flow.filter (fun tk env ->
          match tk with
          | T_return _ -> false
          | _ -> true
        ) man flow
      in

      (* Add parameters and local variables to the environment *)
      let new_vars = f.fun_parameters @ f.fun_locvars in
      (* let new_vars_declaration_block = List.map (fun v ->
       *     mk_add_var v (tag_range range "variable addition")
       *   ) new_vars |> (fun x -> mk_block x (tag_range range "declaration_block"))
       * in
       * let flow0' = man.exec new_vars_declaration_block flow0 in *)

      (* TODO: mk_add_var for ret? *)
      let ret = f.fun_return_var in

      (* Assign arguments to parameters *)
      let parameters_assign = List.mapi (fun i (param, arg) ->
          mk_assign (mk_var param range) arg range
        ) (List.combine f.fun_parameters args) in

      let init_block = mk_block parameters_assign range in

      (* Update call stack *)
      let flow1 = Callstack.push range flow0 in

      (* Execute body *)
      let flow2 = man.exec init_block flow1 |>
                  man.exec f.fun_body
      in

      (* Remove parameters and local variables from the environment *)
      let ignore_stmt_list =
        List.mapi (fun i v ->
            mk_remove_var v range
          ) (new_vars)
      in
      let ignore_block = mk_block ignore_stmt_list range in

      let flow3 = man.exec ignore_block flow2 in

      Eval.singleton (mk_var ret range) flow3 ~cleaners:[mk_remove_var ret range] |>
      OptionExt.return

    | _ -> None

         
  let exec _ _ _ _ = None

  let ask _ _ _ = None

end


let () =
  Framework.Domains.Stateless.register_domain (module Domain)

