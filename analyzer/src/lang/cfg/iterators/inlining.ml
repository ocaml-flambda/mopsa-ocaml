(****************************************************************************)
(*                   Copyright (C) 2018 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Interprocedural iterator for CFG based on inlining. *)

open Framework.Essentials
open Universal.Ast
open Universal.Zone
open Universal.Iterators.Interproc.Callstack
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
      Flow.set_annot A_call_stack [] flow
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

      (* Assign arguments to parameters *)
      let parameters_assign = List.mapi (fun i (param, arg) ->
          mk_assign (mk_var param range) arg range
        ) (List.combine f.fun_parameters args) in

      let init_block = mk_block parameters_assign range in

      (* Update call stack *)
      let cs = Flow.get_annot A_call_stack flow0 in
      let cs' = range :: cs in
      let flow1 = Flow.set_annot A_call_stack cs' flow0 in

      (* Execute body *)
      let flow2 = man.exec init_block flow1 |>
                  man.exec f.fun_body
      in

      (* Create a temporary variable to store return expressions *)
      let typ = OptionExt.option_dfl T_int f.fun_return_type in
      let tmp = mk_tmp ~vtyp:typ () in

      (* Iterate over return flows and assign the returned value to tmp *)
      let flow3 =
        Flow.fold (fun acc tk env ->
            match tk with
            | T_return(_, None) -> Flow.add T_cur env man acc

            | T_return(_, Some e) ->
              Flow.set T_cur env man acc |>
              (* man.exec (mk_add_var tmp (tag_range range "adding tmp")) |> *)
              man.exec (mk_assign (mk_var tmp range) e range) |>
              Flow.join man acc

            | _ -> Flow.add tk env man acc
          )
          (Flow.remove T_cur man flow)
          man flow2
      in

      (* Remove parameters and local variables from the environment *)
      let ignore_stmt_list =
        List.mapi (fun i v ->
            mk_remove_var v range
          ) (new_vars)
      in
      let ignore_block = mk_block ignore_stmt_list range in

      let flow4 = man.exec ignore_block flow3 in

      Eval.singleton (mk_var tmp range) flow4 ~cleaners:[mk_remove_var tmp range] |>
      OptionExt.return

    | _ -> None

         
  let exec _ _ _ _ = None

  let ask _ _ _ = None

end


let () =
  Framework.Domains.Stateless.register_domain (module Domain)

