(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Inter-procedural iterator by inlining, caching the last analysis
   results for each flow *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast
open Callstack
open Context
open Common
open MapExt

let name = "universal.iterators.interproc.sequential_cache"

let opt_universal_modular_interproc_cache_size : int ref = ref 10

let () =
  register_domain_option name {
    key = "-mod-interproc-size";
    category = "Interproc";
    doc = " size of the cache in the modular interprocedural analysis";
    spec = ArgExt.Set_int opt_universal_modular_interproc_cache_size;
    default = string_of_int !opt_universal_modular_interproc_cache_size;
  };

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = name
    end)

  let dependencies = []

  let checks = []

  let debug fmt = Debug.debug ~channel:name fmt

  module Fctx = GenContextKey(
    struct
      type 'a t = ('a flow * expr option * 'a flow * stmt list) list StringMap.t
      let print p fmt ctx = Format.fprintf fmt "Function cache context (py): %a@\n"
          (StringMap.fprint
             MapExt.printer_default
             (fun fmt s -> Format.fprintf fmt "%s" s)
             (fun fmt list ->
                Format.pp_print_list
                  (fun fmt (in_flow, oexpr, out_flow, cleaners) ->
                     Format.fprintf fmt "in_flow = %a@\nout_flow = %a@\noexpr = %a@\ncleaners = %a@\n"
                       (Flow.print p) in_flow
                       (Flow.print p) out_flow
                       (OptionExt.print pp_expr) oexpr
                       (Format.pp_print_list pp_stmt) cleaners
                  )
                  fmt list
             )
          )
          ctx
    end)

  let find_signature man funname in_flow =
    try
      let cache = find_ctx Fctx.key (Flow.get_ctx in_flow) in
      let flows = StringMap.find funname cache in
      Some (List.find (fun (flow_in, _, _, _) ->
          Flow.subset man.lattice in_flow flow_in
        ) flows)
    with Not_found -> None


  let store_signature funname in_flow eval_res out_flow cleaners =
    let old_ctx = try find_ctx Fctx.key (Flow.get_ctx out_flow) with Not_found -> StringMap.empty in
    let old_sig = try StringMap.find funname old_ctx with Not_found -> [] in
    let new_sig =
      if List.length old_sig < !opt_universal_modular_interproc_cache_size then (in_flow, eval_res, out_flow, cleaners)::old_sig
      else (in_flow, eval_res, out_flow, cleaners) :: (List.rev @@ List.tl @@ List.rev old_sig) in
    let new_ctx = StringMap.add funname new_sig old_ctx in
    Flow.set_ctx (add_ctx Fctx.key new_ctx (Flow.get_ctx out_flow)) out_flow


  let init prog flow =
    Flow.map_ctx (add_ctx Fctx.key StringMap.empty)

  let split_cur_from_others man flow =
    let bot = Flow.bottom (Flow.get_ctx flow) (Flow.get_alarms flow) in
    let flow_cur = Flow.set T_cur (Flow.get T_cur man.lattice flow) man.lattice bot in
    let flow_other = Flow.remove T_cur flow in
    flow_cur, flow_other

  let eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_call({ekind = E_function (User_defined func)}, args) ->

      if man.lattice.is_bottom (Flow.get T_cur man.lattice flow)
      then Cases.empty flow |> OptionExt.return
      else

      let in_flow = flow in
      let in_flow_cur, in_flow_other = split_cur_from_others man in_flow in
      let params, locals, body, in_flow_cur = init_fun_params func args range man in_flow_cur in
      let in_flow_cur = post_to_flow man in_flow_cur in
      begin match find_signature man func.fun_uniq_name in_flow_cur with
        | None ->
           let ret = Common.mk_return_var exp in
                       (* mk_range_attr_var range (Format.asprintf "ret_var_%s" func.fun_uniq_name) T_any in *)
          inline func params locals body (Some ret) range man in_flow_cur |>
          bind (fun oeval_case out_flow ->
              debug "in bind@\n";
              match oeval_case with
              | NotHandled -> assert false

              | Empty ->
                let out_flow_cur, out_flow_other = split_cur_from_others man out_flow in
                (* let out_flow_cur = exec_block_on_all_flows cleaners man out_flow_cur in *)
                let out_flow = Flow.join man.lattice out_flow_cur out_flow_other in
                let flow = store_signature func.fun_uniq_name in_flow_cur None out_flow [] in
                Cases.case oeval_case (Flow.join man.lattice in_flow_other flow)

              | Result(eval_res,log,cleaners) ->
                (* we have to perform a full bind in order to add
                   in_flow_other to the flow even in the case of an
                   empty eval *)
                man.eval eval_res out_flow |>
                Cases.bind (fun oeval_case out_flow ->
                    match oeval_case with
                    | NotHandled -> assert false

                    | Empty ->
                      Eval.empty (Flow.join man.lattice in_flow_other out_flow)

                    | Result(eval_res,_,_) ->
                      let out_flow_cur, out_flow_other = split_cur_from_others man out_flow in
                      (* let out_flow_cur = exec_block_on_all_flows cleaners man out_flow_cur in *)
                      let out_flow = Flow.join man.lattice out_flow_cur out_flow_other in
                      let flow = store_signature func.fun_uniq_name in_flow_cur (Some eval_res) out_flow (StmtSet.elements cleaners) in
                      Cases.return eval_res (Flow.join man.lattice in_flow_other flow)
                  )
            )

        | Some (_, oout_expr, out_flow, cleaners) ->
          Debug.debug ~channel:"profiling" "reusing %s at range %a" func.fun_orig_name pp_range func.fun_range;
          debug "reusing something in function %s@\nchanging in_flow=%a@\ninto out_flow=%a@\n" func.fun_orig_name (Flow.print man.lattice.print) in_flow (Flow.print man.lattice.print) out_flow;
          match oout_expr with
          | None -> Cases.empty (Flow.join man.lattice in_flow_other out_flow)
          | Some e -> Cases.singleton e (Flow.join man.lattice in_flow_other out_flow) ~cleaners:cleaners
      end
      |> OptionExt.return




    | _ -> None

  let exec _ _ _ = None
  let ask _ _ _ = None

end


let () = register_stateless_domain (module Domain)
