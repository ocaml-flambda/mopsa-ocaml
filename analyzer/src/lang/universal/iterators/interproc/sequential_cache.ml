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
open Ast
open Zone
open Callstack
open Context
open Inlining
open MapExt

let name = "universal.iterators.interproc.sequential_cache"

let opt_universal_modular_interproc : bool ref = ref true
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

  (** Zoning definition *)
  let interface = {
    iexec = { provides = [Z_u]; uses = [] };
    ieval = { provides = [Z_u, Z_any]; uses = [Z_u, Z_any] };
  }

  let debug fmt = Debug.debug ~channel:name fmt

  module Fctx = Context.GenPolyKey(
    struct
      type 'a t = ('a flow * expr option * 'a flow) list StringMap.t
      let print p fmt ctx = Format.fprintf fmt "Function cache context (py): %a@\n"
          (StringMap.fprint
             MapExt.printer_default
             (fun fmt s -> Format.fprintf fmt "%s" s)
             (fun fmt _ -> ())
          )
          ctx
    end)

  let find_signature man funname in_flow =
    if !opt_universal_modular_interproc then
      try
        let cache = Context.find_poly Fctx.key (Flow.get_ctx in_flow) in
        let flows = StringMap.find funname cache in
        Some (List.find (fun (flow_in, _, _) -> Flow.subset man.lattice in_flow flow_in) flows)
      with Not_found -> None
    else
      None

  let store_signature funname in_flow eval_res out_flow =
    let old_ctx = try Context.find_poly Fctx.key (Flow.get_ctx out_flow) with Not_found -> StringMap.empty in
    let old_sig = try StringMap.find funname old_ctx with Not_found -> [] in
    let new_sig =
      if List.length old_sig < !opt_universal_modular_interproc_cache_size then (in_flow, eval_res, out_flow)::old_sig
      else (in_flow, eval_res, out_flow) :: (List.rev @@ List.tl @@ List.rev old_sig) in
    let new_ctx = StringMap.add funname new_sig old_ctx in
    Flow.set_ctx (Context.add_poly Fctx.key new_ctx (Flow.get_ctx out_flow)) out_flow


  let init prog flow =
    Flow.map_ctx (Context.init_poly Fctx.init)

  let eval zs exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_call({ekind = E_function (User_defined func)}, args) ->
      let in_flow = flow in
      let in_flow_cur = Flow.set T_cur (Flow.get T_cur man.lattice in_flow) man.lattice (Flow.bottom (Flow.get_ctx in_flow))
      and in_flow_other = Flow.remove T_cur in_flow in
      begin match find_signature man func.fun_name in_flow_cur with
        | None ->
          (* oups, on peut pas faire ça, comme on recommence l'exécution d'en haut *)
          (* man.eval ~zone:(Zone.Z_u, Z_any) (mk_call func args range) in_flow_cur |> *)
          Inlining.Domain.inline_function man func args range flow func.fun_return_var |>
          Eval.bind_lowlevel (fun oeval_res out_flow cleaners ->
              debug "in bind@\n";
              match oeval_res with
              | None ->
                let out_flow = Framework.Core.Sig.Domain.Stateless.exec_block_on_all_flows cleaners man out_flow in
                let flow = store_signature func.fun_name in_flow_cur oeval_res out_flow in
                Eval.case oeval_res (Flow.join man.lattice in_flow_other flow)

              | Some eval_res ->
                man.eval (*FIXME: Zone issue? ~zone:(Zone.Z_py, Zone.Z_py_obj)*) eval_res out_flow |>
                Eval.bind (fun eval_res out_flow ->
                    debug "eval_res = %a@\ncleaners = %a@\n" pp_expr eval_res pp_stmt (mk_block cleaners range);
                    let out_flow = Framework.Core.Sig.Domain.Stateless.exec_block_on_all_flows cleaners man out_flow in
                    let flow = store_signature func.fun_name in_flow_cur (Some eval_res) out_flow in
                    Eval.singleton eval_res (Flow.join man.lattice in_flow_other flow)
                  )
            )

        | Some (_, oout_expr, out_flow) ->
          Debug.debug ~channel:"profiling" "reusing %s at range %a" func.fun_name pp_range func.fun_range;
          debug "reusing something in function %s@\nchanging in_flow=%a@\ninto out_flow=%a@\n" func.fun_name (Flow.print man.lattice) in_flow (Flow.print man.lattice) out_flow;
          Eval.case oout_expr (Flow.join man.lattice in_flow_other out_flow)
      end
      |> Option.return




    | _ -> None

  (* TODO: copying the S_return from inlining is not modular *)
  let exec zone stmt man flow =
    match skind stmt with
    | S_return e ->
      let cur = Flow.get T_cur man.lattice flow in
      Flow.add (T_return (stmt.srange, e)) cur man.lattice flow |>
      Flow.remove T_cur |>
      Post.return |> Option.return

    | _ -> None

  let ask _ _ _ = None

end


let () = Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
