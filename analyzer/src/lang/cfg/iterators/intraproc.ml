(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2018-2019 The MOPSA Project.                               *)
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

(** General intraprocedural iterator on Control Flow Graphs. *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Universal.Ast
open Universal.Zone
open Ast


let name = "cfg.iterators.intraproc"


(*==========================================================================*)
(**                       {2 Command line options}                          *)
(*==========================================================================*)

             
let opt_decreasing_iter : int ref = ref 1
(** Number of decreasing iterations after widening stabilisation. *)

                                        
let () =
  register_domain_option name {
    key = "-decreasing-iter";
    category = "Loops";
    doc = " number of decreasing iterations after stabilization";
    spec = ArgExt.Set_int opt_decreasing_iter;
    default = "1";
  }
 
                                        
(*==========================================================================*)
                       (** {2 Iterator} *)
(*==========================================================================*)


module Domain =
struct

  include GenStatelessDomainId(struct
      let name = name
    end)


  let interface = {
    iexec = { provides = [Z_u]; uses = [] };
    ieval = { provides = []; uses = [] };
  }


  let init prog man flow = flow


  (* flow iterator for CFG *)                         
  let cfg_iterator cfg man flow =
    
    (* apply a transfer function, update all the destination nodes *)
    let apply_edge edge flow =
      let stmt = CFG.edge_data edge in
      debug "applying edge %a: @[%a@]" pp_edge_as_id edge pp_stmt stmt;
      let src, dst = CFG.edge_src edge, CFG.edge_dst edge in
      (* shift source node value into the expected port flow *)
      let flow =
        List.fold_left
          (fun flow (port,node) ->
            let v = Flow.get (T_node (CFG.node_id node)) man.lattice flow in
            debug "input node %a -> port %a, abs = @[%a@]"
                  pp_node_as_id node pp_token port man.lattice.print v;
            Flow.add port v man.lattice flow
          )
          flow src
      in
      (* apply edge transfer function *)
      let flow = man.exec stmt flow
      in
      (* dispatch flow values into nodes *)
      let flow =
        List.fold_left
          (fun flow (port,node) ->
            let v = Flow.get port man.lattice flow in
            debug "output port %a -> node %a, abs = @[%a@]"
                  pp_token port pp_node_as_id node man.lattice.print v;
            Flow.add (T_node (CFG.node_id node)) v man.lattice flow
          )
          flow dst
      in
      (* clean-up flows used by transfer function *)
      let flow =
        List.fold_left
          (fun flow (port,_) -> Flow.remove port flow)
          flow (src@dst)
      in
      flow
    in
    
    (* call apply_edge for all edges out of this node *)
    let update_node_out node flow =
      debug "updating node %a" pp_node_as_id node;
      List.fold_left
        (fun flow (_,e) -> apply_edge e flow)
        flow (CFG.node_out node)
    in
    
    (* analyze a component until its head is stable, with widening *)
    let rec fix_component count lst flow =
      (* get widening node *)
      let wid = match lst with
        | (GraphSig.Simple node)::_ -> CFG.node_id node
        | _ -> Exceptions.panic "node expected at the head of a component"
      in
      let old = Flow.get (T_node wid) man.lattice flow in
      debug "analyzing component @[%a@], widen=%a, count=%i, abs=@[%a@]"
            (Graph.pp_nested_list_list pp_node_as_id) lst
            pp_node_id wid count man.lattice.print old;
      (* analyze the component *)
      let flow = analyze_component lst flow in
      let v = Flow.get (T_node wid) man.lattice flow in
      (* check stability *)
      if man.lattice.subset (Flow.get_unit_ctx flow) v old
      then (
        debug "component head stable %a" pp_node_id wid;
        refine_component count lst flow
      )
      else
        (* apply widening *)
        let widened =
          if count < !Universal.Iterators.Loops.opt_loop_widening_delay
          then v
          else man.lattice.widen (Flow.get_unit_ctx flow) old v
        in
        debug "component head not stable %a" pp_node_id wid;
        let flow = Flow.set (T_node wid) widened man.lattice flow in
        fix_component (count+1) lst flow

    (* decreasing iterations *)
    and refine_component count lst flow =
      if count >= !opt_decreasing_iter then flow
      else (
        (* get widening node *)
        let wid = match lst with
          | (GraphSig.Simple node)::_ -> CFG.node_id node
          | _ -> Exceptions.panic "node expected at the head of a component"
        in
        (* iter *)
        debug "decreasing iteration at head %a, count=%i" pp_node_id wid count;
        let flow = analyze_component lst flow in
        refine_component (count+1) lst flow
      )

                      
    (* analyze a list of components *)
    and analyze_component lst flow =
      List.fold_left
        (fun flow x ->
          match x with
          | GraphSig.Simple node -> update_node_out node flow
          | GraphSig.Composed l -> fix_component 0 l flow
        )
        flow lst
    in
    
    (* shift the current flow into the entry nodes *)
    let flow_in flow =
      let cur = Flow.get T_cur man.lattice flow in
      let flow = Flow.remove T_cur flow in
      List.fold_left
        (fun flow (port,node) ->
          debug "set entry node %a, port %a" pp_node_as_id node pp_token port;
          Flow.set (T_node (CFG.node_id node)) cur man.lattice flow
        )
        flow
        (CFG.entries cfg.cfg_graph)
    in
      
    (* gather the flows from the exit nodes into the current flow *)
    let flow_out flow =
      List.fold_left
        (fun flow (port,node) ->
          debug "get exit node %a, port %a" pp_node_as_id node pp_token port;
          let v = Flow.get (T_node (CFG.node_id node)) man.lattice flow in
          Flow.add T_cur v man.lattice flow
        )
        flow
        (CFG.exits cfg.cfg_graph)
    in
    
    (* remove all node-related flows (keeping cur, etc.) *)
    let cleanup flow =
      CFG.fold_nodes
        (fun id _ flow -> Flow.remove (T_node id) flow)
        cfg.cfg_graph flow
    in
    
    (* all together now *)
    let flow = flow_in flow in
    debug "CFG iteration started:@\nabs = @[%a@]" (Flow.print man.lattice) flow;
    let flow = analyze_component cfg.cfg_order flow in
    debug "CFG iteration finished:@\nabs = @[%a@]" (Flow.print man.lattice) flow;
    let flow = flow |> flow_out |> cleanup in
    debug "returned flow:@\nabs = @[%a@]" (Flow.print man.lattice) flow;
    flow


  (* atomic test function *)
  let test_iterator man cond flow =
    let range = erange cond in
    debug "CFG test [%a] at %a" pp_expr cond pp_range range;
    let tflow = man.exec (mk_assume cond range) flow
    and fflow = man.exec (mk_assume (mk_not cond range) range) flow in
    let tflow = Flow.set T_true  (Flow.get T_cur man.lattice tflow) man.lattice tflow
    and fflow = Flow.set T_false (Flow.get T_cur man.lattice fflow) man.lattice fflow in
    Flow.join man.lattice tflow fflow

    
  let exec zone stmt man flow =
    match skind stmt with

    | S_cfg cfg ->
       Some (Post.return (cfg_iterator cfg man flow))

    | S_test expr ->
       Some (Post.return (test_iterator man expr flow))

    | S_skip ->
       Some (Post.return flow)
      
    | _ ->
       (* S_expression, S_block and S_print are handled by universal's intraproc *)
       None
   
  let eval zone exp man flow = None

  let ask query man flow = None

end


let () =
  register_domain (module Domain)
