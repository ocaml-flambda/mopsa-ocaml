(****************************************************************************)
(*                   Copyright (C) 2018 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** General intraprocedural iterator on Control Flow Graphs. *)

open Framework.Essentials
open Universal.Ast
open Universal.Zone
open Ast

                
(*==========================================================================*)
                       (** {2 Iterator} *)
(*==========================================================================*)


module Domain : Framework.Domains.Stateless.S =
struct

  type _ domain += D_cfg_intraproc : unit domain

  let name = "cfg.iterators.intraproc"

  let id = D_cfg_intraproc

  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_cfg_intraproc -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  let exec_interface = {export = [Z_u]; import = []}
  let eval_interface = {export = []; import = []}

  let init prog man flow = None


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
            let v = Flow.get (T_node (CFG.node_id node)) man flow in
            debug "input node %a -> port %a, abs = @[%a@]"
                  pp_node_as_id node pp_token port man.print v;
            Flow.add port v man flow
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
            let v = Flow.get port man flow in
            debug "output port %a -> node %a, abs = @[%a@]"
                  pp_token port pp_node_as_id node man.print v;
            Flow.add (T_node (CFG.node_id node)) v man flow
          )
          flow dst
      in
      (* clean-up flows used by transfoer function *)
      let flow =
        List.fold_left
          (fun flow (port,_) -> Flow.remove port man flow)
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
        | _ -> Debug.fail "node expected at the head of a component"
      in
      let old = Flow.get (T_node wid) man flow in
      debug "analyzing component @[%a@], widen=%a, abs=@[%a@]"
            (Graph.pp_nested_list_list pp_node_as_id) lst
            pp_node_id wid man.print old;
      (* analyze the component *)
      let flow = analyze_component lst flow in
      let v = Flow.get (T_node wid) man flow in
      (* check stability *)
      if man.subset v old
      then (
        debug "component head stable %a" pp_node_id wid;
        flow
      )
      else
        (* apply widening *)
        let widened =
          if count < !Universal.Iterators.Loops.opt_loop_widening_delay
          then v
          else man.widen flow.annot old v
        in
        debug "component head not stable %a" pp_node_id wid;
        let flow = Flow.set (T_node wid) widened man flow in
        fix_component (count+1) lst flow
        
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
      let cur = Flow.get T_cur man flow in
      let flow = Flow.remove T_cur man flow in
      List.fold_left
        (fun flow (port,node) ->
          debug "set entry node %a, port %a" pp_node_as_id node pp_token port;
          Flow.set (T_node (CFG.node_id node)) cur man flow
        )
        flow
        (CFG.entries cfg.cfg_graph)
    in
      
    (* gather the flows from the exit nodes into the current flow *)
    let flow_out flow =
      List.fold_left
        (fun flow (port,node) ->
          debug "get exit node %a, port %a" pp_node_as_id node pp_token port;
          let v = Flow.get (T_node (CFG.node_id node)) man flow in
          Flow.add T_cur v man flow
        )
        flow
        (CFG.exits cfg.cfg_graph)
    in
    
    (* remove all node-related flows (keeping cur, etc.) *)
    let cleanup flow =
      CFG.fold_nodes
        (fun id _ flow -> Flow.remove (T_node id) man flow)
        cfg.cfg_graph flow
    in
    
    (* all together now *)
    let flow = flow_in flow in
    debug "CFG iteration started:@\nabs = @[%a@]" (Flow.print man) flow;
    let flow = analyze_component cfg.cfg_order flow in
    debug "CFG iteration finished:@\nabs = @[%a@]" (Flow.print man) flow;
    let flow = flow |> flow_out |> cleanup in
    debug "returned flow:@\nabs = @[%a@]" (Flow.print man) flow;
    flow


  (* atomic test function *)
  let test_iterator man cond flow =
    let range = erange cond in
    let tflow = man.exec (mk_assume cond range) flow
    and fflow = man.exec (mk_assume (mk_not cond range) range) flow in
    let tflow = Flow.set T_true  (Flow.get T_cur man tflow) man tflow
    and fflow = Flow.set T_false (Flow.get T_cur man fflow) man fflow in
    Flow.join man tflow fflow

    
  let rec exec zone stmt man flow =
    match skind stmt with

    | S_cfg cfg ->
       Some (Post.of_flow (cfg_iterator cfg man flow))

    | S_test expr ->
       Some (Post.of_flow (test_iterator man expr flow))

    | _ ->
       (* S_expression, S_block and S_print are handled by universal's intraproc *)
       None
   
  let eval _ _ _ _ = None

  let ask _ _ _ = None

end


let () =
  Framework.Domains.Stateless.register_domain (module Domain)
