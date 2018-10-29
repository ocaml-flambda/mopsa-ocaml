(****************************************************************************)
(*                   Copyright (C) 2018 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Converts a Universal program AST into a CFG. *)

open Framework.Essentials
open Universal.Ast
open Ast
open Flow



(* Extract calls from expression.
   Returns:
   - a call-free expression e
   - a sequence of statements of the form v = call(e1,...,en)
   where v is a temporary variable and e1...en are call free
   that must be executed before evaluating e
   - the list of temporary variables introduced
 *)
let extract_calls (e:expr) : expr * stmt list * var list =
  let (assigns, tmps), e =
    Visitor.fold_map_expr
      (fun (assigns,tmps) e ->
        match ekind e with
        | E_call (f, args) ->
           let tmp = mk_tmp ~vtyp:(etyp e) () in
           let v = mk_var tmp (erange e) in
           let assign = mk_assign v e (erange e) in
           (assign::assigns, tmp::tmps), v
        | _ -> (assigns,tmps), e
      )
      (fun acc s -> acc, s)
      ([],[])
      e
  in
  e, List.rev assigns, tmps
  


(* Context used during conversion of a function. *)   
type ctx = {
    ctx_cfg: cfg;
    ctx_return: node;
    ctx_break: node list;
    ctx_continue: node list;
    ctx_return_var: var;
  }

         
(* Add a statement to the graph in the context. *)
let rec stmt_to_cfg (c:ctx) (pre:node) (post:node) (s:stmt) : unit =
  match skind s with

  (* compound statements *)

  | S_if (e,s1,s2) ->
     (* add nodes begining the true and false branches *)
     let tloc = range_begin (get_origin_range (srange s1))
     and floc = range_begin (get_origin_range (srange s2)) in
     let tnode = CFG.add_node c.ctx_cfg tloc ()
     and fnode = CFG.add_node c.ctx_cfg floc () in
     (* add test edge *)
     ignore
       (CFG.add_edge
          c.ctx_cfg (erange e)
          ~src:[T_cur,pre]
          ~dst:[T_true,tnode; T_false,fnode]
          (mk_test e (erange e))
       );
     (* add branches *)
     stmt_to_cfg c tnode post s1;
     stmt_to_cfg c fnode post s2

     
  | S_block l ->
     let rec add pre post = function
       | [] ->
          (* empty block: connect pre and post nodes with skip *)
          ignore
            (CFG.add_edge
               c.ctx_cfg (srange s)
               ~src:[T_cur,pre] ~dst:[T_cur,post]
               (mk_skip (srange s))
            )
       | [s] ->
          (* connect pre to post with statement *)
          stmt_to_cfg c pre post s
       | a::b ->
          (* add a new node after the first statement *)
          let loc = range_begin (get_origin_range (srange a)) in
          let node = CFG.add_node c.ctx_cfg loc () in
          (* add the first statement between pre and node *)
          stmt_to_cfg c pre post s;
          (* add the rest between node and post *)
          add node post b
     in
     add pre post l


  | S_while (e,s) ->
     (* add node at the begining of the loop body *)
     let loc = range_begin (get_origin_range (srange s)) in
     let entry = CFG.add_node c.ctx_cfg loc () in
     (* add test edge *)
     ignore
       (CFG.add_edge
          c.ctx_cfg (erange e)
          ~src:[T_cur,pre]
          ~dst:[T_true,entry; T_false,post]
          (mk_test e (erange e))
       );
     (* add body *)
     let c = 
       { c with
         ctx_break = post::c.ctx_break;
         ctx_continue = pre::c.ctx_continue;
       }
     in
     stmt_to_cfg c entry post s


  (* jump statements *)
                   
  | S_return eo -> failwith "TODO"

  | S_break ->
     if c.ctx_break = [] then
       Debug.fail "break without a loop at %a" pp_range (srange s);
     (* goto edge (skip statement) *)
     ignore
       (CFG.add_edge
          c.ctx_cfg (srange s)
          ~src:[T_cur,pre] ~dst:[T_cur,List.hd c.ctx_break]
          (mk_skip (srange s))
       )

  | S_continue ->
     if c.ctx_continue = [] then
       Debug.fail "continue without a loop at %a" pp_range (srange s);
     (* goto edge (skip statement) *)
     ignore
       (CFG.add_edge
          c.ctx_cfg (srange s)
          ~src:[T_cur,pre] ~dst:[T_cur,List.hd c.ctx_continue]
          (mk_skip (srange s))
       )

     
  (* atomic statements -> add an edge with the statement *)

  | S_assign _
  | S_assume _
  | S_rename_var _
  | S_remove_var _
  | S_project_vars _
  | S_expression _
  | S_rebase_addr _
  | S_simple_assert _
  | S_assert _
  | S_print ->
     ignore
       (CFG.add_edge
          c.ctx_cfg (srange s)
          ~src:[T_cur,pre] ~dst:[T_cur,post]
          s
       )
    

  (* unknown *)

  | _ ->
     Debug.fail "cannot convert statement %a to CFG" pp_stmt s
    
