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

(** Converts a Universal program AST into a CFG. *)

open Option
open Mopsa
open Universal.Ast
open Universal.Iterators.Interproc.Common
open Ast
open Flow


let name = "cfg.frontend"
let debug fmt = Debug.debug ~channel:name fmt

              
let dump_dot = false (** dump CFG in dot file (for debugging) *)

             
(*==========================================================================*)
                      (** {2 Graph conversion} *)
(*==========================================================================*)


(*
  AST are transformed into graphs containing only "simple" statements.
  Each edge is either:
  - a single statement without any sub-statement (no if, while, etc.)
  - a block, where each statement has no sub-statement

  Expressions in statements are also simplified, either:
  - an expression with no function call, or
  - e0 = f(e1, ..., en), where e0, ..., en have no calls

  NOTE: for e1 && e2 and e1 || e2, all calls in e1 and e2 are extracted
  and evaluated before the simplified e1 and e2 are evaluated. 
  This makes it impossible to implement shortcut boolean operations
  (where e2 is not evaluated for e1 && e2 if e1 is false).
 *)


(* Extract calls from expression.
   Returns:
   - a list of temporary variables
   - a sequence of assignments tmp = call(e1, ..., en)
   - the expression with no longer any call
 *)
let extract_calls_expr (e:expr) : (var * range) list * stmt list * expr =
  (* we need to match on AST root after recursively handling its sub-trees,
     hence we cannot use Visitor.fold_map_expr directly
   *)
  let rec doit (tmps,assigns) e =
    (* recursive call on argument expressions *)
    let p, c = Visitor.split_expr e in
    let (tmps,assigns), el = doit_list (tmps,assigns) [] p.exprs in
    (* recombine sub-expressions into expression *)
    let e = c { p with exprs = el } in
    (* extract top-level call *)
    match ekind e with
    | E_call (f, args) ->
       let tmp = mktmp ~typ:(etyp e) () in
       let v = mk_var tmp (erange e) in
       let assign = mk_assign v e (erange e) in
       ((tmp,erange e)::tmps, assign::assigns), v
    | _ ->
       (tmps, assigns), e
  and doit_list x acc = function
    | [] -> x, List.rev acc
    | a::b -> let x,a = doit x a in doit_list x (a::acc) b
  in
  let (tmps, assigns), e' = doit ([],[]) e in
  match ekind e' with
  | E_var _ when List.length assigns = 1 ->
     (* special case: the original expression was call(e1, ..., en) with no
        calls within e1, ..., en
      *)
     [], [], e
  | _ ->
     List.rev tmps, List.rev assigns, e'

  
(* Extract calls from all the expressions used in a statement.
   Retuns the temporary variables, their assignments, and the argument
   statement without calls in its expressions.
   Does not change the sub-statements of the statement.
 *)
let extract_calls_stmt (s:stmt) : (var * range) list * stmt list * stmt =
  let p, c = Visitor.split_stmt s in
  let rec extract tmps assigns exprs = function
    | [] -> tmps, assigns, List.rev exprs
    | e::ee ->
       let tmp, assign, e' = extract_calls_expr e in
       extract (tmp@tmps) (assign@assigns) (e'::exprs) ee
  in
  let tmps, assigns, exprs = extract [] [] [] p.exprs in
  let p = { p with exprs = exprs; } in
  tmps, assigns, c p
  

(* Generate add_var from temporary list. *)
let mk_add_tmps (l:(var*range) list) : stmt list =
  List.map (fun (v,r) -> mk_add_var v r) l
  
(* Generate remove_var from temporary list. *)
let mk_remove_tmps (l:(var*range) list) : stmt list =
  List.map (fun (v,r) -> mk_remove_var v r) l
  

(* Context used during conversion of a function. *)   
type ctx = {
    ctx_cfg: graph;
    ctx_return: node;
    ctx_break: node list;
    ctx_continue: node list;
    ctx_return_var: var option;
  }


(* Add a node, with given id. *)
let add_node (c:ctx) (id:node_id) : node =
  CFG.add_node c.ctx_cfg id ()


(* Add an edge between src and dst nodes, with blk statements. *)
let add_edge (c:ctx) range (src:(port*node) list) (dst:(port*node) list)
             (blk:stmt list) : unit =
  let s = match blk with
    | [] -> mk_skip range
    | [a] -> a
    | _ -> mk_block blk range
  in
  let _ = CFG.add_edge c.ctx_cfg (mk_fresh_edge_id range) ~src ~dst s in
  ()


(* Insert a block after the src node.
   If the block is not empty, add a fresh node to represent the end of 
   the block; the end node is returned.
   If the block is empty, no fresh node is created and src is returned.
 *)  
let add_block_after (c:ctx) range (src:node) (blk:stmt list) : node =
  if blk = [] then src else
    let src' = add_node c (copy_node_id (CFG.node_id src)) in
    add_edge c range [T_cur,src] [T_cur,src'] blk;
    src'
    
(* Insert a block before the dst node. *)
let add_block_before (c:ctx) range (dst:node) (blk:stmt list) : node =
  if blk = [] then dst else
    let dst' = add_node c (copy_node_id (CFG.node_id dst)) in
    add_edge c range [T_cur,dst'] [T_cur,dst] blk;
    dst'
      
         
(* Add a statement to the graph in the context. 
   Handle compound statements by induction.
   Takes care of extracting calls from expressions.
 *)
let rec add_stmt (c:ctx) (pre:node) (post:node) (s:stmt) : unit =
  
  match skind s with

  (* compound statements *)
  
  | S_if (e,s1,s2) ->
    let tmps, assigns, e = extract_calls_expr e in
    let adds, rems = mk_add_tmps tmps, mk_remove_tmps tmps in
    
    (* add nodes begining the true and false branches *)
    let tloc = mk_fresh_node_id (get_range_start (srange s1))
    and floc = mk_fresh_node_id (get_range_end (srange s2)) in
    let tnode = add_node c tloc
    and fnode = add_node c floc in
    
    (* add test edge *)
    let blk = adds @ assigns @ [mk_test e (erange e)] in
    add_edge c (erange e) [T_cur,pre] [T_true,tnode; T_false,fnode] blk;
    let tnode = add_block_after c (erange e) tnode rems
    and fnode = add_block_after c (erange e) fnode rems in
    
    (* add branches *)
    add_stmt c tnode post s1;
    add_stmt c fnode post s2
      
  | S_block (l,locals) ->
    let rec add pre post = function
      | [] ->
        (* empty block: connect pre and post nodes with skip *)
        add_edge c (srange s) [T_cur,pre] [T_cur,post] []
      | [s] ->
        (* connect pre to post with statement *)
        add_stmt c pre post s
      | a::b ->
        (* add a new node after the first statement *)
        let loc = mk_fresh_node_id (get_range_start (srange a)) in
        let node = add_node c loc in
        (* add the first statement between pre and node *)
        add_stmt c pre node a;
        (* add the rest between node and post *)
        add node post b
    in
    let rems = List.map (fun v -> mk_remove_var v (srange s)) locals in
    add pre post (l@rems)
      

  | S_while (e,s) ->
    let tmps, assigns, e = extract_calls_expr e in
    let adds, rems = mk_add_tmps tmps, mk_remove_tmps tmps in
    
    (* add node at the begining of the loop body *)
    let loc = mk_fresh_node_id (get_range_start (srange s)) in
    let entry = add_node c loc in

    (* add test edge *)
     let blk = adds @ assigns @ [mk_test e (erange e)] in
    let post' = add_block_before c (erange e) post rems in
    add_edge c (erange e) [T_cur,pre] [T_true,entry; T_false,post'] blk;
    
     (* add body *)
    let c = 
      { c with
        ctx_break = post::c.ctx_break;
        ctx_continue = pre::c.ctx_continue;
      }
    in
    let entry = add_block_after c (erange e) entry rems in
    add_stmt c entry pre s
      

  (* jump statements *)
      
  | S_return None ->
    (* translate into a simple jump to the exit node *)
    if c.ctx_return_var <> None then
      Exceptions.panic "return without an expression for non-void function at %a" pp_range (srange s);
    (* jump to return node *)
    add_edge c (srange s) [T_cur,pre] [T_cur,c.ctx_return] []
      
  | S_return (Some e) ->
    (* translate to a simple "return tmp_var" and jump to the exit node *)
    let tmps, assigns, e = extract_calls_expr e in
    let adds, rems = mk_add_tmps tmps, mk_remove_tmps tmps in
    (* assigns expression to return variable *)
    let ret, retvar =
      match c.ctx_return_var with
      | Some var -> var, mk_var var (erange e)
      | None ->
        Exceptions.panic "return with an expression for void function at %a" pp_range (srange s);
    in
    let assign_stmt = mk_assign retvar e (erange e) in
    let return_stmt = mk_stmt (S_return (Some retvar)) (erange e) in
    let addret = mk_add_var ret (erange e) in
    let remret = mk_remove_var ret (erange e) in
    (* intermediate node *)
    let loc = mk_fresh_node_id (get_range_start (srange s)) in
    let med = add_node c loc in
    (* from cur to return flow *)
    add_edge
      c (srange s) [T_cur,pre] [T_return (srange s),med]
      (adds @ assigns @ [addret; assign_stmt; return_stmt]);
    (* from intermediate to exit node *)
    add_edge c (srange s) [T_cur,med] [T_cur,c.ctx_return] (remret::rems)
      
  | S_break ->
    if c.ctx_break = [] then
      Exceptions.panic "break without a loop at %a" pp_range (srange s);
    (* goto edge (skip statement) *)
    add_edge c (srange s) [T_cur,pre] [T_cur,List.hd c.ctx_break] []
      
  | S_continue ->
    if c.ctx_continue = [] then
      Exceptions.panic "continue without a loop at %a" pp_range (srange s);
    (* goto edge (skip statement) *)
    add_edge c (srange s) [T_cur,pre] [T_cur,List.hd c.ctx_continue] []
      
  (* atomic statements: add an edge with the statement *)
      
  (* framework *)
  | S_assign _
  | S_assume _
  | S_add _
  | S_remove _
  | S_invalidate _
  | S_rename _
  | S_forget _
  | S_expand _
  | S_fold _

  (* universal *)
  | S_expression _
  | S_assert _
  | S_satisfy _
  | S_print
  | S_free _
    ->
    
    let tmps, assigns, s = extract_calls_stmt s in
    let adds, rems = mk_add_tmps tmps, mk_remove_tmps tmps in
    let blk = adds @ assigns @ [s] @ rems in
    add_edge c (srange s) [T_cur,pre] [T_cur,post] blk
      
  (* unknown *)

  | _ ->
     Exceptions.panic "cannot convert statement %a to CFG" pp_stmt s


(** Creates a new graph and fill-in with the given statement. *)
let convert_stmt ?(name="cfg") ?(ret:var option) (s:stmt) : stmt =
  (* create empty graph *)
  let cfg = CFG.create () in
  (* entry and exit nodes *)
  let entry = CFG.add_node cfg (mk_fresh_node_id (get_range_start (srange s))) ()
  and exit = CFG.add_node cfg (mk_fresh_node_id (get_range_end (srange s))) () in
  CFG.node_set_entry cfg entry (Some T_cur);
  CFG.node_set_exit  cfg exit  (Some T_cur);
  (* fill-in graph *)
  let c =
    { ctx_cfg = cfg;
      ctx_return = exit;
      ctx_break = [];
      ctx_continue = [];
      ctx_return_var = ret;
    }
  in
  add_stmt c entry exit s;
  let c =
    { cfg_graph = cfg;
      cfg_order = CFG.weak_topological_order cfg;
    }
  in
  if dump_dot then Pp.output_dot name ("tmp/"^name^".dot") c;
  mk_cfg c (srange s)

  
(** Converts a function AST to a CFG. *)
let convert_fundec (f:fundec) =
  (* get the variable to denote the return value *)
  let ret = match f.fun_return_type with
    | None -> None
    | Some t -> Some f.fun_return_var
  in
  (* convert the body in-place *)
  f.fun_body <- convert_stmt ~name:f.fun_orig_name ?ret f.fun_body;
  debug "CFG for function %s:@.%a@." f.fun_orig_name pp_stmt f.fun_body


(** Converts a full universal program. *)  
let convert_program (p:program) = 
  match p.prog_kind with
  | P_universal u ->
     List.iter convert_fundec u.universal_fundecs;
     { p with
       prog_kind = P_universal
           { u with
             universal_main = convert_stmt ~name:"__main__" u.universal_main;
           }
     }

  | _ ->        
     Exceptions.panic "cannot convert program to CFG"
    

(** From source to CFG. *)    
let parse_program (files: string list) : program =
  convert_program (Universal.Frontend.parse_program files)
  

(** Front-end registration *)
let () =
  register_frontend {
    lang = "cfg";
    parse = parse_program;
  }
