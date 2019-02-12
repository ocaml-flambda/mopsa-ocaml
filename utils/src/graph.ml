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

(** A simple graph library to represent control-flow graphs.
    Implementation.
 *)

open GraphSig


(*==========================================================================*)
                 (** {2 Ordered, hashable data types} *)
(*==========================================================================*)


(** Use the polymorphic comparison, equality, and hashing. *)
module IdGeneric(T : sig type t end) : (ID_TYPE with type t = T.t) =
struct
  type t = T.t
  let compare (x:t) (y:t) = Pervasives.compare x y
  let equal (x:t) (y:t) = (x = y)
  let hash (x:t) = Hashtbl.hash x
end
                
module IdInt = IdGeneric(struct type t = int end)
module IdString = IdGeneric(struct type t = string end)

module IdUnit : (ID_TYPE with type t = unit) =
struct
  type t = unit
  let compare x y = 0
  let equal x y = true
  let hash x = 0
end
              
module IdPair(A:ID_TYPE)(B:ID_TYPE) : (ID_TYPE with type t = A.t * B.t) =
struct
  type t = A.t * B.t
  let compare (a1,b1) (a2,b2) =
    match A.compare a1 a2 with
    | 0 -> B.compare b2 b2
    | x -> x
  let equal (a1,b1) (a2,b2) = A.equal a1 a2 && B.equal b1 b2
  let hash (a,b) = A.hash a + B.hash b
end

  

(*==========================================================================*)
                    (** {2 Nested lists} *)
(*==========================================================================*)


(** Printers. *)
let rec pp_nested_list pp_elem fmt = function
  | Simple x -> pp_elem fmt x
  | Composed l -> pp_nested_list_list pp_elem fmt l

and pp_nested_list_list pp_elem fmt l =
  Format.fprintf
    fmt "@[<hov 2>%a@]"
    (ListExt.fprint ListExt.printer_list (pp_nested_list pp_elem)) l



(*==========================================================================*)
                  (** {2 Graph Functor} *)
(*==========================================================================*)

              
module Make(P:P) : (S with module P = P) =
struct

                   
  (*========================================================================*)
                         (** {2 Types} *)
  (*========================================================================*)


  module P = P
  
  type node_id = P.NodeId.t
  type edge_id = P.EdgeId.t
  type port = P.Port.t
             
  module NodeHash = Hashtbl.Make(P.NodeId)
  module EdgeHash = Hashtbl.Make(P.EdgeId)                 

  module NodeMap = MapExt.Make(P.NodeId)
  module EdgeMap = MapExt.Make(P.EdgeId)                 

  module NodeSet = SetExt.Make(P.NodeId)
  module EdgeSet = SetExt.Make(P.EdgeId)                 
                 
  type ('n,'e) node = {
      n_id: node_id;
      mutable n_data: 'n;
      mutable n_in: (port * ('n,'e) edge) list;
      mutable n_out: (port * ('n,'e) edge) list;
    }
            
  and ('n,'e) edge = {
      e_id: edge_id;
      mutable e_data: 'e;
      mutable e_src: (port * ('n,'e) node) list;
      mutable e_dst: (port * ('n,'e) node) list;
    }

  and ('n,'e) graph = {
      mutable g_entries: (port * ('n,'e) node) list;
      mutable g_exits: (port * ('n,'e) node) list;
      mutable g_nodes: ('n,'e) node NodeHash.t;
      mutable g_edges: ('n,'e) edge EdgeHash.t;
    }


            
  (*========================================================================*)
                       (** {2 Internal utilities} *)
  (*========================================================================*)


  let node_eq  n1 n2 = P.NodeId.equal n1.n_id n2.n_id
  let node_neq n1 n2 = not (P.NodeId.equal n1.n_id n2.n_id)
  let edge_eq  e1 e2 = P.EdgeId.equal e1.e_id e2.e_id
  let edge_neq e1 e2 = not (P.EdgeId.equal e1.e_id e2.e_id)
  let port_eq  t1 t2  = P.Port.equal t1 t2
  let port_neq t1 t2  = not ( P.Port.equal t1 t2)
  let port_node_eq  (t1,n1) (t2,n2) = node_eq  n1 n2 && port_eq  t1 t2
  let port_node_neq (t1,n1) (t2,n2) = node_neq n1 n2 || port_neq t1 t2
  let port_edge_eq  (t1,e1) (t2,e2) = edge_eq  e1 e2 && port_eq  t1 t2
  let port_edge_neq (t1,e1) (t2,e2) = edge_neq e1 e2 || port_neq t1 t2

  let node_compare n1 n2 = P.NodeId.compare n1.n_id n2.n_id
  let edge_compare e1 e2 = P.EdgeId.compare e1.e_id e2.e_id
  let port_compare t1 t2 = P.Port.compare t1 t2

  let port_node_compare (t1,n1) (t2,n2) =
    match port_compare t1 t2 with 0 -> node_compare n1 n2 | x -> x

  let port_edge_compare (t1,e1) (t2,e2) =
    match port_compare t1 t2 with 0 -> edge_compare e1 e2 | x -> x

  let filter_port port l =
    List.map snd (List.filter (fun (port',_) -> port_eq port port') l)

                         

  (*========================================================================*)
                         (** {2 Construction} *)
  (*========================================================================*)


  let create () = {
      g_entries = [];
      g_exits = [];
      g_nodes = NodeHash.create 16;
      g_edges = EdgeHash.create 16;
    }

  let add_node g id ?(inc=[]) ?(out=[]) ?entry ?exit data =
    if NodeHash.mem g.g_nodes id then
      invalid_arg "Node identifier already present in Graph.add_node";
    let n = {
        n_id = id;
        n_data = data;
        n_in = inc;
        n_out = out;
      }
    in
    List.iter (fun (port,e) -> e.e_dst <- (port,n)::e.e_dst) inc;
    List.iter (fun (port,e) -> e.e_src <- (port,n)::e.e_src) out;
    (match entry with
     | Some entry -> g.g_entries <- (entry,n)::g.g_entries
     | None -> ()
    );
    (match exit with
     | Some exit -> g.g_exits <- (exit,n)::g.g_exits
     | None -> ()
    );
    NodeHash.add g.g_nodes id n;
    n
    
  let add_edge g id ?(src=[]) ?(dst=[]) data =
    if EdgeHash.mem g.g_edges id then
      invalid_arg "Edge identifier already present in Graph.add_node";
    let e = {
        e_id = id;
        e_data = data;
        e_src = src;
        e_dst = dst;
      }
    in
    List.iter (fun (port,n) -> n.n_out <- (port,e):: n.n_out) src;
    List.iter (fun (port,n) -> n.n_in <- (port,e):: n.n_in) dst;
    EdgeHash.add g.g_edges id e;
    e

  let remove_node g n =
    if NodeHash.mem g.g_nodes n.n_id then (
      List.iter (fun (_,e) ->
          e.e_dst <- List.filter (fun (_,n') -> node_neq n n') e.e_dst
        ) n.n_in;
      List.iter (fun (_,e) ->
          e.e_src <- List.filter (fun (_,n') -> node_neq n n') e.e_src
        ) n.n_out;
      n.n_in <- [];
      n.n_out <- [];
      NodeHash.remove g.g_nodes n.n_id;
      g.g_entries <- List.filter (fun (_,n') -> node_neq n n') g.g_entries;
      g.g_exits <- List.filter (fun (_,n') -> node_neq n n') g.g_exits
    )

  let remove_edge g e =
    if EdgeHash.mem g.g_edges e.e_id then (
      List.iter (fun (_,n) ->
          n.n_out <- List.filter (fun (_,e') -> edge_neq e e') n.n_out
        ) e.e_src;
      List.iter (fun (_,n) ->
          n.n_in <- List.filter (fun (_,e') -> edge_neq e e') n.n_in
        ) e.e_dst;
      e.e_src <- [];
      e.e_dst <- [];
      EdgeHash.remove g.g_edges e.e_id;
    )
    
    
  let node_set_entry g n entry =
    g.g_entries <- List.filter (fun (_,n') -> node_neq n n') g.g_entries;
    (match entry with
     | Some entry -> g.g_entries <- (entry,n)::g.g_entries
     | None -> ()
    )

  let node_set_exit g n exit =
    g.g_exits <- List.filter (fun (_,n') -> node_neq n n') g.g_exits;
    (match exit with
     | Some exit -> g.g_exits <- (exit,n)::g.g_exits
     | None -> ()
    )


  let node_add_in n port e =
    n.n_in  <- (port,e)::n.n_in;
    e.e_dst <- (port,n)::e.e_dst

  let node_add_out n port e =
    n.n_out <- (port,e)::n.n_out;
    e.e_src <- (port,n)::e.e_src

  let node_add_in_list n v =
    List.iter (fun (port,e) -> node_add_in n port e) v

  let node_add_out_list n v =
    List.iter (fun (port,e) -> node_add_out n port e) v

  let edge_add_src e port n = node_add_out n port e

  let edge_add_dst e port n = node_add_in n port e
    
  let edge_add_src_list e v =
    List.iter (fun (port,n) -> node_add_out n port e) v

  let edge_add_dst_list e v =
    List.iter (fun (port,n) -> node_add_in n port e) v

    
  let node_remove_in_port n port e =
    n.n_in  <- List.filter (port_edge_neq (port,e)) n.n_in;
    e.e_dst <- List.filter (port_node_neq (port,n)) e.e_dst

  let node_remove_out_port n port e =
    e.e_src <- List.filter (port_node_neq (port,n)) e.e_src;
    n.n_out <- List.filter (port_edge_neq (port,e)) n.n_out

  let node_remove_in n e =
    n.n_in  <- List.filter (fun (_,e') -> edge_neq e e') n.n_in;
    e.e_dst <- List.filter (fun (_,n') -> node_neq n n') e.e_dst
    
  let node_remove_out n e =
    n.n_out <- List.filter (fun (_,e') -> edge_neq e e') n.n_out;
    e.e_src <- List.filter (fun (_,n') -> node_neq n n') e.e_src

  let node_remove_all_in n =
    List.iter (fun (port,e) ->
        e.e_dst <- List.filter (port_node_neq (port,n)) e.e_dst
      ) n.n_in;
    n.n_in <- []
    
  let node_remove_all_out n =
    List.iter (fun (port,e) ->
        e.e_src <- List.filter (port_node_neq (port,n)) e.e_src
      ) n.n_out;
    n.n_out <- []

  let edge_remove_src_port e port n = node_remove_out_port n port e

  let edge_remove_dst_port e port n = node_remove_in_port n port e
    
  let edge_remove_src e n = node_remove_out n e

  let edge_remove_dst e n = node_remove_in n e
    
  let edge_remove_all_src e =
    List.iter (fun (port,n) ->
        n.n_out <- List.filter (port_edge_neq (port,e)) n.n_out
      ) e.e_src;
    e.e_src <- []
    
  let edge_remove_all_dst e =
    List.iter (fun (port,n) ->
        n.n_in <- List.filter (port_edge_neq (port,e)) n.n_in
      ) e.e_dst;
    e.e_dst <- []


  let node_set_in n v =
    node_remove_all_in n;
    node_add_in_list n v

  let node_set_out n v =
    node_remove_all_in n;
    node_add_out_list n v

  let edge_set_src e v =
    edge_remove_all_src e;
    edge_add_src_list e v
    
  let edge_set_dst e v =
    edge_remove_all_dst e;
    edge_add_dst_list e v
        
            
  (*========================================================================*)
                         (** {2 Exploration} *)
  (*========================================================================*)

    
  let node_list g = NodeHash.fold (fun _ n acc -> n::acc) g.g_nodes [] 
  let edge_list g = EdgeHash.fold (fun _ e acc -> e::acc) g.g_edges [] 

  let node_set g =
    NodeSet.of_list (NodeHash.fold (fun id _ acc -> id::acc) g.g_nodes [])

  let edge_set g =
    EdgeSet.of_list (EdgeHash.fold (fun id _ acc -> id::acc) g.g_edges [])

  let map_nodes f g =
    NodeHash.fold
      (fun id n acc -> NodeMap.add id (f id n) acc) g.g_nodes
      NodeMap.empty
    
  let map_edges f g =
    EdgeHash.fold
      (fun id n acc -> EdgeMap.add id (f id n) acc) g.g_edges
      EdgeMap.empty

  let node_map g = map_nodes (fun _ n -> n) g
  let edge_map g = map_edges (fun _ e -> e) g

  let has_node g id = NodeHash.mem g.g_nodes id
  let has_edge g id = EdgeHash.mem g.g_edges id
                    
  let get_node g id = NodeHash.find g.g_nodes id 
  let get_edge g id = EdgeHash.find g.g_edges id

  let entries g = g.g_entries
  let exits g = g.g_exits

  let edge_id e = e.e_id
  let edge_data e = e.e_data
  let edge_set_data e data = e.e_data <- data
  let edge_src e = e.e_src
  let edge_dst e = e.e_dst
  let edge_src_port e port = filter_port port (edge_src e)
  let edge_dst_port e port = filter_port port (edge_dst e)
  let edge_src_size e = List.length (edge_src e)
  let edge_dst_size e = List.length (edge_dst e)
  let edge_src_port_size e port = List.length (edge_src_port e port)
  let edge_dst_port_size e port = List.length (edge_dst_port e port)
                           
  let node_id n = n.n_id
  let node_data n = n.n_data
  let node_set_data n data = n.n_data <- data
  let node_in n = n.n_in
  let node_out n = n.n_out
  let node_in_port n port = filter_port port (node_in n)
  let node_out_port n port = filter_port port (node_out n)
  let node_in_size n = List.length (node_in n)
  let node_out_size n = List.length (node_out n)
  let node_in_port_size n port = List.length (node_in_port n port)
  let node_out_port_size n port = List.length (node_out_port n port)

  let node_entry_port g n =
    try Some (fst (List.find (fun (_,n') -> node_eq n n') g.g_entries))
    with Not_found -> None

  let node_exit_port g n =
    try Some (fst (List.find (fun (_,n') -> node_eq n n') g.g_exits))
    with Not_found -> None

  let node_has_out n e =
    List.exists (fun (_,e') -> edge_eq e e') n.n_out
                       
  let node_has_out_port n port e =
    List.exists (port_edge_eq (port,e)) n.n_out
                       
  let node_has_in n e =
    List.exists (fun (_,e') -> edge_eq e e') n.n_in
                       
  let node_has_in_port n port e =
    List.exists (port_edge_eq (port,e)) n.n_in

  let edge_has_src e n =
    List.exists (fun (_,n') -> node_eq n n') e.e_src
    
  let edge_has_src_port e port n =
    List.exists (port_node_eq (port,n)) e.e_src

  let edge_has_dst e n =
    List.exists (fun (_,n') -> node_eq n n') e.e_dst
    
  let edge_has_dst_port e port n =
    List.exists (port_node_eq (port,n)) e.e_dst

  let node_out_nodes n =
    List.concat
      (List.map (fun (port1,e) ->
           List.map (fun (port2,n2) -> (port1,e,port2,n2)) e.e_dst
         ) n.n_out)
    
  let node_in_nodes n =
    List.concat
      (List.map (fun (port1,e) ->
           List.map (fun (port2,n2) -> (n2,port2,e,port1)) e.e_src
         ) n.n_in)
    
  let node_out_nodes_port n port1 port2 =
    List.concat
      (List.map (fun (port,e) ->
           if port_neq port port1 then []
           else
             List.map
               (fun (_,n2) -> (e,n2))
               (List.filter (fun (port,n2) -> port_eq port port2) e.e_dst)
         ) n.n_out)
    
    
  let node_in_nodes_port n port1 port2 =
    List.concat
      (List.map (fun (port,e) ->
           if port_neq port port1 then []
           else
             List.map
               (fun (_,n2) -> (n2,e))
               (List.filter (fun (port,n2) -> port_eq port port2) e.e_src)
         ) n.n_in)
    
  let node_has_node_out n1 n2 =
    List.exists
      (fun (_,e) -> List.exists (fun (_,n) -> node_eq n n2) e.e_dst)
      n1.n_out

  let node_has_node_in n1 n2 =
    List.exists
      (fun (_,e) -> List.exists (fun (_,n) -> node_eq n n2) e.e_src)
      n1.n_in

  let node_has_node_out_port n1 port1 port2 n2 =
    List.exists
      (fun (port,e) ->
        port_eq port port1 &&
          List.exists (port_node_eq (port2,n2)) e.e_dst
      ) n1.n_out

  let node_has_node_in_port n1 port1 port2 n2 =
    List.exists
      (fun (port,e) ->
        port_eq port port1 &&
          List.exists (port_node_eq (port2,n2)) e.e_src
      ) n1.n_in




  let node_add_in_unique n port e =
    if not (node_has_in_port n port e) then node_add_in n port e
    
  let node_add_out_unique n port e =
    if not (node_has_out_port n port e) then node_add_out n port e

  let node_add_in_list_unique n v =
    List.iter (fun (port,e) -> node_add_in_unique n port e) v

  let node_add_out_list_unique n v =
    List.iter (fun (port,e) -> node_add_out_unique n port e) v

  let edge_add_src_unique e port n = node_add_out_unique n port e

  let edge_add_dst_unique e port n = node_add_in_unique n port e
    
  let edge_add_src_list_unique e v =
    List.iter (fun (port,n) -> node_add_out_unique n port e) v

  let edge_add_dst_list_unique e v =
    List.iter (fun (port,n) -> node_add_in_unique n port e) v


  let node_set_in_unique n v =
    node_remove_all_in n;
    node_add_in_list_unique n v

  let node_set_out_unique n v =
    node_remove_all_in n;
    node_add_out_list_unique n v

  let edge_set_src_unique e v =
    edge_remove_all_src e;
    edge_add_src_list_unique e v
    
  let edge_set_dst_unique e v =
    edge_remove_all_dst e;
    edge_add_dst_list_unique e v

    
    
  (*========================================================================*)
                        (** {2 Maps and folds} *)
  (*========================================================================*)


  let clone_map nmap emap g =
    let gg = create () in
    (* map data *)
    NodeHash.iter
      (fun id n -> ignore (add_node gg id (nmap n.n_data))) g.g_nodes;
    EdgeHash.iter
      (fun id e -> ignore (add_edge gg id (emap e.e_data))) g.g_edges;
    (* fix in/out, src/dst *)
    NodeHash.iter
      (fun id n ->
        let nn = get_node gg id in
        nn.n_in  <- List.map (fun (port,e) -> port, get_edge gg e.e_id) n.n_in;
        nn.n_out <- List.map (fun (port,e) -> port, get_edge gg e.e_id) n.n_out
      ) g.g_nodes;
    EdgeHash.iter
      (fun id e ->
        let ee = get_edge gg id in
        ee.e_src <- List.map (fun (port,n) -> port, get_node gg n.n_id) e.e_src;
        ee.e_dst <- List.map (fun (port,n) -> port, get_node gg n.n_id) e.e_dst
      ) g.g_edges;
    gg.g_entries <-
      List.map (fun (port,n) -> port, get_node gg n.n_id) g.g_entries;
    gg.g_exits   <-
      List.map (fun (port,n) -> port, get_node gg n.n_id) g.g_exits;
    gg

  let clone g = clone_map (fun n -> n) (fun e -> e) g
    
  let transpose g =
    NodeHash.iter
      (fun _ n -> let a = n.n_in in n.n_in <- n.n_out; n.n_out <- a)
      g.g_nodes;
    EdgeHash.iter
      (fun _ e -> let a = e.e_src in e.e_src <- e.e_dst; e.e_dst <- a)
      g.g_edges;
    let a = g.g_entries in g.g_entries <- g.g_exits; g.g_exits <- a
                           

  let iter_nodes f g = NodeHash.iter (fun id n -> f id n) g.g_nodes
  let iter_edges f g = EdgeHash.iter (fun id e -> f id e) g.g_edges
                     
  let fold_nodes f g a = NodeHash.fold (fun id n a -> f id n a) g.g_nodes a
  let fold_edges f g a = EdgeHash.fold (fun id e a -> f id e a) g.g_edges a

  let map_nodes_ordered f g = NodeMap.mapi f (node_map g)
  let map_edges_ordered f g = EdgeMap.mapi f (edge_map g)
                             
  let iter_nodes_ordered f g = NodeMap.iter f (node_map g)
  let iter_edges_ordered f g = EdgeMap.iter f (edge_map g)

  let fold_nodes_ordered f g a = NodeMap.fold f (node_map g) a
  let fold_edges_ordered f g a = EdgeMap.fold f (edge_map g) a
 

                               
  (*========================================================================*)
                         (** {Simplification} *)
  (*========================================================================*)

                               
  let remove_orphan g =
    iter_nodes
      (fun _ n -> if n.n_in = [] && n.n_out = [] then remove_node g n)
      g;
    iter_edges
      (fun _ e -> if e.e_src = [] && e.e_dst = [] then remove_edge g e)
      g



                                   
  (*========================================================================*)
                      (** {Topological ordering} *)
  (*========================================================================*)


  (* Bourdoncle's algorithm to compute a weak topological order by 
     hierarchical decomposition into strongly connected components
     (FMPA'93, p. 128-141, 1993, Springer).
   *)
  let weak_topological_order g =
    let stack = Stack.create () in
    let index = NodeHash.create 16 in
    let idx = ref 0 in
    (* Tarjan's strongly connected component algorithm *)
    let rec visit node acc =
      Stack.push (node_id node) stack;
      incr idx;
      let orghead = !idx in
      NodeHash.replace index (node_id node) orghead;
      let acc,head,loop =
        List.fold_left
          (fun (acc,head,loop) (_,_,_,succ) ->
            let acc, min =
              if NodeHash.mem index (node_id succ)
              then acc, NodeHash.find index (node_id succ)
              else visit succ acc
            in
            if min >= 0 && min <= head then acc, min, true
            else acc, head ,loop
          )            
          (acc,orghead,false)
          (node_out_nodes node)
      in
      let acc = 
        if head = orghead then (
          NodeHash.replace index (node_id node) (-1);
          let elem = Stack.pop stack in
          if loop then
            let rec pop_all elem =
              if not (P.NodeId.equal (node_id node) elem) then (
                NodeHash.remove index elem;
                pop_all (Stack.pop stack)
              )
            in
            pop_all elem;
            (Composed (component node))::acc
          else
            (Simple node)::acc
        )
        else acc
      in
      acc, head
    (* recursively decompose a strongly connected component *)      
    and component node =
      let acc =
        List.fold_left
          (fun acc (_,_,_,succ) ->
            if NodeHash.mem index (node_id succ) then acc
            else fst (visit succ acc)
          )
          []
          (node_out_nodes node)
      in
      (Simple node)::acc
    in
    List.fold_left
      (fun acc (_,node) ->
        if NodeHash.mem index (node_id node) then acc
        else fst (visit node acc)
      )
      [] (entries g)
    

  let widening_points l =
    let rec add_head acc = function
      | (Simple x)::_ -> x::acc
      | _ -> acc
    and iter acc = function
      | Simple _ -> acc
      | Composed l -> List.fold_left iter (add_head acc l) l
    in
    List.fold_left iter [] l
    
    
    
  (*========================================================================*)
                         (** {2 Printing} *)
  (*========================================================================*)


  type ('n,'e) printer = {
      print_node: Format.formatter -> ('n,'e) node -> unit;
      print_edge: Format.formatter -> ('n,'e) edge  -> unit;
      print_src: Format.formatter -> ('n,'e) node -> port -> ('n,'e) edge
                 -> unit;
      print_dst: Format.formatter -> ('n,'e) edge -> port -> ('n,'e) node
                 -> unit;
      print_entry: Format.formatter -> ('n,'e) node -> port -> unit;
      print_exit: Format.formatter -> ('n,'e) node -> port -> unit;
    }
    

  let print p fmt g =
    (* ordering *)
    let nodes = NodeHash.fold NodeMap.add g.g_nodes NodeMap.empty in
    (* ensure that each edge is printer only once *)
    let edges = EdgeHash.create 16 in
    (* print each node *)
    NodeMap.iter
      (fun id n ->
        (match node_entry_port g n with
         | None -> ()
         | Some port -> p.print_entry fmt n port
        );
        p.print_node fmt n;
        (match node_exit_port g n with
         | None -> ()
         | Some port -> p.print_exit fmt n port
        );
        List.iter
          (fun (_,e) ->
            if not (EdgeHash.mem edges e.e_id) then (
              EdgeHash.add edges e.e_id ();
              List.iter
                (fun (port,n) -> p.print_src fmt n port e)
                (List.sort port_node_compare e.e_src);
              p.print_edge fmt e;
              List.iter
                (fun (port,n) -> p.print_dst fmt e port n)
                (List.sort port_node_compare e.e_dst)
            )
          )
          (List.sort port_edge_compare n.n_out)
      ) nodes

    

  type ('n,'e) dot_printer = {
      dot_pp_node: Format.formatter -> ('n,'e) node -> unit;
      dot_pp_edge: Format.formatter -> ('n,'e) edge -> unit;
      dot_pp_port: Format.formatter -> port -> unit;
      dot_filter_node: ('n,'e) node -> bool;
      dot_filter_edge: ('n,'e) edge -> bool;
      dot_filter_port: port -> bool;
    }
                           
  let print_dot p name fmt g =
    (* printing with escaped new lines *)
    let buf = Buffer.create 16 in
    let sfmt = Format.formatter_of_buffer buf in
    let to_string p x =
      Buffer.clear buf;
      Format.fprintf sfmt "@[<v>%a@]@?" p x;
      let s = Buffer.contents buf in
      let ss = String.split_on_char '\n' s in
      if List.length ss <= 1 then s
      else (String.concat "\\l" ss)^"\\l"
    in      
    (* numbering node and edge id *)
    let nid = NodeHash.create 16
    and eid = EdgeHash.create 16
    and count = ref 0 in
    EdgeHash.iter
      (fun id _ -> incr count; EdgeHash.add eid id !count) g.g_edges;    
    (* header *)
    Format.fprintf fmt "digraph %s {\n" name;
    (* emit dot nodes for nodes and edges *)
    NodeHash.iter
      (fun id n ->
        if p.dot_filter_node n then (
          incr count;
          NodeHash.add nid id !count;
          Format.fprintf
            fmt "  n%i [label=\"%s\"];\n"
            !count (to_string p.dot_pp_node n)
        )
      ) g.g_nodes;
    EdgeHash.iter
      (fun id e ->
        if p.dot_filter_edge e then (
          incr count;
          EdgeHash.add eid id !count;
          Format.fprintf
            fmt "  n%i [shape=box label=\"%s\"];\n"
            !count (to_string p.dot_pp_edge e)
        )
      ) g.g_edges;
    (* emit dot edges to connect nodes and edges *)
    EdgeHash.iter
      (fun id e ->
        let did1 = EdgeHash.find eid id in
        List.iter
          (fun (port,n) ->
            if p.dot_filter_node n && p.dot_filter_port port && p.dot_filter_edge e then (
              let did2 = NodeHash.find nid n.n_id in
              Format.fprintf
                fmt "  n%i -> n%i [label=\"%s\"];\n"
                did2 did1 (to_string p.dot_pp_port port)
            )
          ) e.e_src;
        List.iter
          (fun (port,n) ->
            if p.dot_filter_node n && p.dot_filter_port port && p.dot_filter_edge e then (
              let did2 = NodeHash.find nid n.n_id in
              Format.fprintf
                fmt "  n%i -> n%i [label=\"%s\"];\n"
                did1 did2 (to_string p.dot_pp_port port)
            )
          ) e.e_dst
      ) g.g_edges;
    (* entry / exit nodes *)
    List.iter
      (fun (port,n) ->
        if p.dot_filter_node n && p.dot_filter_port port then (
          incr count;
          let did = NodeHash.find nid n.n_id in
          Format.fprintf
            fmt "  n%i [shape=point label=\"\"];\n  n%i -> n%i [label=\"%s\"];\n"
            !count !count did (to_string p.dot_pp_port port)
        )
      ) g.g_entries;
    List.iter
      (fun (port,n) ->
        if p.dot_filter_node n && p.dot_filter_port port then (
          incr count;
          let did = NodeHash.find nid n.n_id in
          Format.fprintf
            fmt "  n%i [shape=point label=\"\"];\n  n%i -> n%i [label=\"%s\"];\n"
            !count did !count (to_string p.dot_pp_port port)
        )
      ) g.g_exits;
    (* footer *)
    Format.fprintf fmt "}\n"
                     
    
end
              
