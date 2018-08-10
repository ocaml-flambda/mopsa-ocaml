(****************************************************************************)
(*                   Copyright (C) 2018 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
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
module IdGeneric(T : sig type t end) =
  (struct
    type t = T.t
    let compare (x:t) (y:t) = compare x y
    let equal (x:t) (y:t) = (x = y)
    let hash (x:t) = Hashtbl.hash x
  end: ID_TYPE with type t = T.t)        
                
module IdInt = IdGeneric(struct type t = int end)
module IdString = IdGeneric(struct type t = string end)

module IdUnit =
  (struct
    type t = unit
    let compare x y = 0
    let equal x y = true
    let hash x = 0
  end: ID_TYPE with type t = unit)
              
module IdPair(A:ID_TYPE)(B:ID_TYPE) =
  (struct
    type t = A.t * B.t
    let compare (a1,b1) (a2,b2) =
      match A.compare a1 a2 with
      | 0 -> B.compare b2 b2
      | x -> x
    let equal (a1,b1) (a2,b2) = A.equal a1 a2 && B.equal b1 b2
    let hash (a,b) = A.hash a + B.hash b
  end: ID_TYPE with type t = A.t * B.t)

  

(*==========================================================================*)
                  (** {2 Graph Functor} *)
(*==========================================================================*)

              
module Make(P:P) = (struct

                   
  (*========================================================================*)
                         (** {2 Types} *)
  (*========================================================================*)


  module P = P
  
  type node_id = P.NodeId.t
  type edge_id = P.EdgeId.t
  type tag = P.Tag.t
             
  module NodeHash = Hashtbl.Make(P.NodeId)
  module EdgeHash = Hashtbl.Make(P.EdgeId)                 

  module NodeMap = MapExt.Make(P.NodeId)
  module EdgeMap = MapExt.Make(P.EdgeId)                 

  module NodeSet = SetExt.Make(P.NodeId)
  module EdgeSet = SetExt.Make(P.EdgeId)                 
                 
  type ('n,'e) node = {
      n_id: node_id;
      mutable n_data: 'n;
      mutable n_in: (tag * ('n,'e) edge) list;
      mutable n_out: (tag * ('n,'e) edge) list;
    }
            
  and ('n,'e) edge = {
      e_id: edge_id;
      mutable e_data: 'e;
      mutable e_src: (tag * ('n,'e) node) list;
      mutable e_dst: (tag * ('n,'e) node) list;
    }

  and ('n,'e) graph = {
      mutable g_entries: (tag * ('n,'e) node) list;
      mutable g_exits: (tag * ('n,'e) node) list;
      mutable g_nodes: ('n,'e) node NodeHash.t;
      mutable g_edges: ('n,'e) edge EdgeHash.t;
    }


            
  (*========================================================================*)
                       (** {2 Internal utilities} *)
  (*========================================================================*)


  let node_eq  n1 n2 = (P.NodeId.compare n1.n_id n2.n_id == 0)
  let node_neq n1 n2 = (P.NodeId.compare n1.n_id n2.n_id != 0)
  let edge_eq  e1 e2 = (P.EdgeId.compare e1.e_id e2.e_id == 0)
  let edge_neq e1 e2 = (P.EdgeId.compare e1.e_id e2.e_id != 0)
  let tag_eq  t1 t2  = P.Tag.compare t1 t2 == 0
  let tag_neq t1 t2  = P.Tag.compare t1 t2 != 0
  let tag_node_eq  (t1,n1) (t2,n2) = node_eq  n1 n2 && tag_eq  t1 t2
  let tag_node_neq (t1,n1) (t2,n2) = node_neq n1 n2 || tag_neq t1 t2
  let tag_edge_eq  (t1,e1) (t2,e2) = edge_eq  e1 e2 && tag_eq  t1 t2
  let tag_edge_neq (t1,e1) (t2,e2) = edge_neq e1 e2 || tag_neq t1 t2

  let filter_tag tag l =
    List.map snd (List.filter (fun (tag',_) -> tag_eq tag tag') l)

                         

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
    List.iter (fun (tag,e) -> e.e_dst <- (tag,n)::e.e_dst) inc;
    List.iter (fun (tag,e) -> e.e_src <- (tag,n)::e.e_src) out;
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
    List.iter (fun (tag,n) -> n.n_out <- (tag,e):: n.n_out) src;
    List.iter (fun (tag,n) -> n.n_in <- (tag,e):: n.n_in) dst;
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


  let node_add_in n tag e =
    n.n_in  <- (tag,e)::n.n_in;
    e.e_dst <- (tag,n)::e.e_dst

  let node_add_out n tag e =
    n.n_out <- (tag,e)::n.n_out;
    e.e_src <- (tag,n)::e.e_src

  let node_remove_tag_in n tag e =
    n.n_in  <- List.filter (tag_edge_neq (tag,e)) n.n_in;
    e.e_dst <- List.filter (tag_node_neq (tag,n)) e.e_dst

  let node_remove_tag_out n tag e =
    e.e_src <- List.filter (tag_node_neq (tag,n)) e.e_src;
    n.n_out <- List.filter (tag_edge_neq (tag,e)) n.n_out

  let node_remove_in n e =
    n.n_in  <- List.filter (fun (_,e') -> edge_neq e e') n.n_in;
    e.e_dst <- List.filter (fun (_,n') -> node_neq n n') e.e_dst
    
  let node_remove_out n e =
    n.n_out <- List.filter (fun (_,e') -> edge_neq e e') n.n_out;
    e.e_src <- List.filter (fun (_,n') -> node_neq n n') e.e_src

  let node_remove_all_in n =
    List.iter (fun (tag,e) ->
        e.e_dst <- List.filter (tag_node_neq (tag,n)) e.e_dst
      ) n.n_in;
    n.n_in <- []
    
  let node_remove_all_out n =
    List.iter (fun (tag,e) ->
        e.e_src <- List.filter (tag_node_neq (tag,n)) e.e_src
      ) n.n_out;
    n.n_out <- []
    
  let edge_remove_all_src e =
    List.iter (fun (tag,n) ->
        n.n_out <- List.filter (tag_edge_neq (tag,e)) n.n_out
      ) e.e_src;
    e.e_src <- []
    
  let edge_remove_all_dst e =
    List.iter (fun (tag,n) ->
        n.n_in <- List.filter (tag_edge_neq (tag,e)) n.n_in
      ) e.e_dst;
    e.e_dst <- []
    
        
            
  (*========================================================================*)
                         (** {2 Exploration} *)
  (*========================================================================*)

    
  let node_list g = NodeHash.fold (fun _ n acc -> n::acc) g.g_nodes [] 
  let edge_list g = EdgeHash.fold (fun _ e acc -> e::acc) g.g_edges [] 

  let node_set g =
    NodeSet.of_list (NodeHash.fold (fun id _ acc -> id::acc) g.g_nodes [])

  let edge_set g =
    EdgeSet.of_list (EdgeHash.fold (fun id _ acc -> id::acc) g.g_edges [])

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
  let edge_tag_src e tag = filter_tag tag (edge_src e)
  let edge_tag_dst e tag = filter_tag tag (edge_dst e)
                           

  let node_id n = n.n_id
  let node_data n = n.n_data
  let node_set_data n data = n.n_data <- data
  let node_in n = n.n_in
  let node_out n = n.n_out
  let node_tag_in n tag = filter_tag tag (node_in n)
  let node_tag_out n tag = filter_tag tag (node_out n)

  let node_entry_tag g n =
    try Some (fst (List.find (fun (_,n') -> node_eq n n') g.g_entries))
    with Not_found -> None

  let node_exit_tag g n =
    try Some (fst (List.find (fun (_,n') -> node_eq n n') g.g_exits))
    with Not_found -> None

  let node_has_edge_out n e =
    List.exists (fun (_,e') -> edge_eq e e') n.n_out
                       
  let node_has_edge_tag_out n tag e =
    List.exists (tag_edge_eq (tag,e)) n.n_out
                       
  let node_has_edge_in n e =
    List.exists (fun (_,e') -> edge_eq e e') n.n_in
                       
  let node_has_edge_tag_in n tag e =
    List.exists (tag_edge_eq (tag,e)) n.n_in

  let edge_has_node_src e n =
    List.exists (fun (_,n') -> node_eq n n') e.e_src
    
  let edge_has_node_tag_src e tag n =
    List.exists (tag_node_eq (tag,n)) e.e_src

  let edge_has_node_dst e n =
    List.exists (fun (_,n') -> node_eq n n') e.e_dst
    
  let edge_has_node_tag_dst e tag n =
    List.exists (tag_node_eq (tag,n)) e.e_dst

  let node_out_nodes n =
    List.concat
      (List.map (fun (tag1,e) ->
           List.map (fun (tag2,n2) -> (tag1,e,tag2,n2)) e.e_dst
         ) n.n_out)
    
  let node_in_nodes n =
    List.concat
      (List.map (fun (tag1,e) ->
           List.map (fun (tag2,n2) -> (n2,tag2,e,tag1)) e.e_src
         ) n.n_in)
    
  let node_tag_out_nodes n tag1 tag2 =
    List.concat
      (List.map (fun (tag,e) ->
           if tag_neq tag tag1 then []
           else
             List.map
               (fun (_,n2) -> (e,n2))
               (List.filter (fun (tag,n2) -> tag_eq tag tag2) e.e_dst)
         ) n.n_out)
    
    
  let node_tag_in_nodes n tag1 tag2 =
    List.concat
      (List.map (fun (tag,e) ->
           if tag_neq tag tag1 then []
           else
             List.map
               (fun (_,n2) -> (n2,e))
               (List.filter (fun (tag,n2) -> tag_eq tag tag2) e.e_src)
         ) n.n_in)
    
  let node_has_node_out n1 n2 =
    List.exists
      (fun (_,e) -> List.exists (fun (_,n) -> node_eq n n2) e.e_dst)
      n1.n_out

  let node_has_node_in n1 n2 =
    List.exists
      (fun (_,e) -> List.exists (fun (_,n) -> node_eq n n2) e.e_src)
      n1.n_in

  let node_has_node_tag_out n1 tag1 tag2 n2 =
    List.exists
      (fun (tag,e) ->
        tag_eq tag tag1 &&
          List.exists (tag_node_eq (tag2,n2)) e.e_dst
      ) n1.n_out

  let node_has_node_tag_in n1 tag1 tag2 n2 =
    List.exists
      (fun (tag,e) ->
        tag_eq tag tag1 &&
          List.exists (tag_node_eq (tag2,n2)) e.e_src
      ) n1.n_in


    
    
  (*========================================================================*)
                       (** {2 Global operations} *)
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
        nn.n_in  <- List.map (fun (tag,e) -> tag, get_edge gg e.e_id) n.n_in;
        nn.n_out <- List.map (fun (tag,e) -> tag, get_edge gg e.e_id) n.n_out
      ) g.g_nodes;
    EdgeHash.iter
      (fun id e ->
        let ee = get_edge gg id in
        ee.e_src <- List.map (fun (tag,n) -> tag, get_node gg n.n_id) e.e_src;
        ee.e_dst <- List.map (fun (tag,n) -> tag, get_node gg n.n_id) e.e_dst
      ) g.g_edges;
    gg.g_entries <-
      List.map (fun (tag,n) -> tag, get_node gg n.n_id) g.g_entries;
    gg.g_exits   <-
      List.map (fun (tag,n) -> tag, get_node gg n.n_id) g.g_exits;
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
                           

  let iter_nodes f g = NodeHash.iter (fun _ n -> f n) g.g_nodes
  let iter_edges f g = EdgeHash.iter (fun _ e -> f e) g.g_edges


                                     (* TODO *)
                     
    
  (*========================================================================*)
                         (** {2 Printing} *)
  (*========================================================================*)


                                     (* TODO *)
    
end: S with module P = P)
                 
