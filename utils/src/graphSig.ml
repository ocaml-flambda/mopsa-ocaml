(****************************************************************************)
(*                   Copyright (C) 2018 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** A simple graph library to represent control-flow graphs.

We actually use an oriented hyper-graph structure: an edge connects
a set of (source) nodes to a set of (destination) nodes.
Equivalently, this can be seen as a bipartite graph where nodes and
edges correspond to the two node partitions.

Nodes model program locations, where an invariant can be stored.
Edges model transfer functions for basic blocs.
An edge can have several source nodes, to model control-flow joins
at the entry of basic blocks. It is also possible to mode joins by
having several edges incoming into the same node.
An edge can have several destination nodes, to model conditionals with 
several outputs from a basic block. Alternatively, several edges outgoing
from the same node can be used to model conditionals.
We use "ports" to distinguish and classify between the different
source and destination nodes of an edge, and ports carry tags.
It is possible to have a node connected to the same edge several times,
either on equal or different ports.

Nodes and edges have unique identifiers, ordered to serve as map keys.
Each node and edge has a mutable data field of polymorphic type.
However, data attached to nodes and edges can also be maintained 
outside of the graph structure, using hash-tables or maps from identifiers 
to the actual data.

Graphs are mutable to make it easier to construct them.
It is expected that, after creation in the front-end, graphs remain 
unmodified (except maybe for the data fields).
 *)


(*==========================================================================*)
                 (** {2 Ordered, hashable data types} *)
(*==========================================================================*)


(** Data-type that can be used both in sets and maps, and hash-tables. *)
module type ID_TYPE = sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val hash: t -> int
end


                    
(*==========================================================================*)
                     (** {2 Parameter signature} *)
(*==========================================================================*)

    
module type P = sig

  module NodeId: ID_TYPE
  (** Unique node identifiers, that can serve as keys in maps.  *)
       
  module EdgeId: ID_TYPE
  (** Unique edge identifiers, that can serve as keys in maps.  *)

  module Port: ID_TYPE
  (** Edges connect to node through tagged ports. *)
     
end



(*==========================================================================*)
                  (** {2 Graph signature} *)
(*==========================================================================*)


              
module type S = sig

  
  (*========================================================================*)
                         (** {2 Types} *)
  (*========================================================================*)

  
  type ('node_data, 'edge_data) graph
  (** Type of graphs.
      Custom arbitrary data of type ['node_data] and ['edge_data] can be
      stored, respectively, in nodes and edges.
   *)

  type ('node_data, 'edge_data) node
  (** A node in the graph. *)
     
  type ('node_data, 'edge_data) edge
  (** An (hyper)-edge in the graph. *)

  module P: P
  type node_id = P.NodeId.t
  type edge_id = P.EdgeId.t
  type port = P.Port.t
  (** Export back types from the functor parameter. *)
              
  module NodeSet: SetExtSig.S with type elt = node_id
  module EdgeSet: SetExtSig.S with type elt = edge_id
  (** Sets constructed from ordered type parameters. *)
       
  module NodeMap: MapExtSig.S with type key = node_id
  module EdgeMap: MapExtSig.S with type key = edge_id
  (** Maps constructed from ordered type parameters. *)
       
                   
           
  (*========================================================================*)
                         (** {2 Construction} *)
  (*========================================================================*)

           
  val create: unit -> ('n,'e) graph
  (** Creates an empty graph. *)

  val add_node: ('n,'e) graph -> node_id
                -> ?inc:(port * ('n,'e) edge) list
                -> ?out:(port * ('n,'e) edge) list
                -> ?entry:port -> ?exit:port
                -> 'n -> ('n,'e) node
  (** Adds a node to the graph.
      Optionally connects the node to incoming or outgoing edges.
      Optionally sets as an entry or exit node on a port (defaults to None,
      i.e., not entry nor exit).
      The node identifier must be unique among the node in the graph;
      raises [Invalid_argument] otherwise.
   *)
    
  val add_edge: ('n,'e) graph -> edge_id
                -> ?src:(port * ('n,'e) node) list
                -> ?dst:(port * ('n,'e) node) list
                -> 'e -> ('n,'e) edge
  (** Adds an edge to the graph.
      Optionally connects the edge to source or destination nodes.
      The edge identifier must be unique among the edges in the graph;
      raises [Invalid_argument] otherwise.
   *)

    
  val remove_node: ('n,'e) graph -> ('n,'e) node -> unit
  (** Removes a node if it exists in the graph; otherwise, do nothing.
      All connections to incoming and outgoing edges are removed.
   *)
    
  val remove_edge: ('n,'e) graph -> ('n,'e) edge -> unit
  (** Removes an edge if it exists in the graph; otherwise, do nothing.
      All connections to source and destination nodes are removed.
   *)

    
  val node_set_entry: ('n,'e) graph -> ('n,'e) node -> port option -> unit
  (** Sets whether a node is an entry node on some port, or not (None). 
      A given node can be entry for a single port at a time; 
      previous entry ports are removed.
   *)

  val node_set_exit: ('n,'e) graph -> ('n,'e) node -> port option -> unit
  (** Sets whether a node is an exit node on some port, or not (None). 
      A given node can be exit on a single port at a time; 
      previous exit ports are removed.
   *)

    
  val node_add_in: ('n,'e) node -> port -> ('n,'e) edge -> unit
  (** Adds an incoming edge to the node, on the given port. 
      A node can be connected to the same edge several times, on
      different or equal ports.
      Use the [_unique] version to ensure that a node is connected to
      an given edge on a given port only once.
   *)

  val node_add_out: ('n,'e) node -> port -> ('n,'e) edge -> unit
  (** Adds an outgoing edge to the node, on the given port. *)

  val node_add_in_list: ('n,'e) node -> ((port * ('n,'e) edge) list) -> unit
  (** Adds incoming edges to the node, on the given ports. *)

  val node_add_out_list: ('n,'e) node -> ((port * ('n,'e) edge) list) -> unit
  (** Adds outgoing edges to the node, on the given ports. *)

  val edge_add_src: ('n,'e) edge -> port -> ('n,'e) node -> unit
  (** Adds a source node to the edge, on the given port. *)

  val edge_add_dst: ('n,'e) edge -> port -> ('n,'e) node -> unit
  (** Adds a destination node to the edge, on the given port. *)

  val edge_add_src_list: ('n,'e) edge -> ((port * ('n,'e) node) list) -> unit
  (** Adds source nodes to the edge, on the given ports. *)

  val edge_add_dst_list: ('n,'e) edge -> ((port * ('n,'e) node) list) -> unit
  (** Adds destination nodes to the edge, on the given ports. *)    


  val node_add_in_unique: ('n,'e) node -> port -> ('n,'e) edge -> unit
  (** Adds an incoming edge to the node, on the given port, 
      but only if not already present on this port.
      Does not prevent the node to be connected to the edge on
      other ports.
   *)

  val node_add_out_unique: ('n,'e) node -> port -> ('n,'e) edge -> unit
  (** Adds an outgoing edge to the node, on the given port,
      but only if not already present on this port.
   *)

  val node_add_in_list_unique: ('n,'e) node -> ((port * ('n,'e) edge) list) -> unit
  (** Adds incoming edges to the node, on the given ports,
      but only if not already present on this port.
   *)

  val node_add_out_list_unique: ('n,'e) node -> ((port * ('n,'e) edge) list) -> unit
  (** Adds outgoing edges to the node, on the given ports,
      but only if not already present on this port.
   *)

  val edge_add_src_unique: ('n,'e) edge -> port -> ('n,'e) node -> unit
  (** Adds a source node to the edge, on the given port,
      but only if not already present on this port.
   *)
    
  val edge_add_dst_unique: ('n,'e) edge -> port -> ('n,'e) node -> unit
  (** Adds a destination node to the edge, on the given port,
      but only if not already present on this port.     
   *)

  val edge_add_src_list_unique: ('n,'e) edge -> ((port * ('n,'e) node) list) -> unit
  (** Adds source nodes to the edge, on the given ports,
      but only if not already present on this port.
   *)

  val edge_add_dst_list_unique: ('n,'e) edge -> ((port * ('n,'e) node) list) -> unit
  (** Adds destination nodes to the edge, on the given ports,
      but only if not already present on this port.
   *)    

    
  val node_remove_in_port: ('n,'e) node -> port -> ('n,'e) edge -> unit
  (** Removes all the connections to the node incoming from the edge 
      on the given port. 
      No edge nor node is deleted.
   *)

  val node_remove_out_port: ('n,'e) node -> port -> ('n,'e) edge -> unit
  (** Removes all the connections from the node outgoing into the edge 
      on the given port.
      No edge nor node is deleted.
   *)

  val node_remove_in: ('n,'e) node -> ('n,'e) edge -> unit
  (** Removes all the connections to the node incoming into from the edge,
      on all ports.
      No edge nor node is deleted.
   *)
    
  val node_remove_out: ('n,'e) node -> ('n,'e) edge -> unit
  (** Removes all the connections from the node incoming into the edge,
      on all ports.
      No edge nor node is deleted.
   *)

  val node_remove_all_in: ('n,'e) node -> unit
  (** Removes all the connections incoming into the node, 
      for all edges and ports.
      No edge nor node is deleted.
   *)
    
  val node_remove_all_out: ('n,'e) node -> unit
  (** Removes all the connections outgoing from the node, 
      for all edges and ports.
      No edge nor node is deleted.
   *)

  val edge_remove_src_port: ('n,'e) edge -> port -> ('n,'e) node -> unit
  (** Removes all the connections to the edge from the node
      on the given port. 
      No edge nor node is deleted.
   *)

  val edge_remove_dst_port: ('n,'e) edge -> port -> ('n,'e) node -> unit
  (** Removes all the connections from the edge to the node
      on the given port.
      No edge nor node is deleted.
   *)

  val edge_remove_src: ('n,'e) edge -> ('n,'e) node -> unit
  (** Removes all the connections to the edge from the node,
      on all ports.
      No edge nor node is deleted.
   *)
    
  val edge_remove_dst: ('n,'e) edge -> ('n,'e) node -> unit
  (** Removes all the connections from the edge to the node
      on all ports.
      No edge nor node is deleted.
   *)

  val edge_remove_all_src: ('n,'e) edge -> unit
  (** Removes all the connections at the source of the edge, 
      for all nodes and ports.
      No edge nor node is deleted.
   *)

  val edge_remove_all_dst: ('n,'e) edge -> unit
  (** Removes all the connections at the destination of the edge, 
      for all nodes and ports.
      No edge nor node is deleted 
   *)
    
  val node_set_in: ('n,'e) node -> ((port * ('n,'e) edge) list) -> unit
  (** Sets the incoming edges to the node, on the given ports. *)

  val node_set_out: ('n,'e) node -> ((port * ('n,'e) edge) list) -> unit
  (** Sets the outgoing edges to the node, on the given ports. *)

  val edge_set_src: ('n,'e) edge -> ((port * ('n,'e) node) list) -> unit
  (** Sets the source nodes to the edge, on the given ports. *)

  val edge_set_dst: ('n,'e) edge -> ((port * ('n,'e) node) list) -> unit
  (** Sets destination nodes to the edge, on the given ports. *)    


  val node_set_in_unique: ('n,'e) node -> ((port * ('n,'e) edge) list) -> unit
  (** Sets the incoming edges to the node, on the given ports. *)

  val node_set_out_unique: ('n,'e) node -> ((port * ('n,'e) edge) list) -> unit
  (** Sets the outgoing edges to the node, on the given ports. *)

  val edge_set_src_unique: ('n,'e) edge -> ((port * ('n,'e) node) list) -> unit
  (** Sets the source nodes to the edge, on the given ports. *)

  val edge_set_dst_unique: ('n,'e) edge -> ((port * ('n,'e) node) list) -> unit
  (** Sets destination nodes to the edge, on the given ports. *)    

        
            
  (*========================================================================*)
                         (** {2 Exploration} *)
  (*========================================================================*)

    
  val node_list: ('n,'e) graph -> ('n,'e) node list
  (** The set of nodes in the graph as a list. *)
    
  val edge_list: ('n,'e) graph -> ('n,'e) edge list
  (** The set of edges in the graph as a list. *)

  val node_set: ('n,'e) graph -> NodeSet.t
  (** The set of nodes in the graph as a set of identifiers. *)

  val edge_set: ('n,'e) graph -> EdgeSet.t
  (** The set of edges in the graph as a set of identifiers. *)

  val node_map: ('n,'e) graph -> ('n,'e) node NodeMap.t
  (** The set of nodes in the graph as a map from identifiers to nodes. *)

  val edge_map: ('n,'e) graph -> ('n,'e) edge EdgeMap.t
  (** The set of edges in the graph as a map from identifiers to edges. *)

  val has_node: ('n,'e) graph -> node_id -> bool
  (** Whether the graph contains a node with this identifier. *)

  val has_edge: ('n,'e) graph -> edge_id -> bool
  (** Whether the graph contains an edge with this identifier. *)
                    
  val get_node: ('n,'e) graph -> node_id -> ('n,'e) node
  (** The node corresponding to the identifier in the graph. 
      Raises [Not_found] if [has_node] is false.
   *)

  val get_edge: ('n,'e) graph -> edge_id -> ('n,'e) edge
  (** The edge corresponding to the identifier in the graph. 
      Raises [Not_found] if [has_edge] is false.
   *)

  val entries: ('n,'e) graph -> (port * ('n,'e) node) list
  (** List of entry nodes, with port. *)
    
  val exits: ('n,'e) graph -> (port * ('n,'e) node) list
  (** List of exist nodes, with port. *)
    
  val edge_id: ('n,'e) edge -> edge_id
  (** Edge identifier. *)
    
  val edge_data: ('n,'e) edge -> 'e
  (** Gets custom data associated to the edge. *)
    
  val edge_set_data: ('n,'e) edge -> 'e -> unit
  (** Sets custom data associated to the edge. *)
                           
  val edge_src: ('n,'e) edge -> (port * ('n,'e) node) list
  (** List of edge source nodes. *)
                 
  val edge_dst: ('n,'e) edge -> (port * ('n,'e) node) list
  (** List of edge destination nodes. *)
    
  val edge_src_port: ('n,'e) edge -> port -> ('n,'e) node list
  (** List of edge source nodes on the given port. *)

  val edge_dst_port: ('n,'e) edge -> port -> ('n,'e) node list
  (** List of edge destination nodes on the given port. *)

  val edge_src_size: ('n,'e) edge -> int
  (** Number of edge source nodes. *)
                 
  val edge_dst_size: ('n,'e) edge -> int
  (** Number of edge destination nodes. *)
    
  val edge_src_port_size: ('n,'e) edge -> port -> int
  (** Number of edge source nodes on the given port. *)

  val edge_dst_port_size: ('n,'e) edge -> port -> int
  (** Number of edge destination nodes on the given port. *)

    
  val node_id: ('n,'e) node -> node_id
  (** Node identifier. *)

  val node_data: ('n,'e) node -> 'n
  (** Gets custom data associated to the node. *)
    
  val node_set_data: ('n,'e) node -> 'n -> unit
  (** Sets custom data associated to the node. *)
    
  val node_in: ('n,'e) node -> (port * ('n,'e) edge) list
  (** List of edges incoming into the node. *)
                                           
  val node_out: ('n,'e) node -> (port * ('n,'e) edge) list
  (** List of edges outgoing from the node. *)
    
  val node_in_port: ('n,'e) node -> port -> ('n,'e) edge list
  (** List of edges incoming into the node on the given port. *)
    
  val node_out_port: ('n,'e) node -> port -> ('n,'e) edge list
  (** List of edges outgoing from the node on the given port. *)
                         
  val node_in_size: ('n,'e) node -> int
  (** Number of edges incoming into the node. *)
                                           
  val node_out_size: ('n,'e) node -> int
  (** Number of edges outgoing from the node. *)
    
  val node_in_port_size: ('n,'e) node -> port -> int
  (** Number of edges incoming into the node on the given port. *)
    
  val node_out_port_size: ('n,'e) node -> port -> int
  (** Number of edges outgoing from the node on the given port. *)
                         
  val node_entry_port: ('n,'e) graph -> ('n,'e) node -> port option
  (** If the node is an entry node, returns its port, otherwise None. *)
    
  val node_exit_port: ('n,'e) graph -> ('n,'e) node -> port option
  (** If the node is an exit node, returns its port, otherwise None. *)

  val node_has_out: ('n,'e) node -> ('n,'e) edge -> bool
  (** Whether the given edge is outgoing from the node. *)
    
  val node_has_out_port: ('n,'e) node -> port -> ('n,'e) edge -> bool
  (** Whether the given edge is outgoing from the node on the given port. *)
                       
  val node_has_in: ('n,'e) node -> ('n,'e) edge -> bool
  (** Whether the given edge is incoming into the node. *)
                       
  val node_has_in_port: ('n,'e) node -> port -> ('n,'e) edge -> bool
  (** Whether the given edge is incoming into the node on the given port. *)

  val edge_has_src: ('n,'e) edge -> ('n,'e) node -> bool
  (** Whether the edge has the node as source. *)
    
  val edge_has_src_port: ('n,'e) edge -> port -> ('n,'e) node -> bool
  (** Whether the edge has the node as source on the given port. *)

  val edge_has_dst: ('n,'e) edge -> ('n,'e) node -> bool
  (** Whether the edge has the node as destination. *)
    
  val edge_has_dst_port: ('n,'e) edge -> port -> ('n,'e) node -> bool
  (** Whether the edge has the node as destination on the given port. *)

  val node_out_nodes: ('n,'e) node
                      -> (port * ('n,'e) edge * port * ('n,'e) node) list
  (** Successors nodes of a given node. 
      Each returned element [(port1,edge,port2,node)] gives the port [port1]
      connecting the argument node to an edge [edge], the edge [edge], 
      the port [port2] connecting the edge [edge] to the successor node [node], 
      and finally the successor node [node].
   *)

  val node_in_nodes: ('n,'e) node
                      -> (('n,'e) node * port * ('n,'e) edge * port) list
  (** Predecessor nodes of a given node. 
      Each returned element [(node,port1,edge,port2)] gives the predecessor 
      node [node], the port [port1] connecting it to an edge [edge], 
      the edge [edge], and finally the port [port2] connecting the edge 
      [edge] to the argument node.
   *)

  val node_out_nodes_port: ('n,'e) node -> port -> port
                          -> (('n,'e) edge * ('n,'e) node) list
  (** Successor nodes of a given node connected through an edge
      on the specified ports.
      [node_out_nodes_port node port1 port2] returns a list of
      [(edge,node')] elements, where [port1] connects the argument [node] 
      to an edge [edge] and [port2] connects the edge [edge] to a
      successor node [node'].
   *)

  val node_in_nodes_port: ('n,'e) node -> port -> port
                          -> (('n,'e) node * ('n,'e) edge) list
  (** Predecessor nodes of a given node connected through an edge
      on the specified ports.
      [node_in_nodes_port node port1 port2] returns a list of
      [(node',edge)] elements, where [port1] connects the predecessor 
      [node'] to an edge [edge] and [port2] connects the edge [edge] to a
      the argument node [node.
   *)

  val node_has_node_out: ('n,'e) node -> ('n,'e) node -> bool
  (** Whether the first node has an outgoing edge into the second node. *)

  val node_has_node_in: ('n,'e) node -> ('n,'e) node -> bool
  (** Whether the first node has an incoming edge from the second node. *)
    
  val node_has_node_out_port: ('n,'e) node -> port -> port -> ('n,'e) node -> bool
  (** Whether the first node has an outgoing edge on the first port,
      going into the second node on the second port.
   *)

  val node_has_node_in_port: ('n,'e) node -> port -> port -> ('n,'e) node -> bool
  (** Whether the first node has an incoming edge on the first port,
      coming from the second node on the second port.
   *)

    
    
  (*========================================================================*)
                       (** {2 Global operations} *)
  (*========================================================================*)


  val clone: ('n,'e) graph -> ('n,'e) graph
  (** Creates a copy of the graph. *)

  val clone_map: ('n1 -> 'n2) -> ('e1 -> 'e2) -> ('n1,'e1) graph
                 -> ('n2,'e2) graph
  (** Creates a copy of the graph, applying a function to each
      node data and each edge data.
   *)

  val transpose: ('n,'e) graph -> unit
  (** Reverses the direction of all edges.
      Also switches entry and exit nodes. 
   *)
    
  val iter_nodes: (node_id -> ('n,'e) node -> unit) -> ('n,'e) graph -> unit
  (** Iterates a function on all nodes.
      The order in which the function is called is not specified.
   *)
    
  val iter_edges: (edge_id -> ('n,'e) edge -> unit) -> ('n,'e) graph -> unit
  (** Iterates a function on all edges.
      The order in which the function is called is not specified.
   *)

  val map_nodes: (node_id -> ('n,'e) node -> 'a) -> ('n,'e) graph
                 -> 'a NodeMap.t
  (** Constructs a map from node identifiers, applying the given function.
      The order in which the function is called is not specified.
   *)
    
  val map_edges: (edge_id -> ('n,'e) edge -> 'a) -> ('n,'e) graph
                 -> 'a EdgeMap.t
  (** Constructs a map from edge identifiers, applying the given function. 
      The order in which the function is called is not specified.
   *)
    
  val fold_nodes: (node_id -> ('n,'e) node -> 'a -> 'a) -> ('n,'e) graph
                  -> 'a -> 'a
  (** Accumulates a function on all nodes.
      The order in which the function is called is not specified.
   *)
    
  val fold_edges: (edge_id -> ('n,'e) edge -> 'a -> 'a) -> ('n,'e) graph
                  -> 'a -> 'a
  (** Accumulates a function on all edges.    
      The order in which the function is called is not specified.
 *)

  val iter_nodes_ordered: (node_id -> ('n,'e) node -> unit)
                          -> ('n,'e) graph -> unit
  (** Iterates a function on all nodes.
      The function is called in increasing identifier order.
   *)
    
  val iter_edges_ordered: (edge_id -> ('n,'e) edge -> unit)
                          -> ('n,'e) graph -> unit
  (** Iterates a function on all edges.
      The function is called in increasing identifier order.
   *)

  val map_nodes_ordered: (node_id -> ('n,'e) node -> 'a)
                         -> ('n,'e) graph -> 'a NodeMap.t
  (** Constructs a map from node identifiers, applying the given function.
      The function is called in increasing identifier order.
   *)
    
  val map_edges_ordered: (edge_id -> ('n,'e) edge -> 'a)
                         -> ('n,'e) graph -> 'a EdgeMap.t
  (** Constructs a map from edge identifiers, applying the given function. 
      The function is called in increasing identifier order.
   *)
    
  val fold_nodes_ordered: (node_id -> ('n,'e) node -> 'a -> 'a)
                          -> ('n,'e) graph -> 'a -> 'a
  (** Accumulates a function on all nodes.
      The function is called in increasing identifier order.
   *)
    
  val fold_edges_ordered: (edge_id -> ('n,'e) edge -> 'a -> 'a)
                          -> ('n,'e) graph -> 'a -> 'a
  (** Accumulates a function on all edges.    
      The function is called in increasing identifier order.
 *)

  val remove_orphan: ('n,'e) graph -> unit
  (** Removes orphan nodes and edges.
      Orphan nodes have no incoming nor outgoing edges, and orphan
      edges have no source nor destination nodes.
   *)

    

  (*========================================================================*)
                         (** {2 Printing} *)
  (*========================================================================*)


  (** Print in dot graph format. *)

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

  val print: ('n,'e) printer -> Format.formatter -> ('n,'e) graph -> unit


  (** Print in dot graph format. *)
    
  type ('n,'e) dot_printer = {
      dot_node: Format.formatter -> ('n,'e) node -> unit;
      dot_edge: Format.formatter -> ('n,'e) edge -> unit;
      dot_port: Format.formatter -> port -> unit;
    }
                           
  val print_dot: ('n,'e) dot_printer -> string -> Format.formatter
                 -> ('n,'e) graph -> unit
  (** [print_dot channel graph name printer].
      outputs the graph in the specified channel in dot format.
      In addition [name] gives the dot graph name.
      The node, edge and port printer functions are user-specified.
   *)

    
end
