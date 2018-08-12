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
Tags help distinguish between the different mode sources and node 
destinations of an edge.

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

  module Tag: ID_TYPE
  (** Connections between edges and nodes are tagged. *)
     
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
  type tag = P.Tag.t
  (** Export back types from the functor parameter. *)
              
  module NodeSet: SetExtSig.S with type elt = node_id
  module EdgeSet: SetExtSig.S with type elt = edge_id
  (** Sets constructed from ordered type parameters. *)
       
                   
           
  (*========================================================================*)
                         (** {2 Construction} *)
  (*========================================================================*)

           
  val create: unit -> ('n,'e) graph
  (** Creates an empty graph. *)

  val add_node: ('n,'e) graph -> node_id
                -> ?inc:(tag * ('n,'e) edge) list
                -> ?out:(tag * ('n,'e) edge) list
                -> ?entry:tag -> ?exit:tag
                -> 'n -> ('n,'e) node
  (** Adds a node to the graph.
      Optionally connects the node to incoming or outgoing edges.
      Optionally sets as an entry or exit node with a tag (defaults to None,
      i.e., not entry nor exit).
      The node identifier must be unique among the node in the graph;
      raises [Invalid_argument] otherwise.
   *)
    
  val add_edge: ('n,'e) graph -> edge_id
                -> ?src:(tag * ('n,'e) node) list
                -> ?dst:(tag * ('n,'e) node) list
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

  val node_set_entry: ('n,'e) graph -> ('n,'e) node -> tag option -> unit
  (** Sets whether a node is an entry node with some tag, or not (None). *)

  val node_set_exit: ('n,'e) graph -> ('n,'e) node -> tag option -> unit
  (** Sets whether a node is an exit node with some tag, or not (None). *)

  val node_add_in: ('n,'e) node -> tag -> ('n,'e) edge -> unit
  (** Adds an incoming edge to the node, with the given tag. *)

  val node_add_out: ('n,'e) node -> tag -> ('n,'e) edge -> unit
  (** Adds an outgoing edge to the node, with the given tag. *)

  val node_add_in_list: ('n,'e) node -> ((tag * ('n,'e) edge) list) -> unit
  (** Adds incoming edges to the node, with the given tags. *)

  val node_add_out_list: ('n,'e) node -> ((tag * ('n,'e) edge) list) -> unit
  (** Adds outgoing edges to the node, with the given tags. *)

  val edge_add_src_list: ('n,'e) edge -> ((tag * ('n,'e) node) list) -> unit
  (** Adds source nodes to the edge, with the given tags. *)

  val edge_add_dst_list: ('n,'e) edge -> ((tag * ('n,'e) node) list) -> unit
  (** Adds destinatino nodes to the edge, with the given tags. *)    

  val node_remove_tag_in: ('n,'e) node -> tag -> ('n,'e) edge -> unit
  (** Removes all the connections to the node incoming from the edge 
      with the given tag. 
      No edge nor node is deleted.
   *)

  val node_remove_tag_out: ('n,'e) node -> tag -> ('n,'e) edge -> unit
  (** Removes all the connections from the node outgoing into the edge 
      with the given tag.
      No edge nor node is deleted.
   *)

  val node_remove_in: ('n,'e) node -> ('n,'e) edge -> unit
  (** Removes all the connections to the node incoming into from the edge,
      for all tags.
      No edge nor node is deleted.
   *)
    
  val node_remove_out: ('n,'e) node -> ('n,'e) edge -> unit
  (** Removes all the connections from the node incoming into the edge,
      for all tags.      
      No edge nor node is deleted.
   *)

  val node_remove_all_in: ('n,'e) node -> unit
  (** Removes all the connections incoming into the node, 
      for all edges and tags.
      No edge nor node is deleted.
   *)
    
  val node_remove_all_out: ('n,'e) node -> unit
  (** Removes all the connections outgoing from the node, 
      for all edges and tags.
      No edge nor node is deleted.
   *)

  val edge_remove_all_src: ('n,'e) edge -> unit
  (** Removes all the connections at the source of the edge, 
      for all nodes and tags.
      No edge nor node is deleted.
   *)

  val edge_remove_all_dst: ('n,'e) edge -> unit
  (** Removes all the connections at the destination of the edge, 
      for all nodes and tags.
      No edge nor node is deleted 
   *)
    
  val node_set_in: ('n,'e) node -> ((tag * ('n,'e) edge) list) -> unit
  (** Sets the incoming edges to the node, with the given tags. *)

  val node_set_out: ('n,'e) node -> ((tag * ('n,'e) edge) list) -> unit
  (** Sets the outgoing edges to the node, with the given tags. *)

  val edge_set_src: ('n,'e) edge -> ((tag * ('n,'e) node) list) -> unit
  (** Sets the source nodes to the edge, with the given tags. *)

  val edge_set_dst: ('n,'e) edge -> ((tag * ('n,'e) node) list) -> unit
  (** Sets destinatino nodes to the edge, with the given tags. *)    

        
            
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

  val entries: ('n,'e) graph -> (tag * ('n,'e) node) list
  (** List of entry nodes, with tag. *)
    
  val exits: ('n,'e) graph -> (tag * ('n,'e) node) list
  (** List of exist nodes, with tag. *)
    
  val edge_id: ('n,'e) edge -> edge_id
  (** Edge identifier. *)
    
  val edge_data: ('n,'e) edge -> 'e
  (** Gets custom data associated to the edge. *)
    
  val edge_set_data: ('n,'e) edge -> 'e -> unit
  (** Sets custom data associated to the edge. *)
                           
  val edge_src: ('n,'e) edge -> (tag * ('n,'e) node) list
  (** List of edge source nodes. *)
                 
  val edge_dst: ('n,'e) edge -> (tag * ('n,'e) node) list
  (** List of edge destination nodes. *)
    
  val edge_tag_src: ('n,'e) edge -> tag -> ('n,'e) node list
  (** List of edge source nodes with the given tag. *)

  val edge_tag_dst: ('n,'e) edge -> tag -> ('n,'e) node list
  (** List of edge destination nodes with the given tag. *)

  val edge_src_size: ('n,'e) edge -> int
  (** Number of edge source nodes. *)
                 
  val edge_dst_size: ('n,'e) edge -> int
  (** Number of edge destination nodes. *)
    
  val edge_tag_src_size: ('n,'e) edge -> tag -> int
  (** Number of edge source nodes with the given tag. *)

  val edge_tag_dst_size: ('n,'e) edge -> tag -> int
  (** Number of edge destination nodes with the given tag. *)

    
  val node_id: ('n,'e) node -> node_id
  (** Node identifier. *)

  val node_data: ('n,'e) node -> 'n
  (** Gets custom data associated to the node. *)
    
  val node_set_data: ('n,'e) node -> 'n -> unit
  (** Sets custom data associated to the node. *)
    
  val node_in: ('n,'e) node -> (tag * ('n,'e) edge) list
  (** List of edges incoming into the node. *)
                                           
  val node_out: ('n,'e) node -> (tag * ('n,'e) edge) list
  (** List of edges outgoing from the node. *)
    
  val node_tag_in: ('n,'e) node -> tag -> ('n,'e) edge list
  (** List of edges incoming into the node with the given tag. *)
    
  val node_tag_out: ('n,'e) node -> tag -> ('n,'e) edge list
  (** List of edges outgoing from the node with the given tag. *)
                         
  val node_in_size: ('n,'e) node -> int
  (** Number of edges incoming into the node. *)
                                           
  val node_out_size: ('n,'e) node -> int
  (** Number of edges outgoing from the node. *)
    
  val node_tag_in_size: ('n,'e) node -> tag -> int
  (** Number of edges incoming into the node with the given tag. *)
    
  val node_tag_out_size: ('n,'e) node -> tag -> int
  (** Number of edges outgoing from the node with the given tag. *)
                         
  val node_entry_tag: ('n,'e) graph -> ('n,'e) node -> tag option
  (** If the node is an entry node, returns its tag, otherwise None. *)
    
  val node_exit_tag: ('n,'e) graph -> ('n,'e) node -> tag option
  (** If the node is an exit node, returns its tag, otherwise None. *)

  val node_has_edge_out: ('n,'e) node -> ('n,'e) edge -> bool
  (** Whether the given edge is outgoing from the node. *)
    
  val node_has_edge_tag_out: ('n,'e) node -> tag -> ('n,'e) edge -> bool
  (** Whether the given edge is outgoing from the node with the given tag. *)
                       
  val node_has_edge_in: ('n,'e) node -> ('n,'e) edge -> bool
  (** Whether the given edge is incoming into the node. *)
                       
  val node_has_edge_tag_in: ('n,'e) node -> tag -> ('n,'e) edge -> bool
  (** Whether the given edge is incoming into the node with the given tag. *)

  val edge_has_node_src: ('n,'e) edge -> ('n,'e) node -> bool
  (** Whether the edge has the node as source. *)
    
  val edge_has_node_tag_src: ('n,'e) edge -> tag -> ('n,'e) node -> bool
  (** Whether the edge has the node as source with the given tag. *)

  val edge_has_node_dst: ('n,'e) edge -> ('n,'e) node -> bool
  (** Whether the edge has the node as destination. *)
    
  val edge_has_node_tag_dst: ('n,'e) edge -> tag -> ('n,'e) node -> bool
  (** Whether the edge has the node as destination with the given tag. *)

  val node_out_nodes: ('n,'e) node
                      -> (tag * ('n,'e) edge * tag * ('n,'e) node) list
  (** Successors nodes of a given node. 
      Each returned element [(tag1,edge,tag2,node)] gives the tag [tag1]
      connecting the argument node to an edge [edge], the edge [edge], 
      the tag [tag2] connecting the edge [edge] to the successor node [node], 
      and finally the successor node [node].
   *)

  val node_in_nodes: ('n,'e) node
                      -> (('n,'e) node * tag * ('n,'e) edge * tag) list
  (** Predecessor nodes of a given node. 
      Each returned element [(node,tag1,edge,tag2)] gives the predecessor 
      node [node], the tag [tag1] connecting it to an edge [edge], 
      the edge [edge], and finally the tag [tag2] connecting the edge 
      [edge] to the argument node.
   *)

  val node_tag_out_nodes: ('n,'e) node -> tag -> tag
                          -> (('n,'e) edge * ('n,'e) node) list
  (** Successor nodes of a given node connected through an edge
      with the specified tags.
      [node_tag_out_nodes node tag1 tag2] returns a list of
      [(edge,node')] elements, where [tag1] connects the argument [node] 
      to an edge [edge] and [tag2] connects the edge [edge] to a
      successor node [node'].
   *)

  val node_tag_in_nodes: ('n,'e) node -> tag -> tag
                          -> (('n,'e) node * ('n,'e) edge) list
  (** Predecessor nodes of a given node connected through an edge
      with the specified tags.
      [node_tag_in_nodes node tag1 tag2] returns a list of
      [(node',edge)] elements, where [tag1] connects the predecessor 
      [node'] to an edge [edge] and [tag2] connects the edge [edge] to a
      the argument node [node.
   *)

  val node_has_node_out: ('n,'e) node -> ('n,'e) node -> bool
  (** Whether the first node has an outgoing edge into the second node. *)

  val node_has_node_in: ('n,'e) node -> ('n,'e) node -> bool
  (** Whether the first node has an incoming edge from the second node. *)
    
  val node_has_node_tag_out: ('n,'e) node -> tag -> tag -> ('n,'e) node -> bool
  (** Whether the first node has an outgoing edge with the first tag,
      going into the second node with the second tag.
   *)

  val node_has_node_tag_in: ('n,'e) node -> tag -> tag -> ('n,'e) node -> bool
  (** Whether the first node has an incoming edge with the first tag,
      coming from the second node with the second tag.
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
    
  val iter_nodes: (('n,'e) node -> unit) -> ('n,'e) graph -> unit
  (** Iterates a function on all nodes, in no particular order. *)
    
  val iter_edges: (('n,'e) edge -> unit) -> ('n,'e) graph -> unit
  (** Iterates a function on all edges, in no particular order. *)

  val remove_orphan: ('n,'e) graph -> unit
  (** Removes orphan nodes and edges.
      Orphan nodes have no incoming nor outgoing edges, and orphan
      edges have no source nor destination nodes.
   *)

    

  (*========================================================================*)
                         (** {2 Printing} *)
  (*========================================================================*)


  val print: Format.formatter -> ('n,'e) graph 
                 -> (Format.formatter -> ('n,'e) node -> unit)
                 -> (Format.formatter -> ('n,'e) edge  -> unit)
                 -> (Format.formatter -> ('n,'e) node -> tag -> ('n,'e) edge -> unit)
                 -> (Format.formatter -> ('n,'e) edge -> tag -> ('n,'e) node -> unit)
                 -> unit
    
  val print_dot: Format.formatter -> ('n,'e) graph -> string
                 -> (Format.formatter -> ('n,'e) node -> unit)
                 -> (Format.formatter -> ('n,'e) edge -> unit)
                 -> (Format.formatter -> tag -> unit)
                 -> unit
  (** [print_dot channel graph name print_node print_edge print_tag]
      outputs the graph in the specified channel in dot format.
      In addition [name] gives the dot graph name.
      The node, edge and tag printer functions are user-specified.
   *)

    
end
