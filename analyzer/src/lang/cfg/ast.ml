(****************************************************************************)
(*                   Copyright (C) 2018 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Extends the simple Universal language with Control Flow Graphs. *)

open Framework.Essentials
open Framework.Manager

   
   
(*==========================================================================*)
                           (** {2 Graphs} *)
(*==========================================================================*)


module Loc =
struct    
  type t = loc (* maybe add a unique tag? *)
  let compare = compare_location
  let hash = Hashtbl.hash
  let equal l1 l2 = compare l1 l2 = 0
end

module Range =
struct
  type t = range
  let compare = compare_range
  let hash = Hashtbl.hash
  let equal l1 l2 = compare l1 l2 = 0
end

module Port =
struct
  type t = token
  let compare = compare_token
  let hash = Hashtbl.hash
  let equal l1 l2 = compare l1 l2 = 0
end

module LocSet = SetExt.Make(Loc)
module LocMap = MapExt.Make(Loc)
module LocHash = Hashtbl.Make(Loc)

module RangeSet = SetExt.Make(Range)
module RangeMap = MapExt.Make(Range)
module RangeHash = Hashtbl.Make(Range)
  
(** Build CFG module. *)
module CFG_Param =
struct
  module NodeId = Loc
  (** Identify nodes by source location. *)
                  
  module EdgeId = Range
  (** Identify edges by source range. *)

  module Port = Port
  (** Edge outputs are distinguished by flow tokens. *)
end
  
module CFG = Graph.Make(CFG_Param)

(** Edges are labelled with a statement.
    Nodes have no information in the graph structure.
    Abstract invariant information will be kept in maps separately
    from the CFG. 
    This way, CFG can be kept immutable.
 *)
type cfg  = (unit, stmt) CFG.graph
type node = (unit, stmt) CFG.node
type edge = (unit, stmt) CFG.edge

          
(*==========================================================================*)
                           (** {2 Flows} *)
(*==========================================================================*)


(** Associate a flow to each CFG node.
    We can store abstract information for the whole graph in a 
    single abstract state, using node flows.
 *)
type token +=
   | T_loc of Loc.t

(** Flow for true and false branch of tests. *)            
type token +=
   | T_true
   | T_false

   
(*==========================================================================*)
                           (** {2 Statements} *)
(*==========================================================================*)


type stmt_kind +=
   | S_cfg of cfg              
   | S_test of expr (** test nodes, with a true and a false branch *)
   | S_skip (** empty node *)

   
let mk_skip range =
  mk_stmt S_skip range

let mk_test e range =
  mk_stmt (S_test e) range

let mk_cfg cfg range =
  mk_stmt (S_cfg cfg) range
