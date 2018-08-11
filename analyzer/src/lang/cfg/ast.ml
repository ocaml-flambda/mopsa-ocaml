(****************************************************************************)
(*                   Copyright (C) 2018 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Extends the simple Universal language with Control Flow Graphs. *)

open Framework.Ast
open Framework.Flow
open Universal.Ast

   
(*==========================================================================*)
                           (** {2 Statements} *)
(*==========================================================================*)



module Location =
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

module Tag =
  struct
    type t = token
    let compare = compare_token
    let hash = Hashtbl.hash
    let equal l1 l2 = compare l1 l2 = 0
  end

  
(** Build CFG module. *)
module CFG_Param =
  struct
    module NodeId = Location
    (** Identify nodes by source location. *)
                  
    module EdgeId = Range
    (** Identify edges by source range. *)

    module Tag = Tag
    (** Edge outputs are distinguished by flow tokens. *)
  end
  
module CFG = Graph.Make(CFG_Param)

(** Edges are labelled with a statement.
    Node have no information in the graph structure.
    Abstract invariant information will be kept in maps separately
    from the CFG. 
    This way, CFG can be kept immutable.
 *)
type cfg = (unit, stmt) CFG.graph
           
(** Adds CFG to statement. *)
type stmt_kind +=
   | S_CFG of cfg
              
              

   
