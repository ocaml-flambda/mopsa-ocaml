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
open Framework.Pp
open Universal.Ast

   
   
(*==========================================================================*)
                           (** {2 Statements} *)
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

module Tag =
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
type cfg  = (unit, stmt) CFG.graph
type node = (unit, stmt) CFG.node
type edge = (unit, stmt) CFG.edge

          
(** Adds CFG to statement. *)
type stmt_kind +=
   | S_CFG of cfg
              
              

(*==========================================================================*)
                           (** {2 Flows} *)
(*==========================================================================*)


(** Associate a flow to each CFG node.
    We can store abstract information for the whole graph in a 
    single abstract state, using node flows.
 *)
type token +=
   | TLoc of Loc.t

           

(*==========================================================================*)
                       (** {2 Register new types} *)
(*==========================================================================*)


let cfg_printer = {
    CFG.print_node = (fun fmt n ->
      Format.fprintf fmt "%a:@;" pp_location (CFG.node_id n)
    );
    CFG.print_edge = (fun fmt e ->
      Format.fprintf fmt "  @[<v>%a@]@;" pp_stmt (CFG.edge_data e)
    );
    CFG.print_src = (fun fmt n tag e -> 
      Format.fprintf
        fmt "  %a --[%a]-->@;"
        pp_location (CFG.node_id n) pp_token tag
    );
    CFG.print_dst = (fun fmt e tag n -> 
      Format.fprintf
        fmt "  --[%a]--> %a@;"
        pp_token tag pp_location (CFG.node_id n)
    );
    CFG.print_entry = (fun fmt n tag ->
      Format.fprintf
        fmt "  entry --[%a]--> %a@;"
        pp_token tag pp_location (CFG.node_id n)
    );
    CFG.print_exit = (fun fmt n tag ->
      Format.fprintf
        fmt "  %a --[%a]--> exit@;"
        pp_location (CFG.node_id n) pp_token tag
    );
  }
                
           
let () =
  register_token_compare (fun next t1 t2 ->
      match t1, t2 with
      | TLoc l1, TLoc l2 -> compare_location l1 l2
      | _ -> next t1 t2
    );
  register_pp_token (fun next fmt t ->
      match t with
      | TLoc l -> pp_location fmt l
      | _ -> next fmt t
    );
  register_pp_stmt (fun next fmt s ->
      match s.skind with
      | S_CFG g -> Format.fprintf fmt "@[<v>%a@]" (CFG.print cfg_printer) g
      | _ -> next fmt s
    )
  
