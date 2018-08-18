(****************************************************************************)
(*                   Copyright (C) 2018 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** General iterator on Control Flow Graphs. *)

open Framework.Ast
open Framework.Flow
open Framework.Pp
open Universal.Ast
open Ast


(*==========================================================================*)
                      (** {2 Domain signature} *)
(*==========================================================================*)


module type DOMAIN = sig

  type t
  (** Abstract information attached to nodes. *)

  type manager
  (** Global information. *)

  val bot: manager -> t
    
  val entry: manager -> loc -> node -> token -> t
    
  val exec: manager -> loc -> (token * t) list -> edge -> (token * t) list

  val join: manager -> loc -> t -> t -> t 

  val widen: manager -> loc -> t -> t -> t

  val subset: manager -> loc -> t -> t -> bool

end


                   
(*==========================================================================*)
                        (** {2 Strategies} *)
(*==========================================================================*)

                   
(** Signature of modules encoding an iteration strategy, through
    a worklist data-structure.
 *)
module type STRATEGY = sig

  type t
  (** Abstract type of worklists. *)
     
  val create: cfg -> t
  (** Creates a new worklist.
      The CFG argument can be used to pre-compute an ordering of nodes
      and widening points.
   *)

  val is_empty: t -> bool
  (** Whether there are dirty nodes at all. *)

  val dirty: t -> loc -> unit
  (** Marks a node as dirty. *)
    
  val get: t -> loc
  (** Gets the next dirty node to update.
      Raises [Not_found] if [is_empty] is true.
   *)

  val is_widen: t -> loc -> bool
  (** Whether we should use widening at that point. *)
    
end


(** Simple set-based worklist strategy.
    We always return the smallest dirty node (for the location order).
    Widening points are nodes with back-edges (i.e., with nodes with 
    a greater location flowing into it).
 *)
module SetWorklist = (
  struct

    type t =
      { dirty: LocSet.t ref; (** dirty nodes *)
        widen: LocSet.t; (** widening points *)
      }

    let create g =
      (* precompute widening points *)
      let w =
        CFG.fold_nodes
          (fun id n acc ->
            (* whether n has a back-edge *)
            let i = CFG.node_in_nodes n in
            if
              List.exists
                (fun (nn,_,_,_) ->
                  Loc.compare (CFG.node_id nn) id > 0
                ) i
            then LocSet.add id acc
            else acc
          )
          g LocSet.empty
      in
      { dirty = ref LocSet.empty;
        widen = w;
      }

    let is_empty w =
      LocSet.is_empty !(w.dirty)
                 
    let dirty w id =
      w.dirty := LocSet.add id !(w.dirty)

    let get w =
      let id = LocSet.min_elt !(w.dirty) in
      w.dirty := LocSet.remove id !(w.dirty);
      id

    let is_widen w id =
      LocSet.mem id w.widen
    
  end
  : STRATEGY)

                    
        
(*==========================================================================*)
                         (** {2 Iterator} *)
(*==========================================================================*)

                    
module Make(D:DOMAIN)(S:STRATEGY) = struct


  let iterate (man:D.manager) (g:cfg) =
    
    (* create *)
    let a = LocHash.create 16 in
    let wl = S.create g in
    let bot = D.bot man in

    (* update *)
    let get id = LocHash.find a id
    and set id v =
      let org = LocHash.find a id in
      if not (D.subset man id v org) then (
        LocHash.replace a id v;
        S.dirty wl id
      )
    in

    (* initial state *)    
    CFG.iter_nodes (fun id _ -> LocHash.add a id bot) g;
    List.iter
      (fun (f,n) ->
        let id = CFG.node_id n in
        set id (D.entry man id n f)
      )
      (CFG.entries g);

    (* propagation loop *)
    while not (S.is_empty wl) do

      (* choose dirty node *)
      let id = S.get wl in
      let n = CFG.get_node g id in

      (* for each outgoing edge *)
      List.iter
        (fun (_,e) ->

          (* get source nodes *)
          let src =
            List.map
              (fun (ff,nn) -> ff, get (CFG.node_id nn))
              (CFG.edge_src e)
          in

          (* call domain *)
          let dst = D.exec man id src e in
          
          (* update destination nodes *)
          List.iter
            (fun (ff,nn) ->
              (* find exec flow matching the destination input *)
              match
                try
                  Some (List.find (fun (fff,v) -> compare_token ff fff = 0) dst)
                with Not_found ->
                  None
              with
              | None -> ()
              | Some (_,v) ->
                 (* found: accumulate and update *)
                 let idd = CFG.node_id nn in
                 let op = if S.is_widen wl idd then D.widen else D.join in
                 set idd (op man idd (get idd) v)
            )
            (CFG.edge_dst e)
          
        )
      (CFG.node_out n)
    done;

    (* return the result *)
    a
  
end
                    
   
