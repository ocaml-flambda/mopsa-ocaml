(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Flows are handled continuations and are identified by a finite
   set of tokens. We use a partitioning abstraction to aggregate together
   the abstract environments of each flow token.
*)

open Top
open Lattice
open Manager

type token = Manager.token

module FlowMap = Manager.FlowMap

type 'a fmap = 'a Manager.fmap

type 'a flow = 'a Manager.flow

let bottom annot : 'a flow = {
  map = Nt FlowMap.empty;
  annot;
}

let top annot : 'a flow = {
  map = TOP;
  annot;
}

let is_bottom (man: ('a, _) man) (flow: 'a flow) : bool =
  top_dfl1 false (fun m ->
      FlowMap.for_all (fun _ v -> man.is_bottom v) m
    ) flow.map


let is_top (man: ('a, _) man) (flow: 'a flow) : bool =
  top_dfl1 true (fun _ -> false) flow.map


let subset (man: ('a, _) man) (flow1: 'a flow) (flow2: 'a flow) : bool =
  top_included
    (FlowMap.for_all2zo
       (fun _ v1 -> man.is_bottom v1) (* non-⊥ ⊈ ⊥ *)
       (fun _ v2 -> true)  (* ⊥ ⊆ non-⊥ *)
       (fun _ v1 v2 -> man.subset v1 v2)
    )
    flow1.map flow2.map

let join (man: ('a, _) man) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  (* FIXME: we choose here one annotation, which is correct but too
     coarse. We need to fold the annotation through the two flows *)
  let annot = flow2.annot in
  let map = top_lift2
      (FlowMap.map2zo
         (fun _ v1 -> v1)
         (fun _ v2 -> v2)
         (fun _ v1 v2 -> man.join annot v1 v2)
      )
      flow1.map flow2.map
  in
  {map; annot}

let meet (man: ('a, _) man) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  (* FIXME: we choose here one annotation, which is correct but too
     coarse. We need to fold the annotation through the two flows *)
  let annot = flow2.annot in
  let map = top_neutral2
      (fun b1 b2 ->
         FlowMap.map2zo
           (fun _ v1 -> man.bottom)
           (fun _ v2 -> man.bottom)
           (fun _ v1 v2 -> man.meet annot v1 v2)
           b1 b2
      )
      flow1.map flow2.map
  in
  {map; annot}

let widen (man: ('a, _) man) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  (* FIXME: we choose here one annotation, which is correct but too
     coarse. We need to fold the annotation through the two flows *)
  let annot = flow2.annot in
  let map = top_lift2
      (FlowMap.map2zo
         (fun _ v1 -> v1)
         (fun _ v2 -> v2)
         (fun _ v1 v2 -> man.widen annot v1 v2)
      )
      flow1.map flow2.map
  in
  {map; annot}

let print (man: ('a, _) man) fmt (flow : 'a flow) : unit =
  top_fprint (FlowMap.print man.print) fmt flow.map


let get (tk: token) (man: ('a, _) man) (flow: 'a flow) : 'a =
  try
    let m = top_to_exn flow.map in
    try FlowMap.find tk m with Not_found -> man.bottom
  with Found_TOP -> man.top


let set (tk: token) (a: 'a) (man: ('a, _) man) (flow: 'a flow) : 'a flow =
  let map = top_lift1 (fun m ->
      if man.is_bottom a then FlowMap.remove tk m
      else FlowMap.add tk a m
    ) flow.map
  in
  {flow with map}

let add (tk: token) (a: 'a) (man: ('a, _) man) (flow: 'a flow) : 'a flow =
  let annot = flow.annot in
  let map = top_lift1 (fun m ->
      if man.is_bottom a then m
      else
        let a' =
          try
            let old = FlowMap.find tk m in
            man.join annot a old
          with Not_found ->
            a
        in
        FlowMap.add tk a' m
    ) flow.map
  in
  {map; annot}

let remove (tk: token) (man: ('a, _) man) (flow: 'a flow) : 'a flow =
  let map = top_lift1 (FlowMap.remove tk) flow.map in
  {flow with map}

let filter (f: token -> 'a -> bool) (man: ('a, _) man) (flow: 'a flow) : 'a flow =
  let map = top_lift1 (FlowMap.filter f) flow.map in
  {flow with map}


let map (f: token -> 'a -> 'a) (man: ('a, _) man) (flow: 'a flow) : 'a flow =
  let map = top_lift1 (FlowMap.mapi f) flow.map in
  {flow with map}

let fold (f: 'b -> token -> 'a -> 'b) (init: 'b) (man: ('a, _) man) (flow: 'a flow) : 'b =
  let m = top_to_exn flow.map in
  FlowMap.fold (fun tk a acc -> f acc tk a) m init

let merge (f: token -> 'a option -> 'a option -> 'a option) (man: ('a, _) man) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  (* FIXME: we choose here one annotation, which is correct but too
     coarse. We need to fold the annotation through the two flows *)
  let annot = flow2.annot in
  let fopt tk a b =
    match f tk a b with
    | None -> man.bottom
    | Some v -> v
  in
  let map = top_lift2
    (FlowMap.map2zo
       (fun tk v1 -> fopt tk (Some v1) None)
       (fun tk v2 -> fopt tk None (Some v2))
       (fun tk v1 v2 -> fopt tk (Some v1) (Some v2))
    )
    flow1.map flow2.map
  in
  {map; annot}


let set_domain_env (tk: token) (a:'t) (man:('a, 't) man) (flow:'a flow) : 'a flow =
  set tk (man.set a (get tk man flow)) man flow

let get_domain_env (tk:token) (man:('a, 't) man) (flow:'a flow) : 't =
  man.get (get tk man flow)

let map_domain_env (tk:token) (f:'t -> 't) (man:('a, 't) man) (flow:'a flow) : 'a flow =
  set_domain_env tk (f (get_domain_env tk man flow)) man flow
