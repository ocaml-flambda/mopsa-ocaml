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

let make annot fm =
  { map = Nt fm;
    annot;
  }

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

let join_list man ?(annot=Annotation.empty) l =
  match l with
  | [] -> bottom annot
  | hd :: tl -> List.fold_left (join man) hd tl

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

let meet_list man ?(annot=Annotation.empty) l =
  match l with
  | [] -> bottom annot
  | hd :: tl -> List.fold_left (meet man) hd tl


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


let singleton (annot:'a Annotation.annot) (tk:token) (env:'a) : 'a flow =
  make annot (FlowMap.singleton tk env)

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

let copy (tk1:token) (tk2:token) (man:('a,'t) man) (flow1:'a flow) (flow2:'a flow) : 'a flow =
  set tk2 (get tk1 man flow1) man flow2

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

let map_token tk (f: 'a -> 'a) (man: ('a, _) man) (flow: 'a flow) : 'a flow =
  set tk (f (get tk man flow)) man flow


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

let map_list (f:'b -> 'a flow -> 'a flow) (flow: 'a flow) (l: 'b list) : 'a flow list =
  let flows, _ = List.fold_left (fun (acc, annot) x ->
      let flow' = { flow with annot } in
      let flow'' = f x flow' in
      flow'' :: acc, flow''.annot
    ) ([], flow.annot) l
  in
  flows

let map_list_opt (f:'b -> 'a flow -> 'a flow option) (flow:'a flow) (l:'b list) : 'a flow list =
  let flows, _ = List.fold_left (fun (acc, annot) x ->
      let flow' = { flow with annot } in
      match f x flow' with
      | None -> acc, annot
      | Some flow'' -> flow'' :: acc, flow''.annot
    ) ([], flow.annot) l
  in
  flows

let set_domain_env (tk: token) (a:'t) (man:('a, 't) man) (flow:'a flow) : 'a flow =
  set tk (man.set a (get tk man flow)) man flow

let get_domain_env (tk:token) (man:('a, 't) man) (flow:'a flow) : 't =
  man.get (get tk man flow)

let map_domain_env (tk:token) (f:'t -> 't) (man:('a, 't) man) (flow:'a flow) : 'a flow =
  set_domain_env tk (f (get_domain_env tk man flow)) man flow

let set_domain_cur (a: 't) (man: ('a, 't) man) (flow: 'a flow): 'a flow =
  set_domain_env T_cur a man flow

let get_domain_cur (man: ('a, 't) man) (flow: 'a flow): 't =
  get_domain_env T_cur man flow

let map_domain_cur (f:'t -> 't) (man:('a, 't) man) (flow:'a flow) : 'a flow =
  map_domain_env T_cur f man flow

let is_cur_bottom (man : ('a, 't) man) (flow :'a flow) : bool =
  man.is_bottom (get T_cur man flow)


let test_domain_env (tk:token) (f:'t -> bool) (man:('a,'t) man) (flow:'a flow) : bool =
  get_domain_env tk man flow |>
  f

let test_domain_cur (f:'t -> bool) (man:('a,'t) man) (flow:'a flow) : bool =
  test_domain_env T_cur f man flow


let get_all_annot flow = flow.annot
let set_all_annot annot flow = {flow with annot}
let map_all_annot f flow = set_all_annot (f @@ get_all_annot flow) flow

let get_annot k flow =
  get_all_annot flow |> Annotation.find k

let set_annot k v flow =
  get_all_annot flow
  |> Annotation.add k v
  |> fun annot -> set_all_annot annot flow

let rm_annot k flow =
  get_all_annot flow
  |> Annotation.remove k
  |> fun annot -> set_all_annot annot flow

let mem_annot k flow =
  get_all_annot flow |>
  Annotation.mem k

let copy_annot flow1 flow2 =
  let annot = get_all_annot flow1 in
  set_all_annot annot flow2

let without_callbacks flow =
  { flow; callbacks = [] }
