(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
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

(** Abstraction of control flows *)

open Top
open Lattice
open Annotation


(****************************************************************************)
(**                             {2 Tokens}                                  *)
(****************************************************************************)

type token = ..

type token += T_cur

let token_compare_chain = Chain.mk_compare_chain (fun tk1 tk2 ->
    match tk1, tk2 with
    | T_cur, T_cur -> 0
    | _ -> compare tk1 tk2
  )

let compare_token tk1 tk2 = Chain.compare token_compare_chain tk1 tk2

let token_print_chain = Chain.mk_print_chain (fun fmt tk ->
    match tk with
    | T_cur -> Format.fprintf fmt "cur"
    | _ -> Exceptions.panic ~loc:__LOC__ "unknown token"
  )

let pp_token fmt tk = Chain.print token_print_chain fmt tk

let register_token (info:token Chain.info) =
  Chain.register info (token_compare_chain, token_print_chain)


(****************************************************************************)
(**                             {2 Flows}                                   *)
(****************************************************************************)


(** Map of flows binding tokens to abstract elements *)

module FlowMap =
struct
  include MapExt.Make(
    struct
      type t = token
      let compare = compare_token
      let print = pp_token
    end
    )

  let print pp_value fmt m =
    if is_empty m then Format.pp_print_string fmt "⊥"
    else
      Format.fprintf fmt "@[<v>%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
           (fun fmt (k, v) -> Format.fprintf fmt "⏵ %a ↦@\n@[<hov4>    %a@]" pp_token k pp_value v)
        ) (bindings m)
end

type 'a fmap = 'a FlowMap.t Top.with_top
(** ['a fmap] decorates a flow map with a ⊤ map *)

type 'a flow = {
  map   : 'a fmap;
  annot : 'a annot;
}
(** A flow is a flow map augmented with an annotation *)


let bottom annot : 'a flow = {
  map = Nt FlowMap.empty;
  annot;
}

let top annot : 'a flow = {
  map = TOP;
  annot;
}

let singleton (annot:'a Annotation.annot) (tk:token) (env:'a) : 'a flow =
  {
    map = Nt (FlowMap.singleton tk env);
    annot
  }

let is_bottom (lattice: 'a lattice) (flow: 'a flow) : bool =
  top_dfl1 false (fun m ->
      FlowMap.for_all (fun _ v -> lattice.is_bottom v) m
    ) flow.map

let is_top (lattice: 'a lattice) (flow: 'a flow) : bool =
  top_dfl1 true (fun _ -> false) flow.map


let subset (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : bool =
  top_included
    (FlowMap.for_all2zo
       (fun _ v1 -> lattice.is_bottom v1) (* non-⊥ ⊈ ⊥ *)
       (fun _ v2 -> true)  (* ⊥ ⊆ non-⊥ *)
       (fun _ v1 v2 -> lattice.subset v1 v2)
    )
    flow1.map flow2.map

let join (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  let map = top_lift2
      (FlowMap.map2zo
         (fun _ v1 -> v1)
         (fun _ v2 -> v2)
         (fun _ v1 v2 -> lattice.join v1 v2)
      )
      flow1.map flow2.map
  in
  {map; annot = flow2.annot}

let join_list lattice ?(annot=Annotation.empty) l =
  match l with
  | [] -> bottom annot
  | hd :: tl -> List.fold_left (join lattice) hd tl

let meet (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  let map = top_neutral2
      (fun b1 b2 ->
         FlowMap.map2zo
           (fun _ v1 -> lattice.bottom)
           (fun _ v2 -> lattice.bottom)
           (fun _ v1 v2 -> lattice.meet v1 v2)
           b1 b2
      )
      flow1.map flow2.map
  in
  {map; annot = flow2.annot}

let meet_list lattice ?(annot=Annotation.empty) l =
  match l with
  | [] -> bottom annot
  | hd :: tl -> List.fold_left (meet lattice) hd tl


let widen (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  let annot = flow2.annot in
  let map = top_lift2
      (FlowMap.map2zo
         (fun _ v1 -> v1)
         (fun _ v2 -> v2)
         (fun _ v1 v2 -> lattice.widen annot v1 v2)
      )
      flow1.map flow2.map
  in
  {map; annot}

let print (lattice: 'a lattice) fmt (flow : 'a flow) : unit =
  top_fprint (FlowMap.print lattice.print) fmt flow.map

let get (tk: token) (lattice: 'a lattice) (flow: 'a flow) : 'a =
  try
    let m = top_to_exn flow.map in
    try FlowMap.find tk m with Not_found -> lattice.bottom
  with Found_TOP -> lattice.top


let set (tk: token) (a: 'a) (lattice:'a lattice) (flow: 'a flow) : 'a flow =
  let map = top_lift1 (fun m ->
      if lattice.is_bottom a then FlowMap.remove tk m
      else FlowMap.add tk a m
    ) flow.map
  in
  {flow with map}

let copy (tk1:token) (tk2:token) (lattice:'a lattice) (flow1:'a flow) (flow2:'a flow) : 'a flow =
  set tk2 (get tk1 lattice flow1) lattice flow2

let add (tk: token) (a: 'a) (lattice: 'a lattice) (flow: 'a flow) : 'a flow =
  let map = top_lift1 (fun m ->
      if lattice.is_bottom a then m
      else
        let a' =
          try
            let old = FlowMap.find tk m in
            lattice.join a old
          with Not_found ->
            a
        in
        FlowMap.add tk a' m
    ) flow.map
  in
  {map; annot = flow.annot}

let remove (tk: token) (flow: 'a flow) : 'a flow =
  let map = top_lift1 (FlowMap.remove tk) flow.map in
  {flow with map}

let filter (f: token -> 'a -> bool) (flow: 'a flow) : 'a flow =
  let map = top_lift1 (FlowMap.filter f) flow.map in
  {flow with map}

let map (f: token -> 'a -> 'a) (flow: 'a flow) : 'a flow =
  let map = top_lift1 (FlowMap.mapi f) flow.map in
  {flow with map}

let fold (f: 'b -> token -> 'a -> 'b) (init: 'b) (flow: 'a flow) : 'b =
  let m = top_to_exn flow.map in
  FlowMap.fold (fun tk a acc -> f acc tk a) m init

let merge (f: token -> 'a option -> 'a option -> 'a option) (lattice: 'a lattice) (flow1: 'a flow) (flow2: 'a flow) : 'a flow =
  let annot = flow2.annot in
  let fopt tk a b =
    match f tk a b with
    | None -> lattice.bottom
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
