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

(** Tokens identifying control flows *)

type token = ..

type token += T_cur

let token_compare_chain = TypeExt.mk_compare_chain (fun tk1 tk2 ->
    match tk1, tk2 with
    | T_cur, T_cur -> 0
    | _ -> compare tk1 tk2
  )

let compare_token tk1 tk2 = TypeExt.compare token_compare_chain tk1 tk2

let token_print_chain = TypeExt.mk_print_chain (fun fmt tk ->
    match tk with
    | T_cur -> Format.fprintf fmt "cur"
    | _ -> Exceptions.panic ~loc:__LOC__ "unknown token"
  )

let pp_token fmt tk = TypeExt.print token_print_chain fmt tk

let register_token (info:token TypeExt.info) =
  TypeExt.register info token_compare_chain token_print_chain


module TokenMap =
struct

  open Top
  open Lattice.Sig

  module Map = MapExt.Make(
    struct
      type t = token
      let compare = compare_token
      let print = pp_token
    end
    )

  type +'a t = 'a Map.t with_top

  let bottom : 'a t =
    Nt Map.empty

  let top : 'a t =
    TOP

  let singleton (tk:token) (env:'a) : 'a t =
    Nt (Map.singleton tk env)

  let is_bottom (lattice: 'a lattice) (tmap: 'a t) : bool =
    top_dfl1 false (fun m ->
        Map.for_all (fun _ v -> lattice.is_bottom v) m
      ) tmap

  let is_top (lattice: 'a lattice) (tmap: 'a t) : bool =
    top_dfl1 true (fun _ -> false) tmap


  let subset (lattice: 'a lattice) (tmap1: 'a t) (tmap2: 'a t) : bool =
    top_included
      (Map.for_all2zo
         (fun _ v1 -> lattice.is_bottom v1) (* non-⊥ ⊈ ⊥ *)
         (fun _ v2 -> true)  (* ⊥ ⊆ non-⊥ *)
         (fun _ v1 v2 -> lattice.subset v1 v2)
      )
      tmap1 tmap2

  let join (lattice: 'a lattice) (tmap1: 'a t) (tmap2: 'a t) : 'a t =
    top_lift2
      (Map.map2zo
         (fun _ v1 -> v1)
         (fun _ v2 -> v2)
         (fun _ v1 v2 -> lattice.join v1 v2)
      )
      tmap1 tmap2

  let join_list lattice l =
    match l with
    | [] -> bottom
    | hd :: tl -> List.fold_left (join lattice) hd tl

  let meet (lattice: 'a lattice) (tmap1: 'a t) (tmap2: 'a t) : 'a t =
    top_neutral2
      (fun b1 b2 ->
         Map.map2zo
           (fun _ v1 -> lattice.bottom)
           (fun _ v2 -> lattice.bottom)
           (fun _ v1 v2 -> lattice.meet v1 v2)
           b1 b2
      )
      tmap1 tmap2

  let meet_list lattice l =
    match l with
    | [] -> bottom
    | hd :: tl -> List.fold_left (meet lattice) hd tl

  let widen (lattice: 'a lattice) (ctx: Context.uctx) (tmap1: 'a t) (tmap2: 'a t) : 'a t =
    top_lift2
      (Map.map2zo
         (fun _ v1 -> v1)
         (fun _ v2 -> v2)
         (fun _ v1 v2 -> lattice.widen ctx v1 v2)
      )
      tmap1 tmap2

  let print (lattice: 'a lattice) fmt (tmap : 'a t) : unit =
    top_fprint (fun fmt m ->
        if Map.is_empty m then Format.pp_print_string fmt "⊥"
        else
          Format.fprintf fmt "@[<v>%a@]"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
               (fun fmt (k, v) -> Format.fprintf fmt "⏵ %a ↦@\n@[<hov4>    %a@]" pp_token k lattice.print v)
            ) (Map.bindings m)
      ) fmt tmap


  let get (tk: token) (lattice: 'a lattice) (tmap: 'a t) : 'a =
    try
      let m = top_to_exn tmap in
      try Map.find tk m with Not_found -> lattice.bottom
    with Found_TOP -> lattice.top


  let mem (tk: token) (tmap: 'a t) : bool =
    top_dfl1 true (Map.mem tk) tmap

  let find (tk: token) (tmap: 'a t) : 'a =
    Map.find tk (top_to_exn tmap)

  let find_opt (tk: token) (tmap: 'a t) : 'a option =
    top_apply (fun m -> Some (Map.find tk m)) None tmap

  let set (tk: token) (a: 'a) (lattice:'a lattice) (tmap: 'a t) : 'a t =
    top_lift1 (fun m ->
        if lattice.is_bottom a then Map.remove tk m
        else Map.add tk a m
      ) tmap

  let copy (tk1:token) (tk2:token) (lattice:'a lattice) (tmap1:'a t) (tmap2:'a t) : 'a t =
    set tk2 (get tk1 lattice tmap1) lattice tmap2

  let add (tk: token) (a: 'a) (lattice: 'a lattice) (tmap: 'a t) : 'a t =
    top_lift1 (fun m ->
        if lattice.is_bottom a then m
        else
          let a' =
            try
              let old = Map.find tk m in
              lattice.join a old
            with Not_found ->
              a
          in
          Map.add tk a' m
      ) tmap

  let remove (tk: token) (tmap: 'a t) : 'a t =
    top_lift1 (Map.remove tk) tmap

  let filter (f: token -> 'a -> bool) (tmap: 'a t) : 'a t =
    top_lift1 (Map.filter f) tmap

  let map (f: token -> 'a -> 'b) (tmap: 'a t) : 'b t =
    top_lift1 (Map.mapi f) tmap

  let fold (f: 'b -> token -> 'a -> 'b) (init: 'b) (tmap: 'a t) : 'b =
    let m = top_to_exn tmap in
    Map.fold (fun tk a acc -> f acc tk a) m init

  let merge(f: token -> 'a option -> 'a option -> 'a option) (lattice: 'a lattice) (tmap1: 'a t) (tmap2: 'a t) : 'a t =
    top_lift2
      (Map.map2zo
         (fun tk v1 -> Option.default lattice.bottom (f tk (Some v1) None))
         (fun tk v2 -> Option.default lattice.bottom (f tk None (Some v2)))
         (fun tk v1 v2 -> Option.default lattice.bottom (f tk (Some v1) (Some v2)))
      )
      tmap1 tmap2

  let neutral2 (f: token -> 'a -> 'a -> 'a) (tmap1: 'a t) (tmap2: 'a t) : 'a t =
    top_lift2
      (Map.map2zo
         (fun tk v1 -> v1)
         (fun tk v2 -> v2)
         (fun tk v1 v2 -> f tk v1 v2)
      )
      tmap1 tmap2

  let absorb2 (f: token -> 'a -> 'a -> 'a) (tmap1: 'a t) (tmap2: 'a t) : 'a t =
    top_lift2
      (Map.merge
         (fun tk ov1 ov2->
            match ov1, ov2 with
            | None, _ | _, None -> None
            | Some v1, Some v2 -> Some (f tk v1 v2)
         )
      )
      tmap1 tmap2

  
end
