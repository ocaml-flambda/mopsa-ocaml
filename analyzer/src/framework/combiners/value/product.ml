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

(** Reduced product of value abstractions with n-ary reduction rules *)

open Core.All
open Sig.Combiner.Value
open Sig.Reduction.Value
open Common



module MakeValuePair(V1:VALUE_COMBINER)(V2:VALUE_COMBINER) : VALUE_COMBINER with type t = V1.t * V2.t =
struct

  include Lattices.Pair.Make(V1)(V2)

  let id = V_pair(V1.id,V2.id)

  let name = "framework.combiners.value.product"

  let display =
    match V2.id with
    | V_empty -> V1.display
    | _ ->  "(" ^ V1.display ^ " ∧ " ^ V2.display ^ ")"

  let accept_type t = V1.accept_type t || V2.accept_type t

  let print printer (v1,v2) =
      pp_obj_list printer
        [ pbox V1.print v1;
          pbox V2.print v2 ]
         ~lopen:"" ~lsep:"∧" ~lclose:""


  let hdman (man:('a,'v,t) value_man) : (('a,'v,V1.t) value_man) = {
    man with
    get  = (fun a -> man.get a |> fst);
    set  = (fun x a ->
        let (v1,v2) = man.get a in
        if x == v1 then a else man.set (x,v2) a);
  }

  let tlman (man:('a,'v,t) value_man) : (('a,'v,V2.t) value_man) = {
    man with
    get  = (fun a -> man.get a |> snd);
    set  = (fun x a ->
        let (v1,v2) = man.get a in
        if x == v2 then a else man.set (v1,x) a);
  }

  let constant t c =
    let v1 = if V1.accept_type t then Some (V1.constant t c) else None in
    let v2 = if V2.accept_type t then Some (V2.constant t c) else None in
    match v1, v2 with
    | None, None       -> assert false
    | Some v1, Some v2 -> (v1,v2)
    | Some v1, None    -> (v1,V2.top)
    | None, Some v2    -> (V1.top,v2)

  let unop man t op (a,e) =
    apply
      (fun _ -> V1.unop (hdman man) t op (a,e))
      (fun _ -> V2.unop (tlman man) t op (a,e))
      (man.get a)

  let binop man t op (a1,e1) (a2,e2) =
    apply2
      (fun _ _ -> V1.binop (hdman man) t op (a1,e1) (a2,e2))
      (fun _ _ -> V2.binop (tlman man) t op (a1,e1) (a2,e2))
      (man.get a1) (man.get a2)

  let filter t b =
    apply (V1.filter t b) (V2.filter t b)

  let bwd_unop man t op (a,e) (r1,r2) =
    let aa = V1.bwd_unop (hdman man) t op (a,e) r1 in
    V2.bwd_unop (tlman man) t op (aa,e) r2

  let bwd_binop man op t (a1,e1) (a2,e2) (r1,r2) =
    let aa1,aa2 = V1.bwd_binop (hdman man) op t (a1,e1) (a2,e2) r1 in
    V2.bwd_binop (tlman man) op t (aa1,e1) (aa2,e2) r2

  let predicate t op b =
    apply
      (V1.predicate t op b) (V2.predicate t op b)

  let compare t op b (v1,v2) (w1,w2) =
    let x1,y1 = V1.compare t op b v1 w1 in
    let x2,y2 = V2.compare t op b v2 w2 in
    ((x1,x2),(y1,y2))

  let ask man q =
    OptionExt.neutral2
      (meet_query q
         ~meet:(fun _ _ -> Exceptions.panic "abstract queries called from value abstraction"))
      (V1.ask (hdman man) q)
      (V2.ask (tlman man) q)

  let refine msg ((v1,v2) as v) =
    let r1 = V1.refine msg v1 in
    let r2 = V2.refine msg v2 in
    match r1,r2 with
    | None,None -> None
    | Some r1,Some r2 ->
      if r1 == v1 && r2 == v2 then Some v else Some (r1,r2)
    | Some r1, None ->
      if r1 == v1 then Some v else Some (r1,v2)
    | None, Some r2 ->
      if r2 == v2 then Some v else Some (v1,r2)



end

module Make(V:VALUE_COMBINER)(R:sig val rules: (module VALUE_REDUCTION) list end) : VALUE_COMBINER with type t = V.t =
struct

  include V

  let rman = {
    get = (fun (type a) (idx:a id) (v:t) ->
        let rec aux : type b. b id -> b -> a =
          fun idy v ->
            match idy, v with
            | V_empty, () -> raise Not_found
            | V_pair(hd,tl), (vhd,vtl) ->
              begin
                try aux hd vhd
                with Not_found -> aux tl vtl
              end
            | _ ->
              match equal_id idy idx with
              | Some Eq -> v
              | None -> raise Not_found
        in
        aux id v
      );
    set = (fun (type a) (idx:a id) (x:a) (v:t) ->
        let rec aux : type b. b id -> b -> b =
          fun idy v ->
            match idy, v with
            | V_empty, () -> raise Not_found
            | V_pair(hd,tl), (vhd,vtl) ->
              begin
                try aux hd vhd,vtl
                with Not_found -> vhd,aux tl vtl
              end
            | _ ->
              match equal_id idy idx with
              | Some Eq -> x
              | None -> raise Not_found
        in
        aux id v
      )
  }

  let reduce (v:t) : t =
    let apply v =
      List.fold_left (fun acc r ->
          let module Reduction = (val r : VALUE_REDUCTION) in
          Reduction.reduce rman acc
        ) v R.rules
    in
    let rec lfp v =
      let v' = apply v in
      if subset v v' then v else lfp v'
    in
    lfp v

  let reduce_pair ((v1,v2) as v) =
    let v1' = reduce v1 in
    let v2' = reduce v2 in
    if v1 == v1' && v2 == v2' then v else (v1',v2')

  let reduce_man (man:('a,'v,t) value_man) a =
    man.set (reduce (man.get a)) a

  let reduce_man_pair (man:('a,'v,t) value_man) (a1,a2) =
    let v = man.get a1, man.get a2 in
    let (v1,v2) = reduce_pair v in
    man.set v1 a1, man.set v2 a2

  let constant t c = V.constant t c |> reduce
  let unop man t op v = V.unop man t op v |> reduce
  let binop man t op v1 v2 = V.binop man t op v1 v2 |> reduce
  let filter t b v = V.filter t b v |> reduce
  let bwd_unop man t op v r = V.bwd_unop man t op v r |> reduce_man man
  let bwd_binop man t op v1 v2 r = V.bwd_binop man t op v1 v2 r |> reduce_man_pair man
  let predicate t op b v = V.predicate t op b v |> reduce
  let compare t op b v1 v2 = V.compare t op b v1 v2 |> reduce_pair

end


let make
    (values: (module VALUE_COMBINER) list)
    (rules: (module VALUE_REDUCTION) list)
  : (module VALUE_COMBINER) =

  let rec aux = function
    | [] -> assert false
    | [v] -> v
    | hd::tl ->
      let v = aux tl in
      (module MakeValuePair(val hd : VALUE_COMBINER)(val v : VALUE_COMBINER) : VALUE_COMBINER)
  in
  let v = aux values in
  (module Make(val v)(struct let rules = rules end))
