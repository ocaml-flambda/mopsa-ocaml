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
open Sig.Abstraction.Value
open Sig.Reduction.Value
open Common



module MakeValuePair(V1:VALUE)(V2:VALUE) : VALUE with type t = V1.t * V2.t =
struct

  include Lattices.Pair.Make(V1)(V2)

  let id = V_pair(V1.id,V2.id)

  let name = "framework.combiners.value.product"

  let display =
    match V2.id with
    | V_empty -> V1.display
    | _ ->  "(" ^ V1.display ^ " ∧ " ^ V2.display ^ ")"

  let accept_type t = V1.accept_type t && V2.accept_type t

  let print printer (v1,v2) =
      pp_obj_list printer
        [ pbox V1.print v1;
          pbox V2.print v2 ]
         ~lopen:"" ~lsep:"∧" ~lclose:""


  let v1_man (man:('v,t) value_man) : (('v,V1.t) value_man) = {
    man with
    get  = (fun a -> man.get a |> fst);
    set  = (fun x a ->
        let (v1,v2) = man.get a in
        if x == v1 then a else man.set (x,v2) a);
  }

  let v2_man (man:('v,t) value_man) : (('v,V2.t) value_man) = {
    man with
    get  = (fun a -> man.get a |> snd);
    set  = (fun x a ->
        let (v1,v2) = man.get a in
        if x == v2 then a else man.set (v1,x) a);
  }

  let eval man e =
    let r1 = V1.eval (v1_man man) e in
    let r2 = V2.eval (v2_man man) e in
    (r1,r2)

  let filter b t v =
    let r1 = V1.filter b t (fst v) in
    let r2 = V2.filter b t (snd v) in
    (r1,r2)

  let backward man e ve r =
    let r1 = V1.backward (v1_man man) e (map_vexpr fst ve) r in
    let r2 = V2.backward (v2_man man) e (map_vexpr snd ve) r in
    map2_vexpr
      (fun v1 -> assert false)
      (fun v2 -> assert false)
      (fun v1 v2 -> (v1, v2))
      r1 r2

  let compare man op b e1 v1 e2 v2 =
    let (r11,r12) = V1.compare (v1_man man) op b e1 (fst v1) e2 (fst v2) in
    let (r21,r22) = V2.compare (v2_man man) op b e1 (snd v1) e2 (snd v2) in
    (r11,r21), (r12,r22)

  let eval_ext man e =
    let r1 = V1.eval_ext (v1_man man) e in
    let r2 = V2.eval_ext (v2_man man) e in
    OptionExt.neutral2 man.meet r1 r2

  let backward_ext man e ev r =
    let r1 = V1.backward_ext (v1_man man) e ev r in
    let r2 = V2.backward_ext (v2_man man) e ev r in
    OptionExt.neutral2 (merge_vexpr man.meet) r1 r2

  let compare_ext man op b e1 v1 e2 v2 =
    let r1 = V1.compare_ext (v1_man man) op b e1 v1 e2 v2 in
    let r2 = V2.compare_ext (v2_man man) op b e1 v1 e2 v2 in
    OptionExt.neutral2 (fun (r11,r12) (r21,r22) -> (man.meet r11 r21),(man.meet r12 r22)) r1 r2

  let avalue aval v =
    OptionExt.neutral2
      (meet_avalue aval)
      (V1.avalue aval (fst v))
      (V2.avalue aval (snd v))

  let ask man query =
    OptionExt.neutral2
      (meet_query query)
      (V1.ask (v1_man man) query)
      (V2.ask (v2_man man) query)

end

module Make(V:VALUE)(R:sig val rules: (module VALUE_REDUCTION) list end) : VALUE with type t = V.t =
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

  let reduce_man (man:('v,t) value_man) a =
    man.set (reduce (man.get a)) a

  let reduce_vexpr ve =
    map_vexpr reduce ve

  let reduce_man_vexpr man ve =
    map_vexpr (reduce_man man) ve

  let reduce_pair ((v1,v2) as v) =
    let v1' = reduce v1 in
    let v2' = reduce v2 in
    if v1 == v1' && v2 == v2' then v else (v1',v2')

  let reduce_man_pair (man:('v,t) value_man) (a1,a2) =
    let v1 = man.get a1 in
    let v2 = man.get a2 in
    let (v1',v2') = reduce_pair (v1,v2) in
    man.set v1' a1, man.set v2' a2

  let eval man e = V.eval man e |> reduce
  let filter b t v = V.filter b t v |> reduce
  let backward man e ve r = V.backward man e ve r |> reduce_vexpr
  let compare man op b e1 v1 e2 v2 = V.compare man op b e1 v1 e2 v2 |> reduce_pair
  let eval_ext man e = V.eval_ext man e |> OptionExt.lift (reduce_man man)
  let backward_ext man e ve r = V.backward_ext man e ve r |> OptionExt.lift (reduce_man_vexpr man)
  let compare_ext man op b e1 v1 e2 v2 = V.compare_ext man op b e1 v1 e2 v2 |> OptionExt.lift (reduce_man_pair man)

end


let make
    (values: (module VALUE) list)
    (rules: (module VALUE_REDUCTION) list)
  : (module VALUE) =

  let rec aux = function
    | [] -> assert false
    | [v] -> v
    | hd::tl ->
      let v = aux tl in
      (module MakeValuePair(val hd : VALUE)(val v : VALUE) : VALUE)
  in
  let v = aux values in
  (module Make(val v)(struct let rules = rules end))
