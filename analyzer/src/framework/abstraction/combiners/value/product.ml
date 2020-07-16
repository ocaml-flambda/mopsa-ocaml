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

open Ast.All
open Core.All
open Sig.Domain.Value
open Sig.Reduction.Value
open Common



module MakeValuePair(V1:VALUE)(V2:VALUE) : VALUE =
struct

  include Lattices.Pair.Make(V1)(V2)

  let id = V_pair(V1.id,V2.id)

  let name = "framework.combiners.value.product"

  let display = 
    match V2.id with
    | V_empty -> V1.display
    | _ ->  "(" ^ V1.display ^ " ∧ " ^ V2.display ^ ")"

  let print fmt (v1,v2) =
    match V2.id with
    | V_empty -> V1.print fmt v1
    | _ ->  Format.fprintf fmt "%a ∧ %a" V1.print v1 V2.print v2

  
  let hdman (man:('a,t) value_man) : (('a,V1.t) value_man) = {
    man with
    eval = (fun exp -> man.eval exp |> fst);
  }

  let tlman (man:('a,t) value_man) : (('a,V2.t) value_man) = {
    man with
    eval = (fun exp -> man.eval exp |> snd);
  }

  let constant t c =
    match V1.constant t c, V2.constant t c with
    | None, None       -> None
    | Some v1, Some v2 -> Some (v1,v2)
    | Some v1, None    -> Some (v1,V2.top)
    | None, Some v2    -> Some (V1.top,v2)

  let cast man e t =
    match V1.cast (hdman man) e t, V2.cast (tlman man) e t with
    | None, None       -> None
    | Some v1, Some v2 -> Some (v1,v2)
    | Some v1, None    -> Some (v1,V2.top)
    | None, Some v2    -> Some (V1.top,v2)

  let unop op t = apply (V1.unop op t) (V2.unop op t)

  let binop op t = apply2 (V1.binop op t) (V2.binop op t)

  let filter b t = apply (V1.filter b t) (V2.filter b t)

  let bwd_unop op t = apply2 (V1.bwd_unop op t) (V2.bwd_unop op t)

  let bwd_binop op t (v1,v2) (w1,w2) (r1,r2) =
    let x1,y1 = V1.bwd_binop op t v1 w1 r1 in
    let x2,y2 = V2.bwd_binop op t v2 w2 r2 in
    ((x1,x2),(y1,y2))

  let bwd_cast man t e = apply (V1.bwd_cast (hdman man) t e) (V2.bwd_cast (tlman man) t e)

  let predicate op b t = apply (V1.predicate op b t) (V2.predicate op b t)

  let compare op b t (v1,v2) (w1,w2) =
    let x1,y1 = V1.compare op b t v1 w1 in
    let x2,y2 = V2.compare op b t v2 w2 in
    ((x1,x2),(y1,y2))

  let ask man q =
    OptionExt.neutral2
      (meet_query q
         ~meet:(fun _ _ -> Exceptions.panic "abstract queries called from value abstraction"))
      (V1.ask (hdman man) q)
      (V2.ask (tlman man) q)
    
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

  let reduce_pair ((v1,v2) as v) =
    let v1' = reduce v1 in
    let v2' = reduce v2 in
    if v1 == v1' && v2 == v2' then v else (v1',v2')


  let constant t c = V.constant t c |> OptionExt.lift reduce
  let cast man t e = V.cast man t e |> OptionExt.lift reduce
  let unop op t v = V.unop op t v |> reduce
  let binop op t v1 v2 = V.binop op t v1 v2 |> reduce
  let filter b t v = V.filter b t v |> reduce
  let bwd_unop op t v r = V.bwd_unop op t v r |> reduce
  let bwd_binop op t v1 v2 r = V.bwd_binop op t v1 v2 r |> reduce_pair
  let predicate op b t v = V.predicate op b t v |> reduce
  let compare op b t v1 v2 = V.compare op b t v1 v2 |> reduce_pair                        

end


let make
    (values: (module VALUE) list)
    (rules: (module VALUE_REDUCTION) list)
  : (module VALUE) =

  let rec aux = function
    | [] -> (module EmptyValue : VALUE)
    | [v] -> v
    | hd::tl ->
      let v = aux tl in
      (module MakeValuePair(val hd)(val v))
  in
  let v = aux values in
  (module Make(val v)(struct let rules = rules end))
