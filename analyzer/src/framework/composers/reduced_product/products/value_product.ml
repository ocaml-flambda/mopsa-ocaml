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

(** N-ary reduced product of non-relational value abstractions. *)

open Core
open Value
open Channel
open Pool
open Reductions
open Eq


type _ value +=
  | V_empty_product : unit value
  | V_reduced_product : 'a value * 'b value -> ('a * 'b) value


(** Create a reduced product of a pool of value abstractions and reduction operators *)
module Make
    (Config:
     sig
       type t
       val pool : t value_pool
       val value_rules : (module Value_reduction.REDUCTION) list
     end)  =
struct
  type t = Config.t


  let name = "framework.domains.nonre.value_reduced_product", (
      let rec aux : type a. a value_pool -> string list =
        function
        | Nil -> []
        | Cons(hd, tl) ->
          let module V = (val hd) in
          snd (V.name) :: aux tl
      in
      let names = aux Config.pool in
      String.concat " ⊗ " names
    )

  let id =
    let rec aux : type a. a value_pool -> a value =
      function
      | Nil -> V_empty_product
      | Cons(hd, tl) ->
        let module V = (val hd) in
        V_reduced_product(V.id, aux tl)
    in
    aux Config.pool

  let identify : type a. a value -> (t, a) eq option =
    fun id ->
    let rec aux : type a b. a value_pool -> b value -> (a, b) eq option =
      fun pool id ->
        match pool, id with
        | Nil, V_empty_product -> Some Eq
        | Cons(hd, tl), V_reduced_product(id1, id2) ->
          let module V = (val hd) in
          begin match V.identify id1, aux tl id2 with
            | Some Eq, Some Eq -> Some Eq
            | _ -> None
          end
        | _ -> None
    in
    aux Config.pool id

  let bottom : t =
    let rec aux : type a. a value_pool -> a = fun pool ->
      match pool with
      | Nil -> ()
      | Cons(hd, tl) ->
        let module V = (val hd) in
        let tl = aux tl in
        V.bottom, tl
    in
    aux Config.pool

  let top : t =
    let rec aux : type a. a value_pool -> a = fun pool ->
      match pool with
      | Nil -> ()
      | Cons(hd, tl) ->
        let module V = (val hd) in
        let tl = aux tl in
        V.top, tl
    in
    aux Config.pool

  let is_bottom (v:t) : bool = 
    let rec aux : type a. a value_pool -> a -> bool = fun pool v ->
      match pool, v with
      | Nil, () -> false
      | Cons(hd, tl), (vhd, vtl) ->
        let module V = (val hd) in
        V.is_bottom vhd || aux tl vtl
    in
    aux Config.pool v

  let subset (v1:t) (v2:t) : bool =
    let rec aux : type a. a value_pool -> a -> a -> bool = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Nil, (), () -> true
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.subset vhd1 vhd2 && aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let join annot (v1:t) (v2:t) : t =
    let rec aux : type a. a value_pool -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Nil, (), () -> ()
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.join annot vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let meet annot (v1:t) (v2:t) =
    let rec aux : type a. a value_pool -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Nil, (), () -> ()
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.meet annot vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let widen annot v1 v2 =
    let rec aux : type a. a value_pool -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Nil, (), () -> ()
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.widen annot vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let print fmt v =
    let rec aux : type a. a value_pool -> Format.formatter -> a -> unit = fun pool fmt v ->
      match pool, v with
      | Nil, () -> ()
      | Cons(hd, Nil), (vhd, ()) ->
        let module V = (val hd) in
        Format.fprintf fmt "%a" V.print vhd

      | Cons(hd, tl), (vhd, vtl) ->
        let module V = (val hd) in
        Format.fprintf fmt "%a ∧ %a" V.print vhd (aux tl) vtl
    in
    aux Config.pool fmt v

  let zone =
    (* FIXME: check that all values are defined on the same zone *)
    match Config.pool with
    | Cons(hd, tl) ->
      let module V = (val hd) in
      V.zone
    | _ -> assert false

  let of_constant t (c: Ast.constant) : t =
    let rec aux : type a. a value_pool -> a = fun pool ->
      match pool with
      | Nil -> ()
      | Cons(hd, tl) ->
        let module V = (val hd) in
        V.of_constant t c, aux tl
    in
    aux Config.pool

  let man : t value_man = {
    pool = Config.pool;
    get_value = (
      let f : type a. a value -> t -> a = fun k v ->
        let rec aux : type a b. a value -> b value_pool -> b -> a = fun k pool v ->
          match pool, v with
          | Nil, () -> raise Not_found
          | Cons(hd, tl), (vhd, vtl) ->
            let module V = (val hd) in
            match V.identify k with
            | Some Eq -> vhd
            | None -> aux k tl vtl
        in
        aux k Config.pool v
      in
      f
    );
    set_value = (
      let f : type a. a value -> a -> t -> t = fun k x v ->
        let rec aux : type a b. a value -> a -> b value_pool -> b -> b = fun k x pool v ->
          match pool, v with
          | Nil, () -> raise Not_found
          | Cons(hd, tl), (vhd, vtl) ->
            let module V = (val hd) in
            match V.identify k with
            | Some Eq -> (x, vtl)
            | None -> (vhd, aux k x tl vtl)
        in
        aux k x Config.pool v
      in
      f
    );
  }
  
  let reduce (v: t) : t with_channel =
    let rec apply (l: (module Value_reduction.REDUCTION) list) v : t with_channel =
      match l with
      | [] -> return v
      | hd :: tl ->
        let module R = (val hd : Value_reduction.REDUCTION) in
        R.reduce man v |> bind @@ fun v' ->
        apply tl v'
    in
    let rec lfp v =
      apply Config.value_rules v |> bind @@ fun v' ->
      if subset v v' then return v else lfp v'
    in
    lfp v

  let unop t op v =
    let rec aux : type a. a value_pool -> a -> a with_channel = fun pool v ->
      match pool, v with
      | Nil, () -> return ()
      | Cons(hd, tl), (vhd, vtl) ->
        let module V = (val hd) in
        V.unop t op vhd |> bind @@ fun w1 ->
        aux tl vtl |> bind @@ fun w2 ->
        return (w1, w2)
    in
    aux Config.pool v |> bind @@ fun v' ->
    reduce v'

  let binop t op v1 v2 =
    let rec aux : type a. a value_pool -> a -> a -> a with_channel = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Nil, (), () -> return ()
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.binop t op vhd1 vhd2 |> bind @@ fun w1 ->
        aux tl vtl1 vtl2 |> bind @@ fun w2 ->
        return (w1, w2)
    in
    aux Config.pool v1 v2 |> bind @@ fun v' ->
    reduce v'

  let filter t v b =
    let rec aux : type a. a value_pool -> a -> a with_channel = fun pool v ->
      match pool, v with
      | Nil, () -> return ()
      | Cons(hd, tl), (vhd, vtl) ->
        let module V = (val hd) in
        V.filter t vhd b |> bind @@ fun w1 ->
        aux tl vtl |> bind @@ fun w2 ->
        return (w1, w2)
    in
    aux Config.pool v |> bind @@ fun v' ->
    reduce v'

  let bwd_unop t op v r =
    let rec aux : type a. a value_pool -> a -> a -> a with_channel = fun pool v r ->
      match pool, v, r with
      | Nil, (), () -> return ()
      | Cons(hd, tl), (vhd, vtl), (rhd, rtl) ->
        let module V = (val hd) in
        V.bwd_unop t op vhd rhd |> bind @@ fun w1 ->
        aux tl vtl rtl |> bind @@ fun w2 ->
        return (w1, w2)
    in
    aux Config.pool v r |> bind @@ fun v' ->
    reduce v'

  let bwd_binop t op v1 v2 r =
    let rec aux : type a. a value_pool -> a -> a -> a -> (a * a) with_channel = fun pool v1 v2 r ->
      match pool, v1, v2, r with
      | Nil, (), (), () -> return ((), ())
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2), (rhd, rtl) ->
        let module V = (val hd) in
        V.bwd_binop t op vhd1 vhd2 rhd |> bind @@ fun (vhd1', vhd2') ->
        aux tl vtl1 vtl2 rtl |> bind @@ fun (vtl1', vtl2') ->
        return ((vhd1', vtl1'), (vhd2', vtl2'))
    in
    aux Config.pool v1 v2 r |> bind @@ fun (v1', v2') ->
    reduce v1' |> bind @@ fun r1 ->
    reduce v2' |> bind @@ fun r2 ->
    return (r1, r2)

  let compare t op v1 v2 r =
    let rec aux : type a. a value_pool -> a -> a -> (a * a) with_channel = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Nil, (), () -> return ((), ())
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.compare t op vhd1 vhd2 r |> bind @@ fun (vhd1', vhd2') ->
        aux tl vtl1 vtl2 |> bind @@ fun (vtl1', vtl2') ->
        return ((vhd1', vtl1'), (vhd2', vtl2'))
    in
    aux Config.pool v1 v2 |> bind @@ fun (v1', v2') ->
    reduce v1' |> bind @@ fun r1 ->
    reduce v2' |> bind @@ fun r2 ->
    return (r1, r2)

  let ask : type r. r Query.query -> (Ast.expr -> t) -> r option =
    fun query eval ->
      let rec aux : type a r. a value_pool -> r Query.query -> (Ast.expr -> a) -> r option =
        fun pool query eval ->
          match pool with
          | Nil -> None
          | Cons(hd, tl) ->
            let module V = (val hd) in
            let eval' = (fun exp -> fst @@ eval exp) in
            match V.ask query eval' with
            | None -> aux tl query (fun exp -> snd @@ eval exp)
            | r -> r
      in
      aux Config.pool query eval
end
