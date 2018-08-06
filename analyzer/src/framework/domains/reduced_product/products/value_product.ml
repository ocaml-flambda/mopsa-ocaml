(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** N-ary reduced product of non-relational value abstractions. *)

open Value
open Pool
open Reductions.Value_reduction

type _ value +=
  | V_empty_product : unit value
  | V_reduced_product : 'a value * 'b value -> ('a * 'b) value


(** Create a reduced product of a pool of value abstractions and reduction operators *)
module Make
    (Config:
     sig
       type t
       val pool : t value_pool
       val rules : (module REDUCTION) list
       val display : string
     end) : Value.VALUE =
struct
  type t = Config.t


  let name = "framework.domains.nonre.value_reduced_product", Config.display

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

  let display = Config.display

  let zone =
    (* FIXME: check that all values are defined on the same zone *)
    match Config.pool with
    | Cons(hd, tl) ->
      let module V = (val hd) in
      V.zone
    | _ -> assert false

  let of_constant (c: Ast.constant) : t =
    let rec aux : type a. a value_pool -> a = fun pool ->
      match pool with
      | Nil -> ()
      | Cons(hd, tl) ->
        let module V = (val hd) in
        V.of_constant c, aux tl
    in
    aux Config.pool

  let reduce man v =
    let rec apply v (l: (module REDUCTION) list) =
      match l with
      | [] -> v
      | hd :: tl ->
        let module R = (val hd : REDUCTION) in
        apply (R.reduce man v) tl
    in
    let rec lfp v =
      let v' = apply v Config.rules in
      if subset v v' then v else lfp v'
    in
    lfp v

  let man : t value_man = {
    pool = Config.pool;
    get = (
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
    set = (
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


  let unop op v =
    let rec aux : type a. a value_pool -> a -> a = fun pool v ->
      match pool, v with
      | Nil, () -> ()
      | Cons(hd, tl), (vhd, vtl) ->
        let module V = (val hd) in
        V.unop op vhd, aux tl vtl
    in
    let v' = aux Config.pool v in
    reduce man v'

  let binop op v1 v2 =
    let rec aux : type a. a value_pool -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Nil, (), () -> ()
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        V.binop op vhd1 vhd2, aux tl vtl1 vtl2
    in
    let v' = aux Config.pool v1 v2 in
    reduce man v'

  let filter v b =
    let rec aux : type a. a value_pool -> a -> a = fun pool v ->
      match pool, v with
      | Nil, () -> ()
      | Cons(hd, tl), (vhd, vtl) ->
        let module V = (val hd) in
        V.filter vhd b, aux tl vtl
    in
    let v' = aux Config.pool v in
    reduce man v'

  let bwd_unop op v r =
    let rec aux : type a. a value_pool -> a -> a -> a = fun pool v r ->
      match pool, v, r with
      | Nil, (), () -> ()
      | Cons(hd, tl), (vhd, vtl), (rhd, rtl) ->
        let module V = (val hd) in
        V.bwd_unop op vhd rhd, aux tl vtl rtl
    in
    let v' = aux Config.pool v r in
    reduce man v'

  let bwd_binop op v1 v2 r =
    let rec aux : type a. a value_pool -> a -> a -> a -> a * a = fun pool v1 v2 r ->
      match pool, v1, v2, r with
      | Nil, (), (), () -> (), ()
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2), (rhd, rtl) ->
        let module V = (val hd) in
        let vhd1', vhd2' = V.bwd_binop op vhd1 vhd2 rhd in
        let vtl1', vtl2' = aux tl vtl1 vtl2 rtl in
        (vhd1', vtl1'), (vhd2', vtl2')
    in
    let v1', v2' = aux Config.pool v1 v2 r in
    reduce man v1', reduce man v2'

  let compare op v1 v2 =
    let rec aux : type a. a value_pool -> a -> a -> a * a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Nil, (), () -> (), ()
      | Cons(hd, tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd) in
        let vhd1', vhd2' = V.compare op vhd1 vhd2 in
        let vtl1', vtl2' = aux tl vtl1 vtl2 in
        (vhd1', vtl1'), (vhd2', vtl2')
    in
    let v1', v2' = aux Config.pool v1 v2 in
    reduce man v1', reduce man v2'

end
