(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Reduction of non-relation abstract values. *)

type _ key = ..

type 'a value = Value : 'a key * (module Value.VALUE with type t = 'a) -> 'a value

module Pool =
struct
  type 'a t =
  | [] : unit t
  | (::) : 'a value * 'b t -> ('a * 'b) t

  type (_,_) eq = Eq : ('a, 'a) eq
end


type 'a pool_manager = {
  bottom : 'a;
  top : 'a;
  is_bottom : 'a -> bool;
  is_top : 'a -> bool;
  leq : 'a -> 'a -> bool;
  join : 'a -> 'a -> 'a;
  meet : 'a -> 'a -> 'a;
  widening : Context.context -> 'a -> 'a -> 'a;
  print : Format.formatter -> 'a -> unit;
  get : 't. 't key -> 'a -> 't;
  set : 't. 't key -> 't -> 'a -> 'a;
}

module type REDUCTION =
sig
  val reduce : 'a pool_manager -> 'a -> 'a
end

module Make
    (P: sig
       type t
       val pool : t Pool.t
       val eq : 'a key -> 'b key -> ('a, 'b) Pool.eq option
     end)
    (Reduction: REDUCTION) : Value.VALUE =
struct
  type t = P.t

  let bottom =
    let rec aux : type a. a Pool.t -> a = fun pool ->
      match pool with
      | Pool.[] -> ()
      | Pool.(hd :: tl) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        let tl = aux tl in
        V.bottom, tl
    in
    aux P.pool

  let top =
    let rec aux : type a. a Pool.t -> a = fun pool ->
      match pool with
      | Pool.[] -> ()
      | Pool.(hd :: tl) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        let tl = aux tl in
        V.top, tl
    in
    aux P.pool

  let is_bottom v =
    let rec aux : type a. a Pool.t -> a -> bool = fun pool v ->
      match pool, v with
      | Pool.[], () -> false
      | Pool.(hd :: tl), (vhd, vtl) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        V.is_bottom vhd
        || aux tl vtl
    in
    aux P.pool v

  let is_top v =
    let rec aux : type a. a Pool.t -> a -> bool = fun pool v ->
      match pool, v with
      | Pool.[], () -> true
      | Pool.(hd :: tl), (vhd, vtl) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        V.is_top vhd
        && aux tl vtl
    in
    aux P.pool v

  let leq v1 v2 =
    let rec aux : type a. a Pool.t -> a -> a -> bool = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> true
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        V.leq vhd1 vhd2
        && aux tl vtl1 vtl2
    in
    aux P.pool v1 v2

  let join v1 v2 =
    let rec aux : type a. a Pool.t -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        V.join vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux P.pool v1 v2

  let meet v1 v2 =
    let rec aux : type a. a Pool.t -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        V.meet vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux P.pool v1 v2

  let widening ctx v1 v2 =
    let rec aux : type a. a Pool.t -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        V.widening ctx vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux P.pool v1 v2

  let print fmt v =
    let rec aux : type a. a Pool.t -> Format.formatter -> a -> unit = fun pool fmt v ->
      match pool, v with
      | Pool.[], () -> ()
      | Pool.(hd :: tl), (vhd, vtl) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        Format.fprintf fmt "%a, %a" V.print vhd (aux tl) vtl
    in
    aux P.pool fmt v

  let of_bool b =
    let rec aux : type a. a Pool.t -> a = fun pool ->
      match pool with
      | Pool.[] -> ()
      | Pool.(hd::tl) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        V.of_bool b, aux tl
    in
    aux P.pool

  let of_constant c =
    let rec aux : type a. a Pool.t -> a = fun pool ->
      match pool with
      | Pool.[] -> ()
      | Pool.(hd::tl) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        V.of_constant c, aux tl
    in
    aux P.pool

  let man : t pool_manager = {
    bottom;
    top;
    is_bottom;
    is_top;
    leq;
    join;
    meet;
    widening;
    print;
    get = (
      let f : type b. b key -> t -> b = fun k v ->
        let rec aux : type a. a Pool.t -> a -> b = fun pool v ->
          match pool, v with
          | Pool.[], () -> raise Not_found
          | Pool.(hd::tl), (vhd, vtl) ->
            let Value (k',_) = hd in
            match P.eq k k' with
            | Some Pool.Eq -> vhd
            | None -> aux tl vtl
        in
        aux P.pool v
      in
      f
    );
    set = (
      let f : type b. b key -> b -> t -> t = fun k x v ->
        let rec aux : type a. a Pool.t -> a -> a = fun pool v ->
          match pool, v with
          | Pool.[], () -> raise Not_found
          | Pool.(hd::tl), (vhd, vtl) ->
            let Value (k',_) = hd in
            match P.eq k k' with
            | Some Pool.Eq -> (x, vtl)
            | None -> (vhd, aux tl vtl)
        in
        aux P.pool v
      in
      f
    );
  }
  
  let fwd_unop op v =
    let rec aux : type a. a Pool.t -> a -> a = fun pool v ->
      match pool, v with
      | Pool.[], () -> ()
      | Pool.(hd :: tl), (vhd, vtl) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        V.fwd_unop op vhd, aux tl vtl
    in
    let v' = aux P.pool v in
    Reduction.reduce man v'

  let fwd_binop op v1 v2 =
    let rec aux : type a. a Pool.t -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        V.fwd_binop op vhd1 vhd2, aux tl vtl1 vtl2
    in
    let v' = aux P.pool v1 v2 in
    Reduction.reduce man v'

  let bwd_unop op v r =
    let rec aux : type a. a Pool.t -> a -> a -> a = fun pool v r ->
      match pool, v, r with
      | Pool.[], (), () -> ()
      | Pool.(hd :: tl), (vhd, vtl), (rhd, rtl) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        V.bwd_unop op vhd rhd, aux tl vtl rtl
    in
    let v' = aux P.pool v r in
    Reduction.reduce man v'

  
  let bwd_binop op v1 v2 r =
    let rec aux : type a. a Pool.t -> a -> a -> a -> a * a = fun pool v1 v2 r ->
      match pool, v1, v2, r with
      | Pool.[], (), (), () -> (), ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2), (rhd, rtl) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        let vhd1', vhd2' = V.bwd_binop op vhd1 vhd2 rhd in
        let vtl1', vtl2' = aux tl vtl1 vtl2 rtl in
        (vhd1', vtl1'), (vhd2', vtl2')
    in
    let v1', v2' = aux P.pool v1 v2 r in
    Reduction.reduce man v1', Reduction.reduce man v2'


  let fwd_filter op v1 v2 =
    let rec aux : type a. a Pool.t -> a -> a -> bool = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> true
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        V.fwd_filter op vhd1 vhd2
        && aux tl vtl1 vtl2
    in
    aux P.pool v1 v2

  let bwd_filter op v1 v2 =
    let rec aux : type a. a Pool.t -> a -> a -> a * a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> (), ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        let vhd1', vhd2' = V.bwd_filter op vhd1 vhd2 in
        let vtl1', vtl2' = aux tl vtl1 vtl2 in
        (vhd1', vtl1'), (vhd2', vtl2')
    in
    let v1', v2' = aux P.pool v1 v2 in
    Reduction.reduce man v1', Reduction.reduce man v2'

  let assume_true v =
    let rec aux : type a. a Pool.t -> a -> a = fun pool v ->
      match pool, v with
      | Pool.[], () -> ()
      | Pool.(hd :: tl), (vhd, vtl) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        V.assume_true vhd, aux tl vtl
    in
    let v' = aux P.pool v in
    Reduction.reduce man v'

  let assume_false v =
    let rec aux : type a. a Pool.t -> a -> a = fun pool v ->
      match pool, v with
      | Pool.[], () -> ()
      | Pool.(hd :: tl), (vhd, vtl) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        V.assume_false vhd, aux tl vtl
    in
    let v' = aux P.pool v in
    Reduction.reduce man v'

  let can_be_true v =
    let rec aux : type a. a Pool.t -> a -> bool = fun pool v ->
      match pool, v with
      | Pool.[], () -> true
      | Pool.(hd :: tl), (vhd, vtl) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        V.can_be_true vhd
        && aux tl vtl
    in
    aux P.pool v

  let can_be_false v =
    let rec aux : type a. a Pool.t -> a -> bool = fun pool v ->
      match pool, v with
      | Pool.[], () -> true
      | Pool.(hd :: tl), (vhd, vtl) ->
        let Value (_,v) = hd in
        let module V = (val v) in
        V.can_be_false vhd
        && aux tl vtl
    in
    aux P.pool v

  let zone =
    match P.pool with
    | Pool.(hd :: tl) ->
      let Value (_,v) = hd in
      let module V = (val v) in
      V.zone
    | _ -> assert false

end

type 'a info = {
  name : string;
  pool: 'a Pool.t;
  eq : 'b 'c. 'b key -> 'c key -> ('b, 'c) Pool.eq option
}


let register_reduction (type a) (info: a info) (reduction: (module REDUCTION)) =
  let module V = Make(struct
      type t = a
      let pool = info.pool
      let eq = info.eq
    end)
      (val reduction : REDUCTION) in
  Value.register_value info.name (module V)
