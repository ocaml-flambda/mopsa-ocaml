(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** N-ary reduced product of non-relational value abstractions. *)

open Value

(** A pool of value abstractions *)
module Pool =
struct

  (** A pool is encoded as GADT tuples *)
  type 'a t =
  | [] : unit t
  | (::) : 'a Value.info * 'b t -> ('a * 'b) t

end

(** Pool manager defines point-wise lattice operators for the product
      value abstraction. It also provides get/set functions to access
      individual values abstractions via keys *)
type 'a pool_man = {
  bottom : 'a;
  top : 'a;
  is_bottom : 'a -> bool;
  subset : 'a -> 'a -> bool;
  join : 'b. 'b Annotation.t -> 'a -> 'a -> 'a;
  meet : 'b. 'b Annotation.t -> 'a -> 'a -> 'a;
  widen : 'b. 'b Annotation.t -> 'a -> 'a -> 'a;
  print : Format.formatter -> 'a -> unit;
  get : 't. 't Value.id -> 'a -> 't;
  set : 't. 't Value.id -> 't -> 'a -> 'a;
}



(** Signature for reduction rules *)
module type REDUCTION =
sig

  (** Reduction operator called after each point-wise application of
     transfer functions (unop, bwd_unop, etc.) *)
  val reduce : 'a pool_man -> 'a -> 'a
end

(** Create a reduced product of a pool of value abstractions and reduction operators *)
module Make
    (Config:
     sig
       type t
       val pool : t Pool.t
       val rules : (module REDUCTION) list
       val display : string
     end) : Value.VALUE =
struct
  type t = Config.t

  let bottom : t =
    let rec aux : type a. a Pool.t -> a = fun pool ->
      match pool with
      | Pool.[] -> ()
      | Pool.(hd :: tl) ->
        let module V = (val hd.domain) in
        let tl = aux tl in
        V.bottom, tl
    in
    aux Config.pool

  let top : t =
    let rec aux : type a. a Pool.t -> a = fun pool ->
      match pool with
      | Pool.[] -> ()
      | Pool.(hd :: tl) ->
        let module V = (val hd.domain) in
        let tl = aux tl in
        V.top, tl
    in
    aux Config.pool

  let is_bottom (v:t) : bool = 
    let rec aux : type a. a Pool.t -> a -> bool = fun pool v ->
      match pool, v with
      | Pool.[], () -> false
      | Pool.(hd :: tl), (vhd, vtl) ->
        let module V = (val hd.domain) in
        V.is_bottom vhd || aux tl vtl
    in
    aux Config.pool v

  let subset (v1:t) (v2:t) : bool =
    let rec aux : type a. a Pool.t -> a -> a -> bool = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> true
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd.domain) in
        V.subset vhd1 vhd2 && aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let join annot (v1:t) (v2:t) : t =
    let rec aux : type a. a Pool.t -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd.domain) in
        V.join annot vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let meet annot (v1:t) (v2:t) =
    let rec aux : type a. a Pool.t -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd.domain) in
        V.meet annot vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let widen annot v1 v2 =
    let rec aux : type a. a Pool.t -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd.domain) in
        V.widen annot vhd1 vhd2, aux tl vtl1 vtl2
    in
    aux Config.pool v1 v2

  let print fmt v =
    let rec aux : type a. a Pool.t -> Format.formatter -> a -> unit = fun pool fmt v ->
      match pool, v with
      | Pool.[], () -> ()
      | Pool.[hd], (vhd, ()) ->
        let module V = (val hd.domain) in
        Format.fprintf fmt "%a" V.print vhd

      | Pool.(hd :: tl), (vhd, vtl) ->
        let module V = (val hd.domain) in
        Format.fprintf fmt "%a ∧ %a" V.print vhd (aux tl) vtl
    in
    aux Config.pool fmt v

  let display = Config.display

  let zone =
    (* FIXME: check that all values are defined on the same zone *)
    match Config.pool with
    | Pool.(hd :: tl) ->
      let module V = (val hd.domain) in
      V.zone
    | _ -> assert false

  let of_constant (c: Ast.constant) : t =
    let rec aux : type a. a Pool.t -> a = fun pool ->
      match pool with
      | Pool.[] -> ()
      | Pool.(hd::tl) ->
        let module V = (val hd.domain) in
        V.of_constant c, aux tl
    in
    aux Config.pool

  let man : t pool_man = {
    bottom;
    top;
    is_bottom;
    subset;
    join;
    meet;
    widen;
    print;
    get = (
      let f : type a. a id -> t -> a = fun k v ->
        let rec aux : type a b. a id -> b Pool.t -> b -> a = fun k pool v ->
          match pool, v with
          | Pool.[], () -> raise Not_found
          | Pool.(hd::tl), (vhd, vtl) ->
            match hd.eq k with
            | Some Eq -> vhd
            | None -> aux k tl vtl
        in
        aux k Config.pool v
      in
      f
    );
    set = (
      let f : type a. a id -> a -> t -> t = fun k x v ->
        let rec aux : type a b. a id -> a -> b Pool.t -> b -> b = fun k x pool v ->
          match pool, v with
          | Pool.[], () -> raise Not_found
          | Pool.(hd::tl), (vhd, vtl) ->
            match hd.eq k with
            | Some Eq -> (x, vtl)
            | None -> (vhd, aux k x tl vtl)
        in
        aux k x Config.pool v
      in
      f
    );
  }

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
  
  let unop op v =
    let rec aux : type a. a Pool.t -> a -> a = fun pool v ->
      match pool, v with
      | Pool.[], () -> ()
      | Pool.(hd :: tl), (vhd, vtl) ->
        let module V = (val hd.domain) in
        V.unop op vhd, aux tl vtl
    in
    let v' = aux Config.pool v in
    reduce man v'

  let binop op v1 v2 =
    let rec aux : type a. a Pool.t -> a -> a -> a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd.domain) in
        V.binop op vhd1 vhd2, aux tl vtl1 vtl2
    in
    let v' = aux Config.pool v1 v2 in
    reduce man v'

  let filter v b =
    let rec aux : type a. a Pool.t -> a -> a = fun pool v ->
      match pool, v with
      | Pool.[], () -> ()
      | Pool.(hd :: tl), (vhd, vtl) ->
        let module V = (val hd.domain) in
        V.filter vhd b, aux tl vtl
    in
    let v' = aux Config.pool v in
    reduce man v'

  let bwd_unop op v r =
    let rec aux : type a. a Pool.t -> a -> a -> a = fun pool v r ->
      match pool, v, r with
      | Pool.[], (), () -> ()
      | Pool.(hd :: tl), (vhd, vtl), (rhd, rtl) ->
        let module V = (val hd.domain) in
        V.bwd_unop op vhd rhd, aux tl vtl rtl
    in
    let v' = aux Config.pool v r in
    reduce man v'

  let bwd_binop op v1 v2 r =
    let rec aux : type a. a Pool.t -> a -> a -> a -> a * a = fun pool v1 v2 r ->
      match pool, v1, v2, r with
      | Pool.[], (), (), () -> (), ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2), (rhd, rtl) ->
        let module V = (val hd.domain) in
        let vhd1', vhd2' = V.bwd_binop op vhd1 vhd2 rhd in
        let vtl1', vtl2' = aux tl vtl1 vtl2 rtl in
        (vhd1', vtl1'), (vhd2', vtl2')
    in
    let v1', v2' = aux Config.pool v1 v2 r in
    reduce man v1', reduce man v2'

  let compare op v1 v2 =
    let rec aux : type a. a Pool.t -> a -> a -> a * a = fun pool v1 v2 ->
      match pool, v1, v2 with
      | Pool.[], (), () -> (), ()
      | Pool.(hd :: tl), (vhd1, vtl1), (vhd2, vtl2) ->
        let module V = (val hd.domain) in
        let vhd1', vhd2' = V.compare op vhd1 vhd2 in
        let vtl1', vtl2' = aux tl vtl1 vtl2 in
        (vhd1', vtl1'), (vhd2', vtl2')
    in
    let v1', v2' = aux Config.pool v1 v2 in
    reduce man v1', reduce man v2'

end

let reductions : (string * (module REDUCTION)) list ref = ref []

let register_reduction name rule =
  reductions := (name, rule) :: !reductions

let find_reduction name = List.assoc name !reductions

type xpool = P : 'a Pool.t -> xpool

let of_string (values: string list) (rules: string list) (display: string) : (module Value.VALUE) =
  let pool = find_pool values in
  let rules = List.map find_reduction rules in
  
  let open Pool in
  let rec aux : pool -> xpool = function
    | Nil -> P []
    | Cons(hd, tl) ->
      let P tl = aux tl in
      P (hd :: tl)
  in

  let doit (type a) (pool: a Pool.t) =
    let module V = Make(struct type t = a let pool = pool let rules = rules let display = display end) in
    (module V : Value.VALUE)
  in

  let P pool = aux pool in
  doit pool
