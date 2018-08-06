(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Pool of state and value abstractions *)

open Manager
open Domain
open Value

type 'a domain_pool =
  | Nil : unit domain_pool
  | Cons : (module DOMAIN with type t = 'a) * 'b domain_pool -> ('a * 'b) domain_pool

type 'a value_pool =
  | Nil : unit value_pool
  | Cons : (module VALUE with type t = 'a) * 'b value_pool -> ('a * 'b) value_pool

type (_, _) pool = Pool : 'd domain_pool * 'v value_pool -> ('d, 'v) pool

(** Pool evaluations *)
(** **************** *)

(** Reduction rules process each conjunction of then point-wise evaluations individually *)
type 'a pool_evl = ('a, Ast.expr) evl_case option list

(** Transform a conjunction into an evaluation *)
let return_evl (c: 'a pool_evl) : ('a, Ast.expr) evl option =
  match List.partition (function None -> true | _ -> false) c with
  | _, [] -> None
  | _, l ->
    Some (
      List.map (function Some c -> Eval.case c | None -> assert false) l |>
      Eval.meet_list
    )


(** Pool manager *)
(** ************ *)

type 'v value_man = {
  pool : 'v value_pool;

  get  : 't. 't value -> 'v -> 't;
  set  : 't. 't value -> 't -> 'v -> 'v;
}

type ('a, 'v) nonrel_man = {
  pool : 'v value_pool;

  get : 't. 't value -> Ast.var -> 'a -> 't;
  set : 't. 't value -> Ast.var -> 't -> 'a -> 'a;
}


type ('a, 'd) domain_man = {
  pool : 'd domain_pool;

  get_state : 't. 't domain -> 'a -> 't;
  set_state : 't. 't domain -> 't -> 'a -> 'a;

  get_eval : 't. 't domain -> 'a pool_evl -> (Ast.expr option * 'a flow) option;
  set_eval : 't. 't domain -> Ast.expr -> 'a flow -> 'a pool_evl -> 'a pool_evl;
  remove_eval : 't. 't domain -> 'a pool_evl -> 'a pool_evl;
}
