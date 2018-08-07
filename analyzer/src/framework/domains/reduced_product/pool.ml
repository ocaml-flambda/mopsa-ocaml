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

(** {2 Pool of abstract domains} *)
(** **************************** *)

type 'a domain_pool =
  | Nil : unit domain_pool
  | Cons : (module DOMAIN with type t = 'a) * 'b domain_pool -> ('a * 'b) domain_pool

(** Product evaluations.  
    We do not represent the product of all evaluation DNFs.  
    The product is first written as a DNF, and reduction rules need to process 
    each sub-conjunction individually.  
*)
type 'a evl_conj = ('a, Ast.expr) evl_case option list

(** Transform a product conjunction into an evaluation *)
let conj_to_evl (c: 'a evl_conj) : ('a, Ast.expr) evl option =
  match List.partition (function None -> true | _ -> false) c with
  | _, [] -> None
  | _, l ->
    Some (
      List.map (function Some c -> Eval.case c | None -> assert false) l |>
      Eval.meet_list
    )

(** Manager of a pool of abstract domains *)
type ('a, 'd) domain_man = {
  (** Domains in the pool *)
  pool : 'd domain_pool;

  (** Get the abstract element of a member domain *)
  get_state : 't. 't domain -> 'a -> 't;

  (** Set the abstract element of a member domain *)
  set_state : 't. 't domain -> 't -> 'a -> 'a;

  (** Get the evaluation of a member domain *)
  get_eval : 't. 't domain -> 'a evl_conj -> (Ast.expr option * 'a flow) option;

  (** Change the evaluation of a member domain *)
  set_eval : 't. 't domain -> Ast.expr -> 'a flow -> 'a evl_conj -> 'a evl_conj;

  (** Remove the evaluation of a member domains *)
  remove_eval : 't. 't domain -> 'a evl_conj -> 'a evl_conj;
}


(** {2 Pool of abstract values} *)
(** *************************** *)

type 'a value_pool =
  | Nil : unit value_pool
  | Cons : (module VALUE with type t = 'a) * 'b value_pool -> ('a * 'b) value_pool


(** Value manager, used by value reduction rules *)
type 'v value_man = {
  pool : 'v value_pool;
  get  : 't. 't value -> 'v -> 't;
  set  : 't. 't value -> 't -> 'v -> 'v;
}

(** Non-relational manager allows members of a domain pool to access
    abstract elements of a member non-relational abstract domains *)
type ('a, 'v) nonrel_man = {
  pool : 'v value_pool;
  get : 't. 't value -> Ast.var -> 'a -> 't;
  set : 't. 't value -> Ast.var -> 't -> 'a -> 'a;
}


(** {2 Mixed pool} *)
(** ************** *)

type (_, _) pool = Pool : 'd domain_pool * 'v value_pool -> ('d, 'v) pool







