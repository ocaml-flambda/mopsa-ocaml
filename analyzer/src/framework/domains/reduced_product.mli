(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** N-ary reduced product of abstract abstractions. *)

open Manager
open Domain


(** Pool of abstract domains *)
(** ************************ *)

type 'a pool

(** Point-wise evaluations within a pool are decomposed into conjunctions *)
type 'a econj


(** Pool manager *)
type 'a pool_man = {
  get_state : 't. 't domain -> 'a -> 't;
  set_state : 't. 't domain -> 't -> 'a -> 'a;
  get_eval : 't. 't domain -> 'a econj -> (Ast.expr option * 'a flow) option;
  set_eval : 't. 't domain -> Ast.expr -> 'a flow -> 'a econj -> 'a econj;
  remove_eval : 't. 't domain -> 'a econj -> 'a econj;

}



(** State reductions *)
(** **************** *)

(** Operator signature *)
module type STATE_REDUCTION =
sig
  val reduce : Ast.stmt -> 'a pool_man -> ('a, 'b) man -> 'a flow -> 'a flow
end

(** Registration *)
val register_state_reduction : string -> (module STATE_REDUCTION) -> unit
val find_state_reduction : string -> (module STATE_REDUCTION)



(** Evaluation reductions *)
(** ********************* *)

(** Operator signature *)
module type EVAL_REDUCTION =
sig
  val reduce : Ast.expr -> 'a pool_man -> ('a, 'b) man -> 'a econj -> 'a econj
end

(** Registration *)
val register_eval_reduction : string -> (module EVAL_REDUCTION) -> unit
val find_eval_reduction : string -> (module EVAL_REDUCTION)


(** Domain identification *)
(** ********************* *)

type _ domain +=
  | D_empty : unit domain
  | D_reduced_product : 'a domain * 'b domain -> ('a * 'b) domain


(* Domain factory from string identifiers *)
(* ************************************** *)

val of_string : string list -> string list -> (module DOMAIN)

(* Utility functions *)
type 'a fld = {
  doit : 't. 'a -> 't domain -> 'a;
}

val fold : 'a fld -> 'a -> 't pool -> 'a
