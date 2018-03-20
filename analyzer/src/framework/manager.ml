(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

open Lattice
open Flow

(**
   A manager provides to a domain:
   - the operators of global flow abstraction and its the underlying environment
   abstraction,
   - the accessor structure to its own abstraction
   - and the transfer functions of the top-level analyzer.
*)


let debug fmt = Debug.debug ~channel:"framework.manager" fmt

(*==========================================================================*)
(**                            {2 Evaluations}                              *)
(*==========================================================================*)


type 'a eval_case = Ast.expr option * 'a * Ast.stmt list
type 'a eval_output = 'a eval_case Dnf.t

let eval_singleton (case: 'a eval_case) : 'a eval_output =
  Dnf.singleton case

let eval_join (e1: 'a eval_output) (e2: 'a eval_output) : 'a eval_output =
  Dnf.mk_or e1 e2

let eval_meet (e1: 'a eval_output) (e2: 'a eval_output) : 'a eval_output =
  Dnf.mk_and e1 e2

let eval_flatten (e: 'a eval_output) : 'a eval_case list =
  Dnf.to_list e |>
  List.fold_left (fun acc conj ->
      List.fold_left (fun acc x ->
          x :: acc
        ) acc conj
    ) []

let eval_collapse
    (f: 'a eval_case -> 'b)
    (join: 'b -> 'b -> 'b)
    (meet: 'b -> 'b -> 'b)
    (evl: 'a eval_output)
    : 'b
  =
  Dnf.substitute f join meet evl

let eval_map
    (f: 'a eval_case -> 'b eval_case)
    (evl: 'a eval_output)
  : 'b eval_output =
  Dnf.map f evl

(*==========================================================================*)
(**                            {2 Accessors}                                *)
(*==========================================================================*)


(**
   An accessor of type [('a, 'b) accessor] allows retrieving/updating a domain
    abstraction of type ['b] within the global analyzer abstraction ['a]
*)
type ('a, 'b) accessor = {
  get : 'a -> 'b; (** Returns the domain's abstract element. *)
  set : 'b -> 'a -> 'a; (** Modifies the domain's abstract element and returns
                            the updated global abstraction . *)
}



(*==========================================================================*)
(**                            {2 Manager}                                  *)
(*==========================================================================*)


(** An instance of type [('a, 'b) manager] encapsulates the lattice operators
    of the global environment abstraction ['a] and its flow abstraction
    ['a Flow.t], the top-level transfer functions [exec], [eval] and [ask],
    and the accessor to the domain abstraction ['b] within ['a].
*)
type ('a, 'b) manager = {
  (** Environment abstraction. *)
  env : 'a lattice_manager;

  (** Flow abstraction. *)
  flow : 'a flow_manager;

  (** Statement transfer function. *)
  exec :
    Ast.stmt -> Context.context -> 'a flow ->
    'a flow;

  (** Expression transfer function. *)
  eval :
    Ast.expr -> Context.context -> 'a flow ->
    'a flow  eval_output;

  (** Query transfer function. *)
  ask : 'r. 'r Query.query -> Context.context -> 'a flow -> 'r option;

  (** Domain accessor. *)
  ax : ('a, 'b) accessor;
}

(** Update the domain abstraction of the TCur flow *)
let map_domain_cur f man flow =
  let cur = man.flow.get TCur flow in
  let a = man.ax.get cur in
  let a' = f a in
  let cur' = man.ax.set a' cur in
  man.flow.set TCur cur' flow

let set_domain_cur a man flow =
  map_domain_cur (fun _ -> a) man flow

(** Retrieve the domain abstraction of the TCur flow *)
let get_domain_cur man flow =
    man.flow.get TCur flow |>
    man.ax.get


(*==========================================================================*)
(**                         {2 Utility functions}                           *)
(*==========================================================================*)

let if_flow
    (true_cond: 'a flow)
    (false_cond: 'a flow)
    (true_branch: 'a flow -> 'b)
    (false_branch: 'a flow -> 'b)
    (bottom_branch: unit -> 'b)
    (merge: 'a flow -> 'a flow -> 'b)
    man flow
  : 'b =
  debug "true cond:@\n  @[%a@]@\nfalse cond:@\n  @[%a@]"
    man.flow.print true_cond
    man.flow.print false_cond
  ;
  match man.flow.is_cur_bottom true_cond, man.flow.is_cur_bottom false_cond with
  | false, true -> debug "true branch"; true_branch true_cond
  | true, false -> debug "true branch"; false_branch false_cond
  | false, false -> debug "merge branch"; merge true_cond false_cond
  | true, true -> debug "bottom branch"; bottom_branch ()


(*==========================================================================*)
                        (** {2 Panic management} *)
(*==========================================================================*)


exception Panic
(**
   This exception is raised by abstract domains when they encounter an
   unsupported language construct.
*)

exception StmtPanic of Ast.stmt
exception ExprPanic of Ast.expr

let panic fmt =
  Format.kasprintf (fun str ->
      Debug.warn "%s" str;
      raise Panic
    ) fmt
