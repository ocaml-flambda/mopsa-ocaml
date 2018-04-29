(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

open Lattice
open Flow
open Eval
    
(**
   A manager provides to a domain:
   - the operators of global flow abstraction and its the underlying environment
   abstraction,
   - the accessor structure to its own abstraction
   - and the transfer functions of the top-level analyzer.
*)


let debug fmt = Debug.debug ~channel:"framework.manager" fmt


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
    Context.context -> Ast.stmt -> 'a flow ->
    'a flow;

  (** Expression transfer function. *)
  eval :
    Context.context -> Ast.expr -> 'a flow ->
    (Ast.expr, 'a) Eval.evals;

  (** Query transfer function. *)
  ask : 'r. Context.context -> 'r Query.query -> 'a flow -> 'r option;

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
  let cur = man.flow.get TCur flow in
  let cur' = man.ax.set a cur in
  man.flow.set TCur cur' flow


(** Retrieve the domain abstraction of the TCur flow *)
let get_domain_cur man flow =
    man.flow.get TCur flow |>
    man.ax.get


(*==========================================================================*)
(**                         {2 Utility functions}                           *)
(*==========================================================================*)

let if_flow
    (true_cond: 'a flow -> 'a flow)
    (false_cond: 'a flow -> 'a flow)
    (true_branch: 'a flow -> 'b)
    (false_branch: 'a flow -> 'b)
    (bottom_branch: unit -> 'b)
    (merge: 'a flow -> 'a flow -> 'b)
    man flow
  : 'b =
  let true_flow = true_cond flow and false_flow = false_cond flow in
  debug "true cond:@\n  @[%a@]@\nfalse cond:@\n  @[%a@]"
    man.flow.print true_flow
    man.flow.print false_flow
  ;
  match man.flow.is_cur_bottom true_flow, man.flow.is_cur_bottom false_flow with
  | false, true -> debug "true branch"; true_branch true_flow
  | true, false -> debug "true branch"; false_branch false_flow
  | false, false -> debug "merge branch"; merge true_flow false_flow
  | true, true -> debug "bottom branch"; bottom_branch ()

let if_flow_eval
    true_flow false_flow
    true_case false_case man flow
    ?(bottom_case=(fun () -> oeval_singleton (None, flow, [])))
    ?(merge_case=(fun flow1 flow2 -> oeval_join (true_case flow1) (false_case flow2)))
    () =
  if_flow true_flow false_flow true_case false_case bottom_case merge_case man flow
