(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   A manager provides to a domain:
   - the operators of global flow abstraction and its the underlying environment
   abstraction,
   - the accessor structure to its own abstraction
   - and the transfer functions of the top-level analyzer.
*)

open Lattice
open Flow

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
(**                         {2 Lattice manager}                             *)
(*==========================================================================*)


(** Lattice manager. *)
type 'a lattice_manager = {
  bottom : 'a;
  top : 'a;
  is_bottom : 'a -> bool;
  is_top : 'a -> bool;
  leq : 'a -> 'a -> bool;
  join : 'a -> 'a -> 'a;
  meet : 'a -> 'a -> 'a;
  widening : Context.context -> 'a -> 'a -> 'a;
  print : Format.formatter -> 'a -> unit;
}



(*==========================================================================*)
                           (** {2 Flow manager} *)
(*==========================================================================*)


type 'a flow_manager = {
  bottom : 'a flow;
  top : 'a flow;
  is_bottom : 'a flow -> bool;
  is_top : 'a flow -> bool;
  leq : 'a flow -> 'a flow -> bool;
  join : 'a flow -> 'a flow -> 'a flow;
  meet : 'a flow -> 'a flow -> 'a flow;
  widening : Context.context -> 'a flow -> 'a flow -> 'a flow;
  print : Format.formatter -> 'a flow -> unit;
  get : token -> 'a flow -> 'a;
  set : token -> 'a -> 'a flow -> 'a flow;
  add : token -> 'a -> 'a flow -> 'a flow;
  remove : token -> 'a flow -> 'a flow;
  filter : (token -> 'a -> bool) -> 'a flow -> 'a flow;
  map : 'b. (token -> 'a -> 'b) -> 'a flow -> 'b flow;
  fold : 'b. (token -> 'a -> 'b -> 'b) -> 'a flow -> 'b -> 'b;
  merge : (token -> 'a option -> 'a option -> 'a option) -> 'a flow -> 'a flow -> 'a flow;
}

let flow_of_lattice_manager (value: 'a lattice_manager) : ('a flow_manager) = {
  bottom = Flow.bottom;
  top = Flow.top;
  is_bottom = Flow.is_bottom ~is_value_bottom:value.is_bottom;
  is_top = Flow.is_top;
  leq = Flow.leq ~is_value_bottom:value.is_bottom ~value_leq:value.leq;
  join = Flow.join ~value_join:value.join;
  meet = Flow.meet ~value_meet:value.meet ~value_bottom:value.bottom;
  widening = Flow.widening ~value_widening:value.widening;
  print = Flow.print ~value_print:value.print;
  get = Flow.get ~value_bottom:value.bottom ~value_top:value.top;
  set = Flow.set ~is_value_bottom:value.is_bottom;
  add = Flow.add ~is_value_bottom:value.is_bottom ~value_join:value.join;
  remove = Flow.remove;
  filter = Flow.filter;
  map = Flow.map;
  fold = Flow.fold;
  merge = Flow.merge ~value_bottom:value.bottom;
}



(*==========================================================================*)
                           (** {2 Analysis manager} *)
(*==========================================================================*)


(** An instance of type [('a, 't) manager] encapsulates the lattice operators
    of the global environment abstraction ['a] and its flow abstraction
    ['a Flow.t], the top-level transfer functions [exec], [eval] and [ask],
    and the accessor to the domain abstraction ['t] within ['a].
*)
type ('a, 't) manager = {
  (** Environment abstraction. *)
  env : 'a lattice_manager;

  (** Flow abstraction. *)
  flow : 'a flow_manager;

  (** Statement transfer function. *)
  exec : ?zone:Zone.t -> Ast.stmt -> Context.context -> 'a flow -> 'a flow;

  (** Expression evaluation function. *)
  eval : ?zpath:Zone.path -> Ast.expr -> Context.context -> 'a flow -> (Ast.expr, 'a) Eval.t;

  (** Query transfer function. *)
  ask : 'r. 'r Query.query -> Context.context -> 'a flow -> 'r option;

  (** Domain accessor. *)
  ax : ('a, 't) accessor;
}




(*==========================================================================*)
                           (** {2 Utility functions} *)
(*==========================================================================*)

let is_cur_bottom man flow =
  man.flow.get TCur flow |>
  man.env.is_bottom


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


let map_eval
    ?(zpath = Zone.path_top) e man ctx flow
    (f: Ast.expr -> 'a flow -> (Ast.expr, 'a) Eval.t option)
  : (Ast.expr, 'a) Eval.t option =
  man.eval ~zpath e ctx flow |>
  Eval.map f

let post_eval
    ?(zpath = Zone.path_top) e man ctx flow
    (f: Ast.expr -> 'a flow -> 'a Post.t option)
  : 'a Post.t option =
  let join_option x y =
    match x, y with
    | None, a | a, None -> a
    | Some a, Some b -> Some (Post.join ~flow_join:man.flow.join a b)
  in
  let open Eval in
  man.eval ~zpath e ctx flow |>
  Eval.merge
    (fun eval ->
       let post = match eval.case with
         | None -> Some (Post.of_flow eval.flow)
         | Some exp -> f exp eval.flow
       in
       match post with
       | None -> None
       | Some post ->
         let post' = List.fold_left (fun acc stmt ->
             let flow = acc.Post.flow in
             let flow' = man.exec stmt ctx flow in
             {acc with Post.flow = flow'}
           ) post eval.cleaner
         in
         Some post'
    )~join:join_option



(** [eval_list zpath el man ctx flow] folds the evaluations of expressions in [el] *)
let eval_list
    ?(zpath = Zone.path_top)
    (l: Ast.expr list)
    (man: ('a, 't) manager) ctx (flow: 'a flow)
  : (Ast.expr list, 'a) Eval.t =
  let rec aux expl flow clean = function
    | [] ->
      Eval.singleton (Some (List.rev expl)) flow ~cleaner:clean

    | exp :: tl ->
      man.eval ~zpath exp ctx flow |>
      Eval.map_clause
        (fun exp' flow clean' ->
           aux (exp' :: expl) flow (clean @ clean') tl
        )
  in
  aux [] flow [] l
