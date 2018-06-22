(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(*==========================================================================*)
(**                            {2 Accessors}                                *)
(*==========================================================================*)


(**
   An accessor of type [('a, 'b) accessor] allows retrieving/updating a domain
    abstraction of type ['b] within the global analyzer abstraction ['a]
*)
type ('a, 'b) accessor = {
  get : 'a -> 'b;       (** Returns the domain's abstract element. *)
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

(** Flow manager *)
type 'a flow_manager = {
  bottom : 'a Flow.flow;
  top : 'a Flow.flow;
  is_bottom : 'a Flow.flow -> bool;
  is_top : 'a Flow.flow -> bool;
  leq : 'a Flow.flow -> 'a Flow.flow -> bool;
  join : 'a Flow.flow -> 'a Flow.flow -> 'a Flow.flow;
  meet : 'a Flow.flow -> 'a Flow.flow -> 'a Flow.flow;
  widening : Context.context -> 'a Flow.flow -> 'a Flow.flow -> 'a Flow.flow;
  print : Format.formatter -> 'a Flow.flow -> unit;
  get : Flow.token -> 'a Flow.flow -> 'a;
  set : Flow.token -> 'a -> 'a Flow.flow -> 'a Flow.flow;
  add : Flow.token -> 'a -> 'a Flow.flow -> 'a Flow.flow;
  remove : Flow.token -> 'a Flow.flow -> 'a Flow.flow;
  filter : (Flow.token -> 'a -> bool) -> 'a Flow.flow -> 'a Flow.flow;
  map : 'b. (Flow.token -> 'a -> 'b) -> 'a Flow.flow -> 'b Flow.flow;
  fold : 'b. (Flow.token -> 'a -> 'b -> 'b) -> 'a Flow.flow -> 'b -> 'b;
  merge : (Flow.token -> 'a option -> 'a option -> 'a option) -> 'a Flow.flow -> 'a Flow.flow -> 'a Flow.flow;
}

val flow_of_lattice_manager : 'a lattice_manager -> 'a flow_manager


(*==========================================================================*)
(**                           {2 Evaluations}                               *)
(*==========================================================================*)


type ('e, 'a) eval_case = {
  result : 'e option;
  flow: 'a Flow.flow;
  cleaners: Ast.stmt list;
}

type ('e, 'a) eval

val singleton_eval : 'e option -> 'a Flow.flow -> Ast.stmt list -> ('e, 'a) eval

val empty_eval : 'a Flow.flow -> ('e, 'a) eval

val join_eval : ('e, 'a) eval -> ('e, 'a) eval -> ('e, 'a) eval

val add_eval : 'e option -> 'a Flow.flow -> Ast.stmt list -> ('e, 'a) eval -> ('e, 'a) eval

val map_eval :
    ('e -> 'a Flow.flow -> Ast.stmt list -> ('f, 'a) eval_case) ->
    ('e, 'a) eval ->
    ('f, 'a) eval

val bind_eval :
    ('e -> 'a Flow.flow -> ('f, 'a) eval) ->
    ('e, 'a) eval ->
    ('f, 'a) eval

val fold_eval :
    ('b -> ('e, 'a) eval_case -> 'b) -> 'b -> ('e, 'a) eval -> 'b


val add_cleaners : Ast.stmt list -> ('e, 'a) eval -> ('e, 'a) eval


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
  exec : ?zone:Zone.t -> Ast.stmt -> Context.context -> 'a Flow.flow -> 'a Flow.flow;

  (** Expression evaluation function. *)
  eval : ?zpath:Zone.path -> Ast.expr -> Context.context -> 'a Flow.flow -> (Ast.expr, 'a) eval;

  (** Query transfer function. *)
  ask : 'r. 'r Query.query -> Context.context -> 'a Flow.flow -> 'r option;

  (** Domain accessor. *)
  ax : ('a, 't) accessor;
}



(*==========================================================================*)
                           (** {2 Utility functions} *)
(*==========================================================================*)

val is_cur_bottom : ('a, 't) manager -> 'a Flow.flow -> bool
(** Check whether TCur flows are empty *)

val map_cur : ('t -> 't) -> ('a, 't) manager -> 'a Flow.flow -> 'a Flow.flow
(** [map_cur f man flow] applies function [f] on the domain's
   abstract element (as pointed by the accessor in [man]) in the
   [TCur] flow *)

val set_cur : 't -> ('a, 't) manager -> 'a Flow.flow -> 'a Flow.flow
(** [set_cur a man flow] changes the domain's abstract element to [a] in the [TCur] flow *)


val get_cur : ('a, 't) manager -> 'a Flow.flow -> 't
(** [get_cur] retrieves the domain' abstract element in the [TCur] flow *)
