(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Evaluations of expressions *)


type ('e, 'a) clause = {
  case    : 'e option;          (** Evaluation case *)
  flow    : 'a Flow.flow;       (** Case flow *)
  cleaner : Ast.stmt list;      (** Post-eval cleaners *)
}
(** Evaluation clause *)

type ('e, 'a) t
(** Evaluations *)


val singleton : 'e option -> ?cleaner:Ast.stmt list -> 'a Flow.flow -> ('e, 'a) t
(** Evaluation singleton *)

val join : ('e, 'a) t -> ('e, 'a) t -> ('e, 'a) t
(** Compute the union of two evaluations *)

val append_cleaner : Ast.stmt list -> ('e, 'a) t -> ('e, 'a) t
(** Add cleaners to an evaluation *)

val map_clause:
    ('e -> 'a Flow.flow -> Ast.stmt list -> ('x, 'a) t) ->
    ('e, 'a) t ->
    ('x, 'a) t
(** [map_clause f evls] applies the evaluation function [f] on each
   clause of [evls] and joins the results *)

val map:
    ('e -> 'a Flow.flow -> ('x, 'a) t) ->
    ('e, 'a) t ->
    ('x, 'a) t
(** [map f evls] is similar to [map_clause f evls] with the difference
   that [f] is given only the case and the flow of the clause, but not
    the cleaners *)

val map_option:
  ('e -> 'a Flow.flow -> ('x, 'a) t option) ->
  ('e, 'a) t ->
  ('x, 'a) t option
(** [map_option f evls] is similar to [map] but returns optional evaluations *)

val iter:
    ('e -> 'a Flow.flow -> unit) ->
    ('e, 'a) t ->
    unit
(** Iterate over the clauses of an evaluation *)

val merge:
    (('e, 'a) clause -> 'b) ->
    join:('b -> 'b -> 'b) ->
    ('e, 'a) t ->
    'b
(** Merge the clauses of an evaluation *)

val print : Format.formatter -> ('e, 'a) t -> print_case:(Format.formatter -> 'e -> unit) -> unit
