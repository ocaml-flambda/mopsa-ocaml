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

type ('e, 'a) evals
(** Evaluations *)


val singleton : 'e option -> ?cleaner:Ast.stmt list -> 'a Flow.flow -> ('e, 'a) evals
(** Evaluation singleton *)

val join : ('e, 'a) evals -> ('e, 'a) evals -> ('e, 'a) evals
(** Compute the union of two evaluations *)

val append_cleaner : ('e, 'a) evals -> Ast.stmt list -> ('e, 'a) evals 
(** Add cleaners to an evaluation *)

val map_clause:
    ('e -> 'a Flow.flow -> Ast.stmt list -> ('x, 'a) evals) ->
    ('e, 'a) evals ->
    ('x, 'a) evals
(** [map_clause f evls] applies the evaluation function [f] on each
   clause of [evls] and joins the results *)

val map:
    ('e -> 'a Flow.flow -> ('x, 'a) evals) ->
    ('e, 'a) evals ->
    ('x, 'a) evals
(** [map f evls] is similar to [map_clause f evls] with the difference
   that [f] is given only the case and the flow of the clause, but not
   the cleaners *)

val iter:
    ('e -> 'a Flow.flow -> unit) ->
    ('e, 'a) evals ->
    unit
(** Iterate over the clauses of an evaluation *)

val merge:
    (('e, 'a) clause -> 'b) ->
    join:('b -> 'b -> 'b) ->
    ('e, 'a) evals ->
    'b
(** Merge the clauses of an evaluation *)

val print : Format.formatter -> ('e, 'a) evals -> print_case:(Format.formatter -> 'e -> unit) -> unit
