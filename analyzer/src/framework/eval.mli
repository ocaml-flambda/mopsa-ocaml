(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


(** Evaluations of expressions *)


type ('e, 'a) case = {
  result    : 'e option;          (** Evaluation result *)
  flow      : 'a Flow.flow;       (** Result flow *)
  cleaners   : Ast.stmt list;      (** Post-eval cleaners *)
}
(** Evaluation case *)

type ('e, 'a) t
(** Evaluations *)


val singleton : 'e option -> ?cleaners:Ast.stmt list -> 'a Flow.flow -> ('e, 'a) t
(** Evaluation singleton *)

val join : ('e, 'a) t -> ('e, 'a) t -> ('e, 'a) t
(** Compute the union of two evaluations *)

val add_cleaners : Ast.stmt list -> ('e, 'a) t -> ('e, 'a) t
(** Add cleaners to an evaluation *)

val map:
    ('e -> 'a Flow.flow -> Ast.stmt list -> ('x, 'a) t) ->
    ('e, 'a) t ->
    ('x, 'a) t
(** [map f evls] applies the evaluation function [f] on each
   case of [evls] and joins the results *)

val fold: ('b -> ('e, 'a) case -> 'b) -> 'b -> ('e, 'a) t -> 'b

val iter:
    ('e -> 'a Flow.flow -> unit) ->
    ('e, 'a) t ->
    unit
(** Iterate over the cases of an evaluation *)

val merge:
    (('e, 'a) case -> 'b) ->
    join:('b -> 'b -> 'b) ->
    ('e, 'a) t ->
    'b
(** Merge the cases of an evaluation *)

val print : Format.formatter -> ('e, 'a) t -> print_result:(Format.formatter -> 'e -> unit) -> unit
