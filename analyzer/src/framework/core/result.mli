(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Result - data structure encapsulating return values of transfer functions.

    A result encodes three pieces of information:

    1. It embeds a DNF formula of partitioned outputs. Each output comes with
    a flow abstraction and a log of statements performed during the
    computation of the output. Outputs are optional: an empty output is useful
    to represent suspended computations.

    2. It contains also a flow-insenstive context over all visited control flows.

    3. Finally, it has a list of cleaner statements for removing temporary results
    introduced during the computation.
*)


open Token
open Ast.All
open Flow
open Log
open Context




(** Results of type ['r] over an abstraction ['a] *)
type ('a,'r) result



(****************************************************************************)
(**                      {2 Utility functions}                              *)
(****************************************************************************)

(** Create a result with a single output *)
val return : ?cleaners:block -> ?log:log -> 'r option -> 'a flow -> ('a,'r) result


(** Create a result with a single non-empty output *)
val singleton : ?cleaners:block -> 'r -> 'a flow -> ('a,'r) result


(** Create a result with a single empty output *)
val empty : 'a flow -> ('a,'r) result
val empty_singleton : 'a flow -> ('a,'r) result


(** Get the flow-insensitive context of a result *)
val get_ctx : ('a,'r) result -> 'a ctx


(** Set the flow-insensitive context of a result *)
val set_ctx : 'a ctx -> ('a,'r) result -> ('a,'r) result


(** Copy context from a result to another *)
val copy_ctx : ('a,'r) result -> ('a,'s) result -> ('a,'s) result


(** Add cleaners to all cases of a result *)
val add_cleaners : block -> ('a,'r) result -> ('a,'r) result


(** Apply a map function on the logs of all cases of a result *)
val map_log : (log -> log) -> ('a,'r) result -> ('a,'r) result


(** Map each case with with function [f] if the return value is
    non-empty, otherwise remove the case *)
val map_opt : ('r -> 's option option) -> ('a,'r) result -> ('a,'s) result


(** Map outputs of a result *)
val map : ('r->'s) -> ('a,'r) result -> ('a,'s) result


(** [apply f join meet r] collapses a formula to a single value
    by applying [f] on each case of [r] and merging cases using [join]
    and [meet] *)
val apply : ('r option -> 'a flow -> 'b) -> ('b -> 'b -> 'b) -> ('b -> 'b -> 'b) -> ('a,'r) result -> 'b


(** Similar to [apply] but passes to [f] also logs and cleaners *)
val apply_full : ('r option -> 'a flow -> log -> block -> 'b) -> ('b -> 'b -> 'b) -> ('b -> 'b -> 'b) -> ('a,'r) result -> 'b



val print_full :
  (Format.formatter -> 'r option -> 'a flow -> unit) ->
  Format.formatter -> ('a,'r) result -> unit


val print :
  (Format.formatter -> 'r -> 'a flow -> unit) ->
  Format.formatter ->
  ('a,'r) result ->
  unit



(****************************************************************************)
(**                         {2 Lattice operators}                           *)
(****************************************************************************)

(** Join two results *)
val join : ('a,'r) result -> ('a,'r) result -> ('a,'r) result


(** Meet two results *)
val meet : ('a,'r) result -> ('a,'r) result -> ('a,'r) result


(** Join a list of results *)
val join_list : empty:('a,'r) result -> ('a,'r) result list -> ('a,'r) result


(** Meet a list of results *)
val meet_list : empty:('a,'r) result -> ('a,'r) result list -> ('a,'r) result


(** Merge flows of conjunctions of two results and keep outputs unmodified *)
val merge_conjunctions_flow :
  ('a flow * log -> 'a flow * log -> 'a flow) ->
  ('a,'r) result -> ('a,'r) result


(****************************************************************************)
(**                        {2 Monadic binders}                              *)
(****************************************************************************)

(** Bind cases of a result with a partial transfer function.
    All properties of the case (i.e. output, flow, logs and cleaners) are
    passed to the function.
*)
val bind_full_opt :
  ('r option -> 'a flow -> log -> stmt list -> ('a,'s) result option) ->
  ('a,'r) result ->
  ('a,'s) result option


(** Bind operator for [bind_full_opt] *)
val (>>*?) :
  ('a,'r) result ->
  ('r option -> 'a flow -> log -> stmt list -> ('a,'s) result option) ->
  ('a,'s) result option


(** Bind cases of a result with a transfer function.
    All properties of the case (i.e. output, flow, logs and cleaners) are
    passed to the function.
*)
val bind_full :
  ('r option -> 'a flow -> log -> stmt list -> ('a,'s) result) ->
  ('a,'r) result ->
  ('a,'s) result


(** Bind operator for [bind_full] *)
val (>>*) :
  ('a,'r) result ->
  ('r option -> 'a flow -> log -> stmt list -> ('a,'s) result) ->
  ('a,'s) result


(** Same as [bind_full_opt], but without passing cleaners and logs to the partial transfer function. *)
val bind_opt :
  ('r option -> 'a flow -> ('a,'s) result option) ->
  ('a,'r) result ->
  ('a,'s) result option


(** Bind operator for [bind_opt] *)
val (>>=?) :
  ('a,'r) result ->
  ('r option -> 'a flow -> ('a,'s) result option) ->
  ('a,'s) result option


(** Same as [bind_full], but without passing cleaners and logs to the transfer function. *)
val bind :
  ('r option -> 'a flow -> ('a,'s) result) ->
  ('a,'r) result ->
  ('a,'s) result


(** Bind operator for [bind] *)
val (>>=) :
  ('a,'r) result ->
  ('r option -> 'a flow -> ('a,'s) result) ->
  ('a,'s) result


(** Same as [bind_opt], but without passing empty outputs to the partial transfer function.
    Identify function is applied to empty outputs.
*)
val bind_some_opt :
  ('r -> 'a flow -> ('a,'s) result option) ->
  ('a,'r) result ->
  ('a,'s) result option


(** Bind operator for [bind_some_opt] *)
val (>>$?) :
  ('a,'r) result ->
  ('r -> 'a flow -> ('a,'s) result option) ->
  ('a,'s) result option


(** Same as [bind], but without passing empty outputs to the transfer function.
    Identify function is applied to empty outputs.
*)
val bind_some :
  ('r -> 'a flow -> ('a,'s) result) ->
  ('a,'r) result ->
  ('a,'s) result


(** Bind operator for [bind_some] *)
val (>>$) :
  ('a,'r) result ->
  ('r -> 'a flow -> ('a,'s) result) ->
  ('a,'s) result


(** Bind a list of results with a partial transfer function. *)
val bind_list_opt :
  'r list -> ('r -> 'a flow -> ('a,'s) result option) ->
  'a flow -> ('a, 's list) result option


(** Bind a list of results with a transfer function *)
val bind_list :
  'r list -> ('r -> 'a flow -> ('a,'s) result) ->
  'a flow -> ('a, 's list) result

(** Remove duplicate results *)
val remove_duplicates : ('r -> 'r -> int) -> 'a Lattice.lattice -> ('a,'r) result -> ('a,'r) result

(** Return the number of outputs in a result *)
val cardinal : ('a,'r) result -> int
