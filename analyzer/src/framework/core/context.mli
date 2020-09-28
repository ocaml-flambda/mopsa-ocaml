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

(** Context - Storage for flow-insensitive information

    The context is a heterogeneous key-value map that stores non-semantical
    information, such as a description of the program being currently
    analyzed, the callstack, etc.

    For instance, to create a new kind of entries for storing strings, first
    generate a new context key:
    {[
      module K = GenContextKey
          (struct
            type 'a t = string
            let print pp fmt s = fprintf fmt "string: %s" s
          end)
      let string_key = K.key
    ]}
    Then, given a context ['a ctx], you can add/remove elements as follows:
    {[
      let ctx'  = add_ctx string_key "a" ctx in
      let ctx'' = remove_ctx string_key ctx'
    ]}

    Note that the context can contain the abstract state.
    For instance, to store a cache of input/output states for each function,
    you can do:
    {[
      module K = GenContextKey
          (struct
            type 'a t = (string*'a flow*'a post) list
            let print pp fmt cache =
              fprintf fmt "functions I/O cache:@,  @[<v>%a@]"
                (pp_print_list
                  ~pp_sep:(fun fmt () -> fprintf fmt "@,")
                  (fun (f,input,output) ->
                     fprintf "%s:@, input:@[%a]@, output:@[%a@]"
                       f
                       (Flow.print pp) input
                       (Post.print pp) output
                  )
                ) cache
          end)
      let cache_key = K.key
    ]}
*)

open Eq

type ('a,_) ctx_key = ..
(** Key to access an element in the context *)

type 'a ctx
(** The context *)

val empty_ctx : 'a ctx
(** Empty context *)

val singleton_ctx : ('a,'v) ctx_key -> 'v -> 'a ctx
(** Context with one element *)

val mem_ctx : ('a,'v) ctx_key -> 'a ctx -> bool
(** [mem_ctx k ctx] returns [true] when an element at key [k] is in
    the context [ctx]. *)

val find_ctx : ('a,'v) ctx_key -> 'a ctx -> 'v
(** [find_ctx k ctx] returns the element at key [k] in the context [ctx].
    Raises [Not_found] if no element is found. *)

val find_ctx_opt : ('a,'v) ctx_key -> 'a ctx -> 'v option
(** [find_ctx k ctx] returns the element of the key [k] in the context [ctx].
    Returns [None] if no element is found. *)

val add_ctx : ('a,'v) ctx_key -> 'v -> 'a ctx -> 'a ctx
(** [add_ctx k v ctx] add element [v] at key [k] in the context [ctx].
    The previous element is overwritten if present.*)

val remove_ctx : ('a,'v) ctx_key -> 'a ctx -> 'a ctx
(** [add_ctx k v ctx] removes the element at key [k] in the context [ctx].
    If key [k] was not in [ctx], [ctx] is returned unchanged. *)

val most_recent_ctx : 'a ctx -> 'a ctx -> 'a ctx
(** Get the most recent context between two *)

val pp_ctx : (Print.printer -> 'a -> unit) -> Format.formatter -> 'a ctx -> unit
(** Print a context *)

(** Pool registered keys *)
type ctx_pool = {
  ctx_pool_equal: 'a 'v 'w. ('a,'v) ctx_key -> ('a,'w) ctx_key -> ('v,'w) eq option;
  ctx_pool_print: 'a 'v. (Print.printer -> 'a -> unit) -> Format.formatter -> ('a,'v) ctx_key -> 'v -> unit;
}

(** Registration information for a new key *)
type ctx_info = {
  ctx_equal : 'a 'v 'w. ctx_pool -> ('a,'v) ctx_key -> ('a,'w) ctx_key -> ('v,'w) eq option;
  ctx_print   : 'a 'v. ctx_pool -> (Print.printer -> 'a -> unit) -> Format.formatter -> ('a,'v) ctx_key -> 'v -> unit;
}

val register_ctx : ctx_info -> unit
(** Register a new key *)

(** Generate a new key *)
module GenContextKey
    (Value:sig
       type 'a t
       val print : (Print.printer -> 'a -> unit) -> Format.formatter -> 'a t -> unit
     end)
  :
  sig
    val key : ('a,'a Value.t) ctx_key
  end

val callstack_ctx_key : ('a,Callstack.callstack) ctx_key
(** Key for storing the callstack *)
