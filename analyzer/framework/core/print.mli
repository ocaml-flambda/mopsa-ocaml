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

(** Print - structured pretty-printing *)

open Ast.Var
open Ast.Expr
open Yojson.Basic
open Mopsa_utils


(****************************************************************************)
(**                          {1 Print objects}                              *)
(****************************************************************************)

(** Symbols for printing maps, lists and sets *)
type symbols = {
  sopen : string;
  ssep   : string;
  sbind  : string;
  sclose   : string;
}

(** Structured print objects *)
type ('k,'v) map = ('k,'v) MapExtPoly.t
type 'v set = 'v SetExtPoly.t

type print_object =
  | Empty
  | Bool   of bool
  | Int    of Z.t
  | Float  of float
  | String of string
  | Var    of var
  | Map    of (print_object, print_object) map * symbols
  | List   of print_object list * symbols
  | Set    of print_object set * symbols

(****************************************************************************)
(**                            {1 Printers}                                 *)
(****************************************************************************)

(** Printers encapsulate the structured objects to print along the
   history of printed expression *)
type printer

val empty_printer : unit -> printer
(** Create an empty printer *)

val get_printed_object : printer -> print_object
(** Get the structured print object of a printer *)

val get_printed_exprs : printer -> expr list
(** Get the expressions that were already printed *)

val add_printed_expr : printer -> expr -> unit
(** Mark an expression as printed *)

val mem_printed_expr : printer -> expr -> bool
(** Check whether an expression was already printed *)

(****************************************************************************)
(**                           {1 Print paths}                               *)
(****************************************************************************)

(** Selectors of print objects *)
type print_selector =
  | Key    of string
  | Index  of int
  | Head
  | Tail

type print_path = print_selector list
(** Path of a print object *)

val find_print_object : print_path -> print_object -> print_object
(** [find_print_object path obj] returns the object placed at [path] in [obj] *)

val match_print_object_keys : Str.regexp -> print_object -> print_object
(** [match_print_object_keys re obj] slices [obj] to paths containing keys that match [re] *)

(****************************************************************************)
(**                    {1 Generic print functions}                          *)
(****************************************************************************)

val pprint : ?path:print_path -> (printer -> print_object -> unit)
(** [pprint ~path:p printer o] prints object [o] at path [p] in [printer]. *)

val pbox : (printer -> 'a -> unit) -> ('a -> print_object)
(** [pbox f x] returns a boxed object created by [f] when applied to [x].
    It is equivalent to
    {[
      let printer = empty_printer () in
      f printer x;
      get_printed_object printer
    ]}
*)

val fbox : ('a, Format.formatter, unit, print_object) format4 -> 'a
(** [fbox fmt] returns a string object of a formatted value *)

val sprint : (printer -> 'a -> unit) -> ('a -> string)
(** [sprint f x] returns the string representing the boxed object [pbox f x] *)

val fkey : ('a, Format.formatter, unit, print_selector) format4 -> 'a
(** [fkey fmt] returns a key selector with a formatted string *)

val pkey : (printer -> 'a -> unit) -> 'a -> print_selector
(** [pkey f x] returns a key selector with a printed string *)


(****************************************************************************)
(**                      {1 Typed print functions}                          *)
(****************************************************************************)

val pp_string : ?path:print_path -> (printer -> string -> unit)
(** Print a string object *)

val pp_bool : ?path:print_path -> (printer -> bool -> unit)
(** Print a boolean object *)

val pp_int : ?path:print_path -> (printer -> int -> unit)
(** Print an integer object *)

val pp_z : ?path:print_path -> (printer -> Z.t -> unit)
(** Print an integer object *)

val pp_float : ?path:print_path -> (printer -> float -> unit)
(** Print a float object *)

val pp_variable : ?path:print_path -> (printer -> var -> unit)
(** Print a variable *)

val pp_list :
  ?path:print_path -> ?lopen:string -> ?lsep:string -> ?lclose:string ->
  (printer -> 'a -> unit) ->
  (printer -> 'a list -> unit)
(** [pp_list ~path:p f printer l] prints a list [l] at path [p] by boxing [f] on every element of [l] *)

val pp_obj_list :
  ?path:print_path -> ?lopen:string -> ?lsep:string -> ?lclose:string ->
  (printer -> print_object list -> unit)
(** [pp_obj_list ~path:p printer l] prints a list of objects at path [p].
    Useful for printing heterogenous lists.
*)

val pp_map :
  ?path:print_path -> ?mopen:string -> ?msep:string -> ?mclose:string -> ?mbind:string ->
  (printer -> 'k -> unit) ->
  (printer -> 'v -> unit) ->
  (printer -> ('k * 'v) list -> unit)
(** [pp_smap ~path:p fk fv printer l] prints a map from a list [l] of pairs of keys and values.
    Keys are boxed with function [fk] and values with function [fv]. *)

val pp_obj_map :
  ?path:print_path -> ?mopen:string -> ?msep:string -> ?mclose:string -> ?mbind:string ->
  (printer -> (print_object * print_object) list -> unit)
(** [pp_obj_smap ~path:p printer l] prints a map from a list of pairs of print objects *)


val pp_obj_set :
  ?path:print_path -> ?sopen:string -> ?ssep:string -> ?sclose:string ->
  (printer -> print_object set -> unit)
(** [pp_obj_set ~path:p printer l] prints a set of objects at path [p].
    Useful for printing heterogenous sets.
*)

val pp_set :
  ?path:print_path -> ?sopen:string -> ?ssep:string -> ?sclose:string ->
  (printer -> 'a -> unit) ->
  (printer -> 'a set -> unit)
(** [pp_set ~path:p f printer l] prints a set from a list [l] at path [p] by boxing [f] on every element of [l] *)

(****************************************************************************)
(**                              {1 Format}                                 *)
(****************************************************************************)

val pp_print_object : Format.formatter -> print_object -> unit
(** Pretty-print a printer objct *)

val pflush : Format.formatter -> printer -> unit
(** Pretty-print the printer output in a format string *)

val format : (printer -> 'a -> unit) -> (Format.formatter -> 'a -> unit)
(** Convert a printer function into a format function *)

val unformat : ?path:print_path -> (Format.formatter -> 'a -> unit) -> (printer -> 'a -> unit)
(** Convert a format function into a printer *)

(****************************************************************************)
(**                               {1 JSON}                                  *)
(****************************************************************************)

val print_object_to_json : print_object -> Yojson.Basic.t
(** Convert a printer object to JSON *)

val json_to_print_object : Yojson.Basic.t -> print_object
(** Convert JSON to a printer object *)
