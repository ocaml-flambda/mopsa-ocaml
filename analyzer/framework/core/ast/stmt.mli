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

(** Statements

    This module is responsible for extending the Mopsa AST with new statements.
    This is done by creating new variant constructor of the extensible type
    [stmt_kind], for instance
    {[ 
      type stmt_kind += S_assign of expr * expr
    ]}
    creates a new assignment statement, which needs to be registered 
    {[
      let () = register_stmt {
          compare = (fun next s1 s2 ->
              match skind s1, skind s2 with
              | S_assign(x1,e1), S_assign(x2,e2) ->
                Compare.pair compare_expr compare_expr (x1,e1) (x2,e2)
              | _ -> next s1 s2
            );
          print = (fun next s ->
              match skind s with
              | S_assign(x,e) -> Format.fprintf fmt "%a = %a;" pp_expr x pp_expr e
              | _    -> next fmt s
            );
        }
    ]}
 *)

open Mopsa_utils
open Var
open Program
open Expr
open Location


type stmt_kind = ..
(** Extensible kinds of statements *)

type stmt = {
  skind : stmt_kind;       (** kind of the statement *)
  srange : Location.range; (** location range of the statement *)
}
(** Statements *)


val mk_stmt : stmt_kind -> range -> stmt
(** Create a statement *)

val skind : stmt -> stmt_kind
(** Get the kind of a statement *)

val srange : stmt -> range
(** Get the location range of a statement *)

val compare_stmt : stmt -> stmt -> int
(** Total order between statements *)

val pp_stmt : Format.formatter -> stmt -> unit
(** Pretty-printer of statements *)


(****************************************************************************)
(**                            {1 Registration}                             *)
(****************************************************************************)

val register_stmt : stmt TypeExt.info -> unit
(** [register_stmt info] registers a new statement by registering its 
    compare function [info.compare] and pretty-printer [info.print] *)

val register_stmt_compare : stmt TypeExt.compare -> unit
(** Register a comparison function for statements *)

val register_stmt_pp : stmt TypeExt.print -> unit
(** Register a pretty-printer function for statements *)


(****************************************************************************)
(**                               {1 Blocks}                                *)
(****************************************************************************)

type block = stmt list
(** Blocks are sequences of statements *)

val pp_block : Format.formatter -> block -> unit
(** Pretty-printer for blocks *)


(****************************************************************************)
(**                           {1 Common statements}                         *)
(****************************************************************************)

(** {2 Programs} *)
type stmt_kind += S_program of program            (** program *) *
                               string list option (** Command-line arguments
                                                      as given to Mopsa after 
                                                      [--] *)

(** {2 Assignments} *)
type stmt_kind += S_assign of expr (** lhs *) * expr (** rhs *)
               

val mk_assign : expr -> expr -> range -> stmt
(** [mk_assign lhs rhs range] creates the assignment [lhs = rhs;] *)


(** {2 Tests} *)
type stmt_kind += S_assume of expr (** condition *)

val mk_assume : expr -> range -> stmt
(** Create a test statement *)


(** {2 Dimensions} *)
type stmt_kind +=
  | S_add of expr
  (** Add a dimension to the environment *)

  | S_remove of expr
  (** Remove a dimension from the environment and invalidate all references 
      to it *)

  | S_invalidate of expr
  (** Invalidate all references to a dimension without removing it *)

  | S_rename of expr (** old *) * expr (** new *)
  (** Rename a dimension into another *)

  | S_forget of expr
  (** Forget a dimension by putting ⊤ (all possible values) in it.
      Note that the dimension is not removed *)

  | S_project of expr list
  (** Project the environment on the given list of dimensions.
      All other dimensions are removed *)

  | S_expand of expr * expr list
  (** Expand a dimension into a list of other dimensions.
      The expanded dimension is untouched *)

  | S_fold of expr * expr list
  (** Fold a list of dimensions into a single one.
      The folded dimensions are removed *)

  | S_block of stmt list (** Sequence block of statements *) * var list (** local variables declared within the block *)
  | S_havoc (** statment to represent external function call *)

(** Utility functions to create various statements for dimension management *)
val mk_rename : expr -> expr -> range -> stmt
val mk_rename_var : var -> var -> range -> stmt
val mk_remove : expr -> range -> stmt
val mk_remove_var : var -> range -> stmt
val mk_invalidate : expr -> range -> stmt
val mk_invalidate_var : var -> range -> stmt
val mk_add : expr -> range -> stmt
val mk_add_var : var -> range -> stmt
val mk_project : expr list -> range -> stmt
val mk_project_vars : var list -> range -> stmt
val mk_forget : expr -> range -> stmt
val mk_forget_var : var -> range -> stmt
val mk_havoc : range -> stmt
val mk_expand : expr -> expr list -> range -> stmt
val mk_expand_var : var -> var list -> range -> stmt
val mk_fold : expr -> expr list -> range -> stmt
val mk_fold_var : var -> var list -> range -> stmt


(****************************************************************************)
(**                    {1 Containers of statements}                         *)
(****************************************************************************)

module StmtSet : SetExtSig.S with type elt = stmt
(** Sets of statements *)                                    

module StmtMap : MapExtSig.S with type key = stmt
(** Maps of statements *)
