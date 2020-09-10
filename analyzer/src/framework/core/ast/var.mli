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

(** Variables

    The type [var] represents variables in Mopsa. In addition to a unique
    string name and a type, a variable is decorated with an extensible type
    [var_kind] that allows annotating the variable with extra information that
    is specific to its language.

    New kinds of variables can be added by extending [var_kind] with a
    new variant. For example, for some toy language [Toy] in which variables,
    in addition to a name and a type, have an initial value, a new variable
    kind can be created with
    {[
      type var_kind += V_toy of expr
    ]}
    This new variable kind needs to be registered
    {[
      let () = register_var {
          compare = (fun next v1 v2 ->
              match vkind v1, vkind v2 with
              | V_toy init1, V_toy init2 -> compare_expr init1 init2
              | _ -> next v1 v2
            );
          print = (fun fmt next v ->
              match vkind v with
              | V_toy v -> Format.pp_print_string fmt v.vname
              | _ -> next fmt v
            );
        }
    ]}

    Another property of variables is their access mode. When a variable
    represents a single dimension in the concrete, assignments on this variable
    are performed with strong updates, i.e. the update destroys the previous
    value.
    
    On the other hand, when an abstract domain summarizes the value of several
    concrete dimensions into a single variable, only weak updates can be
    performed to preserve the value of unmodified concrete dimensions.
*)

open Semantic

(****************************************************************************)
(**                            {1 Access modes}                             *)
(****************************************************************************)

(** Extensible kind of variables *)
type var_kind = ..

(** Access mode of a variable *)
type mode =
  | WEAK
  (** Weak updates are used when the variable summarizes several concrete 
      dimensions *)

  | STRONG
  (** Strong updates are used when the variable represents a single concrete 
      dimension *)

val pp_mode : Format.formatter -> mode -> unit
(** Pretty-print an access mode *)

val compare_mode : mode -> mode -> int
(** Compare two access modes *)



(****************************************************************************)
(**                             {1 Variables}                               *)
(****************************************************************************)

type var = {
  vname     : string;     (** unique name of the variable*)
  vkind     : var_kind;   (** kind the variable *)
  vtyp      : Typ.typ;    (** type of the variable *)
  vmode     : mode;       (** access mode of the variable *)
  vsemantic : semantic;   (** semantic of the variable *)
}
(** Variables *)


(** Accessor function to the fields of a variable *)
val vname : var -> string
val vkind : var -> var_kind
val vtyp  : var -> Typ.typ
val vmode : var -> mode
val vsemantic : var -> semantic


val mkv : string -> var_kind -> ?mode:mode -> ?semantic:semantic -> Typ.typ -> var
(** Create a variable with a unique name, a kind, a type and an access mode 
    (STRONG if not given) *)

val pp_var : Format.formatter -> var -> unit
(** Pretty-print a variable *)

val compare_var : var -> var -> int
(** Total order between variables *)


(****************************************************************************)
(**                            {1 Registration}                             *)
(****************************************************************************)

val register_var : var TypeExt.info -> unit
(** Register a new kind of variables *)

val register_var_compare : var TypeExt.compare -> unit
(** Register a new variable comparison *)

val register_var_pp : var TypeExt.print -> unit
(** Register a new variable pretty-printer *)


(****************************************************************************)
(**                          {1 Common variables}                           *)
(****************************************************************************)

type var_kind +=
  (** Variables identified by a unique numeric identifier *)
  | V_uniq of string (** Original name *) *
              int    (** Unique ID *)

  (** Temporary variables *)
  | V_tmp of int (** Unique ID *)

  (** Attribute attached to a variable *)
  | V_var_attr of var    (** Attach variable *) *
                  string (** Attribute *)

  (** Attribute attached to a program location *)
  | V_range_attr of Location.range (** Attach range *) *
                    string (** Attribute *)


val mk_uniq_var : string -> int -> ?mode:mode -> Typ.typ -> var
(** Create a unique variable *)

val mk_fresh_uniq_var : string -> ?mode:mode -> Typ.typ -> unit -> var
(** Create a fresh variable with a fresh ID *)

val mktmp : ?typ:Typ.typ -> ?mode:mode -> unit -> var
(** Create a fresh temporary variable *)

val mk_attr_var : var -> string -> ?mode:mode -> ?semantic:semantic -> Typ.typ -> var
(** [mk_attr_var v a t] creates a variable representing an attribute [a] of
    another variable [v] *)

val mk_range_attr_var : Location.range -> string -> ?mode:mode -> ?semantic:semantic -> Typ.typ -> var
(** [mk_range_attr_var r a t] creates a variable representing an attribute [a]
    of a program location [r] *)


(****************************************************************************)
(**                     {1 Containers for variables}                        *)
(****************************************************************************)

module VarSet : SetExtSig.S with type elt = var
(** Sets of variables *)

module VarMap : MapExtSig.S with type key = var
(** Maps of variables *)


(****************************************************************************)
(**                            {1 Deprecated}                               *)
(****************************************************************************)

val start_vcounter_at : int -> unit
val get_vcounter_val : unit -> int
val get_orig_vname : var -> string
val set_orig_vname : string -> var -> var
