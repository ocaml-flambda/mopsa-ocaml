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


(** Non-relational abstraction of pointer values *)


open Mopsa
open Framework.Core.Sig.Value.Simplified
open Universal.Ast
open Ast
open Zone
open Universal.Zone
open Common.Points_to
open Common.Base



(** {2 Pointer values} *)
(** ****************** *)

type ptr =
  | Null (** Null pointer *)
  | Invalid (** Invalid pointer, such as non-null integers casted to pointers *)
  | Base of base (** Pointer to a base (i.e. a memory block) *)
  | Fun of c_fundec (** Function pointer *)

let compare_pointer p1 p2 =
  match p1, p2 with
  | Null, Null -> 0
  | Invalid, Invalid -> 0
  | Base b1, Base b2 -> compare_base b1 b2
  | Fun f1, Fun f2 -> compare f1.c_func_unique_name f2.c_func_unique_name
  | _ -> compare p1 p2

let pp_pointer fmt v =
  match v with
  | Null -> Format.pp_print_string fmt "NULL"
  | Invalid -> Format.pp_print_string fmt "INVALID"
  | Base b -> pp_base fmt b
  | Fun f -> Format.fprintf fmt "λ%s" f.c_func_org_name



(** {2 Sets of pointer values} *)
(** ************************** *)

module PointerSet = Framework.Lattices.Powerset.Make
    (struct
      type t = ptr
      let compare = compare_pointer
      let print = pp_pointer
    end)

include PointerSet


(** {2 Utility functions} *)
(** ********************* *)

(** NULL address *)
let null : t = singleton Null


(** INVALID address *)
let invalid : t = singleton Invalid


(** Base address *)
let base (b:base) : t = singleton (Base b)


(** Function address *)
let cfun (f:c_fundec) : t = singleton (Fun f)


(** Check if the pointer *may* point to valid base addresses *)
let is_valid (a:t) : bool =
  try exists (function
      | Base _ -> true
      | _ -> false
    ) a
  with Top.Found_TOP -> true


(** Fold over pointed objects *)
let fold_points_to
    (f: t -> points_to -> 'a -> 'a)
    (a:t)
    (offset:expr)
    (init:'a)
  : 'a
  =
  try fold (fun p acc ->
      match p with
      | Null -> f null P_null acc
      | Invalid -> f invalid P_invalid acc
      | Base b -> f (base b) (P_block(b,offset)) acc
      | Fun func -> f (cfun func) (P_fun func) acc
    ) a init
  with Top.Found_TOP ->
    f a P_top init


(** Check if a base belongs to an abstract value *)
let mem_base (b:base) (a:t) : bool =
  mem (Base b) a


(** Add a base to an abstract value *)
let add_base (b:base) (a:t) : t =
  add (Base b) a


(** Remove a base from an abstract value *)
let remove_base (b:base) (a:t) : t =
  remove (Base b) a


(** Rename a base *)
let rename_base (b1:base) (b2:base) (a:t) : t =
  if not (mem_base b1 a)
  then a
  else remove_base b1 a |>
       add_base b2


(** Test if an abstract value is a singleton element *)
let is_singleton a =
  try cardinal a = 1 (* FIXME: computing the cardinal for this check is inefficient *)
  with Top.Found_TOP -> false



(** Compute the difference between an abstract value and a singleton *)
let singleton_diff (a1:t) (a2:t) : t =
  if is_singleton a2
  then PointerSet.diff a1 a2
  else a1


(** Filter valid pointers *)
let filter_valid a =
  filter (function
      | Base _ | Fun _ -> true
      | Null | Invalid -> false
    ) a


(** Filter non-valid pointers *)
let filter_non_valid a =
  filter (function
      | Base _ | Fun _ -> false
      | Null | Invalid -> true
    ) a
