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
open Sig.Abstraction.Value
open Universal.Ast
open Ast
open Common.Points_to
open Common.Base
open Top



(** {2 Pointer values} *)
(** ****************** *)
module PointerValue =
struct

  type t =
    | Null (** Null pointer *)
    | Invalid (** Invalid pointer, such as non-null integers casted to pointer *)
    | Base of base (** Pointer to a base (i.e. a memory block) *)
    | Fun of c_fundec (** Function pointer *)

  let compare p1 p2 =
    match p1, p2 with
    | Null, Null -> 0
    | Invalid, Invalid -> 0
    | Base b1, Base b2 -> compare_base b1 b2
    | Fun f1, Fun f2 -> compare f1.c_func_unique_name f2.c_func_unique_name
    | _ -> compare p1 p2

  let print printer v =
    match v with
    | Null -> pp_string printer "NULL"
    | Invalid -> pp_string printer "INVALID"
    | Base b -> unformat pp_base printer b
    | Fun f -> pprint printer (fbox "λ%s" f.c_func_org_name)

  let is_valid = function
    | Base _ | Fun _ -> true
    | Null | Invalid -> false
end


(** {2 Sets of pointer values} *)
(** ************************** *)

module PointerSet =
struct

  include Framework.Lattices.Powerset.Make(PointerValue)


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
    top_apply (Set.exists (function
        | Base _ -> true
        | _ -> false
      )) true a


  (** Fold over pointed objects *)
  let fold_points_to
      (f: t -> points_to -> 'a -> 'a)
      (a:t)
      (offset:expr)
      (init:'a)
    : 'a
    =
    match a with
    | TOP -> f a P_top init
    | Nt s ->
      Set.fold (fun p acc ->
          match p with
          | Null -> f null P_null acc
          | Invalid -> f invalid P_invalid acc
          | Base b -> f (base b) (P_block(b,offset,None)) acc
          | Fun func -> f (cfun func) (P_fun func) acc
        ) s init



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
    top_apply (fun s ->
        Set.cardinal s = 1 (* FIXME: computing the cardinal for this check is inefficient *)
      ) false a



  (** Compute the difference between an abstract value and a singleton *)
  let singleton_diff (a1:t) (a2:t) : t =
    if is_singleton a2
    then diff a1 a2
    else a1


  (** Filter valid pointers *)
  let filter_valid = filter PointerValue.is_valid

  (** Filter non-valid pointers *)
  let filter_non_valid = filter (fun p -> not (PointerValue.is_valid p))

  (** Returns true if there exist valid pointer values in [a]. *)
  let there_are_valid_pointers a = is_top a || cardinal (filter_valid a) > 0

  (** Returns true if there exist non-valid pointer values in [a]. *)
  let there_are_non_valid_pointers a = is_top a || cardinal (filter_non_valid a) > 0

end
