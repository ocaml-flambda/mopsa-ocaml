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


(** Abstraction of pointer values *)


open Mopsa
open Framework.Core.Sig.Value.Simplified
open Universal.Ast
open Ast
open Zone
open Universal.Zone
open Common.Points_to
open Common.Base



(** {2 Valid pointers} *)
(** ****************** *)

type v =
  | Base of base
  | Fun of c_fundec

let compare_valid v1 v2 =
  match v1, v2 with
  | Base b1, Base b2 -> compare_base b1 b2
  | Fun f1, Fun f2 -> compare f1.c_func_unique_name f2.c_func_unique_name
  | _ -> compare v1 v2

let pp_valid fmt v =
  match v with
  | Base b -> pp_base fmt b
  | Fun f -> Format.fprintf fmt "λ%s" f.c_func_org_name

module ValidSet = Framework.Lattices.Powerset.Make(
  struct
    type t = v
    let compare = compare_valid
    let print = pp_valid
  end
  )


(** {2 Value header} *)
(** **************** *)

(** Type of the abstract value *)
type t = {
  null    : bool;       (** is it a NULL pointer? *)
  invalid : bool;       (** is it an invalid pointer? *)
  valid   : ValidSet.t; (** set of valid addresses *)
}

(** Generate a unique ID of this abstraction *)
include Framework.Core.Id.GenValueId(struct
    type typ = t
    let name = "c.memory.pointers"
    let display = "pointers"
  end)


(** Zones of the abstracted values *)
let zones = [Zone.Z_c_scalar]


(** Types of the abstracted values *)
let types = []


(** Bottom value *)
let bottom = {
  null = false;
  invalid = false;
  valid = ValidSet.bottom;
}


(** Top value *)
let top = {
  null = true;
  invalid = true;
  valid = ValidSet.top;
}


(** Pretty-printer of an abstract value *)
let print fmt (a:t) =
  let l =
    (if a.null then [fun fmt () -> Format.pp_print_string fmt "NULL"] else []) @
    (if a.invalid then [fun fmt () -> Format.pp_print_string fmt "INVALID"] else []) @
    (if not (ValidSet.is_empty a.valid) then [fun fmt () -> ValidSet.print fmt a.valid] else [])
  in

  if l = []
  then Format.pp_print_string fmt Bot.bot_string
  else Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " v ")
      (fun fmt pp -> pp fmt ())
      fmt l


(** {2 Lattice operators} *)
(** ********************* *)

(** Emptiness test *)
let is_bottom (a:t) : bool =
  a.null = false &&
  a.invalid = false &&
  ValidSet.is_bottom a.valid


(** Approximate partial order *)
let subset (a1:t) (a2:t) : bool =
  a1.null = a2.null &&
  a1.invalid = a2.invalid &&
  ValidSet.subset a1.valid a2.valid


(** Join operator *)
let join (a1:t) (a2:t) : t = {
  null = a1.null || a2.null;
  invalid = a1.invalid || a2.invalid;
  valid = ValidSet.join a1.valid a2.valid;
}


(** Meet operator *)
let meet (a1:t) (a2:t) : t = {
  null = a1.null && a2.null;
  invalid = a1.invalid && a2.invalid;
  valid = ValidSet.meet a1.valid a2.valid;
}


(** Widening operator *)
let widen (a1:t) (a2:t) : t = {
  null = a1.null || a2.null;
  invalid = a1.invalid || a2.invalid;
  valid = ValidSet.widen a1.valid a2.valid;
}


(** {2 Utility functions} *)
(** ********************* *)

(** NULL address *)
let null : t = {
  null = true;
  invalid = false;
  valid = ValidSet.empty;
}


(** INVALID address *)
let invalid : t = {
  null = false;
  invalid = true;
  valid = ValidSet.empty;
}

let valid (v:v) : t = {
  null = false;
  invalid = false;
  valid = ValidSet.singleton v;
}

(** Address to a base *)
let base (b:base) : t =
  valid (Base b)


(** Address to a C function *)
let cfun (f:c_fundec) : t =
  valid (Fun f)


let valid_top : t = {
  null = false;
  invalid = false;
  valid = ValidSet.top;
}


(** Check if the pointer may point to valid addresses *)
let is_valid (a:t) : bool =
  not (ValidSet.is_empty a.valid)


let fold_points_to
    (f: t -> points_to -> 'a -> 'a)
    (a:t)
    (offset:expr)
    (init:'a)
  : 'a
  =
  let init = if a.null then f null P_null init else init in
  let init = if a.invalid then f invalid P_invalid init else init in
  if ValidSet.is_top a.valid then f valid_top P_valid init
  else ValidSet.fold (fun v acc ->
      let pt =
        match v with
        | Base b -> P_block (b, offset)
        | Fun f -> P_fun f
      in
      f (valid v) pt acc
    ) a.valid init


(** Check if a base belongs to an abstract value *)
let mem_base (b:base) (a:t) : bool =
  ValidSet.mem (Base b) a.valid


(** Remove a base from an abstract value *)
let remove_base (b:base) (a:t) : t =
  { a with valid = ValidSet.remove (Base b) a.valid }


(** Rename a base *)
let rename_base (b1:base) (b2:base) (a:t) : t =
  if not (mem_base b1 a)
  then a
  else { a with valid = ValidSet.remove (Base b1) a.valid |>
                        ValidSet.add (Base b2)
       }


(** Flag an abstract value as an invalid pointer *)
let invalidate (a:t) : t =
  if a.invalid then a else { a with invalid = true }


(** Check if an abstract value represents a singleton value *)
let is_singleton (a:t) : bool =
  a.null = true <>
  a.invalid = true <>
  (
    if ValidSet.is_top a.valid
    then false
    else ValidSet.cardinal a.valid = 1
  )


(** Compute the difference between two values *)
let diff (a1:t) (a2:t) : t =
  Debug.debug ~channel:"foo" "diff %a and %a" print a1 print a2;
  if is_singleton a2
  then {
    null = if a2.null then false else a1.null;
    invalid = if a2.invalid then false else a1.invalid;
    valid = ValidSet.diff a1.valid a2.valid;
  }
  else
    a1


(** {2 Forward evaluations} *)
(** *********************** *)

(** Create an abstract value from a constant *)
let of_constant c =
  match c with
  | C_int n when Z.equal n Z.zero -> null
  | C_c_invalid -> invalid
  | _ -> top


(** Forward evaluation of unary operators *)
let unop op v = top


(** Forward evaluation of binary operators *)
let binop op v1 v2 = top


(** Filter values in [v] that represent the boolean value [b] *)
let filter v b =
  { v with null = not b }


(** {2 Backward evaluations} *)
(** ************************ *)

(** Backward evaluation of unary operators *)
let bwd_unop = default_bwd_unop


(** Backward evaluation of binary operators *)
let bwd_binop = default_bwd_binop


(** Backward refinement of boolean comparisons *)
let compare = default_compare


let ask _ _ = None


let refine channel v = Channel.return v
