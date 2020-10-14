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

(** Effect are logs of all statements executed during the computation of
    a post-state *)

open Ast.Var
open Ast.Stmt
open Ast.Expr
open Mopsa_utils

type effect =
  | Effect_empty
  | Effect_block of stmt list
  | Effect_seq   of effect list
  | Effect_join  of effect * effect
  | Effect_meet  of effect * effect

let rec pp_effect fmt = function
  | Effect_empty -> ()
  | Effect_block b ->
    Format.fprintf fmt "@[<hv2>{ %a }@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ") pp_stmt)
      b
  | Effect_seq seq ->
    Format.fprintf fmt "@[<hv2>{ %a }@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "⨟@ ") pp_effect)
      seq
  | Effect_join (e1,e2) ->
    Format.fprintf fmt "@[<hv2>( %a ∨ @ %a )@]"
      pp_effect e1
      pp_effect e2
  | Effect_meet (e1,e2) ->
    Format.fprintf fmt "@[<hv2>( %a ∧: @ %a )@]"
      pp_effect e1
      pp_effect e2

let rec compare_effect e1 e2 =
  if e1 == e2 then 0
  else
    match e1,e2 with
    | Effect_empty, Effect_empty -> 0

    | Effect_block s1, Effect_block s2 ->
      Compare.list compare_stmt s1 s2

    | Effect_seq s1, Effect_seq s2 ->
      Compare.list compare_effect s1 s2

    | Effect_join (e1,e2), Effect_join (f1,f2) ->
      Compare.pair compare_effect compare_effect (e1,e2) (f1,f2)

    | Effect_meet (e1,e2), Effect_meet (f1,f2) ->
      Compare.pair compare_effect compare_effect (e1,e2) (f1,f2)

    | _ -> compare e1 e2

let empty_effect = Effect_empty

let rec is_empty_effect = function
  | Effect_empty -> true
  | Effect_block _ -> false
  | Effect_seq l -> List.for_all is_empty_effect l
  | Effect_join(e1,e2) -> is_empty_effect e1 && is_empty_effect e2
  | Effect_meet(e1,e2) -> is_empty_effect e1 && is_empty_effect e2

let join_effect e1 e2 =
  if e1 == e2 then e1 else
  if is_empty_effect e1 then e2 else
  if is_empty_effect e2 then e1 else
    match e1,e2 with
    | Effect_empty, x -> x
    | x, Effect_empty -> x
    | _ -> Effect_join(e1,e2)

let meet_effect e1 e2 =
  if e1 == e2 then e1 else
  if is_empty_effect e1 then e2 else
  if is_empty_effect e2 then e1 else
    match e1,e2 with
    | Effect_empty, x -> x
    | x, Effect_empty -> x
    | _ -> Effect_meet(e1,e2)

let add_stmt_to_effect s = function
  | Effect_empty -> Effect_block [s]
  | Effect_block b -> Effect_block (s::b)
  | Effect_seq l -> Effect_seq (Effect_block [s]::l)
  | e -> Effect_seq [Effect_block [s];e]

let rec concat_effect ~old ~recent =
  if is_empty_effect old then recent else
  if is_empty_effect recent then old
  else
    match old,recent with
    | Effect_empty, Effect_empty -> Effect_empty
    | Effect_block b1, Effect_block b2 -> Effect_block (b2@b1)
    | Effect_seq l1, Effect_seq l2 -> Effect_seq (l2@l1)
    | Effect_seq l, x -> Effect_seq(x::l)
    | x, Effect_seq l -> Effect_seq(l@[x])
    | _ -> Effect_seq [recent;old]

let rec fold_stmt_effect f acc = function
  | Effect_empty -> acc
  | Effect_block s -> List.fold_left f acc s
  | Effect_seq l -> List.fold_left (fold_stmt_effect f) acc l
  | Effect_join(e1,e2)
  | Effect_meet(e1,e2) ->
    let acc' = fold_stmt_effect f acc e1 in
    fold_stmt_effect f acc' e2

type teffect =
  | Teffect_empty
  | Teffect_node of effect * teffect * teffect

let empty_teffect = Teffect_empty

let rec pp_teffect fmt = function
  | Teffect_empty -> ()
  | Teffect_node(e,l,r) ->
    Format.fprintf fmt "@[<v2>%a@,%a@,%a@]"
      pp_effect e
      pp_teffect l
      pp_teffect r

let rec is_empty_teffect = function
  | Teffect_empty -> true
  | Teffect_node(e,l,r) ->
    is_empty_effect e &&
    is_empty_teffect l &&
    is_empty_teffect r

let rec compare_teffect te1 te2 =
  if te1 == te2 then 0 else
  match te1,te2 with
    | Teffect_empty,Teffect_empty -> 0
    | Teffect_node(e1,left1,right1),Teffect_node(e2,left2,right2) ->
      Compare.triple compare_effect compare_teffect compare_teffect
        (e1,left1,right1)
        (e2,left2,right2)
    | _ -> compare te1 te2

let empty_teffect = Teffect_empty

let mk_teffect e left right =
  Teffect_node (e,left,right)

let get_root_effect  = function
  | Teffect_empty       -> Effect_empty
  | Teffect_node(e,_,_) -> e

let get_left_teffect = function
  | Teffect_empty          -> Teffect_empty
  | Teffect_node(_,left,_) -> left

let get_right_teffect = function
  | Teffect_empty           -> Teffect_empty
  | Teffect_node(_,_,right) -> right

let set_left_teffect left = function
  | Teffect_empty           -> Teffect_node(Effect_empty, left, Teffect_empty)
  | Teffect_node(e,l,right) as te ->
    if l == left then te else Teffect_node(e,left,right)

let set_right_teffect right = function
  | Teffect_empty          -> Teffect_node(Effect_empty, Teffect_empty, right)
  | Teffect_node(e,left,r) as te ->
    if right == r then te else Teffect_node(e,left,right)

let map_left_teffect f = function
  | Teffect_empty              -> Teffect_node(Effect_empty,f Teffect_empty,Teffect_empty)
  | Teffect_node(e,left,right) -> Teffect_node(e,f left,right)

let map_right_teffect f = function
  | Teffect_empty              -> Teffect_node(Effect_empty,Teffect_empty,f Teffect_empty)
  | Teffect_node(e,left,right) -> Teffect_node(e,left,f right)

let add_stmt_to_teffect stmt = function
  | Teffect_empty              -> Teffect_node(Effect_block [stmt],Teffect_empty,Teffect_empty)
  | Teffect_node(e,left,right) -> Teffect_node(add_stmt_to_effect stmt e,left,right)

let rec merge_teffect f1 f2 f teffect1 teffect2 =
  if teffect1 == teffect2 then teffect1 else
  match teffect1, teffect2 with
  | Teffect_empty, Teffect_empty -> Teffect_empty
  | Teffect_empty, Teffect_node(e,left,right) ->
    let ee = f1 e in
    if ee == e then teffect2 else Teffect_node (ee, left, right)
  | Teffect_node(e,left,right), Teffect_empty ->
    let ee = f2 e in
    if ee == e then teffect1 else Teffect_node (ee, left, right)
  | Teffect_node(e1,left1,right1), Teffect_node(e2,left2,right2) ->
    let e = f e1 e2 in
    let l = merge_teffect f1 f2 f left1 left2 in
    let r = merge_teffect f1 f2 f right1 right2 in
    if e == e1 && l == left1 && r == right1 then teffect1 else
    if e == e2 && l == left2 && r == right2 then teffect2
    else Teffect_node (e, l, r)

let concat_teffect ~old ~recent =
  merge_teffect
    (fun old -> old)
    (fun recent -> recent)
    (fun old recent -> concat_effect ~old ~recent)
    old recent

let meet_teffect teffect1 teffect2 =
  merge_teffect
    (fun e1 -> e1)
    (fun e2 -> e2)
    (fun e1 e2 -> meet_effect e1 e2)
    teffect1 teffect2


let join_teffect teffect1 teffect2 =
  merge_teffect
    (fun e1 -> e1)
    (fun e2 -> e2)
    (fun e1 e2 -> join_effect e1 e2)
    teffect1 teffect2

let rec fold_stmt_teffect f acc = function
  | Teffect_empty -> acc
  | Teffect_node(e,l,r) ->
    let acc' = fold_stmt_effect f acc e in
    let acc'' = fold_stmt_teffect f acc' l in
    fold_stmt_teffect f acc'' r

(** {2 Generic merge} *)
(** ***************** *)

(** Effect of a statement in terms of modified and removed variables *)
type var_effect = {
  modified: VarSet.t;
  removed: VarSet.t;
}

let compare_var_effect ve1 ve2 =
  Compare.pair VarSet.compare VarSet.compare
    (ve1.modified,ve1.removed)
    (ve2.modified,ve2.removed)

let is_empty_var_effect e =
  VarSet.is_empty e.modified &&
  VarSet.is_empty e.removed

(** Get the effect of a statement *)
let rec get_stmt_var_effect ~custom stmt : var_effect =
  match custom stmt with
  | Some ve -> ve
  | None ->
    match skind stmt with
    | S_add { ekind = E_var (var, _) } ->
      { modified = VarSet.singleton var;
        removed = VarSet.empty }

    | S_remove { ekind = E_var (var, _) } ->
      { modified = VarSet.empty ;
        removed = VarSet.singleton var; }

    | S_assign ({ ekind = E_var (var, _) },_) ->
      { modified = VarSet.singleton var;
        removed = VarSet.empty }

    | S_assume (e) ->
      { modified = VarSet.empty;
        removed = VarSet.empty }

    | S_rename ( {ekind = E_var (var1, _)}, {ekind = E_var (var2, _)} ) ->
      { modified = VarSet.singleton var2;
        removed = VarSet.singleton var1 }

    | S_expand({ekind = E_var(var,_)}, vl) ->
      { modified = VarSet.of_list (List.map (function {ekind = E_var(v,_)} -> v | _ -> assert false) vl);
        removed = VarSet.empty }

    | S_fold({ekind = E_var(var,_)}, vl) ->
      { modified = VarSet.singleton var;
        removed = VarSet.of_list (List.map (function {ekind = E_var(v,_)} -> v | _ -> assert false) vl) }

    | S_forget { ekind = E_var (var, _) } ->
      { modified = VarSet.singleton var;
        removed = VarSet.empty }

    | _ -> Exceptions.panic "generic merge: unsupported statement %a" pp_stmt stmt

let rec get_var_effect ~custom = function
  | Effect_empty -> { modified = VarSet.empty; removed = VarSet.empty }
  | Effect_block b ->
    (* Fold from the right because effects are stored in reverse order
       (head of the list is the last recorded effect) *)
    List.fold_right
      (fun s acc ->
         let effect = get_stmt_var_effect ~custom s in
         { modified = VarSet.union effect.modified (VarSet.diff acc.modified effect.removed);
           removed  = VarSet.union effect.removed (VarSet.diff acc.removed effect.modified); }
      ) b {modified = VarSet.empty; removed = VarSet.empty}
  | Effect_seq l ->
    (* Fold from the right because effects are stored in reverse order
       (head of the list is the last recorded effect) *)
    List.fold_right
      (fun e acc ->
         let effect = get_var_effect ~custom e in
         { modified = VarSet.union effect.modified (VarSet.diff acc.modified effect.removed);
           removed  = VarSet.union effect.removed (VarSet.diff acc.removed effect.modified); }
      ) l {modified = VarSet.empty; removed = VarSet.empty}
  | Effect_join(e1,e2) ->
    let ve1 = get_var_effect ~custom e1
    and ve2 = get_var_effect ~custom e2 in
    { modified = VarSet.union ve1.modified ve2.modified;
      removed = VarSet.union (VarSet.diff ve1.removed ve2.modified) (VarSet.diff ve2.removed ve1.modified); }
  | Effect_meet(e1,e2) ->
    let ve1 = get_var_effect ~custom e1
    and ve2 = get_var_effect ~custom e2 in
    { modified = VarSet.union ve1.modified ve2.modified;
      removed = VarSet.union (VarSet.diff ve1.removed ve2.modified) (VarSet.diff ve2.removed ve1.modified); }

(** Apply the effect of a log on an abstract element *)
let apply_var_effect effect ~add ~remove ~find (other:'a) (this:'a) : 'a =
  let a = VarSet.fold (fun v acc ->
      try add v (find v other) acc
      with _ -> Exceptions.panic "generic merge: error while adding variable %a" pp_var v
    ) effect.modified this
  in
  VarSet.fold (fun v acc ->
      try remove v acc
      with _ -> Exceptions.panic "generic merge: error while removing variable %a" pp_var v
    ) effect.removed a
  

(** Generic merge operator for non-relational domains *)
let generic_merge ~add ~find ~remove ?(custom=(fun stmt -> None)) (a1, e1) (a2, e2) =
  if e1 == e2 then a1,a1 else
  let ve1 = get_var_effect ~custom e1 in
  let ve2 = get_var_effect ~custom e2 in
  if compare_var_effect ve1 ve2 = 0 then a1,a2
  else
    let a2' = apply_var_effect ve1 a1 a2 ~add ~remove ~find in
    let a1' = apply_var_effect ve2 a2 a1 ~add ~remove ~find in
    a1',a2'
