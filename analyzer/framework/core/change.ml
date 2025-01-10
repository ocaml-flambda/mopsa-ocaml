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

(** Changes are used to log the statements executed during the computation of
    a post-state *)

open Ast.Var
open Ast.Stmt
open Ast.Expr
open Mopsa_utils
open Path

type change =
  | Change_empty
  | Change_block of stmt list
  | Change_seq   of change list
  | Change_join  of change * change
  | Change_meet  of change * change

let rec pp_change fmt = function
  | Change_empty -> ()
  | Change_block b ->
    Format.fprintf fmt "@[<hv2>{ %a }@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ") pp_stmt)
      b
  | Change_seq seq ->
    Format.fprintf fmt "@[<hv2>{ %a }@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "⨟@ ") pp_change)
      seq
  | Change_join (e1,e2) ->
    Format.fprintf fmt "@[<hv2>( %a ∨ @ %a )@]"
      pp_change e1
      pp_change e2
  | Change_meet (e1,e2) ->
    Format.fprintf fmt "@[<hv2>( %a ∧ @ %a )@]"
      pp_change e1
      pp_change e2

let rec compare_change e1 e2 =
  if e1 == e2 then 0
  else
    match e1,e2 with
    | Change_empty, Change_empty -> 0

    | Change_block s1, Change_block s2 ->
      Compare.list compare_stmt s1 s2

    | Change_seq s1, Change_seq s2 ->
      Compare.list compare_change s1 s2

    | Change_join (e1,e2), Change_join (f1,f2) ->
      Compare.pair compare_change compare_change (e1,e2) (f1,f2)

    | Change_meet (e1,e2), Change_meet (f1,f2) ->
      Compare.pair compare_change compare_change (e1,e2) (f1,f2)

    | _ -> compare e1 e2

let empty_change = Change_empty

let rec is_empty_change = function
  | Change_empty -> true
  | Change_block _ -> false
  | Change_seq l -> List.for_all is_empty_change l
  | Change_join(e1,e2) -> is_empty_change e1 && is_empty_change e2
  | Change_meet(e1,e2) -> is_empty_change e1 && is_empty_change e2

let join_change e1 e2 =
  if e1 == e2 then e1 else
  if is_empty_change e1 then e2 else
  if is_empty_change e2 then e1 else
    match e1,e2 with
    | Change_empty, x -> x
    | x, Change_empty -> x
    | _ -> Change_join(e1,e2)

let meet_change e1 e2 =
  if e1 == e2 then e1 else
  if is_empty_change e1 then e2 else
  if is_empty_change e2 then e1 else
    match e1,e2 with
    | Change_empty, x -> x
    | x, Change_empty -> x
    | _ -> Change_meet(e1,e2)

(** Join two changes *)
let add_stmt_to_change s = function
  | Change_empty -> Change_block [s]
  | Change_block b -> Change_block (s::b)
  | Change_seq l -> Change_seq (Change_block [s]::l)
  | e -> Change_seq [Change_block [s];e]

(** Meet two changes *)
let rec concat_change old recent =
  if is_empty_change old then recent else
  if is_empty_change recent then old
  else
    match old,recent with
    | Change_empty, Change_empty -> Change_empty
    | Change_block b1, Change_block b2 -> Change_block (b2@b1)
    | Change_seq l1, Change_seq l2 -> Change_seq (l2@l1)
    | Change_seq l, x -> Change_seq(x::l)
    | x, Change_seq l -> Change_seq(l@[x])
    | _ -> Change_seq [recent;old]

(** Compose two changes *)
type change_map = change PathMap.t

let pp_change_map fmt map =
  if PathMap.is_empty map then
    Format.pp_print_string fmt ""
  else
    Format.fprintf fmt "@[<v>%a@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         (fun fmt (path, change) -> Format.fprintf fmt "%a: %a" pp_path path pp_change change)
      ) (PathMap.bindings map)

let compare_change_map m1 m2 =
  if m1 == m2 then 0
  else PathMap.compare compare_change m1 m2 

let empty_change_map = PathMap.empty

let singleton_change_map path change = PathMap.singleton path change

let is_empty_change_map m =
  PathMap.for_all (fun _ e -> is_empty_change e) m

let concat_change_map old recent =
  PathMap.map2zo
    (fun p1 e1 -> e1)
    (fun p2 e2 -> e2)
    (fun p e1 e2 -> concat_change e1 e2)
    old recent

let join_change_map map1 map2 =
  PathMap.map2zo
    (fun p1 e1 -> e1)
    (fun p1 e2 -> e2)
    (fun p e1 e2 -> join_change e1 e2)
    map1 map2

let meet_change_map map1 map2 =
  PathMap.map2zo
    (fun p1 e1 -> e1)
    (fun p1 e2 -> e2)
    (fun p e1 e2 -> meet_change e1 e2)
    map1 map2

let get_change path map =
  match PathMap.find_opt path map with
  | None   -> Change_empty
  | Some e -> e

let add_stmt_to_change_map stmt path map =
  let change =
    match PathMap.find_opt path map with
    | None ->
      Change_block [stmt]
    | Some old ->
      add_stmt_to_change stmt old
  in
  PathMap.add path change map


(** {2 Generic merge} *)
(** ***************** *)

(** Change of a statement in terms of modified and removed variables *)
type change_vars = {
  modified: VarSet.t;
  removed: VarSet.t;
}

let compare_change_vars ve1 ve2 =
  Compare.pair VarSet.compare VarSet.compare
    (ve1.modified,ve1.removed)
    (ve2.modified,ve2.removed)

let is_empty_change_vars e =
  VarSet.is_empty e.modified &&
  VarSet.is_empty e.removed

(** Get the effect of a statement *)
let rec get_stmt_change_vars ~custom stmt : change_vars =
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

let rec get_change_vars ~custom = function
  | Change_empty -> { modified = VarSet.empty; removed = VarSet.empty }
  | Change_block b ->
    (** Fold from the right because changes are stored in reverse order
       (head of the list is the last recorded change) *)
    List.fold_right
      (fun s acc ->
         let effect = get_stmt_change_vars ~custom s in
         { modified = VarSet.union effect.modified (VarSet.diff acc.modified effect.removed);
           removed  = VarSet.union effect.removed (VarSet.diff acc.removed effect.modified); }
      ) b {modified = VarSet.empty; removed = VarSet.empty}
  | Change_seq l ->
    (** Fold from the right because changes are stored in reverse order
       (head of the list is the last recorded change) *)
    List.fold_right
      (fun e acc ->
         let effect = get_change_vars ~custom e in
         { modified = VarSet.union effect.modified (VarSet.diff acc.modified effect.removed);
           removed  = VarSet.union effect.removed (VarSet.diff acc.removed effect.modified); }
      ) l {modified = VarSet.empty; removed = VarSet.empty}
  | Change_join(e1,e2) ->
    let ve1 = get_change_vars ~custom e1
    and ve2 = get_change_vars ~custom e2 in
    { modified = VarSet.union ve1.modified ve2.modified;
      removed = VarSet.union (VarSet.diff ve1.removed ve2.modified) (VarSet.diff ve2.removed ve1.modified); }
  | Change_meet(e1,e2) ->
    let ve1 = get_change_vars ~custom e1
    and ve2 = get_change_vars ~custom e2 in
    { modified = VarSet.union ve1.modified ve2.modified;
      removed = VarSet.union (VarSet.diff ve1.removed ve2.modified) (VarSet.diff ve2.removed ve1.modified); }

(** Apply the effect of a log on an abstract element *)
let apply_change_vars effect ~add ~remove ~find (other:'a) (this:'a) : 'a =
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
  let ve1 = get_change_vars ~custom e1 in
  let ve2 = get_change_vars ~custom e2 in
  if compare_change_vars ve1 ve2 = 0 then a1,a2
  else
    let a2' = apply_change_vars ve1 a1 a2 ~add ~remove ~find in
    let a1' = apply_change_vars ve2 a2 a1 ~add ~remove ~find in
    a1',a2'

let opt_change_tracker_enabled = ref false
let enable_change_tracker () = opt_change_tracker_enabled := true
let disable_change_tracker () = opt_change_tracker_enabled := false
let is_change_tracker_enabled () = !opt_change_tracker_enabled
let set_change_tracker_state b = opt_change_tracker_enabled := b

let with_change_tracker f =
  enable_change_tracker ();
  let r = f () in
  disable_change_tracker ();
  r
