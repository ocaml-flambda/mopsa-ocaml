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

(** Logs for recording statements executed by the domains to compute
    post-conditions *)

open Ast.Var
open Ast.Stmt
open Ast.Expr


type log =
  | Empty
  | Node of stmt list * log * log

let pp_log_entries fmt = function
  | [] -> ()
  | [s] -> pp_stmt fmt s
  | stmts -> Format.fprintf fmt "{@[<v2>%a@]}"
               (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,") pp_stmt)
               stmts

let rec pp_log fmt = function
  | Empty         -> ()
  | Node(stmts,left,right) -> Format.fprintf fmt "(%a,%a,%a)" pp_log_entries stmts pp_log left pp_log right

let rec compare_log l1 l2 =
  if l1 == l2 then 0 else
  match l1,l2 with
    | Empty,Empty -> 0
    | Node(s1,left1,right1),Node(s2,left2,right2) ->
      Compare.compose [
        (fun () -> Compare.list compare_stmt s1 s2);
        (fun () -> compare_log left1 left2);
        (fun () -> compare_log right1 right2)
      ]
    | _ -> compare l1 l2

let empty_log = Empty

let rec is_empty_log log =
  match log with
  | Empty         -> true
  | Node(stmts,left,right) -> stmts = [] && is_empty_log left && is_empty_log right

let mk_log stmts left right =
  Node (stmts,left,right)

let get_log_stmts = function
  | Empty           -> []
  | Node(stmts,_,_) -> stmts

let get_left_log = function
  | Empty          -> Empty
  | Node(_,left,_) -> left

let get_right_log = function
  | Empty           -> Empty
  | Node(_,_,right) -> right

let set_left_log left = function
  | Empty               -> Node([], left, Empty)
  | Node(stmts,_,right) -> Node(stmts,left,right)

let set_right_log right = function
  | Empty              -> Node([], Empty, right)
  | Node(stmts,left,_) -> Node(stmts,left,right)

let map_left_log f = function
  | Empty                  -> Node([],f Empty,Empty)
  | Node(stmts,left,right) -> Node(stmts,f left,right)

let map_right_log f = function
  | Empty                  -> Node([],Empty,f Empty)
  | Node(stmts,left,right) -> Node(stmts,left,f right)

let add_stmt_to_log stmt = function
  | Empty                  -> Node([stmt],Empty,Empty)
  | Node(stmts,left,right) -> Node(stmt::stmts,left,right)

let rec merge_log f1 f2 f log1 log2 =
  if log1 == log2 then log1 else
  match log1, log2 with
  | Empty, Empty -> Empty
  | Empty, Node(stmts,left,right) -> Node (f1 stmts, left, right)
  | Node(stmts,left,right), Empty -> Node (f2 stmts, left, right)
  | Node(stmts1,left1,right1), Node(stmts2,left2,right2) -> Node (f stmts1 stmts2, merge_log f1 f2 f left1 left2, merge_log f1 f2 f right1 right2)

let concat_log log1 log2 =
  merge_log
    (fun stmts1 -> stmts1)
    (fun stmts2 -> stmts2)
    (fun stmts1 stmts2 -> stmts1 @ stmts2)
    log1 log2

let meet_log log1 log2 =
  (* When intersecting two post-states, logs are simply
     concatenated. The reason is that intersections are used in
     reduced products and the domains in the reduced product *must*
     modify distinct dimensions in the shared underlying
     domain. Therefore, statements are not in conflict and can be
     executed in sequence *)
  concat_log log1 log2


(* When joining two post-states, statements in logs can not be
   concatenated because joins are performed within the same domain. We
   need to introduce a new statement for expressing non-deterministic
   executions *)
type stmt_kind += S_ndet of stmt list * stmt list

let () = register_stmt {
    print = (fun next fmt s ->
        match skind s with
        | S_ndet (s1,s2) ->
          Format.fprintf fmt "%a ⁠‖ %a"
            pp_log_entries s1
            pp_log_entries s2
        | _ -> next fmt s
      );
    compare = (fun next s s' ->
        match skind s, skind s' with
        | S_ndet(s1,s2), S_ndet(s1',s2') ->
          Compare.pair (Compare.list compare_stmt) (Compare.list compare_stmt)
            (s1,s2) (s1',s2')
        | _ -> next s s'
      );
  }

let mk_ndet stmts1 stmts2 range =
  mk_stmt (S_ndet (stmts1,stmts2)) range

let join_range = Location.(tag_range (mk_fresh_range ()) "log-join")

let join_log log1 log2 =
  merge_log
    (fun stmts1 -> stmts1)
    (fun stmts2 -> stmts2)
    (fun stmts1 stmts2 ->
       if stmts1 = [] || stmts2 = []
       then stmts1 @ stmts2
       else [mk_ndet stmts1 stmts2 join_range]
    )
    log1 log2


(** {2 Generic merge} *)
(** ***************** *)

(** Effect of a statement in terms of modified and removed variables *)
type effect = {
  modified: VarSet.t;
  removed: VarSet.t;
}

(** Get the effect of a statement *)
let rec get_stmt_effect stmt : effect =
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

  | S_ndet(e1,e2) ->
    let effect1 = get_entries_effect e1
    and effect2 = get_entries_effect e2 in
    { modified = VarSet.union effect1.modified effect2.modified;
      removed = VarSet.union (VarSet.diff effect1.removed effect2.modified) (VarSet.diff effect2.removed effect1.modified); }

  | _ -> Exceptions.panic "get_stmt_effect: unsupported statement %a" pp_stmt stmt


(** Get the effect of a log *)
and get_entries_effect (entries:stmt list) : effect =
  List.fold_right
    (fun stmt acc ->
      let effect = get_stmt_effect stmt in
      { modified = VarSet.union effect.modified (VarSet.diff acc.modified effect.removed);
        removed  = VarSet.union effect.removed (VarSet.diff acc.removed effect.modified); }
    ) entries {modified = VarSet.empty; removed = VarSet.empty}


(** Apply the effect of a log on an abstract element *)
let apply_effect effect ~add ~remove ~find (other:'a) (this:'a) : 'a =
  let a = VarSet.fold (fun v acc ->
      add v (find v other) acc
    ) effect.modified this
  in
  VarSet.fold (fun v acc ->
      remove v acc
    ) effect.removed a
  

(** Generic merge operator for non-relational domains *)
let generic_domain_merge ~add ~find ~remove (a1, log1) (a2, log2) =
  (* Clean logs by removing successive duplicates *)
  let rec clean = function
    | [] -> []
    | hd::tl ->
      let tl' = doit hd tl in
      hd::clean tl'
  and doit stmt = function
    | [] -> []
    | (hd::tl) as l -> if compare_stmt stmt hd = 0 then doit stmt tl else l
  in
  let log1 = clean log1 and log2 = clean log2 in
  let () = Debug.debug ~channel:"framework.core.log" "generic merge:@\nlog1 = @[%a@]@\nlog2 = @[%a@]" pp_log_entries log1 pp_log_entries log2 in
  if log1 = [] then a2,a2 else
  if log2 = [] then a1,a1 else
  if Compare.list compare_stmt log1 log2 = 0 then a1,a2 else
  let e1 = get_entries_effect log1 in
  let a2' = apply_effect e1 a1 a2 ~add ~remove ~find in
  let e2 = get_entries_effect log2 in
  let a1' = apply_effect e2 a2 a1 ~add ~remove ~find in
  a1',a2'
