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

(** Non-relational abstraction of C pointers *)

open Mopsa
open Sig.Abstraction.Domain
open Universal.Ast
open Ast
open Common
open Common.Points_to
open Common.Base
open Common.Alarms
open Value
open Stubs.Ast (* for the printing functions *)



module Domain =
struct



  (** {2 Domain header} *)
  (** ================= *)

  (** Map from variables to set of pointer values *)
  module Stat = Framework.Lattices.Const.Make(Status)
  module Root = Framework.Lattices.Const.Make(Roots)
  module Val  = Framework.Lattices.Pair.Make(Stat)(Root)
  module Map  = Framework.Lattices.Partial_map.Make(Var)(Val)
  module Lock = Framework.Lattices.Const.Make(RuntimeLock)
  module Dom  = Framework.Lattices.Pair.Make(Map)(Lock)

  type t = Dom.t

  include GenDomainId(struct
      type nonrec t = t
      let name = "c.memory.runtime"
    end)

  let bottom = Dom.bottom

  let top = Dom.top

  let checks = [ ]

  (** {2 Lattice operators} *)
  (** ===================== *)

  let is_bottom = Dom.is_bottom

  let subset = Dom.subset

  let join = Dom.join

  let meet = Dom.meet

  let widen ctx = Dom.join

  let merge pre (a,e) (a',e') =
    let aa,aa' =
      generic_merge (a,e) (a',e')
        ~add:(fun v b (m, l) -> (Map.add v b m, l))  
        ~find:(fun v (m, l) -> Map.find v m)
        ~remove:(fun v (m, l) -> (Map.remove v m, l))
        ~custom:(fun stmt -> None)
  in Dom.meet aa aa'


  let update_status var status' map = 
    let (_, roots) = Map.find var map in
    Map.add var (status', roots) map

  (* let set p v a = Map.add p (Stat.embed v, ) a *)
  (* let remove p a = Map.remove p a *)

  (** {2 Initialization} *)
  (** ================== *)

  let init prog man flow =
    set_env T_cur (Map.empty, Lock.embed Locked) man flow



  (** {2 Offset variables} *)
  (** ==================== *)


  (** {2 Utility functions for symbolic evaluations} *)
  (** ============================================== *)

  let exec_add var man flow =
    let (m, l) = get_env T_cur man flow in
    let m' = Map.add var (Stat.embed Untracked, Root.embed NotRooted) m in
    let flow = set_env T_cur (m', l) man flow in
    let () = Debug.debug ~channel:"runtime" "added %a" pp_var var in
    Post.return flow 

  let exec_remove var man flow =
    let (m, l) = get_env T_cur man flow in
    let m' = Map.remove var m in
    let flow = set_env T_cur (m', l) man flow in
    let () = Debug.debug ~channel:"runtime" "removed %a" pp_var var in
    Post.return flow 

  let exec_garbage_collect man flow =
    let (m, l)  = get_env T_cur man flow in 
    let upd_set (s, r) = if Stat.is_const s Alive && not (Root.is_const r Rooted) then (Stat.embed Collected, r) else (s, r) in 
    let m' = Map.map (fun s -> upd_set s) m in
    let flow = set_env T_cur (m', l) man flow in
    let () = Debug.debug ~channel:"runtime" "garbage collect" in
    Post.return flow

  let exec_assert_valid_var var m exp man flow =
    match Map.find var m with
    | BOT, _ -> Post.return flow 
    | Nbt Alive, _ -> 
      let flow = safe_ffi_inactive_value_check exp.erange man flow in 
      Post.return flow
    | Nbt Collected, _ | Nbt Untracked, _ -> 
      let flow = raise_ffi_inactive_value ~bottom:true exp man flow in
      Post.return flow
    | TOP, _ -> 
      let flow = raise_ffi_inactive_value ~bottom:false exp man flow in
      Post.return flow


  let exec_assert_valid exp man flow = 
    man.eval exp flow >>$ fun exp flow ->
    let (m, l) = get_env T_cur man flow in 
    match ekind exp with 
    | E_var (var, _) -> exec_assert_valid_var var m exp man flow
    | _ -> 
      failwith (Format.asprintf "validity checks are only supported for variables, %a is not a variable" pp_expr exp)
  
  
  
  let exec_register_root exp man flow =
    man.eval exp flow >>$ fun exp flow ->
    let (m, l) = get_env T_cur man flow in 
    match ekind exp with 
    | E_var (var, _) -> 
      begin match Map.find var m with
      | (Nbt Alive, Nbt NotRooted) -> 
        let (stat, rt) = Map.find var m in
        let m' = Map.add var (stat, Root.embed Rooted) m in 
        let flow = set_env T_cur (m', l) man flow in      
        Post.return flow
      | (Nbt Alive, Nbt Rooted) -> failwith (Format.asprintf "variable %a has been rooted already" pp_var var)
      | (_, _) -> failwith (Format.asprintf "cannot prove that variable %a is alive and not rooted" pp_var var)
      end
    | _ -> 
        failwith (Format.asprintf "registering roots is only supported for variables, %a is not a variable" pp_expr exp)
    

  let exec_assert_runtime_lock range man flow =
    let (m, l): _ * Lock.t = get_env T_cur man flow in 
    match l with 
    | Nbt Locked ->
      let flow = safe_ffi_runtime_lock_check range man flow in 
      Post.return flow
    | _ -> 
      let flow = raise_ffi_runtime_lock ~bottom:true range man flow in
      Post.return flow

  let exec_update_runtime_lock new_state man flow =
    let (m, l): _ * Lock.t = get_env T_cur man flow in 
    let l' = (if new_state then Lock.embed Locked else Lock.embed Unlocked) in
    let flow = set_env T_cur (m, l') man flow in      
    Post.return flow

let rec expr_status m e : Stat.t = 
  match ekind e with 
  | E_var (var, _) -> var_status m var
  | E_constant c -> const_status m c
  | E_binop (op, e1, e2) -> binop_status m op e1 e2
  | E_unop(op, e) -> unop_status m op e
  | E_addr(a, mode) -> TOP
  | E_c_address_of e -> addr_of_status m e
  | E_c_deref e -> deref_status m e
  | E_c_cast (e,c) -> cast_status m e c
  | _ -> TOP

and var_status m var = 
  match Map.find_opt var m with 
  | Some (stat, rt) -> stat
  | None -> 
    (* FIXME: for type var[size], var is not added to the domain *)
    let () = Debug.debug ~channel:"runtime" "missing variable %a" pp_var var in Nbt Untracked
and binop_status m op e1 e2 : Stat.t = 
  match expr_status m e1, expr_status m e2 with 
  | BOT, _ -> BOT
  | _, BOT -> BOT
  | TOP, _ -> TOP
  | _, TOP -> TOP
  | Nbt Collected, _ -> Nbt Collected
  | _, Nbt Collected -> Nbt Collected
  | Nbt Alive, Nbt Alive -> Nbt Alive (* what operation would even make sense here? *)
  | Nbt Alive, Nbt Untracked -> Nbt Alive
  | Nbt Untracked, Nbt Alive -> Nbt Alive
  | Nbt Untracked, Nbt Untracked -> Nbt Untracked
and unop_status m op e : Stat.t = expr_status m e
and addr_of_status m e : Stat.t = expr_status m e
and deref_status m e : Stat.t = expr_status m e
and cast_status m e c : Stat.t = expr_status m e 
and const_status m c = 
  match c with 
  | C_ffi_alive_value -> Nbt Alive 
  | _ -> Nbt Untracked


  let exec_update var expr man flow = 
    let (m, l) = get_env T_cur man flow in
    let status = expr_status m expr in
    let m' = update_status var status m in
    let flow = set_env T_cur (m', l) man flow in
    let () = Debug.debug ~channel:"runtime" "updated %a status to %s" pp_var var (Stat.to_string status) in
    Post.return flow 



  let exec_check_ext_call_arg arg man flow = 
    let () = Debug.debug ~channel:"extcall" "checking %a" pp_expr arg in
    let (m, l) = get_env T_cur man flow in
    let status = expr_status m arg in 
    begin match status with 
    | Nbt Untracked -> Post.return flow
    | _ -> 
      let () = Debug.debug ~channel:"extcall" "status %s" (Stat.to_string status) in
      let flow = raise_ffi_inactive_value arg man flow in Post.return flow
    end

  let exec_ext_call args man flow = 
    let check_ext_call_args exprs = List.fold_left (fun acc c -> Post.bind (exec_check_ext_call_arg c man) acc) (Post.return flow) exprs in
    check_ext_call_args args



  (** {2 Computation of post-conditions} *)
  (** ================================== *)

  (** Entry point of abstract transformers *)
  let exec stmt man flow = 
    match skind stmt with 
    | S_add { ekind = E_var (var, _) } -> 
      exec_add var man flow >>% (fun flow -> man.exec ~route:(Below name) stmt flow) |> OptionExt.return      
    | S_remove { ekind = E_var (var, _) } -> 
      exec_remove var man flow >>% (fun flow -> man.exec ~route:(Below name) stmt flow)   |> OptionExt.return
    | S_forget { ekind = E_var (var, _) } -> 
      let () = Format.printf "forgetting %a\n" pp_var var in 
      exec_remove var man flow >>% (fun flow -> man.exec ~route:(Below name) stmt flow)  |> OptionExt.return
    | S_rename ({ ekind = E_var (from, _) }, { ekind = E_var (into, _) }) -> (Debug.debug ~channel:"runtime" "attempt rename %a into %a" pp_var from pp_var into; None)
    | S_ffi_garbage_collect -> 
      exec_garbage_collect man flow |> OptionExt.return
    | S_ffi_assert_valid e -> 
      exec_assert_valid e man flow |> OptionExt.return
    | S_ffi_register_root e -> 
      exec_register_root e man flow |> OptionExt.return
    | S_ffi_assert_locked -> 
      exec_assert_runtime_lock stmt.srange man flow |> OptionExt.return
    | S_ffi_set_lock new_state -> 
      exec_update_runtime_lock new_state man flow |> OptionExt.return
    | S_ffi_ext_call args ->
      exec_ext_call args man flow |> OptionExt.return
    | S_assign ({ ekind= E_var (var, mode) }, e) ->
      let () = Debug.debug ~channel:"runtime" "assigning %a = %a" pp_var var pp_expr e in 
      exec_update var e man flow >>% (fun flow -> man.exec ~route:(Below name) stmt flow) |> OptionExt.return
    | _ -> None


  (** {2 Pointer evaluation} *)
  (** ====================== *)

  (** Entry point of abstraction evaluations *)
  let eval exp man flow = None

  (** {2 Handler of queries} *)
  (** ====================== *)

  let ask : type a r. (a,r) query -> (a,t) man -> a flow -> r option = fun query man flow -> None


  (** {2 Pretty printer} *)
  (** ****************** *)

  let print_state printer (a, l) =
    let () = pprint ~path:[Key "runtime lock"] printer (String (Lock.to_string l)) in
    pprint ~path:[Key "runtime"] printer (pbox Map.print a)



  let print_expr man flow printer exp = ()

end

let () =
  register_standard_domain (module Domain)
