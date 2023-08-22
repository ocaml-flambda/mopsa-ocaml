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
open Common.Runtime
open Common.Base
open Common.Alarms
open Common.Static_points_to
open Common.Runtime
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

  (** {2 Initialization} *)
  (** ================== *)

  let init prog man flow =
    set_env T_cur (Map.empty, Lock.embed Locked) man flow



  (** {2 Expression Status} *)
  (** ==================== *)
  
  let pp_status fmt (s: Stat.t) = Format.pp_print_string fmt (Stat.to_string s)
 
  (* S(v) *)
  let status_var man flow var =
    let (m, l) = get_env T_cur man flow in
    match Map.find_opt var m with 
    | Some (stat, rt) -> Cases.singleton stat flow
    | None -> 
      (* FIXME: for type var[size], var is not added to the domain *)
      let () = Debug.debug ~channel:"runtime" "missing variable %a" pp_var var 
      in Cases.singleton (Nbt Untracked: Stat.t) flow

 
  (* S(&v) *)
  let status_addr_of_var man flow var = 
    match vkind var with 
    | V_c_stack_var _ -> Cases.singleton (Nbt Untracked: Stat.t) flow
    | V_cvar _ -> Cases.singleton (Nbt Untracked: Stat.t) flow
    (* NOTE: This should only be called right after creating a fresh variable for the address.
      Hence, it is fine to say that the FFI variable is untracked. Alternatively, it could copy 
      the status of the contents [status_var m v], which is untracked or possibly alive inside of loops.  *)
    | V_ffi_ptr _ -> Cases.singleton (Nbt Untracked: Stat.t) flow
    | _ -> failwith "unsupported variable kind for status"

  (* S(c) *)
  let status_const man flow const = 
    Cases.singleton (Nbt Untracked: Stat.t) flow

  let status_binop man flow op (s1: Stat.t) (s2: Stat.t) = 
    match s1, s2 with 
    | BOT, _ | _, BOT -> Cases.singleton (BOT: Stat.t) flow
    | TOP, _ | _, TOP -> Cases.singleton (TOP: Stat.t) flow
    | Nbt Stale, _ | _, Nbt Stale -> 
      Cases.singleton (Nbt Stale: Stat.t) flow
    | Nbt Active, Nbt Active
    | Nbt Active, Nbt Untracked 
    | Nbt Untracked, Nbt Active -> 
      Cases.singleton (Nbt Active: Stat.t) flow
    | Nbt Untracked, Nbt Untracked -> 
      Cases.singleton (Nbt Untracked: Stat.t) flow

  let status_unop man flow op s = 
    Cases.singleton s flow 

  let status_addr_of man flow e = 
    match ekind e with 
    | E_var (v, _) -> status_addr_of_var man flow v
    | _ -> failwith "support only computing the status of the address of variables"

  let status_cast man flow c s =
    Cases.singleton s flow

  let rec status_expr man flow e = 
    match ekind e with 
    | E_var (var, _)       -> 
      status_var man flow var
    | E_constant c         -> 
      status_const man flow c
    | E_binop (op, e1, e2) -> 
      status_expr man flow e1 >>$ fun s1 flow -> 
      status_expr man flow e2 >>$ fun s2 flow ->
      status_binop man flow op s1 s2
    | E_unop(op, e)        -> 
      status_expr man flow e >>$ fun s flow ->
      status_unop man flow op s
    | E_addr(a, mode)      -> 
      Cases.singleton (Nbt Untracked: Stat.t) flow
    | E_c_address_of e     -> 
      status_addr_of man flow e
    | E_c_deref e          -> 
      status_deref man flow e e.erange
    | E_c_cast (e,c)       -> 
      status_expr man flow e >>$ fun s flow ->      
      status_cast man flow c s
    | _ -> failwith "unsupported status"
  
and status_deref man flow e range = 
    match Static_points_to.eval_opt e with
    | None | Some (Fun _) -> failwith (Format.asprintf "*%a is not supported" pp_expr e)
    | Some (Eval(v, _, _)) -> 
      (* NOTE: Thist state should be unreachable, because dereference is taken care of 
         by an earlier domain. However, in some corner cases we can actually still reach 
         this case. Since evaluation of [*e] has not succeeded, we simply return [T], 
         because we do not know the precise status. *)
      let () = Debug.debug ~channel:"unreachable" "status of *%a in *%a unknown" pp_var v pp_expr e in 
      Cases.singleton (TOP: Stat.t) flow
    | Some Null | Some Invalid | Some Top -> 
      Cases.singleton (Nbt Untracked: Stat.t) flow
    | Some (AddrOf ({ base_kind = Var v; }, _, _)) -> 
      status_var man flow v 
    | Some (AddrOf ({ base_kind = Addr a; }, _, _)) -> 
      failwith "Not supported."
    | Some (AddrOf ({ base_kind = String _; }, _, _)) -> 
      Cases.singleton (Nbt Untracked: Stat.t) flow
(* 
  Will not compute status of the following expressions. 
  They should have been taken care of by previous iterators: 
        E_alloc_addr, E_function, E_call, E_array, E_subscript, E_len,
        E_c_conditional, E_c_array_subscript, E_c_member_access, 
        E_c_function, E_c_builtin_function, E_c_builtin_call, E_c_arrow_access,
        E_c_assign, E_c_compound_assign, E_c_comma, E_c_increment, 
        E_c_statement, E_c_predefined, E_c_var_args, E_c_atomic, E_c_block_object, 
        E_c_ffi_call 
*)


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


  let exec_update var expr man flow = 
    status_expr man flow expr >>$ fun status flow -> 
    let (m, l) = get_env T_cur man flow in
    let m' = update_status var status m in
    let flow = set_env T_cur (m', l) man flow in
    Post.return flow 


  let exec_check_ext_call_arg arg man flow = 
    let () = Debug.debug ~channel:"extcall" "checking %a" pp_expr arg in
    status_expr man flow arg >>$ fun status flow ->
    begin match status with 
    | Nbt Untracked -> Post.return flow
    | Nbt Active -> Post.return flow (* we allow for now to pass active values to external functions *)
    | _ -> 
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
    | S_ffi_ext_call args ->
      exec_ext_call args man flow |> OptionExt.return
    | S_assign ({ ekind= E_var (var, mode) }, e) ->
      let () = Debug.debug ~channel:"runtime" "assigning %a = %a" pp_var var pp_expr e in 
      exec_update var e man flow >>% (fun flow -> man.exec ~route:(Below name) stmt flow) |> OptionExt.return
    | _ -> None


  (** {2 FFI function evaluation} *)
  (** ====================== *)
  let eval_mark_active range exp man flow =
    let (m, l) = get_env T_cur man flow in
    (* let () = Debug.debug ~channel:"status" "mark active %a" pp_expr exp in   *)
    match ekind (remove_casts exp) with 
    | E_var (var, _) ->  
      let m' = update_status var (Stat.embed Active) m in 
      let flow = set_env T_cur (m', l) man flow in
      Eval.singleton (mk_unit range) flow
    | _ -> failwith (Format.asprintf "attempting to mark the expression %a active, which is not a variable" pp_expr exp)

  let eval_garbage_collect range man flow =
    let (m, l)  = get_env T_cur man flow in 
    let upd_set (s, r) = if Stat.is_const s Active && not (Root.is_const r Rooted) then (Stat.embed Stale, r) else (s, r) in 
    let m' = Map.map (fun s -> upd_set s) m in
    let flow = set_env T_cur (m', l) man flow in
    let () = Debug.debug ~channel:"runtime" "garbage collect" in
    Eval.singleton (mk_unit range) flow

  let eval_assert_valid_var var m exp man flow =
    let flow = match Map.find_opt var m with
    | Some (BOT, _) -> flow 
    | Some (Nbt Active, _) -> 
      let flow = safe_ffi_value_liveness_check exp.erange man flow in 
      flow
    | Some (Nbt Stale, _) | Some (Nbt Untracked, _) | None -> 
      let flow = raise_ffi_inactive_value ~bottom:true exp man flow in
      flow
    | Some (TOP, _) -> 
      let flow = raise_ffi_inactive_value ~bottom:false exp man flow in
      flow
    in 
      Eval.singleton (mk_unit exp.erange) flow
    

  let eval_begin_end_roots range man flow = 
    let flow = raise_ffi_begin_end_roots range man flow in
    Eval.singleton (mk_unit range) flow
    

  let eval_assert_valid exp man flow = 
    let (m, l) = get_env T_cur man flow in 
    match ekind (remove_casts exp) with 
    | E_var (var, _) -> eval_assert_valid_var var m exp man flow
    | _ -> failwith (Format.asprintf "validity checks are only supported for variables, %a is not a variable" pp_expr exp)
  

  let eval_register_root range exp man flow =
    let (m, l) = get_env T_cur man flow in 
    match ekind (remove_casts exp) with 
    | E_var (var, _) -> 
      begin match Map.find_opt var m with
      | Some (_, Nbt Rooted) -> 
        let flow = raise_ffi_double_root ~bottom:false exp man flow in
        Eval.singleton (mk_unit range) flow
      | Some (Nbt Untracked, _) ->     
        let flow = raise_ffi_inactive_value ~bottom:true exp man flow in
        Eval.singleton (mk_unit range) flow
      | Some (Nbt Active, Nbt NotRooted) ->
        let m' = Map.add var (Nbt Active, Root.embed Rooted) m in 
        let flow = set_env T_cur (m', l) man flow in      
        let flow = safe_ffi_roots_check exp.erange man flow in 
        Eval.singleton (mk_unit range) flow
      | _ -> 
        let flow = raise_ffi_rooting_failed ~bottom:true exp man flow in
        Eval.singleton (mk_unit range) flow
      end
    | _ -> 
      let flow = raise_ffi_rooting_failed ~bottom:true exp man flow in
      Eval.singleton (mk_unit range) flow
    

  let eval_assert_runtime_lock range man flow =
    let (m, l): _ * Lock.t = get_env T_cur man flow in 
    match l with 
    | Nbt Locked ->
      let flow = safe_ffi_runtime_lock_check range man flow in 
      Eval.singleton (mk_unit range) flow
    | _ -> 
      let flow = raise_ffi_runtime_unlocked ~bottom:true range man flow in
      Eval.singleton (mk_unit range) flow

  let eval_update_runtime_lock range new_state man flow =
    let (m, l): _ * Lock.t = get_env T_cur man flow in 
    let l' = (if new_state then Lock.embed Locked else Lock.embed Unlocked) in
    let flow = set_env T_cur (m, l') man flow in      
    Eval.singleton (mk_unit range) flow


  let eval_fresh_pointer range man flow =
    man.eval (mk_alloc_addr A_runtime_resource ~mode:STRONG range) flow >>$ fun addr flow -> 
    match ekind addr with 
    | E_addr (a, _) -> 
      let val_var = (mk_ffi_var_expr a range) in
      (* man.exec (mk_add addr range) flow >>% fun flow -> *)
      man.exec (mk_add val_var range) flow >>% fun flow ->
      Cases.singleton (mk_c_address_of val_var range) flow
    | _ -> failwith "failed to allocate an address"


  let rec eval_ffi_primtive_args args man flow =
    match args with 
    | [] -> Cases.singleton [] flow
    | (arg::args) -> 
      man.eval arg flow >>$ fun arg flow -> 
      eval_ffi_primtive_args args man flow >>$ fun args flow ->
      Cases.singleton (arg :: args) flow

       
  let eval_ffi_primtive f args range man flow =
    eval_ffi_primtive_args args man flow >>$ fun args flow -> 
    match f, args with 
    | "_ffi_garbage_collect", [] -> 
      eval_garbage_collect range man flow 
    | "_ffi_mark_active_value", [e] -> 
      eval_mark_active range e man flow
    | "_ffi_mark_active_ptr", [e] -> 
      eval_mark_active range e man flow
    | "_ffi_register_root", [e] -> 
      eval_register_root range e man flow
    | "_ffi_assert_active", [e] -> 
      eval_assert_valid e man flow
    | "_ffi_assert_locked", [] -> 
      eval_assert_runtime_lock range man flow
    | "_ffi_acquire_lock", [] -> 
      eval_update_runtime_lock range true man flow 
    | "_ffi_release_lock", [] -> 
      eval_update_runtime_lock range false man flow
    | "_ffi_fresh_value_ptr", [] ->
      eval_fresh_pointer range man flow
    | _, _ -> failwith (Format.asprintf "unsupported ffi call %s(%a)" f (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") pp_expr) args) 




  let eval_var_status var man flow =
    let (m, l) = get_env T_cur man flow in
    match Map.find_opt var m with
    | Some (s, _) -> 
      Cases.singleton s flow
    | None -> 
      let () = Debug.debug ~channel:"status" "status of variable %a unknown" pp_var var in 
      Cases.singleton Bot_top.TOP flow 



  (** Entry point of abstraction evaluations *)
  let eval exp man flow = 
    (* let () = Debug.debug ~channel:"eval" "evaluating %a" pp_expr exp in   *)
    match ekind exp with 
    | E_ffi_call (f, args) -> eval_ffi_primtive f args exp.erange man flow |> OptionExt.return
    | E_c_deref e -> 
      let () = Debug.debug ~channel:"eval" "deref %a" pp_expr e in None
    | _ -> None

  (** {2 Handler of queries} *)
  (** ====================== *)


  let ask : type a r. (a,r) query -> (a,t) man -> a flow -> r option = fun query man flow ->
    match query with
    | Q_ffi_status var -> eval_var_status var man flow |> OptionExt.return
    | _ -> None

  (** {2 Pretty printer} *)
  (** ****************** *)

  let print_state printer (a, l) =
    let () = pprint ~path:[Key "runtime lock"] printer (String (Lock.to_string l)) in
    pprint ~path:[Key "runtime"] printer (pbox Map.print a)



  let print_expr man flow printer exp = ()

end

let () =
  register_standard_domain (module Domain)
