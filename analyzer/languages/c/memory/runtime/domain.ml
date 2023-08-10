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


type check +=  
| CHK_FFI_LIVENESS_VALUE 
| CHK_FFI_RUNTIME_LOCK
| CHK_FFI_ROOTS


let () =
  register_check (fun next fmt -> function
      | CHK_FFI_LIVENESS_VALUE -> Format.fprintf fmt "Runtime value liveness"     
      | CHK_FFI_ROOTS -> Format.fprintf fmt "Runtime roots"     
      | CHK_FFI_RUNTIME_LOCK -> Format.fprintf fmt "Runtime lock"
      | a -> next fmt a
    )

type alarm_kind += 
  | A_ffi_non_alive_value of expr
  | A_ffi_double_root of expr
  | A_ffi_non_variable_root of expr
  | A_ffi_runtime_unlocked
  | A_ffi_begin_end_roots 

let () = 
  register_alarm {
    check = (fun next alarm -> 
      match alarm with
      | A_ffi_non_alive_value e -> 
        CHK_FFI_LIVENESS_VALUE
      | A_ffi_runtime_unlocked ->
        CHK_FFI_RUNTIME_LOCK
      | A_ffi_double_root _ | A_ffi_non_variable_root _ | A_ffi_begin_end_roots  -> 
        CHK_FFI_ROOTS
      | a -> next a);
    compare = (fun next a1 a2 -> 
      match a1, a2 with
      | A_ffi_non_alive_value e1, A_ffi_non_alive_value e2 -> compare_expr e1 e2
      | A_ffi_runtime_unlocked, A_ffi_runtime_unlocked -> 0
      | A_ffi_double_root e1, A_ffi_double_root e2 -> compare_expr e1 e2
      | A_ffi_non_variable_root e1, A_ffi_non_variable_root e2 -> compare_expr e1 e2
      | A_ffi_begin_end_roots, A_ffi_begin_end_roots -> 0
      | _ -> next a1 a2 
    );
    print = (fun next fmt a -> 
      match a with 
      | A_ffi_non_alive_value e -> 
        Format.fprintf fmt "'%a' is not alive at this point" (Debug.bold pp_expr) e
      | A_ffi_double_root e -> 
        Format.fprintf fmt "'%a' is already registered as a root" (Debug.bold pp_expr) e
      | A_ffi_non_variable_root e -> 
          Format.fprintf fmt "attempting to register '%a' as a root failed" (Debug.bold pp_expr) e
      | A_ffi_runtime_unlocked -> 
        Format.fprintf fmt "runtime unlocked"
      | A_ffi_begin_end_roots -> 
        Format.fprintf fmt "Begin_roots/End_roots is deprecated" 
      | a -> next fmt a
    ); 
    join = (fun next a1 a2 -> next a1 a2);
  }



let raise_ffi_inactive_value ?(bottom=true) exp man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_ffi_non_alive_value exp) cs exp.erange in
  Flow.raise_alarm alarm ~bottom ~warning:(not bottom) man.lattice flow
    
let raise_ffi_runtime_unlocked ?(bottom=true) range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_ffi_runtime_unlocked) cs range in
  Flow.raise_alarm alarm ~bottom ~warning:(not bottom) man.lattice flow

let raise_ffi_double_root ?(bottom=true) exp man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_ffi_double_root exp) cs exp.erange in
  Flow.raise_alarm alarm ~bottom ~warning:(not bottom) man.lattice flow
  
let raise_ffi_double_root ?(bottom=true) exp man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_ffi_non_variable_root exp) cs exp.erange in
  Flow.raise_alarm alarm ~bottom ~warning:(not bottom) man.lattice flow
      
let raise_ffi_rooting_failed ?(bottom=true) exp man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_ffi_non_variable_root exp) cs exp.erange in
  Flow.raise_alarm alarm ~bottom ~warning:(not bottom) man.lattice flow
        
let raise_ffi_begin_end_roots range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_ffi_begin_end_roots) cs range in
  Flow.raise_alarm alarm ~bottom:false ~warning:true man.lattice flow
          

(* safe checks *)
let safe_ffi_value_liveness_check range man flow =
    Flow.add_safe_check CHK_FFI_LIVENESS_VALUE range flow
  
let safe_ffi_runtime_lock_check range man flow =
  Flow.add_safe_check CHK_FFI_RUNTIME_LOCK range flow
    
let safe_ffi_roots_check range man flow =
  Flow.add_safe_check CHK_FFI_ROOTS range flow
  


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
    | Nbt Alive -> Post.return flow (* we allow for now to pass alive values to external functions *)
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
    | S_ffi_ext_call args ->
      exec_ext_call args man flow |> OptionExt.return
    | S_assign ({ ekind= E_var (var, mode) }, e) ->
      let () = Debug.debug ~channel:"runtime" "assigning %a = %a" pp_var var pp_expr e in 
      exec_update var e man flow >>% (fun flow -> man.exec ~route:(Below name) stmt flow) |> OptionExt.return
    | _ -> None


  (** {2 FFI function evaluation} *)
  (** ====================== *)
  let eval_generate_value range man flow =
    let expr = mk_ffi_alive_value range in
    Eval.singleton expr flow


  let eval_mark_alive range exp man flow =
    let (m, l) = get_env T_cur man flow in 
    match ekind exp with 
    | E_var (var, _) ->  
      let m' = update_status var (Stat.embed Alive) m in 
      let flow = set_env T_cur (m', l) man flow in
      Eval.singleton (mk_unit range) flow
    | _ -> failwith (Format.asprintf "attempting to mark the expression %a alive, which is not a variable" pp_expr exp)

  let eval_garbage_collect range man flow =
    let (m, l)  = get_env T_cur man flow in 
    let upd_set (s, r) = if Stat.is_const s Alive && not (Root.is_const r Rooted) then (Stat.embed Collected, r) else (s, r) in 
    let m' = Map.map (fun s -> upd_set s) m in
    let flow = set_env T_cur (m', l) man flow in
    let () = Debug.debug ~channel:"runtime" "garbage collect" in
    Eval.singleton (mk_unit range) flow

  let eval_assert_valid_var var m exp man flow =
    let flow = match Map.find_opt var m with
    | Some (BOT, _) -> flow 
    | Some (Nbt Alive, _) -> 
      let flow = safe_ffi_value_liveness_check exp.erange man flow in 
      flow
    | Some (Nbt Collected, _) | Some (Nbt Untracked, _) | None -> 
      let flow = raise_ffi_inactive_value ~bottom:true exp man flow in
      flow
    | Some (TOP, _) -> 
      let flow = raise_ffi_inactive_value ~bottom:false exp man flow in
      flow
    in 
      let () = Debug.debug ~channel:"runtime" "garbage collect" in
      Eval.singleton (mk_unit exp.erange) flow
    

  let eval_begin_end_roots range man flow = 
    let flow = raise_ffi_begin_end_roots range man flow in
    Eval.singleton (mk_unit range) flow
    

  let eval_assert_valid exp man flow = 
    let (m, l) = get_env T_cur man flow in 
    match ekind exp with 
    | E_var (var, _) -> eval_assert_valid_var var m exp man flow
    | _ -> failwith (Format.asprintf "validity checks are only supported for variables, %a is not a variable" pp_expr exp)
  

  let eval_register_root range exp man flow =
    let (m, l) = get_env T_cur man flow in 
    match ekind exp with 
    | E_var (var, _) -> 
      begin match Map.find_opt var m with
      | Some (_, Nbt Rooted) -> 
        let flow = raise_ffi_double_root ~bottom:false exp man flow in
        Eval.singleton (mk_unit range) flow
      | Some (Nbt Untracked, _) ->     
        let flow = raise_ffi_inactive_value ~bottom:true exp man flow in
        Eval.singleton (mk_unit range) flow
      | Some (Nbt Alive, Nbt NotRooted) ->
        let m' = Map.add var (Nbt Alive, Root.embed Rooted) m in 
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
    | "_ffi_mark_alive", [e] -> 
        eval_mark_alive range e man flow
    | "_ffi_register_root", [e] -> 
      eval_register_root range e man flow
    | "_ffi_assert_alive", [e] -> 
      eval_assert_valid e man flow
    | "_ffi_assert_locked", [] -> 
      eval_assert_runtime_lock range man flow
    | "_ffi_acquire_lock", [] -> 
      eval_update_runtime_lock range true man flow 
    | "_ffi_release_lock", [] -> 
      eval_update_runtime_lock range false man flow
    | _, _ -> failwith (Format.asprintf "unsupported ffi call %s(%a)" f (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") pp_expr) args) 



  (** Entry point of abstraction evaluations *)
  let eval exp man flow = 
    match ekind exp with 
    | E_ffi_call (f, args) -> eval_ffi_primtive f args exp.erange man flow |> OptionExt.return
    | _ -> None

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
