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
module Itv = Universal.Numeric.Values.Intervals.Integer.Value


module Domain =
struct



  (** {2 Domain header} *)
  (** ================= *)

  (** Map from variables to set of pointer values *)
  module Stat = Framework.Lattices.Const.Make(Status)
  module Root = Framework.Lattices.Const.Make(Roots)
  module ShapeSet  = Framework.Lattices.Powerset.Make(Shape)
  module Val  = Framework.Lattices.Pair.Make(Framework.Lattices.Pair.Make(Stat)(Root))(ShapeSet)
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
    match Map.find_opt var map with 
    | None -> Map.add var ((status', Nbt NotRooted), ShapeSet.singleton NonValue) map
    | Some ((_, roots), shapes) -> Map.add var ((status', roots), shapes)  map
    

  let lookup_status var map = 
    match Map.find_opt var map with 
    | None -> None 
    | Some ((status, _), _) -> Some status 

  let lookup_shapes var map =
    match Map.find_opt var map with 
    | None -> None 
    | Some (_, sh) -> Some sh


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
    | Some ((stat, _), _) -> Cases.singleton stat flow
    | None -> 
      (* FIXME: for type var[size], var is not added to the domain *)
      let () = Debug.debug ~channel:"runtime" "missing variable %a" pp_var var 
      in Cases.singleton (Nbt Untracked: Stat.t) flow

 
  (* S(&v) *)
  let status_addr_of_var man flow var range = 
    match vkind var with 
    | V_c_stack_var _ -> Cases.singleton (Nbt Untracked: Stat.t) flow
    | V_cvar _ -> Cases.singleton (Nbt Untracked: Stat.t) flow
    (* NOTE: This should only be called right after creating a fresh variable for the address.
      Hence, it is fine to say that the FFI variable is untracked. Alternatively, it could copy 
      the status of the contents [status_var m v], which is untracked or possibly alive inside of loops.  *)
    | V_ffi_ptr _ -> Cases.singleton (Nbt Untracked: Stat.t) flow
    | _ -> 
      let flow = raise_or_fail_ffi_unsupported range (Format.asprintf "status(%a) unsupported for this variable kind" pp_var var) man flow in 
      Cases.empty flow

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
    | E_var (v, _) -> status_addr_of_var man flow v e.erange
    | _ -> 
      let flow = raise_or_fail_ffi_unsupported e.erange (Format.asprintf "status(&%a) unsupported for this expression; only variables supported" pp_expr e) man flow in 
      Cases.empty flow
      

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
    | _ -> 
      let flow = raise_or_fail_ffi_unsupported e.erange (Format.asprintf "status(%a) unsupported for this kind of expression" pp_expr e) man flow in 
      Cases.empty flow
and status_deref man flow e range = 
    match Static_points_to.eval_opt e with
    | None | Some (Fun _) -> 
      let flow = raise_or_fail_ffi_unsupported e.erange (Format.asprintf "status(*%a) unsupported for this kind of expression" pp_expr e) man flow in 
      Cases.empty flow
    | Some (Eval(v, _, _)) -> 
      (* NOTE: Thist state should be unreachable, because dereference is taken care of 
         by an earlier domain. However, in some corner cases we can actually still reach 
         this case. Since evaluation of [*e] has not succeeded, we simply return [T], 
         because we do not know the precise status. *)
      let () = Debug.debug ~channel:"status" "status of *%a in *%a unknown" pp_var v pp_expr e in 
      Cases.singleton (TOP: Stat.t) flow
    | Some Null | Some Invalid | Some Top -> 
      Cases.singleton (Nbt Untracked: Stat.t) flow
    | Some (AddrOf ({ base_kind = Var v; }, _, _)) -> 
      status_var man flow v 
    | Some (AddrOf ({ base_kind = Addr a; }, _, _)) -> 
      let flow = raise_or_fail_ffi_unsupported e.erange (Format.asprintf "status(%a) unsupported for addresses" pp_addr a) man flow in 
      Cases.empty flow
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
    let m' = Map.add var ((Stat.embed Untracked, Root.embed NotRooted), ShapeSet.singleton NonValue) m in
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
  let eval_block_as_var base off mode typ range man flow = 
    let lval = mk_lval base off typ mode range in 
    man.eval lval flow >>$ fun exp flow -> 
    match ekind exp with 
    | E_var (v, _) -> Cases.singleton (Some v) flow 
    | _ -> Cases.singleton None flow 

  let eval_deref_to_var exp man flow = 
    resolve_pointer exp man flow >>$ fun ptr flow -> 
    match ptr with 
    | P_block (base, off, mo) ->
      eval_block_as_var base off mo ffi_value_typ exp.erange man flow 
    | P_null | P_invalid | P_top | P_fun _ ->
      Cases.singleton None flow 

  let mark_var_active var range man flow = 
    let (m, l) = get_env T_cur man flow in
    let m' = update_status var (Stat.embed Active) m in 
    let flow = set_env T_cur (m', l) man flow in
    Post.return flow 







  let eval_mark_active_contents exp man flow = 
    eval_deref_to_var exp man flow >>$ fun var flow ->
    begin match var with 
    | None -> 
      let flow = raise_or_fail_ffi_unsupported exp.erange (Format.asprintf "attempting to mark the expression *%a active, which does not evaluate to a variable" pp_expr exp) man flow in 
      Cases.empty flow
    | Some v -> 
      mark_var_active v exp.erange man flow >>% fun flow -> 
      Eval.singleton (mk_unit exp.erange) flow
    end

  let eval_mark_active_pointer range exp man flow =
    match ekind (remove_casts exp) with 
    | E_var (var, _) ->  
      mark_var_active var range man flow >>% fun flow ->
      Eval.singleton (mk_unit range) flow
    | _ -> 
      let flow = raise_or_fail_ffi_unsupported exp.erange (Format.asprintf "attempting to mark the expression %a active, which is not a variable" pp_expr exp) man flow in 
      Cases.empty flow
      
  let eval_garbage_collect range man flow =
    let (m, l)  = get_env T_cur man flow in 
    let upd_set ((s, r), shapes) = if Stat.is_const s Active && not (Root.is_const r Rooted) then ((Stat.embed Stale, r), shapes) else ((s, r), shapes) in 
    let m' = Map.map (fun s -> upd_set s) m in
    let flow = set_env T_cur (m', l) man flow in
    let () = Debug.debug ~channel:"runtime" "garbage collect" in
    Eval.singleton (mk_unit range) flow

  let eval_assert_valid_var var m exp man flow =
    let flow = match lookup_status var m with
    | Some BOT -> flow 
    | Some Nbt Active -> 
      let flow = safe_ffi_value_liveness_check exp.erange man flow in 
      flow
    | Some Nbt Stale | Some Nbt Untracked | None -> 
      let flow = raise_ffi_inactive_value ~bottom:true exp man flow in
      flow
    | Some TOP -> 
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
    | _ -> 
      let flow = raise_or_fail_ffi_unsupported exp.erange (Format.asprintf "validity checks are only supported for variables, %a is not a variable" pp_expr exp) man flow in 
      Cases.empty flow


  let exec_register_root_var var range man flow = 
    let (m, l) = get_env T_cur man flow in 
    let vexp = (mk_var var range) in
    match Map.find_opt var m with
    | Some ((_, Nbt Rooted), _) -> 
      let flow = raise_ffi_double_root ~bottom:false vexp man flow in
      Post.return flow 
    | Some ((Nbt Untracked, _), _) ->     
      let flow = raise_ffi_inactive_value ~bottom:true vexp man flow in
      Post.return flow 
    | Some ((Nbt Active, Nbt NotRooted), shapes) ->
      let m' = Map.add var ((Nbt Active, Root.embed Rooted), shapes) m in 
      let flow = set_env T_cur (m', l) man flow in      
      let flow = safe_ffi_roots_check range man flow in 
      Post.return flow 
    | _ -> 
      let flow = raise_ffi_rooting_failed ~bottom:true vexp man flow in
      Post.return flow 

  let eval_register_root range exp man flow = 
    eval_deref_to_var exp man flow >>$ fun var flow -> 
    match var with 
    | None -> 
      let flow = raise_ffi_rooting_failed ~bottom:true exp man flow in
      Eval.singleton (mk_unit range) flow
    | Some v -> 
      exec_register_root_var v exp.erange man flow >>% fun flow -> 
      Eval.singleton (mk_unit exp.erange) flow
    
    

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
    | _ ->
      let flow = raise_or_fail_ffi_unsupported range (Format.asprintf "failed to allocate a fresh address") man flow in 
      Cases.empty flow


  let rec eval_ffi_primtive_args args man flow =
    match args with 
    | [] -> Cases.singleton [] flow
    | (arg::args) -> 
      man.eval arg flow >>$ fun arg flow -> 
      eval_ffi_primtive_args args man flow >>$ fun args flow ->
      Cases.singleton (arg :: args) flow

  
  let eval_number_exp_to_integer exp man flow = 
    let kind_itv = man.ask (Universal.Numeric.Common.mk_int_interval_query exp) flow in
    match Itv.bounds_opt kind_itv with 
    | Some l, Some r when Z.equal l r ->
      Cases.singleton (Some l) flow 
    | _ -> 
      Cases.singleton None flow 

  let shape_ident_to_shape id : ShapeSet.t option =
    match Z.to_int id with 
    | exception Z.Overflow -> None 
    | 1 -> ShapeSet.singleton Immediate |> OptionExt.return
    | 2 -> ShapeSet.singleton Block |> OptionExt.return
    | 3 -> ShapeSet.singleton Double |> OptionExt.return
    | 4 -> ShapeSet.singleton Int64 |> OptionExt.return
    | 5 -> ShapeSet.singleton Int32 |> OptionExt.return
    | 6 -> ShapeSet.singleton Nativeint |> OptionExt.return
    | 7 -> ShapeSet.singleton String |> OptionExt.return
    | 8 -> ShapeSet.join (ShapeSet.singleton Block) (ShapeSet.singleton Immediate) 
      |> OptionExt.return
    | 9 -> ShapeSet.singleton Bigarray |> OptionExt.return
    | 10 -> ShapeSet.singleton Abstract |> OptionExt.return
    | 11 -> ShapeSet.singleton Any |> OptionExt.return
    | _ -> None


  let eval_assert_shape_var var ss range man flow = 
    let (m, l) = man.get T_cur flow in
    match lookup_shapes var m with 
    | None | Some TOP -> 
      let msg = Format.asprintf "cannot determine shape of %a" pp_var var in
      let flow = raise_or_fail_ffi_unsupported range msg man flow in 
      Cases.empty flow
    | Some s when ShapeSet.mem Any s ->
      Cases.singleton () flow 
    | Some s ->
      if ShapeSet.for_all (fun sh -> ShapeSet.mem sh ss) s 
      then Cases.singleton () flow 
      else 
        let msg = Format.asprintf "shape mismatch" in
        let flow = raise_or_fail_ffi_unsupported range msg man flow in 
        Cases.empty flow


  let eval_assert_shape v sh range man flow =
    match ekind (remove_casts v) with 
    | E_var (v, _) ->
      eval_number_exp_to_integer sh man flow >>$ fun sh' flow -> 
      let shapeset = OptionExt.bind (fun x -> shape_ident_to_shape x) sh' in 
      begin match shapeset with
      | Some ss -> 
        eval_assert_shape_var v ss range man flow >>% fun flow -> 
        Eval.singleton (mk_unit range) flow
      | None -> 
        let msg = Format.asprintf "cannot turn %a into a shape" pp_expr sh in
        let flow = raise_or_fail_ffi_unsupported range msg man flow in 
        Cases.empty flow
      end
    | _ -> 
      let msg = Format.asprintf "cannot determine shape of %a" pp_expr v in
      let flow = raise_or_fail_ffi_unsupported range msg man flow in 
      Cases.empty flow
      






  let eval_ffi_primtive f args range man flow =
    eval_ffi_primtive_args args man flow >>$ fun args flow -> 
    match f, args with 
    | "_ffi_garbage_collect", [] -> 
      eval_garbage_collect range man flow 
    | "_ffi_mark_active_contents", [e] -> 
      eval_mark_active_contents e man flow
    | "_ffi_mark_active_ptr", [e] -> 
      eval_mark_active_pointer range e man flow
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
    | _, _ -> 
      let msg = Format.asprintf "unsupported ffi call %s(%a)" f (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") pp_expr) args in
      let flow = raise_or_fail_ffi_unsupported range msg man flow in 
      Cases.empty flow
      

  let eval_var_status var man flow =
    let (m, l) = get_env T_cur man flow in
    match lookup_status var m with
    | Some s -> 
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
