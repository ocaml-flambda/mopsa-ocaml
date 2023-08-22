
open Mopsa
open Format
open Universal.Numeric.Common
open Universal.Ast
open Base
open Ast


let ffi_silent_analysis = ref false
let () = register_domain_option "c.iterators.program" {
  key = "-silent-failure";
  category="Runtime";
  doc=" do not raise for undefined featurs but raise an alarm";
  spec = ArgExt.Set ffi_silent_analysis;
  default=""
}



(* the type of values *)
let ffi_value_typ = T_c_integer C_signed_long


(* Runtime Status, shared accross domains *)
module Status =
struct
  type t = 
    Untracked | Active | Stale

  let compare a b =
    match a, b with 
    | Untracked, Untracked -> 0
    | Active, Active -> 0
    | Stale, Stale -> 0
    (* FIXME: This is the MOPSA standard, but does not seem great. Same below. *)
    | _, _ -> compare a b 


  let to_string s = 
    match s with
    | Untracked -> "untracked"
    | Active -> "active"
    | Stale -> "stale"
    
  let print printer b = pp_string printer (to_string b)
end





(** Runtime Addresses *)
type addr_kind +=
  | A_runtime_resource

let () =
  register_addr_kind {
    print = (fun next fmt addr_kind ->
        match addr_kind with
        | A_runtime_resource -> Format.pp_print_string fmt "runtime"
        | _ -> next fmt addr_kind
      );
    compare = (fun next ak1 ak2 ->
        match ak1, ak2 with
        | A_runtime_resource , A_runtime_resource  -> 0
        | _ -> next ak1 ak2
      );
    }

let () = Universal.Heap.Policies.register_mk_addr (fun default ak ->
             match ak with
             | A_runtime_resource -> Universal.Heap.Policies.mk_addr_stack_range ak
             | _ -> default ak)


(** Runtime Variables *)
type var_kind +=
  | V_ffi_ptr of addr
           
let pp_ffi_var_addr fmt addr =
  Format.fprintf fmt "var⦃%a⦄" pp_addr addr
           
let () =
  register_var {
    print = (fun next fmt v ->
        match v.vkind with
        | V_ffi_ptr addr -> pp_ffi_var_addr fmt addr
        | _ -> next fmt v
      );

      compare = (fun next v1 v2 ->
          match v1.vkind, v2.vkind with
          | V_ffi_ptr a1, V_ffi_ptr a2 -> compare_addr a1 a2
          | _ -> next v1 v2
        );
    }
  
let mk_ffi_var addr typ =
  let name = Format.asprintf "var%s⦄" (addr_uniq_name addr) in
  mkv name (V_ffi_ptr addr) typ

let mk_ffi_var_expr addr ?(mode=None) ?(typ=ffi_value_typ) range =
  let v = mk_ffi_var addr typ in
  mk_var v ~mode range


(** Runtime Checks and Alarms*)
type check +=  
| CHK_FFI_LIVENESS_VALUE 
| CHK_FFI_RUNTIME_LOCK
| CHK_FFI_ROOTS
| CHK_FFI


let () =
  register_check (fun next fmt -> function
      | CHK_FFI_LIVENESS_VALUE -> Format.fprintf fmt "Runtime value liveness"     
      | CHK_FFI_ROOTS -> Format.fprintf fmt "Runtime roots"     
      | CHK_FFI_RUNTIME_LOCK -> Format.fprintf fmt "Runtime lock"
      | CHK_FFI -> Format.fprintf fmt "FFI analysis failed"
      | a -> next fmt a
    )

type alarm_kind += 
  | A_ffi_non_active_value of expr
  | A_ffi_double_root of expr
  | A_ffi_non_variable_root of expr
  | A_ffi_runtime_unlocked
  | A_ffi_begin_end_roots 
  | A_ffi_abort_analysis of string

let () = 
  register_alarm {
    check = (fun next alarm -> 
      match alarm with
      | A_ffi_non_active_value e -> 
        CHK_FFI_LIVENESS_VALUE
      | A_ffi_runtime_unlocked ->
        CHK_FFI_RUNTIME_LOCK
      | A_ffi_double_root _ | A_ffi_non_variable_root _ | A_ffi_begin_end_roots  -> 
        CHK_FFI_ROOTS
      | A_ffi_abort_analysis _ -> 
        CHK_FFI
      | a -> next a);
    compare = (fun next a1 a2 -> 
      match a1, a2 with
      | A_ffi_non_active_value e1, A_ffi_non_active_value e2 -> compare_expr e1 e2
      | A_ffi_runtime_unlocked, A_ffi_runtime_unlocked -> 0
      | A_ffi_double_root e1, A_ffi_double_root e2 -> compare_expr e1 e2
      | A_ffi_non_variable_root e1, A_ffi_non_variable_root e2 -> compare_expr e1 e2
      | A_ffi_begin_end_roots, A_ffi_begin_end_roots -> 0
      | A_ffi_abort_analysis s1, A_ffi_abort_analysis s2 -> String.compare s1 s2
      | _ -> next a1 a2 
    );
    print = (fun next fmt a -> 
      match a with 
      | A_ffi_non_active_value e -> 
        Format.fprintf fmt "'%a' is not active at this point" (Debug.bold pp_expr) e
      | A_ffi_double_root e -> 
        Format.fprintf fmt "'%a' is already registered as a root" (Debug.bold pp_expr) e
      | A_ffi_non_variable_root e -> 
          Format.fprintf fmt "attempting to register '%a' as a root failed" (Debug.bold pp_expr) e
      | A_ffi_runtime_unlocked -> 
        Format.fprintf fmt "runtime unlocked"
      | A_ffi_begin_end_roots -> 
        Format.fprintf fmt "Begin_roots/End_roots is deprecated" 
      | A_ffi_abort_analysis s ->
        Format.fprintf fmt "Analysis failed: %s" s  
      | a -> next fmt a
    ); 
    join = (fun next a1 a2 -> next a1 a2);
  }



let raise_ffi_inactive_value ?(bottom=true) exp man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_ffi_non_active_value exp) cs exp.erange in
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
          

let raise_or_fail_ffi_unsupported range reason man flow =
  if not !ffi_silent_analysis then 
    failwith (Format.asprintf "failed with reason: %s" reason)
  else
    let cs = Flow.get_callstack flow in
    let alarm = mk_alarm (A_ffi_abort_analysis reason) cs range in
    Flow.raise_alarm alarm ~bottom:true ~warning:false man.lattice flow
            





(* safe checks *)
let safe_ffi_value_liveness_check range man flow =
    Flow.add_safe_check CHK_FFI_LIVENESS_VALUE range flow
  
let safe_ffi_runtime_lock_check range man flow =
  Flow.add_safe_check CHK_FFI_RUNTIME_LOCK range flow
    
let safe_ffi_roots_check range man flow =
  Flow.add_safe_check CHK_FFI_ROOTS range flow
  

(* Runtime Status Query *)
(* Points-to query *)
(* =============== *)

type ('a,_) query += Q_ffi_status : var -> ('a, ('a, Status.t Bot_top.with_bot_top) cases) query

let () = register_query {
    join = (
      let f : type a r. query_pool -> (a,r) query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_ffi_status _ -> Cases.join a b
          | _ -> next.pool_join query a b
      in
      f
    );
    meet = (
      let f : type a r. query_pool -> (a,r) query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_ffi_status _ -> Cases.meet a b
          | _ -> next.pool_meet query a b
      in
      f
    );
  }


let resolve_status var man flow = man.ask (Q_ffi_status var) flow

