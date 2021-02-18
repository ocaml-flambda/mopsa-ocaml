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

(** Alarms for C runtime errors *)

open Mopsa
open Format
open Universal.Numeric.Common
open Universal.Ast
open Base
open Ast



(** {2 Utility print functions} *)
(** *************************** *)

(** Print an interval depending on its cardinal *)
let pp_const_or_interval fmt itv =
  match itv with
  | Bot.Nb (l,u) when I.B.eq l u -> I.B.fprint fmt l
  | _ -> I.fprint_bot fmt itv


(** Print the not-member operator of an interval, depending on its cardinal *)
let pp_const_or_interval_not_eq fmt itv =
  match itv with
  | Bot.Nb (l,u) when I.B.eq l u -> fprintf fmt "∉"
  | _ -> fprintf fmt "⊈"


let pp_interval_plurial fmt itv =
  match itv with
  | Bot.Nb (I.B.Finite l, I.B.Finite u) when Z.(l = u) && Z.(l = one) -> ()
  | _ -> fprintf fmt "s"


let pp_interval_cardinal_plurial fmt itv =
  match itv with
  | Bot.Nb (I.B.Finite l, I.B.Finite u) when Z.(l = u) -> ()
  | _ -> fprintf fmt "s"



let pp_base_verbose fmt base =
  match base.base_kind with
  | Var v    -> fprintf fmt "variable '%a'" (Debug.bold pp_var) v
  | Addr a   -> fprintf fmt "dynamically allocated block"
  | String (s,k,_) when String.length s > 20 ->
    fprintf fmt "string %a\"%s...\"" Pp.pp_character_kind k  (String.escaped (String.sub s 0 20))
  | String (s,k,_) ->
    fprintf fmt "string %a\"%s\"" Pp.pp_character_kind k (String.escaped s)


(** {2 Checks for invalid memory access} *)
(** ************************************ *)

type check += CHK_C_INVALID_MEMORY_ACCESS

let () =
  register_check (fun next fmt -> function
      | CHK_C_INVALID_MEMORY_ACCESS -> fprintf fmt "Invalid memory access"
      | a -> next fmt a
    )

type alarm_kind +=
  | A_c_null_deref of expr (** pointer *)
  | A_c_invalid_deref of expr
  | A_c_out_of_bound of base (** accessed base *) *
                        int_itv (** base size *) *
                        int_itv (** offset *) *
                        int_itv (** accessed bytes *)
  | A_c_dangling_pointer_deref of expr (** pointer *) *
                                  var (** pointed variable *) *
                                  range (** return location *)
  | A_c_use_after_free of expr (** pointer *) *
                          range (** deallocation site *)
  | A_c_modify_read_only of expr (** pointer *) *
                            base (** pointed base *)

let () =
  register_alarm {
    check = (fun next -> function
        | A_c_null_deref _
        | A_c_out_of_bound _
        | A_c_invalid_deref _
        | A_c_dangling_pointer_deref _
        | A_c_use_after_free _
        | A_c_modify_read_only _ ->
          CHK_C_INVALID_MEMORY_ACCESS
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_null_deref(p1), A_c_null_deref(p2) ->
          compare_expr p1 p2
        | A_c_invalid_deref(p1), A_c_invalid_deref(p2) ->
          compare_expr p1 p2
        | A_c_out_of_bound(b1,s1,o1,e1), A_c_out_of_bound(b2,s2,o2,e2) ->
          Compare.compose
            [ (fun () -> compare_base b1 b2);
              (fun () -> compare_int_interval s1 s2);
              (fun () -> compare_int_interval o1 o2);
              (fun () -> compare_int_interval e1 e2); ]
        | A_c_dangling_pointer_deref(p1,v1,r1), A_c_dangling_pointer_deref(p2,v2,r2) ->
          Compare.triple compare_expr compare_var compare_range (p1,v1,r1) (p2,v2,r2)
        | A_c_use_after_free(p1,r1), A_c_use_after_free(p2,r2) ->
          Compare.pair compare_expr compare_range (p1,r1) (p2,r2)
        | A_c_modify_read_only(p1,b1), A_c_modify_read_only(p2,b2) ->
          Compare.pair compare_expr compare_base (p1,b1) (p2,b2)
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_null_deref(pointer) ->
          fprintf fmt "pointer '%a' may be null" (Debug.bold pp_expr) pointer
        | A_c_invalid_deref(pointer) ->
          fprintf fmt "pointer '%a' may be invalid" (Debug.bold pp_expr) pointer
        | A_c_out_of_bound (base,size,offset,elm) ->
          fprintf fmt "accessing %a byte%a at offset%a %a of %a of size %a byte%a"
            pp_const_or_interval elm
            pp_interval_plurial elm
            pp_interval_cardinal_plurial offset
            pp_const_or_interval offset
            pp_base_verbose base
            pp_const_or_interval size
            pp_interval_plurial size
        | A_c_dangling_pointer_deref(p,v,r) ->
          begin match v.vkind with
            | V_cvar { cvar_scope = Variable_local f }
            | V_cvar { cvar_scope = Variable_parameter f }->
              fprintf fmt "'%a' points to dangling local variable '%a' of function '%a' deallocated at %a"
                (Debug.bold pp_expr) p
                (Debug.bold pp_var) v
                (Debug.bold pp_print_string) f.c_func_org_name
                pp_relative_range r

            | _ ->
              fprintf fmt "'%a' points to dangling local variable '%a' deallocated at %a"
                (Debug.bold pp_expr) p
                (Debug.bold pp_var) v
                pp_relative_range r
          end
        | A_c_use_after_free(p,r) ->
          fprintf fmt "'%a' points to memory deallocated at %a" (Debug.bold pp_expr) p pp_relative_range r
        | A_c_modify_read_only(p,b) ->
          fprintf fmt "'%a' points to read-only %a"
            (Debug.bold pp_expr) p
            pp_base_verbose b
        | a -> next fmt a
      );
    join = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_out_of_bound(b1,s1,o1,e1), A_c_out_of_bound(b2,s2,o2,e2) ->
          (* Group alarms if the base is the same *)
          if compare_base b1 b2 = 0 then
            let s = I.join_bot s1 s2 in
            let o = I.join_bot o1 o2 in
            let e = I.join_bot e1 e2 in
            Some (A_c_out_of_bound(b1,s,o,e))
          else
            None
        | _ -> next a1 a2
      );
  }

let raise_c_null_deref_alarm ?(bottom=true) pointer ?(range=pointer.erange) man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm (A_c_null_deref(pointer')) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow


let raise_c_invalid_deref_alarm ?(bottom=true) pointer ?(range=pointer.erange) man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm (A_c_invalid_deref(pointer')) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow

let raise_c_out_bound_alarm ?(bottom=true) base size offset typ range man input_flow error_flow =
  let cs = Flow.get_callstack error_flow in
  let offset_itv = man.ask (mk_int_interval_query offset) input_flow in
  let size_itv = man.ask (mk_int_interval_query size) input_flow in
  let elm_itv = Bot.Nb (void_to_char typ |> sizeof_type |> I.cst) in
  let alarm = mk_alarm (A_c_out_of_bound(base, size_itv, offset_itv, elm_itv)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice error_flow

let raise_c_dangling_deref_alarm ?(bottom=true) ptr var ret_range ?(range=ptr.erange) man flow =
  let cs = Flow.get_callstack flow in
  let ptr' = get_orig_expr ptr in
  let alarm = mk_alarm (A_c_dangling_pointer_deref(ptr',var,untag_range ret_range)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow

let raise_c_use_after_free_alarm ?(bottom=true) pointer dealloc_range ?(range=pointer.erange) man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm (A_c_use_after_free(pointer',untag_range dealloc_range)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow

let raise_c_modify_read_only_alarm ?(bottom=true) ptr base man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_c_modify_read_only(get_orig_expr ptr, base)) cs ptr.erange in
  Flow.raise_alarm alarm ~bottom man.lattice flow

let safe_c_memory_access_check range man flow =
  Flow.add_safe_check CHK_C_INVALID_MEMORY_ACCESS range flow

let unreachable_c_memory_access_check range man flow =
  Flow.add_unreachable_check CHK_C_INVALID_MEMORY_ACCESS range flow

let raise_c_memory_access_warning range man flow =
  Flow.add_warning_check CHK_C_INVALID_MEMORY_ACCESS range flow


(** {2 Division by zero} *)
(** ******************** *)

type check      += CHK_C_DIVIDE_BY_ZERO
type alarm_kind += A_c_divide_by_zero of expr (** denominator *)

let () =
  register_check (fun next fmt -> function
      | CHK_C_DIVIDE_BY_ZERO -> fprintf fmt "Division by zero"
      | a -> next fmt a
    )

let () =
  register_alarm {
    check = (fun next -> function
        | A_c_divide_by_zero _ -> CHK_C_DIVIDE_BY_ZERO
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_divide_by_zero(e1), A_c_divide_by_zero(e2) -> compare_expr e1 e2
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_divide_by_zero(e) -> fprintf fmt "denominator '%a' may be null" (Debug.bold pp_expr) e
        | a -> next fmt a
      );
    join = (fun next -> next);
  }


let raise_c_divide_by_zero_alarm ?(bottom=true) denominator range man flow =
  let cs = Flow.get_callstack flow in
  let denominator' = get_orig_expr denominator in
  let alarm = mk_alarm (A_c_divide_by_zero(denominator')) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow

let safe_c_divide_by_zero_check range man flow =
  Flow.add_safe_check CHK_C_DIVIDE_BY_ZERO range flow

let unreachable_c_divide_by_zero_check range man flow =
  Flow.add_unreachable_check CHK_C_DIVIDE_BY_ZERO range flow


(** {2 Integer overflow} *)
(** ******************** *)


type check      += CHK_C_INTEGER_OVERFLOW

type alarm_kind +=
  (** Overflow raised by integer operations *)
  | A_c_integer_overflow of expr    (** integer expression *) *
                            int_itv (** expression value *) *
                            typ     (** overflowed type *)

  (** Overlow raised by a cast of a pointer to an interger type *)
  | A_c_pointer_to_integer_overflow of expr (** pointer expression *) *
                                       typ  (** cast type*)


let () =
  register_check (fun next fmt -> function
      | CHK_C_INTEGER_OVERFLOW -> fprintf fmt "Integer overflow"
      | a -> next fmt a
    )

let () =
  register_alarm {
    check = (fun next -> function
        | A_c_integer_overflow _            -> CHK_C_INTEGER_OVERFLOW
        | A_c_pointer_to_integer_overflow _ -> CHK_C_INTEGER_OVERFLOW
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_integer_overflow(e1,v1,t1), A_c_integer_overflow(e2,v2,t2) ->
          Compare.triple compare_expr compare_int_interval compare_typ (e1,v1,t1) (e2,v2,t2)
        | A_c_pointer_to_integer_overflow(e1,t1), A_c_pointer_to_integer_overflow(e2,t2) ->
          Compare.pair compare_expr compare_typ (e1,t1) (e2,t2)
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_integer_overflow(e,v,t) ->
          let l,u = rangeof t in
          let type_range = I.of_z l u in
          fprintf fmt "'%a' has value %a that is larger than the range of '%a' = %a"
            (Debug.bold pp_expr) e
            pp_const_or_interval v
            (Debug.bold pp_typ) t
            I.fprint type_range
        | A_c_pointer_to_integer_overflow(e,t) ->
          fprintf fmt "casting pointer '%a' to '%a' may result in an overflow"
            (Debug.bold pp_expr) e
            (Debug.bold pp_typ) t
        | a -> next fmt a
      );
    join = (fun next a1 a2 ->
        match a1,a2 with
        | A_c_integer_overflow(e1,v1,t1), A_c_integer_overflow(e2,v2,t2) ->
          if compare_expr e1 e2 = 0 && compare_typ t1 t2 = 0 then
            let v = I.join_bot v1 v2 in
            Some (A_c_integer_overflow(e1,v,t1))
          else
            None
        | _ -> next a1 a2
      );  }


let raise_c_integer_overflow_alarm ?(warning=false) cexp nexp typ range man input_flow error_flow =
  let cs = Flow.get_callstack error_flow in
  let cexp' = get_orig_expr cexp in
  let itv = man.ask (mk_int_interval_query nexp) input_flow in
  let alarm = mk_alarm (A_c_integer_overflow(cexp',itv,typ)) cs range in
  Flow.raise_alarm alarm ~bottom:false ~warning man.lattice error_flow

let raise_c_pointer_to_integer_overflow_alarm ?(warning=true) exp typ range man flow =
  let cs = Flow.get_callstack flow in
  let exp' = get_orig_expr exp in
  let alarm = mk_alarm (A_c_pointer_to_integer_overflow(exp',typ)) cs range in
  Flow.raise_alarm alarm ~bottom:false ~warning man.lattice flow

let safe_c_integer_overflow_check range man flow =
  Flow.add_safe_check CHK_C_INTEGER_OVERFLOW range flow

let unreachable_c_integer_overflow_check range man flow =
  Flow.add_unreachable_check CHK_C_INTEGER_OVERFLOW range flow


(** {2 Invalid shift} *)
(** ***************** *)

type check      += CHK_C_INVALID_SHIFT
type alarm_kind += A_c_invalid_shift of expr (** shifted expression *) *
                                        expr (** shift expression *) *
                                        int_itv (** shift value *)

let () =
  register_check (fun next fmt -> function
      | CHK_C_INVALID_SHIFT -> fprintf fmt "Invalid shift"
      | a -> next fmt a
    )

let () =
  register_alarm {
    check = (fun next -> function
        | A_c_invalid_shift _ -> CHK_C_INVALID_SHIFT
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_invalid_shift(e1,s1,v1), A_c_invalid_shift(e2,s2,v2) ->
          Compare.triple compare_expr compare_expr compare_int_interval (e1,s1,v1) (e2,s2,v2)
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_invalid_shift(e,shift,v) ->
          let bits = Ast.sizeof_type e.etyp |> Z.mul (Z.of_int 8) in
          let valid_shifts = I.of_z Z.zero (Z.pred bits) in
          fprintf fmt "shift position '%a' = %a %a %a"
            (Debug.bold pp_expr) e
            pp_const_or_interval v
            pp_const_or_interval_not_eq v
            I.fprint valid_shifts
        | a -> next fmt a
      );
    join = (fun next a1 a2 ->
        match a1,a2 with
        | A_c_invalid_shift(e1,s1,v1), A_c_invalid_shift(e2,s2,v2) ->
          if compare_expr e1 e2 = 0 && compare_expr s1 s2 = 0 then
            let v = I.join_bot v1 v2 in
            Some (A_c_invalid_shift(e1,s1,v))
          else
            None
        | _ -> next a1 a2
      );
  }

let raise_c_invalid_shift_alarm ?(bottom=true) e shift range man input_flow error_flow =
  let cs = Flow.get_callstack error_flow in
  let shift' = get_orig_expr shift in
  let shift_itv = man.ask (mk_int_interval_query shift) input_flow in
  let alarm = mk_alarm (A_c_invalid_shift(e,shift',shift_itv)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice error_flow

let safe_c_shift_check range man flow =
  Flow.add_safe_check CHK_C_INVALID_SHIFT range flow

let unreachable_c_shift_check range man flow =
  Flow.add_unreachable_check CHK_C_INVALID_SHIFT range flow


(** {2 Invalid pointer comparison} *)
(** ****************************** *)

type check      += CHK_C_INVALID_POINTER_COMPARE
type alarm_kind += A_c_invalid_pointer_compare of expr (** first pointer *) *
                                                  expr (** second pointer *)

let () =
  register_check (fun next fmt -> function
      | CHK_C_INVALID_POINTER_COMPARE -> fprintf fmt "Invalid pointer comparison"
      | a -> next fmt a
    )

let () =
  register_alarm {
    check = (fun next -> function
        | A_c_invalid_pointer_compare _ -> CHK_C_INVALID_POINTER_COMPARE
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_invalid_pointer_compare(p1,p2), A_c_invalid_pointer_compare(p1',p2') ->
          Compare.pair compare_expr compare_expr (p1,p2) (p1',p2')
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_invalid_pointer_compare(p1,p2) ->
          fprintf fmt "'%a' and '%a' may point to different memory objects"
            (Debug.bold pp_expr) p1
            (Debug.bold pp_expr) p2
        | m -> next fmt m
      );
    join = (fun next -> next);
  }


let raise_c_invalid_pointer_compare ?(bottom=true) p1 p2 range man flow =
  let cs = Flow.get_callstack flow in
  let p1' = get_orig_expr p1 in
  let p2' = get_orig_expr p2 in
  let alarm = mk_alarm (A_c_invalid_pointer_compare(p1',p2')) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow




(** {2 Invalid pointer subtraction} *)
(** ******************************* *)

type check   += CHK_C_INVALID_POINTER_SUB
type alarm_kind += A_c_invalid_pointer_sub of expr (** first pointer *) *
                                              expr (** second pointer *)

let () =
  register_check (fun next fmt -> function
      | CHK_C_INVALID_POINTER_SUB -> fprintf fmt "Invalid pointer subtraction"
      | a -> next fmt a
    )

let () =
  register_alarm {
    check = (fun next -> function
        | A_c_invalid_pointer_sub _ -> CHK_C_INVALID_POINTER_SUB
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_invalid_pointer_sub(p1,p2), A_c_invalid_pointer_sub(p1',p2') ->
          Compare.pair compare_expr compare_expr (p1,p2) (p1',p2')
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_invalid_pointer_sub(p1,p2) ->
          fprintf fmt "'%a' and '%a' may point to different memory objects"
            (Debug.bold pp_expr) p1
            (Debug.bold pp_expr) p2
        | m -> next fmt m
      );
    join = (fun next -> next);
  }


let raise_c_invalid_pointer_sub ?(bottom=true) p1 p2 range man flow =
  let cs = Flow.get_callstack flow in
  let p1' = get_orig_expr p1 in
  let p2' = get_orig_expr p2 in
  let alarm = mk_alarm (A_c_invalid_pointer_sub(p1',p2')) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow



(** {2 Double free} *)
(** *************** *)

type check   += CHK_C_DOUBLE_FREE
type alarm_kind += A_c_double_free of expr (** pointer *) *
                                      range (** deallocation site *)

let () =
  register_check (fun next fmt -> function
      | CHK_C_DOUBLE_FREE -> fprintf fmt "Double free"
      | a -> next fmt a
    )

let () =
  register_alarm {
    check = (fun next -> function
        | A_c_double_free _ -> CHK_C_DOUBLE_FREE
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_double_free(p1,r1), A_c_double_free(p2,r2) ->
          Compare.pair compare_expr compare_range (p1,r1) (p2,r2)
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_double_free(p,r) ->
          fprintf fmt "'%a' points to memory already deallocated at %a"
            (Debug.bold pp_expr) p
            pp_relative_range r
        | m -> next fmt m
      );
    join = (fun next -> next);
  }


let raise_c_double_free_alarm ?(bottom=true) pointer dealloc_range ?(range=pointer.erange) man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm (A_c_double_free(pointer',untag_range dealloc_range)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow


(** {2 Insufficient variadic arguments} *)
(** *********************************** *)

type check      += CHK_C_INSUFFICIENT_VARIADIC_ARGS
type alarm_kind += A_c_insufficient_variadic_args of var (** va_list variable *) *
                                                     int_itv (** va_arg call counter *) *
                                                     int_itv (** number of passed arguments *)

let () =
  register_check (fun next fmt -> function
      | CHK_C_INSUFFICIENT_VARIADIC_ARGS -> fprintf fmt "Insufficient variadic arguments"
      | a -> next fmt a
    )

let () =
  register_alarm {
    check = (fun next -> function
        | A_c_insufficient_variadic_args _ -> CHK_C_INSUFFICIENT_VARIADIC_ARGS
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_insufficient_variadic_args(va1,c1,n1), A_c_insufficient_variadic_args(va2,c2,n2) ->
          Compare.triple compare_var compare_int_interval compare_int_interval (va1,c1,n1) (va2,c2,n2)
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_insufficient_variadic_args (va,counter,nargs) ->
          fprintf fmt "va_arg called %a time%a on va_list object '%a' with %a argument%a"
            pp_const_or_interval counter
            pp_interval_plurial counter
            pp_var va
            pp_const_or_interval nargs
            pp_interval_plurial nargs
        | a -> next fmt a
      );
    join = (fun next a1 a2 ->
        match a1,a2 with
        | A_c_insufficient_variadic_args(va1,c1,n1), A_c_insufficient_variadic_args(va2,c2,n2) ->
          (* Group messages by va_list variables and compute the
             interval of call counter and the number of passed
             arguments *)
          if compare_var va1 va2 = 0 then
            let c = I.join_bot c1 c2 in
            let n = I.join_bot n1 n2 in
            Some (A_c_insufficient_variadic_args(va1,c,n))
          else
            None
        | _ -> next a1 a2
      );
  }


let raise_c_insufficient_variadic_args ?(bottom=true) va_list counter args range man input_flow error_flow =
  let cs = Flow.get_callstack error_flow in
  let nargs = List.length args in
  let counter_itv = man.ask (mk_int_interval_query counter) input_flow in
  let nargs_itv = Bot.Nb (I.cst_int nargs) in
  let alarm = mk_alarm (A_c_insufficient_variadic_args(va_list,counter_itv,nargs_itv)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice error_flow

let safe_variadic_args_number range man flow =
  Flow.add_safe_check CHK_C_INSUFFICIENT_VARIADIC_ARGS range flow


(** {2 Insufficient format arguments} *)
(** ********************************* *)

type check      += CHK_C_INSUFFICIENT_FORMAT_ARGS
type alarm_kind += A_c_insufficient_format_args of int (** number of required arguments *) *
                                                   int (** number of given arguments *)

let () =
  register_check (fun next fmt -> function
      | CHK_C_INSUFFICIENT_FORMAT_ARGS -> fprintf fmt "Insufficient format arguments"
      | a -> next fmt a
    )

let () =
  register_alarm {
    check = (fun next -> function
        | A_c_insufficient_format_args _ -> CHK_C_INSUFFICIENT_FORMAT_ARGS
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_insufficient_format_args(r1,g1), A_c_insufficient_format_args(r2,g2) ->
          Compare.pair compare compare (r1,g1) (r2,g2)
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_insufficient_format_args(required,given) ->
          fprintf fmt "%d argument%a given while %d argument%a required"
            given Debug.plurial_int given
            required Debug.plurial_int required
        | m -> next fmt m
      );
    join = (fun next -> next);
  }


let raise_c_insufficient_format_args_alarm required given range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_c_insufficient_format_args(required,given)) cs range in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_insufficient_format_args_warning range man flow =
  Flow.add_warning_check CHK_C_INSUFFICIENT_FORMAT_ARGS range flow

let safe_c_format_args_number range man flow =
  Flow.add_safe_check CHK_C_INSUFFICIENT_FORMAT_ARGS range flow


(** {2 Invalid type of format argument} *)
(** *********************************** *)

type check      += CHK_C_INVALID_FORMAT_ARG_TYPE
type alarm_kind += A_c_invalid_format_arg_type of expr (** argument *) *
                                                  typ (** expected type *)

let () =
  register_check (fun next fmt -> function
      | CHK_C_INVALID_FORMAT_ARG_TYPE -> fprintf fmt "Invalid type of format argument"
      | a -> next fmt a
    )

let () =
  register_alarm {
    check = (fun next -> function
        | A_c_invalid_format_arg_type _ -> CHK_C_INVALID_FORMAT_ARG_TYPE
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_invalid_format_arg_type(e1,t1), A_c_invalid_format_arg_type(e2,t2) ->
          Compare.pair compare_expr compare_typ (e1,t1) (e2,t2)
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_invalid_format_arg_type(e,t) ->
          fprintf fmt "format expects argument of type '%a', but '%a' has type '%a'"
            (Debug.bold pp_typ) t
            (Debug.bold pp_expr) e
            (Debug.bold pp_typ) e.etyp
        | m -> next fmt m
      );
    join = (fun next -> next);
  }


let raise_c_invalid_format_arg_type_alarm arg typ man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_c_invalid_format_arg_type(get_orig_expr arg, typ)) cs arg.erange in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_invalid_format_arg_type_warning range man flow =
  Flow.add_warning_check CHK_C_INVALID_FORMAT_ARG_TYPE range flow

let safe_c_format_arg_type range man flow =
  Flow.add_safe_check CHK_C_INVALID_FORMAT_ARG_TYPE range flow



(** {2 Float errors} *)
(** **************** *)


type check      += CHK_C_INVALID_FLOAT_CLASS
type alarm_kind += A_c_invalid_float_class of float_itv (** float value *) *
                                              string (** expected class *)

let () =
  register_check (fun next fmt -> function
      | CHK_C_INVALID_FLOAT_CLASS -> fprintf fmt " Invalid floating-point number class"
      | a -> next fmt a
    )

let () =
  register_alarm {
    check = (fun next -> function
        | A_c_invalid_float_class _ -> CHK_C_INVALID_FLOAT_CLASS
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_invalid_float_class(f1,m1), A_c_invalid_float_class(f2,m2) ->
          Compare.compose
            [ (fun () -> compare_float_interval f1 f2);
              (fun () -> compare m1 m2)
            ]
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_invalid_float_class(f,msg) -> fprintf fmt "float '%a' may not be %s" (Debug.bold (F.fprint F.dfl_fmt)) f msg
        | a -> next fmt a
      );
    join = (fun next -> next);
  }

let raise_c_invalid_float_class_alarm ?(bottom=true) float msg range man input_flow error_flow =
  let cs = Flow.get_callstack error_flow in
  let float_itv = man.ask (mk_float_interval_query float) input_flow in
  let alarm = mk_alarm (A_c_invalid_float_class (float_itv,msg)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice error_flow


(** There are five IEEE 754 exceptions.
    We only singal include invalid operation, division by zero and 
    overflow. We don't care about inderflow (rounding to 0) and
    inexact (rounding).
 *)
type check +=
     CHK_C_FLOAT_INVALID_OPERATION
   | CHK_C_FLOAT_DIVISION_BY_ZERO
   | CHK_C_FLOAT_OVERFLOW

let () =
  register_check (fun next fmt -> function
      | CHK_C_FLOAT_INVALID_OPERATION -> fprintf fmt "Invalid floating-point operation"
      | CHK_C_FLOAT_DIVISION_BY_ZERO -> fprintf fmt "Floating-point division by zero"
      | CHK_C_FLOAT_OVERFLOW -> fprintf fmt "Floating-point overflow"
      | a -> next fmt a
    )


type alarm_kind +=
   | A_c_float_invalid_operation of expr (** expression *) * float_itv  (** result interval *) * typ (** destination type *)
   | A_c_float_division_by_zero of expr (** denominator *) * float_itv (** denominator interval *)
   | A_c_float_overflow of expr (** expression *) * float_itv  (** result interval *) * typ (** type *)

let () =
  register_alarm {
    check = (fun next -> function
        | A_c_float_invalid_operation _ -> CHK_C_FLOAT_INVALID_OPERATION
        | A_c_float_division_by_zero _ -> CHK_C_FLOAT_DIVISION_BY_ZERO
        | A_c_float_overflow _ -> CHK_C_FLOAT_OVERFLOW
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_float_invalid_operation (e1,i1,t1),
          A_c_float_invalid_operation (e2,i2,t2) ->
           Compare.triple compare_expr compare compare_typ (e1,i1,t1) (e2,i2,t2)
        | A_c_float_division_by_zero (e1,i1),
          A_c_float_division_by_zero (e2,i2) ->
           Compare.pair compare_expr compare (e1,i1) (e2,i2)
        | A_c_float_overflow (e1,i1,t1),
          A_c_float_overflow (e2,i2,t2) ->
           Compare.triple compare_expr compare compare_typ (e1,i1,t1) (e2,i2,t2)
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_float_invalid_operation (e,i,t) ->
           fprintf fmt "invalid floating-point operations in expression '%a' with range '%a' and type '%a'"
             (Debug.bold pp_expr) e
             (Debug.bold (F.fprint F.dfl_fmt)) i
             (Debug.bold pp_typ) t
        | A_c_float_division_by_zero (e,i) ->
           fprintf fmt "floating-point division by zero on denominator '%a' with range '%a'"
             (Debug.bold pp_expr) e
             (Debug.bold (F.fprint F.dfl_fmt)) i 
        | A_c_float_overflow (e,i,t) ->
           fprintf fmt "floating-point overflow in expression '%a' with range '%a' and type '%a'"
             (Debug.bold pp_expr) e
             (Debug.bold (F.fprint F.dfl_fmt)) i
             (Debug.bold pp_typ) t
        | a -> next fmt a
      );
    join = (fun next -> next);
  }


let raise_c_float_invalid_operation_alarm ?(bottom=false) ?(warning=true) exp itv typ range man flow =
  let cs = Flow.get_callstack flow in
  let exp' = get_orig_expr exp in
  let alarm = mk_alarm (A_c_float_invalid_operation(exp',itv,typ)) cs range in
  Flow.raise_alarm alarm ~bottom ~warning man.lattice flow

let raise_c_float_division_by_zero_alarm ?(bottom=true) ?(warning=false) denominator itv range man flow =
  let cs = Flow.get_callstack flow in
  let denominator' = get_orig_expr denominator in
  let alarm = mk_alarm (A_c_float_division_by_zero(denominator',itv)) cs range in
  Flow.raise_alarm alarm ~bottom ~warning man.lattice flow

let raise_c_float_overflow_alarm ?(bottom=false) ?(warning=true) exp itv t range man flow =
  let cs = Flow.get_callstack flow in
  let exp' = get_orig_expr exp in
  let alarm = mk_alarm (A_c_float_overflow(exp',itv,t)) cs range in
  Flow.raise_alarm alarm ~bottom ~warning man.lattice flow


let safe_c_float_invalid_operation_check range man flow =
  Flow.add_safe_check CHK_C_FLOAT_INVALID_OPERATION range flow

let safe_c_float_division_by_zero_check range man flow =
  Flow.add_safe_check CHK_C_FLOAT_DIVISION_BY_ZERO range flow

let safe_c_float_overflow_check range man flow =
  Flow.add_safe_check CHK_C_FLOAT_OVERFLOW range flow
