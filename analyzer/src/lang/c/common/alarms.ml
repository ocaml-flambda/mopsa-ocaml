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
open Framework.Core.Sig.Stacked.Manager
open Universal.Numeric.Common
open Base


type alarm_class +=
  | A_c_out_of_bound_cls
  | A_c_null_deref_cls
  | A_c_invalid_deref_cls
  | A_c_divide_by_zero_cls
  | A_c_integer_overflow_cls
  | A_c_illegal_pointer_diff_cls
  | A_c_illegal_pointer_compare_cls
  | A_c_invalid_bit_shift_cls
  | A_c_use_after_free_cls
  | A_c_double_free_cls
  | A_c_no_next_va_arg_cls
  | A_c_read_only_modification_cls
  | A_c_insufficient_format_args_cls


type alarm_body +=
  | A_c_out_of_bound of base (** accessed base *) * int_itv (** offset interval *) * int_itv (** base_size *)
  | A_c_null_deref of expr (** pointer *)
  | A_c_invalid_deref of expr (** pointer *)
  | A_c_use_after_free of expr (** pointer *) * range (** deallocation site *)
  | A_c_integer_overflow of expr (** evaluated expression *) * int_itv (** expression value *) * typ (** integer type *)
  | A_c_no_next_va_arg of var (** va_list variable *) * int_itv (** index *) * int (** number of unnamed arguments *)
  | A_c_double_free of expr (** pointer *) * range (** deallocation site *)
  | A_c_read_only_modification of base (** modified base *)
  | A_c_illegal_pointer_diff of expr (** first pointer *) * expr (** second pointer *)
  | A_c_illegal_pointer_compare of expr (** first pointer *) * expr (** second pointer *)
  | A_c_divide_by_zero of expr (** denominator *)
  | A_c_invalid_bit_shift of expr (** shift expression *) * int_itv (** shift value *) * typ (** shifted type *)
  | A_c_insufficient_format_args of int (** number of required arguments *) * int (** number of given arguments *)


let raise_c_out_bound_alarm ~base ~offset ~size range man flow =
  let cs = Flow.get_callstack flow in
  let offset_itv = man.ask (mk_int_interval_query offset) flow in
  let size_itv = man.ask (mk_int_interval_query size) flow in
  let alarm = mk_alarm (A_c_out_of_bound(base,offset_itv,size_itv)) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_out_bound_quantified_alarm ~base ~min ~max ~size range man flow =
  let cs = Flow.get_callstack flow in
  let min_itv = man.ask (mk_int_interval_query min) flow in
  let max_itv = man.ask (mk_int_interval_query max) flow in
  let offset_itv = I.join_bot min_itv max_itv in
  let size_itv = man.ask (mk_int_interval_query size) flow in
  let alarm = mk_alarm (A_c_out_of_bound(base,offset_itv,size_itv)) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_null_deref_alarm pointer range man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm (A_c_null_deref(pointer')) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_invalid_deref_alarm pointer range man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm (A_c_invalid_deref(pointer')) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_use_after_free_alarm pointer dealloc_range range man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm (A_c_use_after_free(pointer',dealloc_range)) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_double_free_alarm pointer dealloc_range range man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm (A_c_double_free(pointer',dealloc_range)) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_integer_overflow_alarm exp typ range man flow =
  let cs = Flow.get_callstack flow in
  let exp' = get_orig_expr exp in
  let itv = man.ask (mk_int_interval_query exp) flow in
  let alarm = mk_alarm (A_c_integer_overflow(exp',itv,typ)) range ~cs in
  Flow.raise_alarm alarm ~bottom:false man.lattice flow

let raise_c_no_next_va_arg ~va_list ~counter ~args range man flow =
  let cs = Flow.get_callstack flow in
  let counter_itv = man.ask (mk_int_interval_query counter) flow in
  let nargs = List.length args in
  let alarm = mk_alarm (A_c_no_next_va_arg(va_list,counter_itv,nargs)) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_read_only_modification_alarm base range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_c_read_only_modification(base)) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_illegal_pointer_diff p1 p2 range man flow =
  let cs = Flow.get_callstack flow in
  let p1' = get_orig_expr p1 in
  let p2' = get_orig_expr p2 in
  let alarm = mk_alarm (A_c_illegal_pointer_diff(p1',p2')) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_illegal_pointer_compare p1 p2 range man flow =
  let cs = Flow.get_callstack flow in
  let p1' = get_orig_expr p1 in
  let p2' = get_orig_expr p2 in
  let alarm = mk_alarm (A_c_illegal_pointer_compare(p1',p2')) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_divide_by_zero_alarm denominator range man flow =
  let cs = Flow.get_callstack flow in
  let denominator' = get_orig_expr denominator in
  let alarm = mk_alarm (A_c_divide_by_zero(denominator')) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_invalid_bit_shift_alarm shift typ range man flow =
  let cs = Flow.get_callstack flow in
  let shift' = get_orig_expr shift in
  let shift_itv = man.ask (mk_int_interval_query shift) flow in
  let alarm = mk_alarm (A_c_invalid_bit_shift(shift',shift_itv,typ)) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow


let raise_c_insufficient_format_args_alarm required given range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_c_insufficient_format_args(required,given)) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow
  

let () =
  register_alarm_class (fun default fmt a ->
      match a with
      | A_c_out_of_bound_cls -> Format.fprintf fmt "Out of bound access"
      | A_c_null_deref_cls -> Format.fprintf fmt "Null pointer dereference"
      | A_c_invalid_deref_cls -> Format.fprintf fmt "Invalid pointer dereference"
      | A_c_divide_by_zero_cls -> Format.fprintf fmt "Division by zero"
      | A_c_integer_overflow_cls -> Format.fprintf fmt "Integer overflow"
      | A_c_illegal_pointer_diff_cls -> Format.fprintf fmt "Illegal pointer difference"
      | A_c_illegal_pointer_compare_cls -> Format.fprintf fmt "Illegal pointer comparison"
      | A_c_use_after_free_cls -> Format.fprintf fmt "Use after free"
      | A_c_double_free_cls -> Format.fprintf fmt "Double free"
      | A_c_no_next_va_arg_cls -> Format.fprintf fmt "No next argument for va_arg"
      | A_c_invalid_bit_shift_cls -> Format.fprintf fmt "Invald bit-shift"
      | A_c_read_only_modification_cls -> Format.fprintf fmt "Modification of a readonly memory"
      | A_c_insufficient_format_args_cls -> Format.fprintf fmt "Inufficient number of format arguments"
      | _ -> default fmt a
    )

let () =
  register_alarm_body {
    classifier = (fun next a ->
        match a with
        | A_c_out_of_bound _ -> A_c_out_of_bound_cls
        | A_c_null_deref _ -> A_c_null_deref_cls
        | A_c_invalid_deref _ -> A_c_invalid_deref_cls
        | A_c_divide_by_zero _ -> A_c_divide_by_zero_cls
        | A_c_integer_overflow _ -> A_c_integer_overflow_cls
        | A_c_illegal_pointer_diff _ -> A_c_illegal_pointer_diff_cls
        | A_c_illegal_pointer_compare _ -> A_c_illegal_pointer_compare_cls
        | A_c_use_after_free _ -> A_c_use_after_free_cls
        | A_c_double_free _ -> A_c_double_free_cls
        | A_c_no_next_va_arg _ -> A_c_no_next_va_arg_cls
        | A_c_invalid_bit_shift _ -> A_c_invalid_bit_shift_cls
        | A_c_read_only_modification _ -> A_c_read_only_modification_cls
        | A_c_insufficient_format_args _ -> A_c_insufficient_format_args_cls
        | _ -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_out_of_bound(b1,o1,s1), A_c_out_of_bound(b2,o2,s2) ->
          Compare.compose [
            (fun () -> compare_base b1 b2);
            (fun () -> compare_int_interval o1 o2);
            (fun () -> compare_int_interval s1 s2);
          ]

        | A_c_null_deref(p1), A_c_null_deref(p2) -> compare_expr p1 p2

        | A_c_invalid_deref(p1), A_c_invalid_deref(p2) -> compare_expr p1 p2

        | A_c_use_after_free(p1,r1), A_c_use_after_free(p2,r2) ->
          Compare.compose [
            (fun () -> compare_expr p1 p2);
            (fun () -> compare_range r1 r2);
          ]

        | A_c_double_free(p1,r1), A_c_double_free(p2,r2) ->
          Compare.compose [
            (fun () -> compare_expr p1 p2);
            (fun () -> compare_range r1 r2);
          ]

        | A_c_integer_overflow(e1,v1,t1), A_c_integer_overflow(e2,v2,t2) ->
          Compare.compose [
            (fun () -> compare_expr e1 e2);
            (fun () -> compare_int_interval v1 v2);
            (fun () -> compare_typ t1 t2);
          ]

        | A_c_no_next_va_arg(v1,i1,n1), A_c_no_next_va_arg(v2,i2,n2) ->
          Compare.compose [
            (fun () -> compare_var v1 v2);
            (fun () -> compare_int_interval i1 i2);
            (fun () -> compare n1 n2);
          ]

        | A_c_read_only_modification(b1), A_c_read_only_modification(b2) ->
          compare_base b1 b2

        | A_c_illegal_pointer_diff(p1,q1), A_c_illegal_pointer_diff(p2,q2) ->
          Compare.compose [
            (fun () -> compare_expr p1 p2);
            (fun () -> compare_expr q1 q2);
          ]

        | A_c_illegal_pointer_compare(p1,q1), A_c_illegal_pointer_compare(p2,q2) ->
          Compare.compose [
            (fun () -> compare_expr p1 p2);
            (fun () -> compare_expr q1 q2);
          ]

        | A_c_divide_by_zero(e1), A_c_divide_by_zero(e2) ->
          compare_expr e1 e2

        | A_c_invalid_bit_shift(e1,i1,t1), A_c_invalid_bit_shift(e2,i2,t2) ->
          Compare.compose [
            (fun () -> compare_expr e1 e2);
            (fun () -> compare_int_interval i1 i2);
            (fun () -> compare_typ t1 t2);
          ]

        | A_c_insufficient_format_args(r1,g1), A_c_insufficient_format_args(r2,g2) ->
          Compare.pair compare compare (r1,g1) (r2,g2)

        | _ -> next a1 a2
      );
    print = (fun next fmt a ->
        let pp_const_or_interval fmt itv =
          match itv with
          | Bot.Nb (l,u) when I.B.eq l u -> I.B.fprint fmt l
          | _ -> I.fprint_bot fmt itv
        in
        let pp_const_or_interval_not_eq fmt itv =
          match itv with
          | Bot.Nb (l,u) when I.B.eq l u -> Format.fprintf fmt "%a ∉" I.B.fprint l
          | _ -> Format.fprintf fmt "%a ⊈" I.fprint_bot itv
        in
        match a with
        | A_c_out_of_bound(base, offset, size) ->
          Format.fprintf fmt "offset %a is out of boundaries of %a which has size %a"
            pp_const_or_interval offset
            pp_base base
            pp_const_or_interval size

        | A_c_null_deref(pointer) ->
          Format.fprintf fmt "%a points to a NULL pointer" pp_expr pointer

        | A_c_invalid_deref(pointer) ->
          Format.fprintf fmt "%a points to an invalid pointer" pp_expr pointer

        | A_c_use_after_free(pointer,r) ->
          Format.fprintf fmt "%a points to invalid memory deallocated at %a" pp_expr pointer pp_range r

        | A_c_double_free(pointer,r) ->
          Format.fprintf fmt "memory pointed by %a already deallocated at %a" pp_expr pointer pp_range r

        | A_c_integer_overflow(e,v,t) ->
          let l,u = Ast.rangeof t in
          Format.fprintf fmt "%a has value %a range(%a) = [%a,%a]"
            pp_expr e
            pp_const_or_interval_not_eq v
            pp_typ t
            Z.pp_print l
            Z.pp_print u

        | A_c_no_next_va_arg(v,i,n) ->
          Format.fprintf fmt "va_list iterator %a has counter %a which greater than the number of unnamed arguments %d"
            pp_var v
            pp_const_or_interval i
            n

        | A_c_read_only_modification(b) ->
          Format.fprintf fmt "%a can not be modified" pp_base b

        | A_c_illegal_pointer_compare(p,q) ->
          Format.fprintf fmt "%a and %a point to different memory blocks"
            pp_expr p
            pp_expr q

        | A_c_illegal_pointer_diff(p,q) ->
          Format.fprintf fmt "%a and %a point to different memory blocks"
            pp_expr p
            pp_expr q

        | A_c_divide_by_zero(e) ->
          Format.fprintf fmt "denominator %a may be null" pp_expr e

        | A_c_invalid_bit_shift(e,i,t) ->
          let bits = Ast.sizeof_type t |> Z.mul (Z.of_int 8) in
          Format.fprintf fmt "shift position %a = %a [0, bits(%a) - 1] = [0, %a]"
            pp_expr e
            pp_const_or_interval_not_eq i
            pp_typ t
            Z.pp_print (Z.pred bits)

        | A_c_insufficient_format_args(required,given) ->
          Format.fprintf fmt "%d argument%a given while %d argument%a required"
            given Debug.plurial_int given
            required Debug.plurial_int required

        | _ -> next fmt a
      );
  }
