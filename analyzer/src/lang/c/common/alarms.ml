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


type alarm_category +=
  | AOutOfBound
  | ANullDeref
  | AInvalidDeref
  | ADivideByZero
  | AIntegerOverflow
  | AIllegalPointerDiff
  | AIllegalPointerCompare
  | AInvalidBitShift
  | AUseAfterFree
  | ADoubleFree
  | AVaArgNoNext
  | AReadOnlyModification


type alarm_detail +=
  | DOutOfBound of base (** accessed base *) * int_itv (** offset interval *) * int_itv (** base_size *)
  | DNullDeref of expr (** pointer *)
  | DInvalidDeref of expr (** pointer *)
  | DUseAfterFree of expr (** pointer *) * range (** deallocation site *)
  | DIntegerOverflow of expr (** evaluated expression *) * int_itv (** expression value *) * typ (** integer type *)
  | DVaArgNoNext of var (** va_list variable *) * int_itv (** index *) * int (** number of unnamed arguments *)
  | DDoubleFree of expr (** pointer *) * range (** deallocation site *)
  | DReadOnlyModification of base (** modified base *)
  | DIllegalPointerDiff of expr (** first pointer *) * expr (** second pointer *)
  | DIllegalPointerCompare of expr (** first pointer *) * expr (** second pointer *)
  | DDivideByZero of expr (** denominator *)
  | DInvalidBitShift of expr (** shift expression *) * int_itv (** shift value *) * typ (** shifted type *)


let raise_c_out_bound_alarm ~base ~offset ~size range man flow =
  let cs = Flow.get_callstack flow in
  let offset_itv = man.ask (mk_int_interval_query offset) flow in
  let size_itv = man.ask (mk_int_interval_query size) flow in
  let alarm = mk_alarm AOutOfBound (DOutOfBound(base,offset_itv,size_itv)) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_null_deref_alarm pointer range man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm ANullDeref (DNullDeref(pointer')) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_invalid_deref_alarm pointer range man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm AInvalidDeref (DInvalidDeref(pointer')) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_use_after_free_alarm pointer dealloc_range range man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm AUseAfterFree (DUseAfterFree(pointer',dealloc_range)) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_double_free_alarm pointer dealloc_range range man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm ADoubleFree (DDoubleFree(pointer',dealloc_range)) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_integer_overflow_alarm exp typ range man flow =
  let cs = Flow.get_callstack flow in
  let exp' = get_orig_expr exp in
  let itv = man.ask (mk_int_interval_query exp) flow in
  let alarm = mk_alarm AIntegerOverflow (DIntegerOverflow(exp',itv,typ)) range ~cs in
  Flow.raise_alarm alarm ~bottom:false man.lattice flow

let raise_c_no_next_va_arg ~va_list ~counter ~args range man flow =
  let cs = Flow.get_callstack flow in
  let counter_itv = man.ask (mk_int_interval_query counter) flow in
  let nargs = List.length args in
  let alarm = mk_alarm AVaArgNoNext (DVaArgNoNext(va_list,counter_itv,nargs)) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_read_only_modification_alarm base range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm AReadOnlyModification (DReadOnlyModification(base)) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_illegal_pointer_diff p1 p2 range man flow =
  let cs = Flow.get_callstack flow in
  let p1' = get_orig_expr p1 in
  let p2' = get_orig_expr p2 in
  let alarm = mk_alarm AIllegalPointerDiff (DIllegalPointerDiff(p1',p2')) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_illegal_pointer_compare p1 p2 range man flow =
  let cs = Flow.get_callstack flow in
  let p1' = get_orig_expr p1 in
  let p2' = get_orig_expr p2 in
  let alarm = mk_alarm AIllegalPointerCompare (DIllegalPointerCompare(p1',p2')) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_divide_by_zero_alarm denominator range man flow =
  let cs = Flow.get_callstack flow in
  let denominator' = get_orig_expr denominator in
  let alarm = mk_alarm ADivideByZero (DDivideByZero(denominator')) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow

let raise_c_invalid_bit_shift_alarm shift typ range man flow =
  let cs = Flow.get_callstack flow in
  let shift' = get_orig_expr shift in
  let shift_itv = man.ask (mk_int_interval_query shift) flow in
  let alarm = mk_alarm AInvalidBitShift (DInvalidBitShift(shift',shift_itv,typ)) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow


let () =
  register_alarm_category {
      compare = (fun default a b -> default a b);
      print = (fun default fmt a ->
          match a with
          | AOutOfBound -> Format.fprintf fmt "Out of bound access"
          | ANullDeref -> Format.fprintf fmt "Null pointer dereference"
          | AInvalidDeref -> Format.fprintf fmt "Invalid pointer dereference"
          | ADivideByZero -> Format.fprintf fmt "Division by zero"
          | AIntegerOverflow -> Format.fprintf fmt "Integer overflow"
          | AIllegalPointerDiff -> Format.fprintf fmt "Illegal pointer difference"
          | AIllegalPointerCompare -> Format.fprintf fmt "Illegal pointer comparison"
          | AUseAfterFree -> Format.fprintf fmt "Use after free"
          | ADoubleFree -> Format.fprintf fmt "Double free"
          | AVaArgNoNext -> Format.fprintf fmt "No next argument for va_arg"
          | AInvalidBitShift -> Format.fprintf fmt "Invald bit-shift"
          | AReadOnlyModification -> Format.fprintf fmt "Modification of a readonly memory"
          | _ -> default fmt a
        );
    }

let () =
  register_alarm_detail {
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | DOutOfBound(b1,o1,s1), DOutOfBound(b2,o2,s2) ->
          Compare.compose [
            (fun () -> compare_base b1 b2);
            (fun () -> compare_int_interval o1 o2);
            (fun () -> compare_int_interval s1 s2);
          ]

        | DNullDeref(p1), DNullDeref(p2) -> compare_expr p1 p2

        | DUseAfterFree(p1,r1), DUseAfterFree(p2,r2) ->
          Compare.compose [
            (fun () -> compare_expr p1 p2);
            (fun () -> compare_range r1 r2);
          ]

        | DDoubleFree(p1,r1), DDoubleFree(p2,r2) ->
          Compare.compose [
            (fun () -> compare_expr p1 p2);
            (fun () -> compare_range r1 r2);
          ]

        | DIntegerOverflow(e1,v1,t1), DIntegerOverflow(e2,v2,t2) ->
          Compare.compose [
            (fun () -> compare_expr e1 e2);
            (fun () -> compare_int_interval v1 v2);
            (fun () -> compare_typ t1 t2);
          ]

        | DVaArgNoNext(v1,i1,n1), DVaArgNoNext(v2,i2,n2) ->
          Compare.compose [
            (fun () -> compare_var v1 v2);
            (fun () -> compare_int_interval i1 i2);
            (fun () -> compare n1 n2);
          ]

        | DReadOnlyModification(b1), DReadOnlyModification(b2) ->
          compare_base b1 b2

        | DIllegalPointerDiff(p1,q1), DIllegalPointerDiff(p2,q2) ->
          Compare.compose [
            (fun () -> compare_expr p1 p2);
            (fun () -> compare_expr q1 q2);
          ]

        | DIllegalPointerCompare(p1,q1), DIllegalPointerCompare(p2,q2) ->
          Compare.compose [
            (fun () -> compare_expr p1 p2);
            (fun () -> compare_expr q1 q2);
          ]

        | DDivideByZero(e1), DDivideByZero(e2) ->
          compare_expr e1 e2

        | DInvalidBitShift(e1,i1,t1), DInvalidBitShift(e2,i2,t2) ->
          Compare.compose [
            (fun () -> compare_expr e1 e2);
            (fun () -> compare_int_interval i1 i2);
            (fun () -> compare_typ t1 t2);
          ]

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
        | DOutOfBound(base, offset, size) ->
          Format.fprintf fmt "offset %a is out of boundaries of %a which has size %a"
            pp_const_or_interval offset
            pp_base base
            pp_const_or_interval size

        | DNullDeref(pointer) ->
          Format.fprintf fmt "%a points to a NULL pointer" pp_expr pointer

        | DUseAfterFree(pointer,r) ->
          Format.fprintf fmt "%a points to invalid memory deallocated at %a" pp_expr pointer pp_range r

        | DDoubleFree(pointer,r) ->
          Format.fprintf fmt "memory pointed by %a already deallocated at %a" pp_expr pointer pp_range r

        | DIntegerOverflow(e,v,t) ->
          let l,u = Ast.rangeof t in
          Format.fprintf fmt "%a has value %a range(%a) = [%a,%a]"
            pp_expr e
            pp_const_or_interval_not_eq v
            pp_typ t
            Z.pp_print l
            Z.pp_print u

        | DVaArgNoNext(v,i,n) ->
          Format.fprintf fmt "va_list iterator %a has counter %a which greater than the number of unnamed arguments %d"
            pp_var v
            pp_const_or_interval i
            n

        | DReadOnlyModification(b) ->
          Format.fprintf fmt "%a can not be modified" pp_base b

        | DIllegalPointerCompare(p,q) ->
          Format.fprintf fmt "%a and %a point to different memory blocks"
            pp_expr p
            pp_expr q

        | DIllegalPointerDiff(p,q) ->
          Format.fprintf fmt "%a and %a point to different memory blocks"
            pp_expr p
            pp_expr q

        | DDivideByZero(e) ->
          Format.fprintf fmt "denominator %a may be null" pp_expr e

        | DInvalidBitShift(e,i,t) ->
          let bits = Ast.sizeof_type t |> Z.mul (Z.of_int 8) in
          Format.fprintf fmt "shift position %a = %a [0, bits(%a) - 1] = [0, %a]"
            pp_expr e
            pp_const_or_interval_not_eq i
            pp_typ t
            Z.pp_print (Z.pred bits)

        | _ -> next fmt a
      );
  }
