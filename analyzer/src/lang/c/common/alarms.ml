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
  | AIllegalPointerOrder
  | AInvalidBitShift
  | AUseAfterFree
  | ADoubleFree
  | AVaArgNoNext
  | AReadOnlyModification


type alarm_detail +=
  | DOutOfBound of base (** accessed base *) * int_itv (** offset interval *) * int_itv (** base_size *)
  | DNullDeref of expr (** dereferenced pointer *)
  | DUseAfterFree of expr (** dereferenced pointer *)
  | DIntegerOverflow of expr (** evaluated expression *) * int_itv (** expression value *) * typ (** integer type *)


let raise_c_alarm a range ?(bottom=false) lattice flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm a range ~cs in
  Flow.raise_alarm alarm ~bottom lattice flow


let raise_c_out_bound_alarm ~base ~offset ~size range man flow =
  let cs = Flow.get_callstack flow in
  let offset_itv = man.ask (mk_int_interval_query offset) flow in
  let size_itv = man.ask (mk_int_interval_query size) flow in
  let alarm = mk_alarm AOutOfBound ~detail:(DOutOfBound(base,offset_itv,size_itv)) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow


let raise_c_null_deref_alarm ~pointer range man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm ANullDeref ~detail:(DNullDeref(pointer')) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow


let raise_c_use_after_free_alarm ~pointer range man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm AUseAfterFree ~detail:(DUseAfterFree(pointer')) range ~cs in
  Flow.raise_alarm alarm ~bottom:true man.lattice flow


let raise_c_integer_overflow_alarm exp typ range man flow =
  let cs = Flow.get_callstack flow in
  let exp' = get_orig_expr exp in
  let itv = man.ask (mk_int_interval_query exp) flow in
  let alarm = mk_alarm AIntegerOverflow ~detail:(DIntegerOverflow(exp',itv,typ)) range ~cs in
  Flow.raise_alarm alarm ~bottom:false man.lattice flow



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
          | AIllegalPointerOrder -> Format.fprintf fmt "Illegal pointer comparison"
          | AUseAfterFree -> Format.fprintf fmt "Use of after free"
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

        | DNullDeref(p1), DNullDeref(p2)
        | DUseAfterFree(p1), DUseAfterFree(p2) ->
          compare_expr p1 p2

        | DIntegerOverflow(e1,v1,t1), DIntegerOverflow(e2,v2,t2) ->
          Compare.compose [
            (fun () -> compare_expr e1 e2);
            (fun () -> compare_int_interval v1 v2);
            (fun () -> compare_typ t1 t2);
          ]

        | _ -> next a1 a2
      );
    print = (fun next fmt a ->
        match a with
        | DOutOfBound(base, offset, size) ->
          let pp_const_or_interval fmt itv =
            match itv with
            | Bot.Nb (l,u) when I.B.eq l u -> I.B.fprint fmt l
            | _ -> I.fprint_bot fmt itv
          in
          Format.fprintf fmt "offset %a is out of boundaries of %a which has size %a"
            pp_const_or_interval offset
            pp_base base
            pp_const_or_interval size

        | DNullDeref(pointer) ->
          Format.fprintf fmt "%a points to a NULL pointer" pp_expr pointer

        | DUseAfterFree(pointer) ->
          Format.fprintf fmt "%a points to a dealloacted memory block" pp_expr pointer

        | DIntegerOverflow(e,v,t) ->
          let l,u = Ast.rangeof t in
          begin match v with
            | Bot.Nb (a,b) when I.B.eq a b ->
              Format.fprintf fmt "%a has value %a ∉ range(%a) = [%a,%a]"
                pp_expr e
                I.B.fprint a
                pp_typ t
                Z.pp_print l
                Z.pp_print u
            | _ ->
              Format.fprintf fmt "%a has value %a ⊈ range(%a) = [%a,%a]"
                pp_expr e
                I.fprint_bot v
                pp_typ t
                Z.pp_print l
                Z.pp_print u
          end

        | _ -> next fmt a
      );
  }
