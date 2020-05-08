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
open Framework.Core.Sig.Stacked.Manager
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


(** {2 NULL pointer dereference} *)
(** **************************** *)

type alarm_class   += A_c_null_deref
type alarm_message += A_c_null_deref_msg of expr
type alarm_message += A_c_null_deref_wo_info_msg

let () =
  register_alarm_class (fun next fmt -> function
      | A_c_null_deref -> fprintf fmt "NULL pointer dereference"
      | a -> next fmt a
    )

let () =
  register_alarm_message {
    classifier = (fun next -> function
        | A_c_null_deref_msg _ -> A_c_null_deref
        | A_c_null_deref_wo_info_msg -> A_c_null_deref
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_null_deref_msg(p1), A_c_null_deref_msg(p2) -> compare_expr p1 p2
        | A_c_null_deref_wo_info_msg, A_c_null_deref_wo_info_msg -> 0
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_null_deref_msg(pointer) -> fprintf fmt "pointer '%a' may be null" (Debug.bold pp_expr) pointer
        | A_c_null_deref_wo_info_msg -> ()
        | a -> next fmt a
      );
  }

let raise_c_null_deref_alarm ?(bottom=true) pointer ?(range=pointer.erange) man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm (A_c_null_deref_msg(pointer')) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow


let raise_c_null_deref_wo_info_alarm ?(bottom=true) range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm A_c_null_deref_wo_info_msg cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow


(** {2 Invalid pointer dereference} *)
(** ******************************* *)

type alarm_class   += A_c_invalid_deref
type alarm_message += A_c_invalid_deref_msg of expr
type alarm_message += A_c_invalid_deref_wo_info_msg

let () =
  register_alarm_class (fun next fmt -> function
      | A_c_invalid_deref -> fprintf fmt "Invalid pointer dereference"
      | a -> next fmt a
    )

let () =
  register_alarm_message {
    classifier = (fun next -> function
        | A_c_invalid_deref_msg _ -> A_c_invalid_deref
        | A_c_invalid_deref_wo_info_msg -> A_c_invalid_deref
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_invalid_deref_msg(p1), A_c_invalid_deref_msg(p2) -> compare_expr p1 p2
        | A_c_invalid_deref_wo_info_msg, A_c_invalid_deref_wo_info_msg -> 0
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_invalid_deref_msg(pointer) -> fprintf fmt "pointer '%a' may be invalid" (Debug.bold pp_expr) pointer
        | A_c_invalid_deref_wo_info_msg -> ()
        | a -> next fmt a
      );
  }

let raise_c_invalid_deref_alarm ?(bottom=true) pointer ?(range=pointer.erange) man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm (A_c_invalid_deref_msg(pointer')) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow


let raise_c_invalid_deref_wo_info_alarm ?(bottom=true) range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm A_c_invalid_deref_wo_info_msg cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow



(** {2 Out-of-bound access} *)
(** *********************** *)

type alarm_class   += A_c_out_of_bound
type alarm_message += A_c_out_of_bound_msg of base (** accessed base *) *
                                              int_itv (** base size *) *
                                              int_itv (** offset *) *
                                              typ (** accessed type *)
type alarm_message += A_c_out_of_bound_wo_info_msg

let () =
  register_alarm_class (fun next fmt -> function
      | A_c_out_of_bound -> fprintf fmt "Out of bound access"
      | a -> next fmt a
    )

let () =
  register_grouped_alarm_message {
    classifier = (fun next -> function
        | A_c_out_of_bound_msg _ -> A_c_out_of_bound
        | A_c_out_of_bound_wo_info_msg -> A_c_out_of_bound
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_out_of_bound_msg(b1,s1,o1,t1), A_c_out_of_bound_msg(b2,s2,o2,t2) ->
          Compare.compose
            [ (fun () -> compare_base b1 b2);
              (fun () -> compare_int_interval s1 s2);
              (fun () -> compare_int_interval o1 o2);
              (fun () -> compare_typ t1 t2); ]

        | A_c_out_of_bound_wo_info_msg, A_c_out_of_bound_wo_info_msg -> 0

        | _ -> next a1 a2
      );
    print = (fun next fmt messages -> function
        | A_c_out_of_bound ->
          (* Group alarms by base *)
          let m = List.fold_left
              (fun acc -> function
                 | A_c_out_of_bound_msg(b,s,o,t) ->
                   let (olds,oldo,olde) = try BaseMap.find b acc with Not_found -> (Bot.BOT,Bot.BOT,Bot.BOT) in
                   let s = I.join_bot olds s in
                   let o = I.join_bot oldo o in
                   let e = I.join_bot olde (Bot.Nb (I.cst (sizeof_type (void_to_char t)))) in
                   BaseMap.add b (s,o,e) acc
                 | A_c_out_of_bound_wo_info_msg -> acc
                 | _ -> assert false
              ) BaseMap.empty messages
          in
          pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
            (fun fmt (base,(size,offset,elm)) ->
               fprintf fmt "accessing %a byte%a at offset%a %a of %a of size %a byte%a"
                 pp_const_or_interval elm
                 pp_interval_plurial elm
                 pp_interval_cardinal_plurial offset
                 pp_const_or_interval offset
                 pp_base_verbose base
                 pp_const_or_interval size
                 pp_interval_plurial size
            ) fmt (BaseMap.bindings m)

        | cls -> next fmt messages cls
      );
  }


let raise_c_out_var_bound_alarm ?(bottom=true) var offset typ range man input_flow error_flow =
  let cs = Flow.get_callstack error_flow in
  let offset_itv = man.ask (mk_int_interval_query offset) input_flow in
  let size_itv = sizeof_type var.vtyp |> I.cst in
  let alarm = mk_alarm (A_c_out_of_bound_msg(mk_var_base var,Bot.Nb size_itv, offset_itv, typ)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice error_flow

let raise_c_out_addr_bound_alarm ?(bottom=true) addr size offset typ range man input_flow error_flow =
  let cs = Flow.get_callstack error_flow in
  let offset_itv = man.ask (mk_int_interval_query offset) input_flow in
  let size_itv = man.ask (mk_int_interval_query size) input_flow in
  let alarm = mk_alarm (A_c_out_of_bound_msg(mk_addr_base addr, size_itv, offset_itv, typ)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice error_flow

let raise_c_out_bound_alarm ?(bottom=true) base size offset typ range man input_flow error_flow =
  let cs = Flow.get_callstack error_flow in
  let offset_itv = man.ask (mk_int_interval_query offset) input_flow in
  let size_itv = man.ask (mk_int_interval_query size) input_flow in
  let alarm = mk_alarm (A_c_out_of_bound_msg(base, size_itv, offset_itv, typ)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice error_flow


let raise_c_quantified_out_bound_alarm ?(bottom=true) base size min max typ range man input_flow error_flow =
  let cs = Flow.get_callstack error_flow in
  let min_itv = man.ask (mk_int_interval_query min) input_flow in
  let max_itv = man.ask (mk_int_interval_query max) input_flow in
  let offset_itv = I.join_bot min_itv max_itv in
  let size_itv = man.ask (mk_int_interval_query size) input_flow in
  let alarm = mk_alarm (A_c_out_of_bound_msg(base,size_itv, offset_itv, typ)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice error_flow

let raise_c_out_bound_wo_info_alarm ?(bottom=true) range  man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm A_c_out_of_bound_wo_info_msg cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow


(** {2 Division by zero} *)
(** ******************** *)

type alarm_class   += A_c_divide_by_zero
type alarm_message += A_c_divide_by_zero_msg of expr (** denominator *)

let () =
  register_alarm_class (fun next fmt -> function
      | A_c_divide_by_zero -> fprintf fmt "Division by zero"
      | a -> next fmt a
    )

let () =
  register_alarm_message {
    classifier = (fun next -> function
        | A_c_divide_by_zero_msg _ -> A_c_divide_by_zero
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_divide_by_zero_msg(e1), A_c_divide_by_zero_msg(e2) -> compare_expr e1 e2
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_divide_by_zero_msg(e) -> fprintf fmt "denominator '%a' may be null" (Debug.bold pp_expr) e
        | a -> next fmt a
      );
  }


let raise_c_divide_by_zero_alarm ?(bottom=true) denominator range man flow =
  let cs = Flow.get_callstack flow in
  let denominator' = get_orig_expr denominator in
  let alarm = mk_alarm (A_c_divide_by_zero_msg(denominator')) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow


(** {2 Integer overflow} *)
(** ******************** *)


type alarm_class   += A_c_integer_overflow
type alarm_message += A_c_integer_overflow_msg of expr (** integer expression *) *
                                                  int_itv (** expression value *)
                   |  A_c_cast_integer_overflow_msg of expr (** integer expression *) *
                                                       int_itv (** expression value *) *
                                                       typ (** cast type *)

let () =
  register_alarm_class (fun next fmt -> function
      | A_c_integer_overflow -> fprintf fmt "Integer overflow"
      | a -> next fmt a
    )

let () =
  register_grouped_alarm_message {
    classifier = (fun next -> function
        | A_c_integer_overflow_msg _      -> A_c_integer_overflow
        | A_c_cast_integer_overflow_msg _ -> A_c_integer_overflow
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_integer_overflow_msg(e1,v1), A_c_integer_overflow_msg(e2,v2) ->
          Compare.pair compare_expr compare_int_interval (e1,v1) (e2,v2)

        | A_c_cast_integer_overflow_msg(e1,v1,t1), A_c_cast_integer_overflow_msg(e2,v2,t2) ->
          Compare.triple compare_expr compare_int_interval compare_typ (e1,v1,t1) (e2,v2,t2)

        | _ -> next a1 a2
      );
    print = (fun next fmt messages -> function
        | A_c_integer_overflow ->
          (* Get the target type. As this is a static information, it should
             be the same for all messages. *)
          let t = match messages with
            | A_c_integer_overflow_msg(e,_) :: _ -> e.etyp
            | A_c_cast_integer_overflow_msg(_,_,t) :: _ -> t
            | _ -> assert false
          in
          let l,u = rangeof t in
          let type_range = I.of_z l u in

          (* Group values by expressions *)
          let m = List.fold_left
              (fun acc -> function
                 | A_c_integer_overflow_msg(e,v) ->
                   let oldv = try ExprMap.find e acc with Not_found -> Bot.BOT in
                   let v = I.join_bot oldv v in
                   ExprMap.add e v acc
                 | A_c_cast_integer_overflow_msg(e,v,t) ->
                   let oldv = try ExprMap.find e acc with Not_found -> Bot.BOT in
                   let v = I.join_bot oldv v in
                   ExprMap.add e v acc
                 | _ -> assert false
              ) ExprMap.empty messages
          in
          pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
            (fun fmt (e,v) ->
               fprintf fmt "'%a' has value %a that is larger than the range of '%a' = %a"
                 (Debug.bold pp_expr) e
                 pp_const_or_interval v
                 (Debug.bold pp_typ) t
                 I.fprint type_range
            ) fmt (ExprMap.bindings m)

        | cls -> next fmt messages cls
      );
  }


let raise_c_integer_overflow_alarm ?(bottom=false) cexp nexp man input_flow error_flow =
  let cs = Flow.get_callstack error_flow in
  let cexp' = get_orig_expr cexp in
  let itv = man.ask (mk_int_interval_query nexp) input_flow in
  let alarm = mk_alarm (A_c_integer_overflow_msg(cexp',itv)) cs cexp'.erange in
  Flow.raise_alarm alarm ~bottom man.lattice error_flow

let raise_c_cast_integer_overflow_alarm ?(bottom=false) cexp nexp t man input_flow error_flow =
  let cs = Flow.get_callstack error_flow in
  let cexp' = get_orig_expr cexp in
  let itv = man.ask (mk_int_interval_query nexp) input_flow in
  let alarm = mk_alarm (A_c_cast_integer_overflow_msg(cexp',itv,t)) cs cexp'.erange in
  Flow.raise_alarm alarm ~bottom man.lattice error_flow


(** {2 Invalid shift} *)
(** ***************** *)

type alarm_class   += A_c_invalid_shift
type alarm_message += A_c_invalid_shift_msg of expr (** shifted expression *) *
                                               expr (** shift expression *) *
                                               int_itv (** shift value *)

let () =
  register_alarm_class (fun next fmt -> function
      | A_c_invalid_shift -> fprintf fmt "Invalid shift"
      | a -> next fmt a
    )

let () =
  register_grouped_alarm_message {
    classifier = (fun next -> function
        | A_c_invalid_shift_msg _ -> A_c_invalid_shift
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_invalid_shift_msg(e1,s1,v1), A_c_invalid_shift_msg(e2,s2,v2) ->
          Compare.triple compare_expr compare_expr compare_int_interval (e1,s1,v1) (e2,s2,v2)
        | _ -> next a1 a2
      );
    print = (fun next fmt messages -> function
        | A_c_invalid_shift ->
          (* Since alarms are grouped by program location, we should
             have the same shifted expressions for all
             messages. *)
          let e,shift = match messages with
            | A_c_invalid_shift_msg(e,shift,_) :: _ -> e, shift
            | _ -> assert false
          in
          let v = List.fold_left
              (fun acc -> function
                 | A_c_invalid_shift_msg(e,shift,v) -> I.join_bot acc v
                 | _ -> assert false
              ) Bot.BOT messages
          in
          let bits = Ast.sizeof_type e.etyp |> Z.mul (Z.of_int 8) in
          let valid_shifts = I.of_z Z.zero (Z.pred bits) in
          fprintf fmt "shift position '%a' = %a %a [0, %a]"
            (Debug.bold pp_expr) e
            pp_const_or_interval v
            pp_const_or_interval_not_eq v
            I.fprint valid_shifts

        | cls -> next fmt messages cls
      );
  }



let raise_c_invalid_shift_alarm ?(bottom=true) e shift man input_flow error_flow =
  let cs = Flow.get_callstack error_flow in
  let shift' = get_orig_expr shift in
  let shift_itv = man.ask (mk_int_interval_query shift) input_flow in
  let alarm = mk_alarm (A_c_invalid_shift_msg(e,shift',shift_itv)) cs shift.erange in
  Flow.raise_alarm alarm ~bottom man.lattice error_flow



(** {2 Invalid pointer comparison} *)
(** ****************************** *)

type alarm_class   += A_c_invalid_pointer_compare
type alarm_message += A_c_invalid_pointer_compare_msg of expr (** first pointer *) *
                                                         expr (** second pointer *)

let () =
  register_alarm_class (fun next fmt -> function
      | A_c_invalid_pointer_compare -> fprintf fmt "Invalid pointer comparison"
      | a -> next fmt a
    )

let () =
  register_alarm_message {
    classifier = (fun next -> function
        | A_c_invalid_pointer_compare_msg _ -> A_c_invalid_pointer_compare
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_invalid_pointer_compare_msg(p1,p2), A_c_invalid_pointer_compare_msg(p1',p2') ->
          Compare.pair compare_expr compare_expr (p1,p2) (p1',p2')
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_invalid_pointer_compare_msg(p1,p2) ->
          fprintf fmt "'%a' and '%a' may point to different memory objects"
            (Debug.bold pp_expr) p1
            (Debug.bold pp_expr) p2

        | m -> next fmt m
      );
  }


let raise_c_invalid_pointer_compare ?(bottom=true) p1 p2 range man flow =
  let cs = Flow.get_callstack flow in
  let p1' = get_orig_expr p1 in
  let p2' = get_orig_expr p2 in
  let alarm = mk_alarm (A_c_invalid_pointer_compare_msg(p1',p2')) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow




(** {2 Invalid pointer subtraction} *)
(** ******************************* *)

type alarm_class   += A_c_invalid_pointer_sub
type alarm_message += A_c_invalid_pointer_sub_msg of expr (** first pointer *) *
                                                     expr (** second pointer *)

let () =
  register_alarm_class (fun next fmt -> function
      | A_c_invalid_pointer_sub -> fprintf fmt "Invalid pointer subtraction"
      | a -> next fmt a
    )

let () =
  register_alarm_message {
    classifier = (fun next -> function
        | A_c_invalid_pointer_sub_msg _ -> A_c_invalid_pointer_sub
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_invalid_pointer_sub_msg(p1,p2), A_c_invalid_pointer_sub_msg(p1',p2') ->
          Compare.pair compare_expr compare_expr (p1,p2) (p1',p2')
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_invalid_pointer_sub_msg(p1,p2) ->
          fprintf fmt "'%a' and '%a' may point to different memory objects"
            (Debug.bold pp_expr) p1
            (Debug.bold pp_expr) p2

        | m -> next fmt m
      );
  }


let raise_c_invalid_pointer_sub ?(bottom=true) p1 p2 range man flow =
  let cs = Flow.get_callstack flow in
  let p1' = get_orig_expr p1 in
  let p2' = get_orig_expr p2 in
  let alarm = mk_alarm (A_c_invalid_pointer_sub_msg(p1',p2')) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow



(** {2 Dangling pointer dereference} *)
(** ******************************** *)

type alarm_class   += A_c_dangling_pointer_deref
type alarm_message += A_c_dangling_pointer_deref_msg of expr (** pointer *) *
                                                        var (** pointed variable *) *
                                                        range (** return location *)
type alarm_message += A_c_dangling_pointer_deref_wo_info_msg

let () =
  register_alarm_class (fun next fmt -> function
      | A_c_dangling_pointer_deref -> fprintf fmt "Dangling pointer dereference"
      | a -> next fmt a
    )

let () =
  register_alarm_message {
    classifier = (fun next -> function
        | A_c_dangling_pointer_deref_msg _ -> A_c_dangling_pointer_deref
        | A_c_dangling_pointer_deref_wo_info_msg -> A_c_dangling_pointer_deref
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_dangling_pointer_deref_msg(p1,v1,r1), A_c_dangling_pointer_deref_msg(p2,v2,r2) ->
          Compare.triple compare_expr compare_var compare_range (p1,v1,r1) (p2,v2,r2)
        | A_c_dangling_pointer_deref_wo_info_msg, A_c_dangling_pointer_deref_wo_info_msg -> 0
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_dangling_pointer_deref_msg(p,v,r) ->
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

        | A_c_dangling_pointer_deref_wo_info_msg -> ()

        | m -> next fmt m
      );
  }


let raise_c_dangling_deref_alarm ?(bottom=true) ptr var ret_range ?(range=ptr.erange) man flow =
  let cs = Flow.get_callstack flow in
  let ptr' = get_orig_expr ptr in
  let alarm = mk_alarm (A_c_dangling_pointer_deref_msg(ptr',var,untag_range ret_range)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow


let raise_c_dangling_deref_wo_info_alarm ?(bottom=true) range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm A_c_dangling_pointer_deref_wo_info_msg cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow



(** {2 Use after free} *)
(** ****************** *)

type alarm_class   += A_c_use_after_free
type alarm_message += A_c_use_after_free_msg of expr (** pointer *) *
                                                range (** deallocation site *)
type alarm_message += A_c_use_after_free_wo_info_msg

let () =
  register_alarm_class (fun next fmt -> function
      | A_c_use_after_free -> fprintf fmt "Use after free"
      | a -> next fmt a
    )

let () =
  register_alarm_message {
    classifier = (fun next -> function
        | A_c_use_after_free_msg _ -> A_c_use_after_free
        | A_c_use_after_free_wo_info_msg -> A_c_use_after_free
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_use_after_free_msg(p1,r1), A_c_use_after_free_msg(p2,r2) ->
          Compare.pair compare_expr compare_range (p1,r1) (p2,r2)
        | A_c_use_after_free_wo_info_msg, A_c_use_after_free_wo_info_msg -> 0
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_use_after_free_msg(p,r) ->
          fprintf fmt "'%a' points to memory deallocated at %a" (Debug.bold pp_expr) p pp_relative_range r

        | A_c_use_after_free_wo_info_msg -> ()

        | m -> next fmt m
      );
  }


let raise_c_use_after_free_alarm ?(bottom=true) pointer dealloc_range ?(range=pointer.erange) man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm (A_c_use_after_free_msg(pointer',untag_range dealloc_range)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow

let raise_c_use_after_free_wo_info_alarm ?(bottom=true) range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm A_c_use_after_free_wo_info_msg cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow


(** {2 Double free} *)
(** *************** *)

type alarm_class   += A_c_double_free
type alarm_message += A_c_double_free_msg of expr (** pointer *) *
                                             range (** deallocation site *)

let () =
  register_alarm_class (fun next fmt -> function
      | A_c_double_free -> fprintf fmt "Double free"
      | a -> next fmt a
    )

let () =
  register_alarm_message {
    classifier = (fun next -> function
        | A_c_double_free_msg _ -> A_c_double_free
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_double_free_msg(p1,r1), A_c_double_free_msg(p2,r2) ->
          Compare.pair compare_expr compare_range (p1,r1) (p2,r2)
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_double_free_msg(p,r) ->
          fprintf fmt "'%a' points to memory already deallocated at %a"
            (Debug.bold pp_expr) p
            pp_relative_range r

        | m -> next fmt m
      );
  }


let raise_c_double_free_alarm ?(bottom=true) pointer dealloc_range ?(range=pointer.erange) man flow =
  let cs = Flow.get_callstack flow in
  let pointer' = get_orig_expr pointer in
  let alarm = mk_alarm (A_c_double_free_msg(pointer',untag_range dealloc_range)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow


(** {2 Insufficient variadic arguments} *)
(** *********************************** *)

type alarm_class   += A_c_insufficient_variadic_args
type alarm_message += A_c_insufficient_variadic_args_msg of var (** va_list variable *) *
                                                            int_itv (** va_arg call counter *) *
                                                            int (** number of passed arguments *)

let () =
  register_alarm_class (fun next fmt -> function
      | A_c_insufficient_variadic_args -> fprintf fmt "Insufficient variadic arguments"
      | a -> next fmt a
    )

let () =
  register_grouped_alarm_message {
    classifier = (fun next -> function
        | A_c_insufficient_variadic_args_msg _ -> A_c_insufficient_variadic_args
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_insufficient_variadic_args_msg(va1,c1,n1), A_c_insufficient_variadic_args_msg(va2,c2,n2) ->
          Compare.triple compare_var compare_int_interval compare (va1,c1,n1) (va2,c2,n2)
        | _ -> next a1 a2
      );
    print = (fun next fmt messages -> function
        | A_c_insufficient_variadic_args ->
          (* Group messages by va_list variables and compute the interval of call counter and the number of passed arguments *)
          let m = List.fold_left
              (fun acc -> function
                 | A_c_insufficient_variadic_args_msg(va,c,n) ->
                   let (oldc,oldn) = try VarMap.find va acc with Not_found -> (Bot.BOT,Bot.BOT) in
                   VarMap.add va (I.join_bot oldc c, I.join_bot oldn (Bot.Nb (I.cst_int n))) acc
                 | _ -> assert false
              ) VarMap.empty messages
          in
          pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
            (fun fmt (va,(counter,nargs)) ->
               fprintf fmt "va_arg called %a time%a on va_list object '%a' with %a argument%a"
                 pp_const_or_interval counter
                 pp_interval_plurial counter
                 pp_var va
                 pp_const_or_interval nargs
                 pp_interval_plurial nargs
            ) fmt (VarMap.bindings m)

        | cls -> next fmt messages cls
      );
  }


let raise_c_insufficient_variadic_args ?(bottom=true) va_list counter args range man input_flow error_flow =
  let cs = Flow.get_callstack error_flow in
  let nargs = List.length args in
  let counter_itv = man.ask (mk_int_interval_query counter) input_flow in
  let alarm = mk_alarm (A_c_insufficient_variadic_args_msg(va_list,counter_itv,nargs)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice error_flow



(** {2 Insufficient format arguments} *)
(** ********************************* *)

type alarm_class   += A_c_insufficient_format_args
type alarm_message += A_c_insufficient_format_args_msg of int (** number of required arguments *) * int (** number of given arguments *)
type alarm_message += A_c_insufficient_format_args_wo_info_msg

let () =
  register_alarm_class (fun next fmt -> function
      | A_c_insufficient_format_args -> fprintf fmt "Insufficient format arguments"
      | a -> next fmt a
    )

let () =
  register_alarm_message {
    classifier = (fun next -> function
        | A_c_insufficient_format_args_msg _       -> A_c_insufficient_format_args
        | A_c_insufficient_format_args_wo_info_msg -> A_c_insufficient_format_args
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_insufficient_format_args_msg(r1,g1), A_c_insufficient_format_args_msg(r2,g2) ->
          Compare.pair compare compare (r1,g1) (r2,g2)
        | A_c_insufficient_format_args_wo_info_msg, A_c_insufficient_format_args_wo_info_msg -> 0
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_insufficient_format_args_msg(required,given) ->
          fprintf fmt "%d argument%a given while %d argument%a required"
            given Debug.plurial_int given
            required Debug.plurial_int required
        | A_c_insufficient_format_args_wo_info_msg -> ()
        | m -> next fmt m
      );
  }


let raise_c_insufficient_format_args_alarm ?(bottom=true) required given range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_c_insufficient_format_args_msg(required,given)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow

let raise_c_insufficient_format_args_wo_info_alarm ?(bottom=true) range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm A_c_insufficient_format_args_wo_info_msg cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow


(** {2 Invalid type of format argument} *)
(** *********************************** *)

type alarm_class   += A_c_invalid_format_arg_type
type alarm_message += A_c_invalid_format_arg_type_msg of expr (** argument *) * typ (** expected type *)
type alarm_message += A_c_invalid_format_arg_type_wo_info_msg

let () =
  register_alarm_class (fun next fmt -> function
      | A_c_invalid_format_arg_type -> fprintf fmt "Invalid type of format argument"
      | a -> next fmt a
    )

let () =
  register_alarm_message {
    classifier = (fun next -> function
        | A_c_invalid_format_arg_type_msg _       -> A_c_invalid_format_arg_type
        | A_c_invalid_format_arg_type_wo_info_msg -> A_c_invalid_format_arg_type
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_invalid_format_arg_type_msg(e1,t1), A_c_invalid_format_arg_type_msg(e2,t2) ->
          Compare.pair compare_expr compare_typ (e1,t1) (e2,t2)
        | A_c_invalid_format_arg_type_wo_info_msg, A_c_invalid_format_arg_type_wo_info_msg -> 0
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_invalid_format_arg_type_msg(e,t) ->
          fprintf fmt "format expects argument of type '%a', but '%a' has type '%a'"
            (Debug.bold pp_typ) t
            (Debug.bold pp_expr) e
            (Debug.bold pp_typ) e.etyp
        | A_c_invalid_format_arg_type_wo_info_msg -> ()
        | m -> next fmt m
      );
  }


let raise_c_invalid_format_arg_type_alarm ?(bottom=false) arg typ man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_c_invalid_format_arg_type_msg(get_orig_expr arg, typ)) cs arg.erange in
  Flow.raise_alarm alarm ~bottom man.lattice flow

let raise_c_invalid_format_arg_type_wo_info_alarm ?(bottom=false) range man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm A_c_invalid_format_arg_type_wo_info_msg cs range in
  Flow.raise_alarm alarm ~bottom man.lattice flow

(** {2 Modification of read-only memory} *)
(** ************************************** *)

type alarm_class   += A_c_modify_read_only
type alarm_message += A_c_modify_read_only_msg of expr (** pointer *) *
                                                  base (** pointed base *)

let () =
  register_alarm_class (fun next fmt -> function
      | A_c_modify_read_only -> fprintf fmt "Modification of read-only memory"
      | a -> next fmt a
    )

let () =
  register_alarm_message {
    classifier = (fun next -> function
        | A_c_modify_read_only_msg _ -> A_c_modify_read_only
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_modify_read_only_msg(p1,b1), A_c_modify_read_only_msg(p2,b2) ->
          Compare.pair compare_expr compare_base (p1,b1) (p2,b2)
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_modify_read_only_msg(p,b) ->
          fprintf fmt "'%a' points to read-only %a"
            (Debug.bold pp_expr) p
            pp_base_verbose b
        | m -> next fmt m
      );
  }


let raise_c_modify_read_only_alarm ?(bottom=true) ptr base man flow =
  let cs = Flow.get_callstack flow in
  let alarm = mk_alarm (A_c_modify_read_only_msg(get_orig_expr ptr, base)) cs ptr.erange in
  Flow.raise_alarm alarm ~bottom man.lattice flow



(** {2 Float errors} *)
(** **************** *)


type alarm_class   += A_c_invalid_float_class
type alarm_message += A_c_invalid_float_class_msg of float_itv (** float value *) * string (** expected class *)

let () =
  register_alarm_class (fun next fmt -> function
      | A_c_invalid_float_class -> fprintf fmt " Invalid floating-point number class"
      | a -> next fmt a
    )

let () =
  register_alarm_message {
    classifier = (fun next -> function
        | A_c_invalid_float_class_msg _ -> A_c_invalid_float_class
        | a -> next a
      );
    compare = (fun next a1 a2 ->
        match a1, a2 with
        | A_c_invalid_float_class_msg(f1,m1), A_c_invalid_float_class_msg(f2,m2) ->
          Compare.compose
            [ (fun () -> compare_float_interval f1 f2);
              (fun () -> compare m1 m2)
            ]
        | _ -> next a1 a2
      );
    print = (fun next fmt -> function
        | A_c_invalid_float_class_msg(f,msg) -> fprintf fmt "float '%a' may not be %s" (Debug.bold (F.fprint F.dfl_fmt)) f msg
        | a -> next fmt a
      );
  }

let raise_c_invalid_float_class_alarm ?(bottom=true) float msg range man input_flow error_flow =
  let cs = Flow.get_callstack error_flow in
  let float_itv = man.ask (mk_float_interval_query float) input_flow in
  let alarm = mk_alarm (A_c_invalid_float_class_msg (float_itv,msg)) cs range in
  Flow.raise_alarm alarm ~bottom man.lattice error_flow
