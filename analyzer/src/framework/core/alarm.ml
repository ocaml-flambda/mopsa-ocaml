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

open Lattice
open Location
open Callstack
open Format


(** {1 Alarm categories} *)
(** ******************** *)

type alarm_category = ..

let alarm_category_print_chain : alarm_category TypeExt.print_chain =
  TypeExt.mk_print_chain (fun _ _ -> Exceptions.panic "Print of unregistered alarm category")

let pp_alarm_category fmt c =
  TypeExt.print alarm_category_print_chain fmt c

let register_alarm_category f =
  TypeExt.register_print f alarm_category_print_chain


(** {1 Alarms} *)
(** ********** *)

type alarm = ..

let alarm_compare_chain : alarm TypeExt.compare_chain =
  TypeExt.mk_compare_chain compare

let alarm_print_chain : alarm TypeExt.print_chain =
  TypeExt.mk_print_chain (fun _ _ -> Exceptions.panic "Print of unregistered alarm")

let alarm_category_chain : (alarm -> alarm_category) ref =
  ref (fun _ -> Exceptions.panic "Category of unregistered alarm")

let compare_alarm a1 a2 =
  TypeExt.compare alarm_compare_chain a1 a2

let pp_alarm fmt a =
  TypeExt.print fmt a

let register_alarm_compare f =
  TypeExt.register_compare f alarm_compare_chain

let register_alarm_pp f =
  TypeExt.register_print f alarm_print_chain

let register_alarm_category f =
  alarm_category_chain := f !alarm_category_chain

type alarm_info = {
  category : (alarm -> alarm_category) -> alarm -> alarm_category;
  compare : (alarm -> alarm -> int) -> alarm -> alarm -> int;
  print : (formatter -> alarm -> unit) -> formatter -> alarm -> unit;
}

let register_alarm info =
  register_alarm_category info.category;
  register_alarm_pp info.print;
  register_alarm_compare info.compare


let category_of_alarm a =
  !alarm_category_chain a

let compare_alarm a1 a2 =
  TypeExt.compare alarm_compare_chain a1 a2

let pp_alarm fmt a =
  TypeExt.print alarm_print_chain fmt a


(** {1 Diagnosis} *)
(** ************* *)

module AlarmSet = SetExt.Make(struct type t = alarm let compare = compare_alarm end)

let pp_alarm_set fmt a =
  match AlarmSet.elements a with
  | []  -> pp_print_string fmt "∅"
  | [a] -> pp_alarm fmt a
  | l   ->
    fprintf fmt "@[<v>{ %a }@]"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
         pp_alarm
      ) l

type diagnosis =
  | Safe
  | Error       of AlarmSet.t
  | Warning     of AlarmSet.t
  | Unreachable

let get_alarms_of_diagnosis = function
  | Safe | Unreachable -> AlarmSet.empty
  | Error a | Warning a -> a

let set_alarms_of_diagnosis a = function
  | Safe | Unreachable as x -> x
  | Error _                 -> Error a
  | Warning _               -> Warning a

exception UnsoundDiagnosis

let pp_diagnosis fmt = function
  | Safe        -> pp_print_string fmt "safe"
  | Error a     -> fprintf fmt "error: %a" pp_alarm_set a
  | Warning a   -> fprintf fmt "warning: %a" pp_alarm_set a
  | Unreachable -> pp_print_string fmt "unreachable"

let subset_diagnosis diag1 diag2 =
  if diag1 == diag2 then true else
  match diag1, diag2 with
  | Unreachable, _ -> true
  | _, Unreachable -> false
  | _, Warning a   -> AlarmSet.subset (get_alarms_of_diagnosis diag1) a
  | Warning _, _   -> false
  | Error a1, Error a2 -> AlarmSet.subset a1 a2
  | Safe, Safe     -> true
  | Error _,Safe     -> false
  | Safe,Error _     -> false

let join_diagnosis diag1 diag2 =
  if diag1 == diag2 then diag1 else
  if subset_diagnosis diag1 diag2 then diag2 else
  if subset_diagnosis diag2 diag1 then diag1 else
  match diag1,diag2 with
  | Unreachable, x | x, Unreachable  -> x
  | Warning a, x     | x, Warning a  -> Warning (AlarmSet.union a (get_alarms_of_diagnosis x))
  | Error a1, Error a2               -> Error (AlarmSet.union a1 a2)
  | Safe, Safe                       -> Safe
  | Error a, Safe    | Safe, Error a -> Warning a

let meet_diagnosis diag1 diag2 =
  if diag1 == diag2 then diag1 else
  if subset_diagnosis diag1 diag2 then diag1 else
  if subset_diagnosis diag2 diag1 then diag2 else
  match diag1,diag2 with
  | Unreachable, _ | _, Unreachable  -> Unreachable
  | Warning a, x     | x, Warning a  -> set_alarms_of_diagnosis (AlarmSet.inter a (get_alarms_of_diagnosis x)) x
  | Error a1, Error a2               -> Error (AlarmSet.inter a1 a2)
  | Safe, Safe                       -> Safe
  | Error _, Safe    | Safe, Error _ -> Unreachable


(** {1 Range report} *)
(** **************** *)

module AlarmCategoryMap = MapExt.Make(struct type t = alarm_category let compare = compare end)

module CallstackSet = SetExt.Make(struct type t = callstack let compare = compare_callstack end)

type 'a range_report = {
  range_alarms     : diagnosis AlarmCategoryMap.t;
  range_env        : 'a option;
  range_callstacks : CallstackSet.t;
}

let pp_range_report pp fmt r =
  fprintf fmt "@[<v>%a%a@,callstack: @[<v>%a@]@]"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
       (fun fmt (alarm_cat,diag) ->
          fprintf fmt "@[<hv>%a:@,%a@]"
            pp_alarm_category alarm_cat
            pp_diagnosis diag
       )
    ) (AlarmCategoryMap.bindings r.range_alarms)
    (fun fmt -> function None -> () | Some env -> fprintf fmt "@,env: @[%a@]" pp env) r.range_env
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,or@,")
       pp_callstack
    ) (CallstackSet.elements r.range_callstacks)

let subseset_range_report lattice ctx r1 r2 =
  (r1 == r2)
  || ( ( r1.range_alarms == r2.range_alarms
         || AlarmCategoryMap.for_all2zo
           (fun _ diag1 -> subset_diagnosis diag1 Unreachable)
           (fun _ diag2 -> true)
           (fun _ diag1 diag2 -> subset_diagnosis diag1 diag2)
           r1.range_alarms r2.range_alarms )
       && (match r1.range_env, r2.range_env with
           | None,None -> true
           | Some e1, Some e2 -> lattice.subset ctx e1 e2
           | _ -> assert false)
       && CallstackSet.subset r1.range_callstacks r2.range_callstacks )


let join_range_report lattice ctx r1 r2 =
  if r1 == r2 then r1 else
  let range_alarms =
    AlarmCategoryMap.map2zo
      (fun _ diag1 -> raise UnsoundDiagnosis)
      (fun _ diag2 -> raise UnsoundDiagnosis)
      (fun _ diag1 diag2 -> join_diagnosis diag1 diag2)
      r1.range_alarms r2.range_alarms
  in
  let range_env = OptionExt.lift2 (lattice.join ctx) r1.range_env r2.range_env in
  let range_callstacks = CallstackSet.union r1.range_callstacks r2.range_callstacks in
  { range_alarms; range_env; range_callstacks }

let meet_range_report lattice ctx r1 r2 =
  if r1 == r2 then r1 else
  let range_alarms =
    AlarmCategoryMap.map2zo
      (fun _ diag1 -> raise UnsoundDiagnosis)
      (fun _ diag2 -> raise UnsoundDiagnosis)
      (fun _ diag1 diag2 -> meet_diagnosis diag1 diag2)
      r1.range_alarms r2.range_alarms
  in
  let range_env = OptionExt.lift2 (lattice.meet ctx) r1.range_env r2.range_env in
  let range_callstacks = CallstackSet.inter r1.range_callstacks r2.range_callstacks in
  { range_alarms; range_env; range_callstacks }


(** {1 Soundness hypothesis} *)
(** ************************ *)

type hypothesis_scope =
  | Hypothesis_global
  | Hypothesis_local of range

type hypothesis_kind = ..

type hypothesis = {
  hypothesis_scope : hypothesis_scope;
  hypothesis_kind  : hypothesis_kind;
}

let hypothesis_compare_chain : hypothesis_kind TypeExt.compare_chain = TypeExt.mk_compare_chain compare

let hypothesis_print_chain : hypothesis_kind TypeExt.print_chain =
  TypeExt.mk_print_chain (fun _ _ -> Exceptions.panic "Print of unregistered hypothesis")

let register_hypothesis info =
  TypeExt.register info hypothesis_compare_chain hypothesis_print_chain

let pp_hypothesis fmt h =
  match h.hypothesis_scope with
  | Hypothesis_global      -> TypeExt.print hypothesis_print_chain fmt h.hypothesis_kind
  | Hypothesis_local range -> fprintf fmt "%a: %a" pp_relative_range range (TypeExt.print hypothesis_print_chain) h.hypothesis_kind

let compare_hypothesis h1 h2 =
  Compare.pair
    (fun scope1 scope2 ->
       match scope1, scope2 with
       | Hypothesis_global, Hypothesis_global     -> 0
       | Hypothesis_local r1, Hypothesis_local r2 -> compare_range r1 r2
       | _ -> compare scope1 scope2 )
    (TypeExt.compare hypothesis_compare_chain)
    (h1.hypothesis_scope, h1.hypothesis_kind)
    (h2.hypothesis_scope, h2.hypothesis_kind)


(** {1 Analysis report} *)
(** ******************* *)

module RangeMap = MapExt.Make(struct type t = range let compare = compare_range end)

type 'a report = {
  report_map      : 'a range_report RangeMap.t;
  report_soundness : hypothesis Dnf.t;
}

let pp_report pp fmt r =
  fprintf fmt "@[<v>soundness: @[<v>%a@]@,%a@]"
    (Dnf.print pp_hypothesis) r.report_soundness
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
       (fun fmt (range,rr) ->
          fprintf fmt "@[<v2>%a:@,@[%a@]@]"
            pp_relative_range range
            (pp_range_report pp) rr
       )
    ) (RangeMap.bindings r.report_map)

let subset_report lattice ctx r1 r2 =
  (r1 == r2)
  || (r1.report_soundness == r2.report_soundness
      && (r1.report_map == r2.report_map
          || (RangeMap.for_all2zo
                (fun _ rr1 -> false)
                (fun _ rr2 -> true)
                (fun _ rr1 rr2 -> subseset_range_report lattice ctx rr1 rr2)
                r1.report_map r2.report_map)))

let join_report lattice ctx r1 r2 =
  if r1 == r2 then r1 else
  if subset_report lattice ctx r1 r2 then r2 else
  if subset_report lattice ctx r2 r1 then r1
  else
    { report_map =
        RangeMap.map2zo
          (fun _ rr1 -> rr1)
          (fun _ rr2 -> rr2)
          (fun _ rr1 rr2 -> join_range_report lattice ctx rr1 rr2)
          r1.report_map r2.report_map;
      report_soundness = Dnf.mk_or r1.report_soundness r2.report_soundness; }

let meet_report lattice ctx r1 r2 =
  if r1 == r2 then r1 else
  if subset_report lattice ctx r1 r2 then r2 else
  if subset_report lattice ctx r2 r1 then r1
  else
    { report_map =
        RangeMap.map2zo
          (fun _ rr1 -> rr1)
          (fun _ rr2 -> rr2)
          (fun _ rr1 rr2 -> join_range_report lattice ctx rr1 rr2)
          r1.report_map r2.report_map;
      report_soundness = Dnf.mk_or r1.report_soundness r2.report_soundness; }

let count_alarms r =
  RangeMap.fold
    (fun range rr acc ->
       AlarmCategoryMap.fold
         (fun alarm_category diag (errors,warnings) ->
            match diag with
            | Error _ -> errors + 1, warnings
            | Warning _ -> errors, warnings + 1
            | Safe | Unreachable -> errors, warnings
         ) rr.range_alarms acc
    ) r.report_map (0,0)
