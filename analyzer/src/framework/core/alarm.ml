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


(** {1 Checks} *)
(** ********** *)

type check = ..

let check_print_chain : check TypeExt.print_chain =
  TypeExt.mk_print_chain (fun _ _ -> Exceptions.panic "Print of unregistered check")

let pp_check fmt c =
  TypeExt.print check_print_chain fmt c

let register_check f =
  TypeExt.register_print f check_print_chain


(** {1 Alarms} *)
(** ********** *)

type alarm_kind = ..

type alarm_kind += A_instance of check

type alarm = {
  alarm_kind      : alarm_kind;
  alarm_range     : range;
  alarm_callstack : callstack;
}

let mk_alarm kind callstack range =
  { alarm_kind      = kind;
    alarm_range     = range;
    alarm_callstack = callstack }

let alarm_compare_chain : alarm_kind TypeExt.compare_chain =
  TypeExt.mk_compare_chain
    (fun a1 a2 ->
       match a1,a2 with
       | A_instance chk1, A_instance chk2 -> compare chk1 chk2
       | _ -> compare a1 a2)

let alarm_print_chain : alarm_kind TypeExt.print_chain =
  TypeExt.mk_print_chain
    (fun fmt -> function
       | A_instance chk -> pp_check fmt chk
       | _ -> Exceptions.panic "Print of unregistered alarm")

let alarm_check_chain : (alarm_kind -> check) ref =
  ref (function
      | A_instance chk -> chk
      | _ -> Exceptions.panic "Check of unregistered alarm")

let alarm_join_chain : (alarm_kind -> alarm_kind -> alarm_kind option) ref =
  ref (fun a1 a2 -> None)

let compare_alarm_kind a1 a2 =
  TypeExt.compare alarm_compare_chain a1 a2

let pp_alarm_kind fmt a =
  TypeExt.print alarm_print_chain fmt a

let join_alarm_kind a1 a2 =
  !alarm_join_chain a1 a2

let compare_alarm a1 a2 =
  Compare.triple
    compare_alarm_kind compare_callstack compare_range
    (a1.alarm_kind,a1.alarm_callstack,a1.alarm_range)
    (a2.alarm_kind,a2.alarm_callstack,a2.alarm_range)

let pp_alarm fmt a =
  pp_alarm_kind fmt a.alarm_kind

let register_alarm_compare f =
  TypeExt.register_compare f alarm_compare_chain

let register_alarm_pp f =
  TypeExt.register_print f alarm_print_chain

let register_alarm_check f =
  alarm_check_chain := f !alarm_check_chain

let register_alarm_join f =
  alarm_join_chain := f !alarm_join_chain

type alarm_info = {
  check : (alarm_kind -> check) -> alarm_kind -> check;
  compare : (alarm_kind -> alarm_kind -> int) -> alarm_kind -> alarm_kind -> int;
  print : (formatter -> alarm_kind -> unit) -> formatter -> alarm_kind -> unit;
  join : (alarm_kind -> alarm_kind -> alarm_kind option) -> alarm_kind -> alarm_kind -> alarm_kind option;
}

let register_alarm info =
  register_alarm_check info.check;
  register_alarm_pp info.print;
  register_alarm_compare info.compare;
  register_alarm_join info.join


let check_of_alarm a =
  !alarm_check_chain a.alarm_kind

let range_of_alarm a = a.alarm_range

let callstack_of_alarm a = a.alarm_callstack


(** {1 Check diagnosis} *)
(** ******************* *)

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

let add_alarm_to_diagnosis alarm = function
  | Error a -> Error (AlarmSet.add alarm a)
  | Warning a -> Warning (AlarmSet.add alarm a)
  | Safe -> Warning (AlarmSet.singleton alarm)
  | Unreachable -> Error (AlarmSet.singleton alarm)


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


(** {1 Alarms report} *)
(** ***************** *)

module RangeMap = MapExt.Make(struct type t = range let compare = compare_range end)

module CheckMap = MapExt.Make(struct type t = check let compare = compare end)

module HypothesisSet = SetExt.Make(struct type t = hypothesis let compare = compare_hypothesis end)

type alarms_report = {
  alarms    : diagnosis CheckMap.t RangeMap.t;
  soundness : HypothesisSet.t;
}

let empty_alarms_report = {
  alarms = RangeMap.empty;
  soundness = HypothesisSet.empty;
}

let is_empty_alarms_report r =
  RangeMap.for_all
    (fun range checks ->
       CheckMap.for_all (fun check -> function
           | Safe | Unreachable  -> true
           | Error _ | Warning _ -> false) checks
    ) r.alarms

let singleton_alarms_report alarm =
  let diag = Error (AlarmSet.singleton alarm) in
  let check = check_of_alarm alarm in
  { alarms = RangeMap.singleton alarm.alarm_range (CheckMap.singleton check diag);
    soundness = HypothesisSet.empty; }

let pp_alarms_report fmt r =
  fprintf fmt "@[<v>soundness hypotheis: @[<v>%a@]@,%a@]"
    (HypothesisSet.fprint
       SetExtSig.{ print_empty = "∅";
                   print_begin = "";
                   print_sep   = "@,";
                   print_end   = ""; }
       pp_hypothesis
    ) r.soundness
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
       (fun fmt (range,checks) ->
          pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
            (fun fmt (check,diag) ->
               fprintf fmt "@[<v2>%a:@,@[%a@]@]"
                 pp_relative_range range
                 pp_diagnosis diag
            ) fmt (CheckMap.bindings checks)
       )
    ) (RangeMap.bindings r.alarms)

let subset_alarms_report r1 r2 =
  (r1 == r2)
  || (HypothesisSet.equal r1.soundness r2.soundness
      && (r1.alarms == r2.alarms
          || (RangeMap.for_all2zo
                (fun _ checks1 -> false)
                (fun _ checks2 -> true)
                (fun _ checks1 checks2 ->
                   CheckMap.for_all2zo
                     (fun _ diag1 -> diag1 == Unreachable)
                     (fun _ diag2 -> true)
                     (fun _ diag1 diag2 -> subset_diagnosis diag1 diag2)
                     checks1 checks2
                )
                r1.alarms r2.alarms)))

let join_alarms_report r1 r2 =
  if r1 == r2 then r1 else
  if subset_alarms_report r1 r2 then r2 else
  if subset_alarms_report r2 r1 then r1
  else
    { soundness = HypothesisSet.union r1.soundness r2.soundness;
      alarms =
        RangeMap.map2zo
          (fun _ checks1 -> checks1)
          (fun _ checks2 -> checks2)
          (fun _ checks1 checks2 ->
             CheckMap.map2zo
               (fun _ diag1 -> diag1)
               (fun _ diag2 -> diag2)
               (fun _ diag1 diag2 -> join_diagnosis diag1 diag2)
               checks1 checks2
          )
          r1.alarms r2.alarms; }

let meet_alarms_report r1 r2 =
  if r1 == r2 then r1 else
  if subset_alarms_report r1 r2 then r1 else
  if subset_alarms_report r2 r1 then r2
  else
    { soundness = HypothesisSet.union r1.soundness r2.soundness;
      alarms =
        RangeMap.fold2zo
          (fun range _ acc -> RangeMap.remove range acc)
          (fun range _ acc -> RangeMap.remove range acc)
          (fun range checks1 checks2 acc ->
             let checks =
               CheckMap.fold2zo
                 (fun check diag1 acc -> CheckMap.remove check acc)
                 (fun check diag2 acc -> CheckMap.remove check acc)
                 (fun check diag1 diag2 acc -> CheckMap.add check (meet_diagnosis diag1 diag2) acc)
                 checks1 checks2 checks1 in
             RangeMap.add range checks acc
          )
          r1.alarms r2.alarms r1.alarms }

let count_alarms r =
  RangeMap.fold
    (fun range checks acc ->
       CheckMap.fold
         (fun check diag (errors,warnings) ->
            match diag with
            | Error _ -> errors + 1, warnings
            | Warning _ -> errors, warnings + 1
            | Safe | Unreachable -> errors, warnings
         ) checks acc
    ) r.alarms (0,0)

let add_alarm_to_report alarm r =
  let check = check_of_alarm alarm in
  let checks = try RangeMap.find alarm.alarm_range r.alarms with Not_found -> CheckMap.empty in
  let diag = try CheckMap.find check checks with Not_found -> Unreachable in
  let diag' = add_alarm_to_diagnosis alarm diag in
  { r with alarms = RangeMap.add alarm.alarm_range (CheckMap.add check diag' checks) r.alarms }

let map2zo_alarms_report f1 f2 f r1 r2 =
  { soundness = HypothesisSet.union r1.soundness r2.soundness;
    alarms = RangeMap.map2zo
        (fun range -> CheckMap.mapi (f1 range))
        (fun range -> CheckMap.mapi (f2 range))
        (fun range ->
           CheckMap.map2zo
             (f1 range)
             (f2 range)
             (f range)
        )
        r1.alarms r2.alarms }

let fold2zo_alarms_report f1 f2 f r1 r2 init =
  RangeMap.fold2zo
    (fun range checks1 acc -> CheckMap.fold (f1 range) checks1 acc)
    (fun range checks2 acc -> CheckMap.fold (f2 range) checks2 acc)
    (fun range checks1 checks2 acc ->
       CheckMap.fold2zo
         (fun check diag1 acc -> f1 range check diag1 acc)
         (fun check diag2 acc -> f2 range check diag2 acc)
         (fun check diag1 diag2 acc -> f range check diag1 diag2 acc)
         checks1 checks2 acc
    )
    r1.alarms r2.alarms init

let alarms_report_to_set r =
  RangeMap.fold
    (fun range checks acc -> CheckMap.fold (fun check diag acc -> AlarmSet.union acc (get_alarms_of_diagnosis diag)) checks acc)
    r.alarms AlarmSet.empty

let group_alarms_set_by_range s =
  AlarmSet.fold
    (fun a acc ->
       let range = a.alarm_range in
       let s = try RangeMap.find range acc |> AlarmSet.add a with Not_found -> AlarmSet.singleton a in
       RangeMap.add range s acc
    ) s RangeMap.empty

let group_alarms_set_by_check s =
  AlarmSet.fold
    (fun a acc ->
       let check = check_of_alarm a in
       let s = try CheckMap.find check acc |> AlarmSet.add a with Not_found -> AlarmSet.singleton a in
       CheckMap.add check s acc
    ) s CheckMap.empty

let find_diagnosis range check report =
  try
    RangeMap.find range report.alarms |>
    CheckMap.find check
  with Not_found ->
    Unreachable

let find_alarms range check report =
  find_diagnosis range check report |>
  get_alarms_of_diagnosis

let set_diagnosis range check diag report =
  let checks = try RangeMap.find range report.alarms with Not_found -> CheckMap.empty in
  let old_diag = try CheckMap.find check checks with Not_found -> Unreachable in
  if old_diag == diag then report
  else { report with alarms = RangeMap.add range (CheckMap.add check diag checks) report.alarms }

let exists_diagnosis f report =
  RangeMap.exists
    (fun range -> CheckMap.exists (f range))
    report.alarms

let forall_diagnosis f report =
  RangeMap.exists
    (fun range -> CheckMap.exists (f range))
    report.alarms

let filter_alarms_report f report =
  let alarms =
    RangeMap.fold
      (fun range checks acc ->
         let checks' = CheckMap.filter (f range) checks in
         if checks == checks' then acc else
         if CheckMap.is_empty checks' then RangeMap.remove range acc
         else RangeMap.add range checks' acc)
      report.alarms report.alarms
  in
  if alarms = report.alarms then report else { report with alarms }
