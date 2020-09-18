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

let compare_check = compare

let register_check f =
  TypeExt.register_print f check_print_chain


(** {1 Alarms} *)
(** ********** *)

type alarm_kind = ..

type alarm_kind += A_instance of check

type alarm = {
  alarm_kind      : alarm_kind;
  alarm_check     : check;
  alarm_range     : range;
  alarm_callstack : callstack;
}

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
  Compare.quadruple
    compare_alarm_kind compare_callstack compare_range compare_check
    (a1.alarm_kind,a1.alarm_callstack,a1.alarm_range,a1.alarm_check)
    (a2.alarm_kind,a2.alarm_callstack,a2.alarm_range,a2.alarm_check)

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

let check_of_alarm a = a.alarm_check

let range_of_alarm a = a.alarm_range

let callstack_of_alarm a = a.alarm_callstack

let mk_alarm kind callstack range =
  { alarm_kind      = kind;
    alarm_check     = !alarm_check_chain kind;
    alarm_range     = range;
    alarm_callstack = callstack }


(** {1 Diagnostic} *)
(** ************** *)

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

type diagnostic_status =
  | Warning
  | Safe
  | Error
  | Unreachable

type diagnostic = {
  diag_range : range;
  diag_check : check;
  diag_status : diagnostic_status;
  diag_alarms : AlarmSet.t;
}

let mk_diagnostic range check status alarms =
  if status == Unreachable || status == Safe then
    assert(AlarmSet.is_empty alarms);
  { diag_range = range;
    diag_check = check;
    diag_status = status;
    diag_alarms = alarms; }

let pp_diagnostic_status fmt = function
  | Warning   -> pp_print_string fmt "warning"
  | Safe      -> pp_print_string fmt "safe"
  | Error     -> pp_print_string fmt "error"
  | Unreachable -> pp_print_string fmt "unreachable"

let pp_diagnostic fmt d =
  match d.diag_status with
  | Safe | Unreachable ->
    pp_diagnostic_status fmt d.diag_status
  | Error | Warning ->
    fprintf fmt "%a: %a"
      pp_diagnostic_status d.diag_status
      pp_alarm_set d.diag_alarms

let subset_diagnostic_status s1 s2 =
  if s1 == s2 then true else
  match s1,s2 with
  | Unreachable, _ -> true
  | _, Unreachable -> false
  | _, Warning     -> true
  | Warning, _     -> false
  | Error, Error   -> true
  | Safe, Safe     -> true
  | Error,Safe     -> false
  | Safe,Error     -> false


let subset_diagnostic d1 d2 =
  if d1 == d2 then true else
  subset_diagnostic_status d1.diag_status d2.diag_status
  && AlarmSet.subset d1.diag_alarms d2.diag_alarms

let join_diagnostic_status s1 s2 =
  if s1 == s2 then s1 else
  match s1,s2 with
  | Unreachable, x | x, Unreachable  -> x
  | Warning, x     | x, Warning      -> Warning
  | Error, Error                     -> Error
  | Safe, Safe                       -> Safe
  | Error, Safe    | Safe, Error     -> Warning


let join_diagnostic d1 d2 =
  if d1 == d2 then d1 else
  if subset_diagnostic d1 d2 then d2 else
  if subset_diagnostic d2 d1 then d1
  else (
    assert(compare_range d1.diag_range d2.diag_range = 0);
    assert(compare_check d1.diag_check d2.diag_check = 0);
    { diag_range = d1.diag_range;
      diag_check = d1.diag_check;
      diag_status = join_diagnostic_status d1.diag_status d2.diag_status;
      diag_alarms = AlarmSet.union d1.diag_alarms d2.diag_alarms; }
  )

let meet_diagnostic_status s1 s2 =
  if s1 == s2 then s1 else
  match s1,s2 with
  | Unreachable, _ | _, Unreachable  -> Unreachable
  | Warning, x     | x, Warning      -> x
  | Error, Error                     -> Error
  | Safe, Safe                       -> Safe
  | Error, Safe    | Safe, Error -> Unreachable

let meet_diagnostic d1 d2 =
  if d1 == d2 then d1 else
  if subset_diagnostic d1 d2 then d1 else
  if subset_diagnostic d2 d1 then d2
  else (
    assert(compare_range d1.diag_range d2.diag_range = 0);
    assert(compare_check d1.diag_check d2.diag_check = 0);
    { diag_range = d1.diag_range;
      diag_check = d1.diag_check;
      diag_status = meet_diagnostic_status d1.diag_status d2.diag_status;
      diag_alarms = AlarmSet.inter d1.diag_alarms d2.diag_alarms; }
  )

let add_alarm_to_diagnostic a d =
  match d.diag_status with
  | Error       -> { d with diag_alarms = AlarmSet.add a d.diag_alarms }
  | Warning     -> { d with diag_alarms = AlarmSet.add a d.diag_alarms }
  | Safe        -> { d with diag_status = Error; diag_alarms = AlarmSet.singleton a }
  | Unreachable -> { d with diag_status = Error; diag_alarms = AlarmSet.singleton a }

let compare_diagnostic d1 d2 =
  Compare.quadruple
    compare_range compare_check compare AlarmSet.compare
    (d1.diag_range,d1.diag_check,d1.diag_status,d1.diag_alarms)
    (d2.diag_range,d2.diag_check,d2.diag_status,d2.diag_alarms)

(** {1 Soundness hypothesis} *)
(** ************************ *)

type hypothesis_scope =
  | Hypo_global
  | Hypo_local of range

type hypothesis_kind = ..

type hypothesis = {
  hypo_scope : hypothesis_scope;
  hypo_kind  : hypothesis_kind;
}

let hypothesis_compare_chain : hypothesis_kind TypeExt.compare_chain = TypeExt.mk_compare_chain compare

let hypothesis_print_chain : hypothesis_kind TypeExt.print_chain =
  TypeExt.mk_print_chain (fun _ _ -> Exceptions.panic "Print of unregistered hypothesis")

let register_hypothesis info =
  TypeExt.register info hypothesis_compare_chain hypothesis_print_chain

let pp_hypothesis fmt h =
  match h.hypo_scope with
  | Hypo_global      -> TypeExt.print hypothesis_print_chain fmt h.hypo_kind
  | Hypo_local range -> fprintf fmt "%a: %a" pp_relative_range range (TypeExt.print hypothesis_print_chain) h.hypo_kind

let compare_hypothesis h1 h2 =
  Compare.pair
    (fun scope1 scope2 ->
       match scope1, scope2 with
       | Hypo_global, Hypo_global     -> 0
       | Hypo_local r1, Hypo_local r2 -> compare_range r1 r2
       | _ -> compare scope1 scope2 )
    (TypeExt.compare hypothesis_compare_chain)
    (h1.hypo_scope, h1.hypo_kind)
    (h2.hypo_scope, h2.hypo_kind)


(** {1 Alarms report} *)
(** ***************** *)

module RangeMap = MapExt.Make(struct type t = range let compare = compare_range end)

module CheckMap = MapExt.Make(struct type t = check let compare = compare end)

module HypothesisSet = SetExt.Make(struct type t = hypothesis let compare = compare_hypothesis end)

type report = {
  report_diagnostics : diagnostic CheckMap.t RangeMap.t;
  report_soundness   : HypothesisSet.t;
}

let empty_report = {
  report_diagnostics = RangeMap.empty;
  report_soundness = HypothesisSet.empty;
}

let is_empty_report r = RangeMap.is_empty r.report_diagnostics

let is_safe_report r =
  RangeMap.for_all
    (fun range checks ->
       CheckMap.for_all
         (fun check diag ->
            match diag.diag_status with
            | Safe  | Unreachable  -> true
            | Error | Warning      -> false
         ) checks
    ) r.report_diagnostics

let is_sound_report r =
  HypothesisSet.is_empty r.report_soundness

let singleton_report alarm =
  let diag = mk_diagnostic alarm.alarm_range (check_of_alarm alarm) Error (AlarmSet.singleton alarm) in
  let check = check_of_alarm alarm in
  { report_diagnostics = RangeMap.singleton alarm.alarm_range (CheckMap.singleton check diag);
    report_soundness = HypothesisSet.empty; }

let pp_report fmt r =
  fprintf fmt "@[<v>soundness hypotheis: @[<v>%a@]@,%a@]"
    (HypothesisSet.fprint
       SetExtSig.{ print_empty = "∅";
                   print_begin = "";
                   print_sep   = "@,";
                   print_end   = ""; }
       pp_hypothesis
    ) r.report_soundness
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
       (fun fmt (range,checks) ->
          pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
            (fun fmt (check,diag) ->
               fprintf fmt "@[<v2>%a:@,@[%a@]@]"
                 pp_relative_range range
                 pp_diagnostic diag
            ) fmt (CheckMap.bindings checks)
       )
    ) (RangeMap.bindings r.report_diagnostics)

let subset_report r1 r2 =
  (r1 == r2)
  || (HypothesisSet.equal r1.report_soundness r2.report_soundness
      && (r1.report_diagnostics == r2.report_diagnostics
          || (RangeMap.for_all2zo
                (fun _ checks1 -> false)
                (fun _ checks2 -> true)
                (fun _ checks1 checks2 ->
                   CheckMap.for_all2zo
                     (fun _ diag1 -> diag1.diag_status == Unreachable)
                     (fun _ diag2 -> true)
                     (fun _ diag1 diag2 -> subset_diagnostic diag1 diag2)
                     checks1 checks2
                )
                r1.report_diagnostics r2.report_diagnostics)))

let join_report r1 r2 =
  if r1 == r2 then r1 else
  if subset_report r1 r2 then r2 else
  if subset_report r2 r1 then r1
  else
    { report_soundness = HypothesisSet.union r1.report_soundness r2.report_soundness;
      report_diagnostics =
        RangeMap.map2zo
          (fun _ checks1 -> checks1)
          (fun _ checks2 -> checks2)
          (fun _ checks1 checks2 ->
             CheckMap.map2zo
               (fun _ diag1 -> diag1)
               (fun _ diag2 -> diag2)
               (fun _ diag1 diag2 -> join_diagnostic diag1 diag2)
               checks1 checks2
          )
          r1.report_diagnostics r2.report_diagnostics; }

let meet_report r1 r2 =
  if r1 == r2 then r1 else
  if subset_report r1 r2 then r1 else
  if subset_report r2 r1 then r2
  else
    { report_soundness = HypothesisSet.union r1.report_soundness r2.report_soundness;
      report_diagnostics =
        RangeMap.fold2zo
          (fun range _ acc -> RangeMap.remove range acc)
          (fun range _ acc -> RangeMap.remove range acc)
          (fun range checks1 checks2 acc ->
             let checks =
               CheckMap.fold2zo
                 (fun check diag1 acc -> CheckMap.remove check acc)
                 (fun check diag2 acc -> CheckMap.remove check acc)
                 (fun check diag1 diag2 acc -> CheckMap.add check (meet_diagnostic diag1 diag2) acc)
                 checks1 checks2 checks1 in
             RangeMap.add range checks acc
          )
          r1.report_diagnostics r2.report_diagnostics r1.report_diagnostics }

let count_alarms r =
  RangeMap.fold
    (fun range checks acc ->
       CheckMap.fold
         (fun check diag (errors,warnings) ->
            match diag.diag_status with
            | Error              -> errors + 1, warnings
            | Warning            -> errors, warnings + 1
            | Safe | Unreachable -> errors, warnings
         ) checks acc
    ) r.report_diagnostics (0,0)

let add_alarm alarm r =
  let check = alarm.alarm_check in
  let range = alarm.alarm_range in
  let checks = try RangeMap.find range r.report_diagnostics with Not_found -> CheckMap.empty in
  let diag = try CheckMap.find check checks with Not_found -> mk_diagnostic range check Unreachable AlarmSet.empty in
  let diag' = add_alarm_to_diagnostic alarm diag in
  { r with report_diagnostics = RangeMap.add alarm.alarm_range (CheckMap.add check diag' checks) r.report_diagnostics }

let map2zo_report f1 f2 f r1 r2 =
  { report_soundness = HypothesisSet.union r1.report_soundness r2.report_soundness;
    report_diagnostics =
      RangeMap.map2zo
        (fun range -> CheckMap.map f1)
        (fun range -> CheckMap.map f2)
        (fun range -> CheckMap.map2zo (fun _ -> f1) (fun _ -> f2) (fun _ -> f))
        r1.report_diagnostics r2.report_diagnostics }

let fold2zo_report f1 f2 f r1 r2 init =
  RangeMap.fold2zo
    (fun range checks1 acc -> CheckMap.fold (fun check -> f1) checks1 acc)
    (fun range checks2 acc -> CheckMap.fold (fun check -> f2) checks2 acc)
    (fun range checks1 checks2 acc ->
       CheckMap.fold2zo
         (fun check diag1 acc -> f1 diag1 acc)
         (fun check diag2 acc -> f2 diag2 acc)
         (fun check diag1 diag2 acc -> f diag1 diag2 acc)
         checks1 checks2 acc
    )
    r1.report_diagnostics r2.report_diagnostics init

let alarms_of_report r =
  RangeMap.fold
    (fun range checks acc -> CheckMap.fold (fun check diag acc -> AlarmSet.union acc diag.diag_alarms) checks acc)
    r.report_diagnostics AlarmSet.empty

let group_alarms_by_range s =
  AlarmSet.fold
    (fun a acc ->
       let range = a.alarm_range in
       let s = try RangeMap.find range acc |> AlarmSet.add a with Not_found -> AlarmSet.singleton a in
       RangeMap.add range s acc
    ) s RangeMap.empty

let group_alarms_by_check s =
  AlarmSet.fold
    (fun a acc ->
       let check = check_of_alarm a in
       let s = try CheckMap.find check acc |> AlarmSet.add a with Not_found -> AlarmSet.singleton a in
       CheckMap.add check s acc
    ) s CheckMap.empty

let find_diagnostic range check report =
  try
    RangeMap.find range report.report_diagnostics |>
    CheckMap.find check
  with Not_found ->
    mk_diagnostic range check Unreachable AlarmSet.empty

let set_diagnostic diag report =
  let checks = try RangeMap.find diag.diag_range report.report_diagnostics with Not_found -> CheckMap.empty in
  let old_diag = try CheckMap.find diag.diag_check checks with Not_found -> mk_diagnostic diag.diag_range diag.diag_check Unreachable AlarmSet.empty in
  if old_diag == diag then report
  else { report with report_diagnostics = RangeMap.add diag.diag_range (CheckMap.add diag.diag_check diag checks) report.report_diagnostics }

let add_diagnostic diag report =
  let checks = try RangeMap.find diag.diag_range report.report_diagnostics with Not_found -> CheckMap.empty in
  let old_diag = try CheckMap.find diag.diag_check checks with Not_found -> mk_diagnostic diag.diag_range diag.diag_check Unreachable AlarmSet.empty in
  let diag' = join_diagnostic diag old_diag in
  if old_diag == diag' then report
  else { report with report_diagnostics = RangeMap.add diag.diag_range (CheckMap.add diag.diag_check diag' checks) report.report_diagnostics }

let remove_diagnostic range check report =
  try
    let checks = RangeMap.find range report.report_diagnostics in
    let checks' = CheckMap.remove check checks in
    if checks == checks' then report
    else { report with report_diagnostics = RangeMap.add range checks' report.report_diagnostics }
  with Not_found ->
    report

let exists_report f report =
  RangeMap.exists
    (fun range -> CheckMap.exists (fun _ diag -> f diag))
    report.report_diagnostics

let forall_report f report =
  RangeMap.exists
    (fun range -> CheckMap.exists (fun _ diag -> f diag))
    report.report_diagnostics

let filter_report f report =
  let diags =
    RangeMap.fold
      (fun range checks acc ->
         let checks' = CheckMap.filter (fun _ diag -> f diag) checks in
         if checks == checks' then acc else
         if CheckMap.is_empty checks' then RangeMap.remove range acc
         else RangeMap.add range checks' acc)
      report.report_diagnostics report.report_diagnostics
  in
  if diags = report.report_diagnostics then report else { report with report_diagnostics = diags }
