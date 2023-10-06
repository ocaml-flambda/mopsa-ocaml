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

open Mopsa_utils
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

type alarm_kind += A_generic of check

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
       | A_generic chk1, A_generic chk2 -> compare chk1 chk2
       | _ -> compare a1 a2)

let alarm_print_chain : alarm_kind TypeExt.print_chain =
  TypeExt.mk_print_chain
    (fun fmt -> function
       | A_generic chk -> pp_check fmt chk
       | _ -> Exceptions.panic "Print of unregistered alarm")

let alarm_check_chain : (alarm_kind -> check) ref =
  ref (function
      | A_generic chk -> chk
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

type diagnostic_kind =
  | Warning
  | Safe
  | Error
  | Unreachable

type 'a diagnostic_ = {
  diag_range : range;
  diag_check : check;
  diag_kind : diagnostic_kind;
  diag_alarms : AlarmSet.t;
  diag_callstack : 'a;
}

type diagnostic = callstack diagnostic_
type diagnosticWoCs = unit diagnostic_

let mk_safe_diagnostic check callstack range =
  { diag_range = range;
    diag_check = check;
    diag_kind = Safe;
    diag_alarms = AlarmSet.empty;
    diag_callstack = callstack }

let mk_unreachable_diagnostic check callstack range =
  { diag_range = range;
    diag_check = check;
    diag_kind = Unreachable;
    diag_alarms = AlarmSet.empty;
    diag_callstack = callstack }

let mk_error_diagnostic alarm =
  { diag_range = alarm.alarm_range;
    diag_check = alarm.alarm_check;
    diag_kind = Error;
    diag_alarms = AlarmSet.singleton alarm;
    diag_callstack = alarm.alarm_callstack }

let mk_warning_diagnostic check callstack range =
  { diag_range = range;
    diag_check = check;
    diag_kind = Warning;
    diag_alarms = AlarmSet.empty;
    diag_callstack = callstack }

let pp_diagnostic_kind fmt = function
  | Warning   -> pp_print_string fmt "warning"
  | Safe      -> pp_print_string fmt "safe"
  | Error     -> pp_print_string fmt "error"
  | Unreachable -> pp_print_string fmt "unreachable"

let pp_diagnostic fmt d =
  match d.diag_kind with
  | Safe | Unreachable ->
    pp_diagnostic_kind fmt d.diag_kind
  | Error | Warning ->
    fprintf fmt "%a: %a"
      pp_diagnostic_kind d.diag_kind
      pp_alarm_set d.diag_alarms

let subset_diagnostic_kind s1 s2 =
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
  subset_diagnostic_kind d1.diag_kind d2.diag_kind
  && AlarmSet.subset d1.diag_alarms d2.diag_alarms
  && compare_callstack d1.diag_callstack d2.diag_callstack = 0

let join_diagnostic_kind s1 s2 =
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
    assert(compare_callstack d1.diag_callstack d2.diag_callstack = 0);
    { diag_range = d1.diag_range;
      diag_check = d1.diag_check;
      diag_kind = join_diagnostic_kind d1.diag_kind d2.diag_kind;
      diag_alarms = AlarmSet.union d1.diag_alarms d2.diag_alarms;
      diag_callstack = d1.diag_callstack;
    }
  )

let meet_diagnostic_kind s1 s2 =
  if s1 == s2 then s1 else
  match s1,s2 with
  | Unreachable, _ | _, Unreachable  -> Unreachable
  | Warning, x     | x, Warning      -> x
  | Error, Error                     -> Error
  | Safe, Safe                       -> Safe
  | Error, Safe    | Safe, Error     -> Exceptions.panic "unsound intersection of diagnostics"

let meet_diagnostic d1 d2 =
  if d1 == d2 then d1 else
  if subset_diagnostic d1 d2 then d1 else
  if subset_diagnostic d2 d1 then d2
  else (
    assert(compare_range d1.diag_range d2.diag_range = 0);
    assert(compare_check d1.diag_check d2.diag_check = 0);
    assert(compare_callstack d1.diag_callstack d2.diag_callstack = 0);
    (* It is currently unsound for one domain to remove alarms raised by
       another domain during reduction, because a "safe" check in a domain
       may correspond to a previous instance of the check at the same range
       and there is actually no information for the current instance in
       the domain.
       The current fix is to perform a union instead of an intersection of
       alarms during reduction. It is sound but imprecise.
       See MR!51 for more details.
       FIXME: implement a better fix, performing intersections when sound
     *)
    { diag_range = d1.diag_range;
      diag_check = d1.diag_check;
      diag_kind = meet_diagnostic_kind d1.diag_kind d2.diag_kind;
      diag_alarms = AlarmSet.union d1.diag_alarms d2.diag_alarms;
      diag_callstack = d1.diag_callstack
    }
  )

let add_alarm_to_diagnostic a d =
  assert(compare_callstack d.diag_callstack a.alarm_callstack = 0);
  match d.diag_kind with
  | Error ->
    { d with diag_alarms = AlarmSet.add a d.diag_alarms; }

  | Warning ->
    { d with diag_alarms = AlarmSet.add a d.diag_alarms; }

  | Safe ->
    { d with diag_kind = Error;
             diag_alarms = AlarmSet.singleton a;
    }

  | Unreachable ->
    { d with diag_kind = Error;
             diag_alarms = AlarmSet.singleton a;
    }

let compare_diagnostic d1 d2 =
  Compare.quadruple
    compare_range compare_check compare (Compare.pair AlarmSet.compare compare_callstack)
    (d1.diag_range,d1.diag_check,d1.diag_kind,(d1.diag_alarms,d1.diag_callstack))
    (d2.diag_range,d2.diag_check,d2.diag_kind,(d2.diag_alarms,d2.diag_callstack))

(** {1 Soundness assumption} *)
(** ************************ *)

type assumption_scope =
  | A_local of range
  | A_global

type assumption_kind = ..

type assumption_kind += A_ignore_unsupported_stmt of Ast.Stmt.stmt
type assumption_kind += A_ignore_unsupported_expr of Ast.Expr.expr

type assumption = {
  assumption_scope : assumption_scope;
  assumption_kind  : assumption_kind;
}

let assumption_compare_chain : assumption_kind TypeExt.compare_chain =
  TypeExt.mk_compare_chain
    (fun a1 a2 ->
       match a1,a2 with
       | A_ignore_unsupported_stmt s1, A_ignore_unsupported_stmt s2 ->
         Ast.Stmt.compare_stmt s1 s2

       | A_ignore_unsupported_expr e1, A_ignore_unsupported_expr e2 ->
         Ast.Expr.compare_expr e1 e2

       | _ -> compare a1 a2
    )


let assumption_print_chain : assumption_kind TypeExt.print_chain =
  TypeExt.mk_print_chain
    (fun fmt -> function
       | A_ignore_unsupported_stmt s ->
         Format.fprintf fmt "ignoring unsupported statement '%a'"
           (Debug.bold Ast.Stmt.pp_stmt) s

       | A_ignore_unsupported_expr e ->
         Format.fprintf fmt "ignoring unsupported expression '%a'"
           (Debug.bold Ast.Expr.pp_expr) e

       | _ -> Exceptions.panic "Print of unregistered assumption"
    )

let register_assumption info =
  TypeExt.register info assumption_compare_chain assumption_print_chain

let pp_assumption_kind fmt h =
  TypeExt.print assumption_print_chain fmt h

let pp_assumption fmt h =
  match h.assumption_scope with
  | A_global      -> pp_assumption_kind fmt h.assumption_kind
  | A_local range -> fprintf fmt "%a: %a"
                       pp_relative_range range
                       pp_assumption_kind h.assumption_kind

let compare_assumption_kind h1 h2 =
  TypeExt.compare assumption_compare_chain h1 h2

let compare_assumption h1 h2 =
  Compare.pair
    (fun scope1 scope2 ->
       match scope1, scope2 with
       | A_global, A_global     -> 0
       | A_local r1, A_local r2 -> compare_range r1 r2
       | _ -> compare scope1 scope2 )
    compare_assumption_kind
    (h1.assumption_scope, h1.assumption_kind)
    (h2.assumption_scope, h2.assumption_kind)

let mk_global_assumption kind =
  { assumption_scope = A_global;
    assumption_kind = kind }

let mk_local_assumption kind range =
  { assumption_scope = A_local range;
    assumption_kind = kind }

(** {1 Alarms report} *)
(** ***************** *)

module RangeMap = MapExt.Make(struct
    type t = range
    let compare = compare_range
  end)

module RangeCallStackMap = MapExt.Make(struct type t = range * callstack
    let compare = Compare.pair compare_range compare_callstack end)

module CheckMap = MapExt.Make(struct type t = check let compare = compare end)

module AssumptionSet = SetExt.Make(struct type t = assumption let compare = compare_assumption end)

type report = {
  report_diagnostics : diagnostic CheckMap.t RangeCallStackMap.t;
  report_assumptions : AssumptionSet.t;
}

let empty_report = {
  report_diagnostics = RangeCallStackMap.empty;
  report_assumptions = AssumptionSet.empty;
}

let is_empty_report r = RangeCallStackMap.is_empty r.report_diagnostics

let is_safe_report r =
  RangeCallStackMap.for_all
    (fun range checks ->
       CheckMap.for_all
         (fun check diag ->
            match diag.diag_kind with
            | Safe  | Unreachable  -> true
            | Error | Warning      -> false
         ) checks
    ) r.report_diagnostics

let is_sound_report r =
  AssumptionSet.is_empty r.report_assumptions

let singleton_report alarm =
  let diag = mk_error_diagnostic alarm in
  let checks = CheckMap.singleton alarm.alarm_check diag in
  { report_diagnostics = RangeCallStackMap.singleton (alarm.alarm_range, alarm.alarm_callstack) checks;
    report_assumptions = AssumptionSet.empty; }

let pp_report fmt r =
  fprintf fmt "@[<v>assumptions: @[<v>%a@]@,%a@]"
    (AssumptionSet.fprint
       SetExtSig.{ print_empty = "∅";
                   print_begin = "";
                   print_sep   = "@,";
                   print_end   = ""; }
       pp_assumption
    ) r.report_assumptions
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
       (fun fmt ((range, cs),checks) ->
          pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
            (fun fmt (check,diag) ->
               fprintf fmt "@[<hov2>%a(%a): %a:@ @[%a@]@]"
                 pp_relative_range range
                 pp_callstack_short cs
                 pp_check check
                 pp_diagnostic diag
            ) fmt (CheckMap.bindings checks)
       )
    ) (RangeCallStackMap.bindings r.report_diagnostics)

let subset_report r1 r2 =
  (r1 == r2)
  || (AssumptionSet.equal r1.report_assumptions r2.report_assumptions
      && (r1.report_diagnostics == r2.report_diagnostics
          || (RangeCallStackMap.for_all2zo
                (fun _ checks1 -> false)
                (fun _ checks2 -> true)
                (fun _ checks1 checks2 ->
                   CheckMap.for_all2zo
                     (fun _ diag1 -> diag1.diag_kind == Unreachable)
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
    { report_assumptions = AssumptionSet.union r1.report_assumptions r2.report_assumptions;
      report_diagnostics =
        RangeCallStackMap.map2zo
          (fun range checks1 -> checks1)
          (fun range checks2 -> checks2)
          (fun range checks1 checks2 ->
             CheckMap.map2zo
               (fun check diag1 -> diag1)
               (fun check diag2 -> diag2)
               (fun _ diag1 diag2 -> join_diagnostic diag1 diag2)
               checks1 checks2
          )
          r1.report_diagnostics r2.report_diagnostics; }

let meet_report r1 r2 =
  if r1 == r2 then r1 else
  if subset_report r1 r2 then r1 else
  if subset_report r2 r1 then r2
  else
    { report_assumptions = AssumptionSet.union r1.report_assumptions r2.report_assumptions;
      report_diagnostics =
        RangeCallStackMap.fold2zo
          (fun (range, _) _ acc -> Exceptions.panic_at range "alarms: unsound meet")
          (fun (range, _) _ acc -> Exceptions.panic_at range "alarms: unsound meet")
          (fun (range, cs) checks1 checks2 acc ->
             let checks =
               CheckMap.fold2zo
                 (fun check diag1 acc -> Exceptions.panic_at range "alarms: unsound meet for check '%a'" pp_check check)
                 (fun check diag2 acc -> Exceptions.panic_at range "alarms: unsound meet for check '%a'" pp_check check)
                 (fun check diag1 diag2 acc -> CheckMap.add check (meet_diagnostic diag1 diag2) acc)
                 checks1 checks2 checks1 in
             RangeCallStackMap.add (range, cs) checks acc
          )
          r1.report_diagnostics r2.report_diagnostics r1.report_diagnostics }

let count_alarms r =
  RangeCallStackMap.fold
    (fun range checks acc ->
       CheckMap.fold
         (fun check diag (errors,warnings) ->
            match diag.diag_kind with
            | Error              -> errors + 1, warnings
            | Warning            -> errors, warnings + 1
            | Safe | Unreachable -> errors, warnings
         ) checks acc
    ) r.report_diagnostics (0,0)

let add_alarm ?(warning=false) alarm r =
  let check = alarm.alarm_check in
  let range = alarm.alarm_range in
  let callstack = alarm.alarm_callstack in
  let checks = try RangeCallStackMap.find (range, alarm.alarm_callstack) r.report_diagnostics with Not_found -> CheckMap.empty in
  let diag = try CheckMap.find check checks with Not_found -> mk_unreachable_diagnostic check callstack range in
  let diag' = add_alarm_to_diagnostic alarm diag in
  let diag'' = if warning && diag'.diag_kind = Error then { diag' with diag_kind = Warning } else diag' in
  { r with report_diagnostics = RangeCallStackMap.add (alarm.alarm_range, alarm.alarm_callstack) (CheckMap.add check diag'' checks) r.report_diagnostics }

let map2zo_report f1 f2 f r1 r2 =
  { report_assumptions = AssumptionSet.union r1.report_assumptions r2.report_assumptions;
    report_diagnostics =
      RangeCallStackMap.map2zo
        (fun range -> CheckMap.map f1)
        (fun range -> CheckMap.map f2)
        (fun range -> CheckMap.map2zo (fun _ -> f1) (fun _ -> f2) (fun _ -> f))
        r1.report_diagnostics r2.report_diagnostics }

let fold2zo_report f1 f2 f r1 r2 init =
  RangeCallStackMap.fold2zo
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

module CallstackSet = SetExt.Make(struct type t = callstack let compare = compare_callstack end)

module RangeDiagnosticWoCsMap = MapExt.Make(
  struct
    type t = range * diagnosticWoCs
    let compare (r1, d1) (r2, d2) =
      Compare.pair
        compare_range 
        (Compare.quadruple
           compare_range compare_check compare AlarmSet.compare)
        (r1, (d1.diag_range,d1.diag_check,d1.diag_kind,d1.diag_alarms))
        (r2, (d2.diag_range,d2.diag_check,d2.diag_kind,d2.diag_alarms))
  end)

let group_diagnostics (rangecs: diagnostic CheckMap.t RangeCallStackMap.t)
  : CallstackSet.t RangeDiagnosticWoCsMap.t =
  RangeCallStackMap.fold (fun (range, cs) cm acc ->
      CheckMap.fold (fun check diagnostic acc ->
          let dWoCs = {diagnostic with diag_callstack = ()} in
          let css =
            try RangeDiagnosticWoCsMap.find (range, dWoCs) acc
            with Not_found -> CallstackSet.empty in
          RangeDiagnosticWoCsMap.add (range, dWoCs) (CallstackSet.add diagnostic.diag_callstack css) acc  
        ) cm acc
    )
    rangecs RangeDiagnosticWoCsMap.empty 

let find_diagnostic rangecs check report =
  RangeCallStackMap.find rangecs report.report_diagnostics |>
  CheckMap.find check

let set_diagnostic diag report =
  let checks =
    try RangeCallStackMap.find (diag.diag_range, diag.diag_callstack) report.report_diagnostics
    with Not_found -> CheckMap.empty
  in
  let checks' = CheckMap.add diag.diag_check diag checks in
  if checks == checks' then report
  else { report with
         report_diagnostics = RangeCallStackMap.add (diag.diag_range, diag.diag_callstack) checks' report.report_diagnostics }

let add_diagnostic diag report =
  let checks =
    try RangeCallStackMap.find (diag.diag_range, diag.diag_callstack) report.report_diagnostics
    with Not_found -> CheckMap.empty in
  try
    let old_diag = CheckMap.find diag.diag_check checks in
    let diag' = join_diagnostic diag old_diag in
    if old_diag == diag' then report
    else
      let checks' = CheckMap.add diag.diag_check diag' checks in
      { report with
        report_diagnostics = RangeCallStackMap.add (diag.diag_range, diag.diag_callstack) checks' report.report_diagnostics }
  with Not_found ->
    let checks' = CheckMap.add diag.diag_check diag checks in
    { report with
      report_diagnostics = RangeCallStackMap.add (diag.diag_range, diag.diag_callstack) checks' report.report_diagnostics }

let remove_diagnostic diag report =
  try
    let checks = RangeCallStackMap.find (diag.diag_range, diag.diag_callstack) report.report_diagnostics in
    let checks' = CheckMap.remove diag.diag_check checks in
    if checks == checks' then report
    else { report with
           report_diagnostics = RangeCallStackMap.add (diag.diag_range, diag.diag_callstack) checks' report.report_diagnostics }
  with Not_found ->
    report

let exists_report f report =
  RangeCallStackMap.exists
    (fun range -> CheckMap.exists (fun _ diag -> f diag))
    report.report_diagnostics

let forall_report f report =
  RangeCallStackMap.exists
    (fun range -> CheckMap.exists (fun _ diag -> f diag))
    report.report_diagnostics

let filter_report f report =
  let diags =
    RangeCallStackMap.fold
      (fun range checks acc ->
         let checks' = CheckMap.filter (fun _ diag -> f diag) checks in
         if checks == checks' then acc else
         if CheckMap.is_empty checks' then RangeCallStackMap.remove range acc
         else RangeCallStackMap.add range checks' acc)
      report.report_diagnostics report.report_diagnostics
  in
  if diags = report.report_diagnostics then report else { report with report_diagnostics = diags }

let add_assumption a report =
  let assumptions = AssumptionSet.add a report.report_assumptions in
  if assumptions == report.report_assumptions then report
  else { report with report_assumptions = assumptions }

let add_global_assumption kind report =
  let a = mk_global_assumption kind in
  add_assumption a report

let add_local_assumption kind range report =
  let a = mk_local_assumption kind range in
  add_assumption a report
