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

(** Alarm -- Potential bugs inferred by abstract domains. *)

open Location



(** {2 Alarm classes} *)
(** ******************** *)

(** Alarm are categorized into a finite set of classes *)
type alarm_class = ..


(** Chain of pretty printers for alarm classes *)
let pp_alarm_class_chain = TypeExt.mk_print_chain (fun fmt alarm -> failwith "Pp: Unknown alarm class")


(** Pretty printer of alarm classes *)
let pp_alarm_class = TypeExt.print pp_alarm_class_chain


(** Register an new alarm class. There is no need for registering a
    compare function, since classes are simple variants without
    arguments. *)
let register_alarm_class pp = TypeExt.register_print pp pp_alarm_class_chain



(** {2 Alarm bodies} *)
(** **************** *)

(** Alarm body provides details about the context of the alarm, such
    as expression intervals, expected values, etc.*)
type alarm_body = ..


(** Chain of compare functions for alarm body  *)
let compare_alarm_body_chain = TypeExt.mk_compare_chain compare


(** Compare two alarm bodies *)
let compare_alarm_body = TypeExt.compare compare_alarm_body_chain


(** Chain of pretty printers of alarm bodies *)
let pp_alarm_body_chain = TypeExt.mk_print_chain (fun fmt alarm -> failwith "Pp: Unknown alarm body")


(** Pretty printer of alarm body *)
let pp_alarm_body = TypeExt.print pp_alarm_body_chain


(** Chaining function to get the class of an alarm body *)
type alarm_classifier = (alarm_body -> alarm_class) -> alarm_body -> alarm_class


(** Chain of alarm classifiers *)
let alarm_classifer_chain : (alarm_body -> alarm_class) ref = ref (fun _ -> failwith "classifier: unknown alarm body")


(** Get the class of an alarm body *)
let classify_alarm_body d = !alarm_classifer_chain d


(** Register an alarm classifer *)
let register_alarm_classifier c = alarm_classifer_chain := c !alarm_classifer_chain


(** Registration information of an alarm body *)
type alarm_body_info = {
  classifier: alarm_classifier;
  compare : alarm_body TypeExt.compare;
  print : alarm_body TypeExt.print;
}
  

(** Register a new alarm body *)
let register_alarm_body info =
  register_alarm_classifier info.classifier;
  TypeExt.register_compare info.compare compare_alarm_body_chain;
  TypeExt.register_print info.print pp_alarm_body_chain;



(** {2 Alarm instance} *)
(** ****************** *)


  (** Alarm instance *)
type alarm = {
  alarm_body : alarm_body;
  alarm_range : range;
  alarm_callstack : Callstack.cs;
}


let mk_alarm body ?(cs=Callstack.empty) range =
  {
    alarm_body = body;
    alarm_range = range;
    alarm_callstack= cs;
  }


let get_alarm_class alarm = classify_alarm_body alarm.alarm_body

let get_alarm_body alarm = alarm.alarm_body

let get_alarm_callstack alarm = alarm.alarm_callstack

let get_alarm_range alarm = alarm.alarm_range

let compare_alarm a1 a2 =
  if a1 == a2 then 0
  else Compare.compose [
      (fun () -> compare_alarm_body a1.alarm_body a2.alarm_body);
      (fun () -> compare_range a1.alarm_range a2.alarm_range);
      (fun () -> Callstack.compare a1.alarm_callstack a2.alarm_callstack);
    ]

let compare_alarm_by_class a1 a2 =
  if a1 == a2 then 0
  else Compare.compose [
      (fun () -> compare (get_alarm_class a1) (get_alarm_class a2));
      (fun () -> compare_range a1.alarm_range a2.alarm_range);
      (fun () -> Callstack.compare a1.alarm_callstack a2.alarm_callstack);
    ]

let pp_alarm fmt alarm =
  let open Format in
  fprintf fmt "@[<v>%a: %a@,%a@]@,"
    pp_range (get_alarm_range alarm)
    pp_alarm_body (get_alarm_body alarm)
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> fprintf fmt "@,")
       (fun fmt c -> fprintf fmt "\tfrom %a: %s" pp_range c.Callstack.call_site c.Callstack.call_fun)
    ) (get_alarm_callstack alarm)



(** {2 Sets of alarms} *)
(** ****************** *)

module AlarmSet =
struct

  include SetExt.Make(struct
      type t = alarm
      let compare = compare_alarm
    end)

  (** Intersection of alarms. We do not take into account the alarm body. *)
  let inter s1 s2 =
    let weak_mem a s = exists (fun a' -> compare_alarm_by_class a a' = 0) s in
    fold2
      (fun a1 acc -> if weak_mem a1 s2 then add a1 acc else acc)
      (fun a2 acc -> if weak_mem a2 s1 then add a2 acc else acc)
      (fun a acc -> add a acc)
      s1 s2 empty

end


(** {2 Maps from ranges to alarms} *)
(** ****************************** *)

module AlarmRangeIndexMap =
struct

  module Map = MapExt.Make(struct
      type t = range
      let compare = compare_range
    end)

  type t = AlarmSet.t Map.t

  let add alarm map =
    let range = get_alarm_range alarm |> untag_range in
    Map.add range (
      try
        let old = Map.find range map in
        AlarmSet.add alarm old
      with Not_found -> AlarmSet.singleton alarm
    ) map

  let of_set s = 
    AlarmSet.fold add s Map.empty

  let singleton alarm =
    let range = get_alarm_range alarm |> untag_range in
    Map.singleton range (AlarmSet.singleton alarm)

  let cardinal m = Map.cardinal m

  let print fmt m =
    let open Format in
    let l = Map.bindings m in
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt "@,")
      (fun fmt (range,alarms) ->
         pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt "@,")
           (fun fmt alarm ->
              fprintf fmt "@[<v>%a: %a@,%a@]@,"
                pp_range range
                pp_alarm_body (get_alarm_body alarm)
                (pp_print_list
                   ~pp_sep:(fun fmt () -> fprintf fmt "@,")
                   (fun fmt c -> fprintf fmt "\tfrom %a: %s" pp_range c.Callstack.call_site c.Callstack.call_fun)
                ) (get_alarm_callstack alarm)
           )
           fmt (AlarmSet.elements alarms)
      )
      fmt l

end


(** {2 Maps from alarm classes to AlarmRangeIndexMap} *)
(** **************************************************** *)

module AlarmMap = struct

  module Map = MapExt.Make(struct
      type t = alarm_class
      let compare = compare
      let print = pp_alarm_class
    end
    )

  type t = AlarmRangeIndexMap.t Map.t

  let of_set s =
    AlarmSet.fold (fun alarm acc ->
        let c = get_alarm_class alarm in
        Map.add c (
          try
            let old = Map.find c acc in
            AlarmRangeIndexMap.add alarm old
          with Not_found -> AlarmRangeIndexMap.singleton alarm
        ) acc
      ) s Map.empty

  let cardinal m =
    Map.fold (fun _ map2 acc ->
        AlarmRangeIndexMap.cardinal map2 + acc
      ) m 0

  let print fmt x =
    let open Format in
    if Map.is_empty x then pp_print_string fmt "∅"
    else
      let l = Map.bindings x in
      fprintf fmt "@[<v>%a@]"
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt "@,")
           (fun fmt (cls, alarms) ->
              let n = AlarmRangeIndexMap.cardinal alarms in
              if n = 0 then ()
              else
                fprintf fmt "@[<v 2>%a x %d:@,%a@]"
                  pp_alarm_class cls
                  n
                  AlarmRangeIndexMap.print alarms
           )
        ) l


end
