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

(** Alarms are potential bugs in the target program. They are reported
    by abstract domains during the analysis. *)

open Location



(** {2 Alarms classes} *)
(** ****************** *)

type alarm_class = ..


let pp_alarm_class_chain = TypeExt.mk_print_chain (fun fmt alarm -> failwith "Pp: Unknown alarm class")


let pp_alarm_class = TypeExt.print pp_alarm_class_chain


let register_alarm_class pp = TypeExt.register_print pp pp_alarm_class_chain



(** {2 Alarm messages} *)
(** ****************** *)

type alarm_message = ..

type alarm_classifier = (alarm_message -> alarm_class) -> alarm_message -> alarm_class

let compare_alarm_message_chain = TypeExt.mk_compare_chain compare

let compare_alarm_message = TypeExt.compare compare_alarm_message_chain

let pp_grouped_alarm_message_chain : (Format.formatter -> alarm_message list -> alarm_class -> unit) ref =
  ref (fun fmt messages cls -> raise Not_found)

let pp_alarm_message_chain : alarm_message TypeExt.print_chain =
  TypeExt.mk_print_chain (fun fmt alarm -> raise Not_found)

let pp_grouped_alarm_message cls fmt messages =
  try
    !pp_grouped_alarm_message_chain fmt messages cls
  with Not_found ->
    Format.(fprintf fmt "@[<v>%a@]"
              (pp_print_list
                 ~pp_sep:(fun fmt () -> fprintf fmt "@,")
                 (TypeExt.print pp_alarm_message_chain)
              ) messages
           )


let alarm_classifer_chain : (alarm_message -> alarm_class) ref = ref (fun _ -> failwith "classifier: unknown alarm message")

let classify_alarm_message d = !alarm_classifer_chain d

let register_alarm_classifier c = alarm_classifer_chain := c !alarm_classifer_chain

type grouped_alarm_message_info = {
  classifier: alarm_classifier;
  compare : alarm_message TypeExt.compare;
  print : (Format.formatter -> alarm_message list -> alarm_class -> unit) -> Format.formatter -> alarm_message list -> alarm_class -> unit;
}

type alarm_message_info = {
  classifier: alarm_classifier;
  compare : alarm_message TypeExt.compare;
  print : alarm_message TypeExt.print;
}

let register_grouped_alarm_message (info:grouped_alarm_message_info) =
  register_alarm_classifier info.classifier;
  TypeExt.register_compare info.compare compare_alarm_message_chain;
  pp_grouped_alarm_message_chain := info.print !pp_grouped_alarm_message_chain
  
  
let register_alarm_message (info:alarm_message_info) =
  register_alarm_classifier info.classifier;
  TypeExt.register_compare info.compare compare_alarm_message_chain;
  TypeExt.register_print info.print pp_alarm_message_chain;



(** {2 Alarm instance} *)
(** ****************** *)


  (** Alarm instance *)
type alarm = {
  alarm_message : alarm_message;
  alarm_range : range;
  alarm_callstack : Callstack.cs;
}


let mk_alarm message cs range =
  {
    alarm_message = message;
    alarm_range = range;
    alarm_callstack= cs;
  }


let get_alarm_class alarm = classify_alarm_message alarm.alarm_message

let get_alarm_message alarm = alarm.alarm_message

let get_alarm_callstack alarm = alarm.alarm_callstack

let get_alarm_range alarm = alarm.alarm_range

let compare_alarm a1 a2 =
  if a1 == a2 then 0
  else Compare.compose [
      (fun () -> compare_alarm_message a1.alarm_message a2.alarm_message);
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



(** {2 Sets of alarms} *)
(** ****************** *)

module AlarmSet =
struct

  include SetExt.Make(struct
      type t = alarm
      let compare = compare_alarm
    end)

  (** Intersection of alarms. We do not take into account the alarm
      message. This is important in reduced products in order to keep
      alarms that have the same class but differ in the reported
      details *)
  let inter s1 s2 =
    let weak_mem a s = exists (fun a' -> compare_alarm_by_class a a' = 0) s in
    fold2
      (fun a1 acc -> if weak_mem a1 s2 then add a1 acc else acc)
      (fun a2 acc -> if weak_mem a2 s1 then add a2 acc else acc)
      (fun a acc -> add a acc)
      s1 s2 empty    

end



(** {2 Alarm indexation} *)
(** ******************** *)

module type SETMAP =
sig
  type k
  type t
  val of_set : AlarmSet.t -> t
  val cardinal : t -> int
  val bindings : t -> (k*AlarmSet.t) list
  val fold : (k -> AlarmSet.t -> 'a -> 'a) -> t -> 'a -> 'a
end

module MakeSetMap(K:sig type t val compare : t -> t -> int val from_alarm : alarm -> t end) : SETMAP with type k = K.t =
struct

  module Map = MapExt.Make(K)

  type k = K.t
  type t = AlarmSet.t Map.t

  let add alarm map =
    let k = K.from_alarm alarm in
    Map.add k (
      try
        let old = Map.find k map in
        AlarmSet.add alarm old
      with Not_found -> AlarmSet.singleton alarm
    ) map

  let of_set s = 
    AlarmSet.fold add s Map.empty

  let singleton alarm =
    let k = K.from_alarm alarm in
    Map.singleton k (AlarmSet.singleton alarm)

  let cardinal = Map.cardinal

  let bindings = Map.bindings

  let fold = Map.fold

end

module RangeMap = MakeSetMap(struct type t = range let compare = compare_range let from_alarm = get_alarm_range end)
module ClassMap = MakeSetMap(struct type t = alarm_class let compare = compare let from_alarm = get_alarm_class end)


let index_alarm_set_by_range (s:AlarmSet.t) : RangeMap.t =
  RangeMap.of_set s


let index_alarm_set_by_class (s:AlarmSet.t) : ClassMap.t =
  ClassMap.of_set s

let count_alarms s =
  if AlarmSet.is_empty s then 0
  else
    let cls_map = index_alarm_set_by_class s in
    ClassMap.fold (fun cls ss acc ->
        let range_map = index_alarm_set_by_range ss in
        let sub_total = RangeMap.cardinal range_map in
        sub_total + acc
      ) cls_map 0
