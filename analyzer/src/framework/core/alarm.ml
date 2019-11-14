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


(** {2 Alarm data structure} *)
(** ************************ *)

type alarm_category = ..

type alarm_detail = ..

type alarm_detail += EmptyDetail

type alarm = {
  alarm_category : alarm_category;
  alarm_detail : alarm_detail;
  alarm_trace : range * Callstack.cs;
}


let mk_alarm cat ?(detail=EmptyDetail) ?(cs=Callstack.empty) range =
  {
    alarm_category = cat;
    alarm_detail = detail;
    alarm_trace = range, cs;
  }


let get_alarm_category alarm = alarm.alarm_category

let get_alarm_detail alarm = alarm.alarm_detail

let get_alarm_trace alarm = alarm.alarm_trace

let get_alarm_callstack alarm = get_alarm_trace alarm |> snd

let get_alarm_range alarm = fst @@ get_alarm_trace alarm


(** {2 Total order comparison function} *)
(** *********************************** *)

let compare_alarm_category_chain = TypeExt.mk_compare_chain (fun a1 a2 -> compare a1 a2)

let compare_alarm_category = TypeExt.compare compare_alarm_category_chain

let compare_alarm_detail_chain = TypeExt.mk_compare_chain (fun a1 a2 ->
    match a1, a2 with
    | EmptyDetail, EmptyDetail -> 0
    | _ -> compare a1 a2
  )

let compare_alarm_detail = TypeExt.compare compare_alarm_detail_chain

let compare_alarm a1 a2 =
  if a1 == a2 then 0
  else Compare.compose [
      (fun () -> compare_alarm_category a1.alarm_category a2.alarm_category);
      (fun () -> compare_alarm_detail a1.alarm_detail a2.alarm_detail);
      (fun () -> Compare.pair compare_range Callstack.compare a1.alarm_trace a2.alarm_trace);
    ]

let weak_compare_alarm a1 a2 =
  if a1 == a2 then 0
  else Compare.compose [
      (fun () -> compare_alarm_category a1.alarm_category a2.alarm_category);
      (fun () -> Compare.pair compare_range Callstack.compare a1.alarm_trace a2.alarm_trace);
    ]



(** {2 Pretty printers} *)
(** ******************* *)

let pp_category_chain = TypeExt.mk_print_chain (fun fmt alarm -> failwith "Pp: Unknown alarm")

let pp_alarm_category = TypeExt.print pp_category_chain

let pp_detail_chain = TypeExt.mk_print_chain (fun fmt alarm ->
    match alarm with
    | EmptyDetail -> ()
    | _ -> failwith "Pp: Unknown alarm"
  )

let pp_alarm_detail = TypeExt.print pp_detail_chain


let pp_callstack fmt (cs:Callstack.cs) =
  (* print in the style of gcc's preprocessor include stack *)
  List.iter
    (fun c -> Format.fprintf fmt "\tfrom %a: %s@\n" pp_range c.Callstack.call_site c.Callstack.call_fun)
    cs


let pp_alarm fmt alarm =
  (* print using a format recognized by emacs: location first *)
  Format.fprintf fmt "%a: %a %a@\n%a"
    pp_range (get_alarm_range alarm |> untag_range)
    ((Debug.color "red") Format.pp_print_string) "✘"
    (fun fmt -> function
       | EmptyDetail -> pp_alarm_category fmt alarm.alarm_category
       | x -> pp_alarm_detail fmt x
    ) alarm.alarm_detail
    pp_callstack (alarm.alarm_trace |> snd)


(** {2 Registration} *)
(** **************** *)


let register_alarm_category info =
  TypeExt.register info compare_alarm_category_chain pp_category_chain


let register_alarm_detail info =
  TypeExt.register info compare_alarm_detail_chain pp_detail_chain


(** {2 Sets of alarms} *)
(** ****************** *)

module AlarmSet =
struct

  include SetExt.Make(struct
      type t = alarm
      let compare = compare_alarm
    end)

  (** Intersection of alarms do not take into account the alarm details *)
  let inter s1 s2 =
    let weak_mem a s = exists (fun a' -> weak_compare_alarm a a' = 0) s in
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
                pp_alarm_detail (get_alarm_detail alarm)
                (pp_print_list
                   ~pp_sep:(fun fmt () -> fprintf fmt "@,")
                   (fun fmt c -> fprintf fmt "\tfrom %a: %s" pp_range c.Callstack.call_site c.Callstack.call_fun)
                ) (get_alarm_callstack alarm)
           )
           fmt (AlarmSet.elements alarms)
      )
      fmt l

end


(** {2 Maps from alarm categories to AlarmRangeIndexMap} *)
(** **************************************************** *)

module AlarmMap = struct

  module Map = MapExt.Make(struct
      type t = alarm_category
      let compare = compare_alarm_category
      let print = pp_alarm_category
    end
    )

  type t = AlarmRangeIndexMap.t Map.t

  let of_set s =
    AlarmSet.fold (fun alarm acc ->
        Map.add alarm.alarm_category (
          try
            let old = Map.find alarm.alarm_category acc in
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
           (fun fmt (category, alarms) ->
              let n = AlarmRangeIndexMap.cardinal alarms in
              if n = 0 then ()
              else
                fprintf fmt "@[<v 2>%a x %d:@,%a@]"
                  pp_alarm_category category
                  n
                  AlarmRangeIndexMap.print alarms
           )
        ) l


end
