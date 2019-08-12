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



(** {2 Alarm data structure} *)
(** ************************ *)

type alarm_kind = ..

type alarm_extra = ..

type alarm_extra += NoExtra

type alarm_level =
  | ERROR
  | WARNING

type alarm = {
  alarm_kind : alarm_kind;
  alarm_extra : alarm_extra;
  alarm_level : alarm_level;
  alarm_trace : Location.range * Callstack.cs;
}


let mk_alarm kind ?(extra=NoExtra) ?(level=WARNING) ?(cs=Callstack.empty) range =
  {
    alarm_kind = kind;
    alarm_extra = extra;
    alarm_level = level;
    alarm_trace = range, cs;
  }


let get_alarm_kind alarm = alarm.alarm_kind

let get_alarm_extra alarm = alarm.alarm_extra

let get_alarm_trace alarm = alarm.alarm_trace

let get_alarm_level alarm = alarm.alarm_level


(** {2 Total order comparison function} *)
(** *********************************** *)

let compare_alarm_kind_chain = TypeExt.mk_compare_chain (fun a1 a2 -> compare a1 a2)

let compare_alarm_kind = TypeExt.compare compare_alarm_kind_chain

let compare_alarm_extra_chain = TypeExt.mk_compare_chain (fun a1 a2 ->
    match a1, a2 with
    | NoExtra, NoExtra -> 0
    | _ -> compare a1 a2
  )

let compare_alarm_extra = TypeExt.compare compare_alarm_extra_chain

let compare_alarm a1 a2 =
  if a1 == a2 then 0
  else Compare.compose [
      (fun () -> compare_alarm_kind a1.alarm_kind a2.alarm_kind);
      (fun () -> compare_alarm_extra a1.alarm_extra a2.alarm_extra);
      (fun () -> Compare.pair Location.compare_range Callstack.compare a1.alarm_trace a2.alarm_trace);
      (fun () -> Pervasives.compare a1.alarm_level a2.alarm_level);
    ]



(** {2 Pretty printers} *)
(** ******************* *)

let pp_kind_chain = TypeExt.mk_print_chain (fun fmt alarm -> failwith "Pp: Unknown alarm")

let pp_alarm_kind = TypeExt.print pp_kind_chain

let pp_extra_chain = TypeExt.mk_print_chain (fun fmt alarm ->
    match alarm with
    | NoExtra -> ()
    | _ -> failwith "Pp: Unknown alarm"
  )

let pp_alarm_extra = TypeExt.print pp_extra_chain


let pp_level fmt = function
  | ERROR -> ((Debug.color "red") Format.pp_print_string) fmt "✘"
  | WARNING -> ((Debug.color "orange") Format.pp_print_string) fmt "⚠"

let pp_callstack fmt (cs:Callstack.cs) =
  (* print in the style of gcc's preprocessor include stack *)
  List.iter
    (fun c -> Format.fprintf fmt "\tfrom %a: %s@\n" Location.pp_range c.Callstack.call_site c.Callstack.call_fun)
    cs


let pp_alarm fmt alarm =
  (* print using a format recognized by emacs: location first *)
  Format.fprintf fmt "%a: %a %a@\n%a"
    Location.pp_range (alarm.alarm_trace |> fst |> Location.untag_range)
    pp_level alarm.alarm_level
    (fun fmt -> function
       | NoExtra -> pp_alarm_kind fmt alarm.alarm_kind
       | x -> pp_alarm_extra fmt x
    ) alarm.alarm_extra
    pp_callstack (alarm.alarm_trace |> snd)


(** {2 Registration} *)
(** **************** *)


let register_alarm_kind info =
  TypeExt.register info compare_alarm_kind_chain pp_kind_chain


let register_alarm_extra info =
  TypeExt.register info compare_alarm_extra_chain pp_extra_chain



(** {2 Set of alarms} *)
(** ***************** *)

module AlarmSet = struct

  module Map = MapExt.Make(struct
      type t = alarm_kind
      let compare = compare_alarm_kind
      let print = pp_alarm_kind
    end
    )

  module Set = SetExt.Make(struct
      type t = alarm
      let compare = compare_alarm
    end)

  type t = Set.t Map.t

  let empty = Map.empty

  let is_empty x = Map.is_empty x

  let cardinal x =
    Map.fold (fun _ s acc -> acc + Set.cardinal s) x 0

  let elements x =
    Map.fold (fun _ s acc -> acc @ Set.elements s) x []

  let singleton alarm = Map.singleton alarm.alarm_kind (Set.singleton alarm)

  let add alarm x =
    Map.add alarm.alarm_kind (
      try
        let old = Map.find alarm.alarm_kind x in
        Set.add alarm old
      with Not_found -> Set.singleton alarm
    ) x


  let join x1 x2 =
    Map.map2zo
      (fun _ s1 -> s1)
      (fun _ s2 -> s2)
      (fun _ s1 s2 -> Set.union s1 s2)
      x1 x2

  let rec join_list l =
    match l with
    | [] -> empty
    | [hd] -> hd
    | hd :: tl -> join hd (join_list tl)


  let meet x1 x2 =
    Map.merge (fun _ s1 s2 ->
        match s1, s2 with
        | None, _ | _, None -> None
        | Some ss1, Some ss2 ->
          let s = Set.inter ss1 ss2 in
          if Set.is_empty s
          then None
          else Some s
      ) x1 x2


  let rec meet_list l =
    match l with
    | [] -> empty
    | [hd] -> hd
    | hd :: tl -> meet hd (meet_list tl)


  let diff x1 x2 =
    Map.merge (fun a os1 os2 ->
        match os1, os2 with
        | None, None -> None
        | Some s, None -> Some s
        | None, Some _ -> None
        | Some s1, Some s2 ->
          let s = Set.diff s1 s2 in
          if Set.is_empty s
          then None
          else Some s
      ) x1 x2


  let print fmt x =
    let open Format in
    if Map.is_empty x then pp_print_string fmt "∅"
    else
      let l = Map.bindings x in
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ", ")
        (fun fmt (kind, alarms) ->
           let n = Set.cardinal alarms in
           if n = 0 then () else
           if n = 1 then pp_alarm_kind fmt kind
           else fprintf fmt "%a x %d" pp_alarm_kind kind n
        ) fmt l


end
