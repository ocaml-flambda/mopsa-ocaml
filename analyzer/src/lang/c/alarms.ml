(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** C Runtime errors. *)

open Framework.Ast
open Framework.Pp
open Framework.Domains.Stateless
open Framework.Manager
open Framework.Visitor
open Framework.Domains
open Framework.Alarm
open Framework.Flow
open Universal.Ast
open Ast


let name = "c.alarms"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                   {2 Errors flows and alarms}                           *)
(*==========================================================================*)


type token +=
  | TOutOfBound of var (** base variable *) * int (** offset *) * range
  | TNullDeref of var (* pointer *) * range
  | TInvalidDeref of var (* pointer *) * range

type alarm_kind +=
  | AOutOfBound of var (** base variable *) * int (** offset *)
  | ANullDeref of var (** pointer *)
  | AInvalidDeref of var (** pointer *)

let is_error_token = function
  | TOutOfBound _ -> true
  | TNullDeref _ -> true
  | TInvalidDeref _ -> true
  | _ -> false

let error_token_range = function
  | TOutOfBound(_, _, r) -> r
  | TNullDeref(_, r) -> r
  | TInvalidDeref(_, r) -> r
  | _ -> assert false

(*==========================================================================*)
(**                       {2 Abstract domain}                               *)
(*==========================================================================*)


module Domain = struct


  (*==========================================================================*)
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)

  let init prog man flow = flow

  let exec stmt man ctx flow = None

  let eval exp man ctx flow = None

  let ask : type r. r Framework.Query.query -> ('a, unit) manager -> Framework.Context.context -> 'a flow -> r option =
    fun query man ctx flow ->
      match query with
      | Framework.Alarm.QGetAlarms ->
        let alarms = man.flow.fold (fun acc env -> function
            | TOutOfBound(v, o, range) ->
              let alarm = {
                alarm_kind = AOutOfBound(v, o);
                alarm_range = range;
                alarm_level = High;
              } in
              alarm :: acc

            | TNullDeref(p, range) ->
              let alarm = {
                alarm_kind = ANullDeref(p);
                alarm_range = range;
                alarm_level = High;
              } in
              alarm :: acc

            | TInvalidDeref(p, range) ->
              let alarm = {
                alarm_kind = AInvalidDeref(p);
                alarm_range = range;
                alarm_level = High;
              } in
              alarm :: acc

            | _ -> acc
          ) [] flow
        in
        Some alarms

      | _ -> None

end

let setup () =
  register_domain name (module Domain);

  register_token_compare (fun next tk1 tk2 ->
      match tk1, tk2 with
      | TOutOfBound (v1, o1, r1), TOutOfBound (v2, o2, r2) ->
        compare_composer [
          (fun () -> compare_var v1 v2);
          (fun () -> compare o1 o2);
          (fun () -> compare_range r1 r2)
        ]

      | TNullDeref(p1, r1), TNullDeref(p2, r2) ->
        compare_composer [
          (fun () -> compare_var p1 p2);
          (fun () -> compare_range r1 r2)
        ]

      | TInvalidDeref(p1, r1), TInvalidDeref(p2, r2) ->
        compare_composer [
          (fun () -> compare_var p1 p2);
          (fun () -> compare_range r1 r2)
        ]

      | _ -> next tk1 tk2
    );

  register_pp_token (fun next fmt -> function
      | TOutOfBound (v, o, r) -> Format.fprintf fmt "outbound@%a" Framework.Pp.pp_range r
      | TNullDeref(p, r) -> Format.fprintf fmt "null@%a" Framework.Pp.pp_range r
      | TInvalidDeref(p, r) -> Format.fprintf fmt "invalid@%a" Framework.Pp.pp_range r
      | tk -> next fmt tk
    );

  register_alarm_compare (fun next a1 a2 ->
      match a1.alarm_kind, a2.alarm_kind with
      | AOutOfBound(v1, o1), AOutOfBound(v2, o2) ->
        compare_composer [
          (fun () -> compare_var v1 v2);
          (fun () -> compare o1 o2);
        ]

      | ANullDeref(p1), ANullDeref(p2) -> compare_var p1 p2

      | AInvalidDeref(p1), AInvalidDeref(p2) -> compare_var p1 p2

      | _ -> next a1 a2
    );

  register_pp_alarm (fun next fmt alarm ->
      match alarm.alarm_kind with
      | AOutOfBound(v, o) -> Format.fprintf fmt "%a  Out of bound access on %s" ((Debug.color "red") Format.pp_print_string) "✘" v.vname
      | ANullDeref(p) -> Format.fprintf fmt "%a  Null pointer dereference on %s" ((Debug.color "red") Format.pp_print_string) "✘" p.vname
      | AInvalidDeref(p) -> Format.fprintf fmt "%a  Invalid pointer dereference on %s" ((Debug.color "red") Format.pp_print_string) "✘" p.vname
      | _ -> next fmt alarm
    );
