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

(** Iterator composer combines domains in sequence. Lattice operations
   ([bottom], [top], [leq], etc.) are defined pointwise. For transfer
   functions ([exec], [eval] and [ask]), the iterator returns the
   result of the first domain giving an non-empty answer.  The order
   of domains follows the order given in the configuration file.  *)

open Core
open Manager
open Domain
open Eq

module Make(Head: DOMAIN)(Tail: DOMAIN) : DOMAIN =
struct

  type t = Head.t * Tail.t

  type _ domain += D_iter : t domain

  let id = D_iter
  let name = Head.name ^ ", " ^ Tail.name
  let identify : type b. b domain -> (t, b) eq option =
    function
    | D_iter -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:"framework.domains.iter" fmt

  let bottom =
    Head.bottom, Tail.bottom

  let top =
    Head.top, Tail.top

  let is_bottom (hd, tl) =
    Head.is_bottom hd || Tail.is_bottom tl

  let subset ((hd1, tl1): t) ((hd2, tl2): t) : bool =
    Head.subset hd1 hd2 && Tail.subset tl1 tl2

  let join annot ((hd1, tl1): t) ((hd2, tl2): t) : t =
    (Head.join annot hd1 hd2), (Tail.join annot tl1 tl2)

  let meet annot ((hd1, tl1): t) ((hd2, tl2): t) : t =
    (Head.meet annot hd1 hd2), (Tail.meet annot tl1 tl2)

  let widen annot ((hd1, tl1): t) ((hd2, tl2): t) : t =
    (Head.widen annot hd1 hd2), (Tail.widen annot tl1 tl2)

  let print fmt (hd, tl) =
    Format.fprintf fmt "%a%a" Head.print hd Tail.print tl

  let head_man man = {
    man with
    get = (fun flow -> fst @@ man.get flow);
    set = (fun hd flow -> man.set (hd, snd @@ man.get flow) flow);
  }

  let tail_man man = {
    man with
    get = (fun flow -> snd @@ man.get flow);
    set = (fun tl flow -> man.set (fst @@ man.get flow, tl) flow);
  }

  let init prog man flow =
    let flow', cb, b =
      match Head.init prog (head_man man) flow with
      | None -> flow, [], false
      | Some flowcb -> flowcb.flow, flowcb.callbacks, true
    in
    match Tail.init prog (tail_man man) flow', b with
    | None, false -> None
    | None, true -> Some { flow = flow'; callbacks = cb }
    | Some x , _ -> Some { flow = x.flow; callbacks = cb @ x.callbacks }

  let exec_interface = Domain.{
    import = Head.exec_interface.import @ Tail.exec_interface.import;
    export = Head.exec_interface.export @ Tail.exec_interface.export;
  }

  let exec zone =
    match List.exists (fun z -> Zone.sat_zone z zone) Head.exec_interface.Domain.export,
          List.exists (fun z -> Zone.sat_zone z zone) Tail.exec_interface.Domain.export
    with
    | false, false -> raise Not_found

    | true, false ->
      let f = Head.exec zone in
      (fun stmt man flow -> f stmt (head_man man) flow)

    | false, true ->
      let f = Tail.exec zone in
      (fun stmt man flow -> f stmt (tail_man man) flow)

    | true, true ->
      let f1 = Head.exec zone in
      let f2 = Tail.exec zone in
      (fun stmt man flow ->
         match f1 stmt (head_man man) flow with
         | Some post -> Some post
         | None -> f2 stmt (tail_man man) flow
      )


  let eval_interface = Domain.{
    import = Head.eval_interface.import @ Tail.eval_interface.import;
    export = Head.eval_interface.export @ Tail.eval_interface.export;
  }

  let eval zpath =
    match List.exists (fun p -> Zone.sat_zone2 p zpath) Head.eval_interface.Domain.export,
          List.exists (fun p -> Zone.sat_zone2 p zpath) Tail.eval_interface.Domain.export
    with
    | false, false -> raise Not_found

    | true, false ->
      let f = Head.eval zpath in
      (fun exp man flow -> f exp (head_man man) flow)

    | false, true ->
      let f = Tail.eval zpath in
      (fun exp man flow -> f exp (tail_man man) flow)

    | true, true ->
      let f1 = Head.eval zpath in
      let f2 = Tail.eval zpath in
      (fun exp man flow ->
         match f1 exp (head_man man)  flow with
         | Some evl -> Some evl
         | None -> f2 exp (tail_man man) flow
      )


  let ask query man flow =
    let head_reply = Head.ask query (head_man man) flow in
    let tail_reply = Tail.ask query (tail_man man) flow in
    OptionExt.option_neutral2 (Query.join query) head_reply tail_reply

end
