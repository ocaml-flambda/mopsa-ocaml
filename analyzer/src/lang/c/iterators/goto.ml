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

(** Control flow abstraction for Goto statements. *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Ast


(*==========================================================================*)
(**                            {2 Flow tokens}                              *)
(*==========================================================================*)

type token +=
  | T_goto of string
  (** Goto environments *)


let () =
  register_token
    {
      print = (fun next fmt -> function
          | T_goto str -> Format.fprintf fmt "goto %s" str
          | tk -> next fmt tk
        );
      compare = (fun next a b ->
          match a,b with
          | T_goto x, T_goto y -> compare x y
          | _ -> next a b
        )
    }

(*==========================================================================*)
(**                        {2 Abstract domain}                              *)
(*==========================================================================*)


module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.iterators.goto"
    end)

  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {provides = [Zone.Z_c]; uses = []};
    ieval = {provides = []; uses = []};
  }

  (** Initialization *)
  (** ============== *)

  let init _ _ flow =  flow

  let exec zone stmt man flow =
    match skind stmt with
    | S_c_goto s ->
      (* Save TCur env in T_goto s token, then set T_cur to bottom. *)
      let cur = Flow.get T_cur man.lattice flow in
      let flow0 = Flow.add (T_goto s) cur man.lattice flow |>
                  Flow.remove T_cur
      in
      Post.return flow0 |>
      Option.return

    | S_c_label s ->
      (* Moves flow in goto label inside current *)
      let fromlbl = Flow.get (T_goto s) man.lattice flow in
      let flow0 = Flow.add T_cur fromlbl man.lattice flow |>
                  Flow.remove (T_goto s)
      in
      Post.return flow0 |>
      Option.return

    | S_c_goto_stab stmt' ->
      (* Stabilization statement for backward gotos *)
      let ctx = Flow.get_ctx flow in
      let alarms = Flow.get_alarms flow in
      let bottom = Flow.bottom ctx alarms in
      let nogotos, gotos = Flow.fold (fun (nogotos, gotos) k v ->
          match k with
          | T_goto s -> (nogotos, Flow.add k v man.lattice gotos)
          | _       -> (Flow.add k v man.lattice nogotos, gotos)
        ) (bottom, bottom) flow in
      let init_alarms = Flow.get_alarms flow in
      let next f f' i wid_limit =
        let get_gotos f = Flow.filter
            (fun t e -> match t with | T_goto s -> true | _ -> false)
            f
        in
        let f1, f1' = get_gotos f, get_gotos f' in
        if Flow.subset man.lattice f1' f1 then
          None
        else if i >= wid_limit then
          Some (Flow.widen man.lattice f1 (Flow.join man.lattice f1 f1'))
        else Some (Flow.join man.lattice f1 f1')
      in
      let rec stabilization f i wid_limit =
        let f = Flow.set_alarms init_alarms f in
        let f' = man.exec stmt' f in
        match next (Flow.copy_ctx f' f) f' i wid_limit with
        | None -> f'
        | Some f'' -> stabilization f'' (i+1) wid_limit
      in
      let flow1 = stabilization nogotos 0 3 in
      let flow1_minus_gotos = Flow.filter (fun k v ->
          match k with
          | T_goto s -> false | _ -> true
        ) flow1
      in
      let flow2 = Flow.join man.lattice gotos flow1_minus_gotos in
      Post.return flow2 |>
      Option.return

    | _ -> None

  let eval zone man exp flow = None

  let ask _ _ _  = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
