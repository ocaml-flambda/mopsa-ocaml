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

(** Bindings between Mopsa variables and Apron variables *)

open Mopsa


module Equiv = Equiv.Make(Var)(Apron.Var)

type t = Equiv.t

let empty : t = Equiv.empty

let mk_apron_var v =
  Apron.Var.of_string v.vname


let var_to_apron (bindings:t) (v:var) : Apron.Var.t * t =
  try Equiv.find_l v bindings, bindings
  with Not_found ->
    let vv = mk_apron_var v in
    vv, Equiv.add (v,vv) bindings

let vars_to_apron  bindings (l:var list) =
  List.fold_left (fun (accv,accb) v ->
      let vv, accb = var_to_apron accb v in
      (vv::accv),accb
    ) ([],bindings) l

let apron_to_var bindings v =
  try Equiv.find_r v bindings
  with Not_found -> panic "Apron variable %s not bound" (Apron.Var.to_string v)


let concat b1 b2 = Equiv.concat b1 b2
