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

(** Flow-insensitive context to save bindings between MOPSA variables and
    Apron variables *)

open Mopsa


module VarEquiv = Equiv.Make(Var)(Apron.Var)

let vars_ctx_key =
  let module C = Context.GenUnitKey(
    struct
      type t = VarEquiv.t
      let print fmt m =
        Format.fprintf fmt "Apron vars: @[%a@]" (VarEquiv.print ?pp_sep:None) m
    end
    )
  in
  C.key


let get_ctx ctx =
  try Context.ufind vars_ctx_key ctx
  with _ -> VarEquiv.empty

let set_ctx bindings ctx =
  Context.uadd vars_ctx_key bindings ctx

let init_ctx ctx =
  set_ctx VarEquiv.empty ctx

let mk_apron_var v =
  let name = uniq_vname v in
  Apron.Var.of_string name


let var_to_apron ctx (v:var) =
  let bindings = get_ctx ctx in
  try VarEquiv.find_l v bindings, ctx
  with Not_found ->
    let vv = mk_apron_var v in
    vv, set_ctx (VarEquiv.add (v,vv) bindings) ctx

let vars_to_apron ctx (l:var list) =
  List.fold_left (fun (accv,ctx) v ->
      let vv, ctx = var_to_apron ctx v in
      (vv::accv),ctx
    ) ([],ctx) l

let apron_to_var ctx v =
  let bindings = get_ctx ctx in
  try VarEquiv.find_r v bindings
  with Not_found -> panic "Apron variable %s not bound in context" (Apron.Var.to_string v)
