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

(** Extensible type of variables. *)

open Typ


(** Languages can extend this type to add extra information on variables *)
type var_kind = ..

type var_kind +=
  | V_common of int     (** common variable kind with uid *)

(** variables *)
type var = {
  org_vname  : string; (** original name of the variable *)
  uniq_vname : string; (** unique name of the variable *)
  vtyp  : typ;         (** type of the variable. *)
  vkind : var_kind;    (** language-dependent info on the variable *)
}

let vtyp v = v.vtyp
let vkind v = v.vkind
let uniq_vname v = v.uniq_vname
let org_vname v = v.org_vname

let var_compare_chain = TypeExt.mk_compare_chain (fun v1 v2 ->
    match vkind v1, vkind v2 with
    | V_common i1, V_common i2 -> Pervasives.compare i1 i2
    | _ -> Pervasives.compare v1 v2
  )

let var_pp_chain = TypeExt.mk_print_chain (fun fmt v ->
    match vkind v with
    | V_common _ -> Format.pp_print_string fmt v.org_vname
    | _ -> Exceptions.panic "pp_var: unknown variable kind"
  )

let register_var info =
  TypeExt.register info var_compare_chain var_pp_chain

let pp_var fmt v = TypeExt.print var_pp_chain fmt v

let compare_var v1 v2 =
  let res = Compare.compose [
    (fun () -> compare v1.uniq_vname v2.uniq_vname);
    (fun () -> TypeExt.compare var_compare_chain v1 v2); (* FIXME: issue in bm_spectral_norm (ranged_var not found during dichotomy in var map) if this compare is above the compare on uniq_vnames... *)
    (fun () -> compare_typ v1.vtyp v2.vtyp);
  ] in
  (* Format.printf "comparing %a and %a = %d@\n" pp_var v1 pp_var v2 res; *)
  res


module Var =
struct
  type t = var
  let compare = compare_var
  let print = pp_var
end


(*==========================================================================*)
(**                  {2 Utility functions for variables}                    *)
(*==========================================================================*)

let mkv orig uniq vkind vtyp =
  {org_vname = orig; uniq_vname = uniq; vtyp; vkind}

(* ref for vcommon *)
let vcounter = ref 0

let mkfresh_common funiq vtyp () =
  incr vcounter;
  let org, uniq = funiq !vcounter in
  mkv org uniq (V_common !vcounter) vtyp

let mktmp ?(typ=T_any) () =
  mkfresh_common (fun uid ->
      let vname = "$tmp" ^ (string_of_int uid) in
      vname, vname
    ) typ ()


let start_vcounter_at (d:int) : unit =
  assert (!vcounter <= d);
  vcounter := d

let get_vcounter_val () = !vcounter


open Location

type var_kind +=
  | V_ranged_var of range

let ranged_var_compare default v1 v2 =
  match vkind v1, vkind v2 with
  | V_ranged_var r1, V_ranged_var r2 ->
    compare_range r1 r2
  | _ -> default v1 v2

let ranged_var_print default fmt v =
  match vkind v with
  | V_ranged_var r -> Format.fprintf fmt "%s" v.uniq_vname
  | _ -> default fmt v

let () = register_var TypeExt.{compare=ranged_var_compare; print=ranged_var_print}

let mkfresh_ranged range ?(vtyp=T_any) () =
  let org = Format.fprintf Format.str_formatter "ranged_var(%a)" pp_range range; Format.flush_str_formatter () in
  let uniq = org in
  mkv org uniq (V_ranged_var range) vtyp
