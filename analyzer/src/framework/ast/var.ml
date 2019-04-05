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
  | V_common     (** common variable kind without extra information *)

(** variables *)
type var = {
  org_vname  : string; (** original name of the variable *)
  uniq_vname : string; (** unique name of the variable *)
  vuid  : int;         (** unique identifier. *)
  vtyp  : typ;         (** type of the variable. *)
  vkind : var_kind;    (** language-dependent info on the variable *)
}

let var_compare_chain = TypeExt.mk_compare_chain (fun v1 v2 ->
    Pervasives.compare v1 v2
  )

let var_pp_chain = TypeExt.mk_print_chain (fun fmt v ->
    match v.vkind with
    | V_common -> Format.pp_print_string fmt v.org_vname
    | _ -> Exceptions.panic "pp_var: unknown variable kind"
  )

let register_var info =
  TypeExt.register info var_compare_chain var_pp_chain

let compare_var v1 v2 =
  Compare.compose [
    (fun () -> compare v1.uniq_vname v2.uniq_vname);
    (fun () -> compare v1.vuid v2.vuid);
    (fun () -> compare_typ v1.vtyp v2.vtyp);
    (fun () -> TypeExt.compare var_compare_chain v1 v2);
  ]

let pp_var fmt v = TypeExt.print var_pp_chain fmt v

let vtyp v = v.vtyp
let vuid v = v.vuid
let vkind v = v.vkind
let uniq_vname v = v.uniq_vname
let org_vname v = v.org_vname

module Var =
struct
  type t = var
  let compare = compare_var
  let print = pp_var
end


(*==========================================================================*)
(**                  {2 Utility functions for variables}                    *)
(*==========================================================================*)

let mkv orig uniq ?(vkind=V_common) vuid vtyp =
  {org_vname = orig; uniq_vname = uniq; vuid; vtyp; vkind}

let vcounter = ref 0

let mkfresh ?(vkind=V_common) funiq vtyp () =
  incr vcounter;
  let uniq = funiq !vcounter in
  mkv uniq uniq ~vkind !vcounter vtyp

let mktmp ?(typ=T_any) () =
  mkfresh (fun uid ->
      let vname = "$tmp" ^ (string_of_int uid) in
      vname
    ) typ ()

let start_vcounter_at (d:int) : unit =
  assert (!vcounter <= d);
  vcounter := d

let get_vcounter_val () = !vcounter
