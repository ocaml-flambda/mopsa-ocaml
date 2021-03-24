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

(** Heap addresses *)

open Mopsa_utils
open Var

(** Kind of heap addresses, used to store extra information. *)
type addr_kind = ..

let addr_kind_compare_chain : (addr_kind -> addr_kind -> int) ref =
  ref (fun a1 a2 -> compare a1 a2)

let addr_kind_pp_chain : (Format.formatter -> addr_kind -> unit) ref =
  ref (fun fmt a -> Exceptions.panic "addr_kind_pp_chain: unknown address")

let pp_addr_kind fmt ak =
  !addr_kind_pp_chain fmt ak

let compare_addr_kind ak1 ak2 =
  if ak1 == ak2 then 0 else
  !addr_kind_compare_chain ak1 ak2

let register_addr_kind (info: addr_kind TypeExt.info) =
  addr_kind_compare_chain := info.compare !addr_kind_compare_chain;
  addr_kind_pp_chain := info.print !addr_kind_pp_chain;
  ()


(** Addresses are grouped by static criteria to make them finite *)
type addr_partitioning = ..

type addr_partitioning +=
  | G_all (** Group all addresses into one *)

let addr_partitioning_compare_chain : (addr_partitioning -> addr_partitioning -> int) ref =
  ref (fun a1 a2 -> compare a1 a2)

let addr_partitioning_pp_chain : (Format.formatter -> addr_partitioning -> unit) ref =
  ref (fun fmt g ->
      match g with
      | _ -> Format.pp_print_string fmt "*"
    )

(** Command line option to use hashes as address format *)
let opt_hash_addr = ref false

let base64_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!%"

let char_base64 n = base64_alphabet.[n]

let digest_to_base64 (d:Digest.t) : string =
  if String.length d <> 16 then invalid_arg "digest_to_base64";
  let result = Bytes.create 21 in
  for i = 0 to 4 do
    let x1 = Char.code d.[i*3] in
    let x2 = Char.code d.[i*3+1] in
    let x3 = Char.code d.[i*3+2] in
    Bytes.unsafe_set result (i*4) (char_base64 (x1 lsr 2));
    Bytes.unsafe_set result (i*4+1) (char_base64 (((x1 land 0x03) lsl 4) lor (x2 lsr 4)));
    Bytes.unsafe_set result (i*4+2) (char_base64 (((x2 land 0x0f) lsl 2) lor (x3 lsr 2)));
    Bytes.unsafe_set result (i*4+3) (char_base64 (x3 land 0x3f));
  done;
  let x = Char.code d.[15] in
  Bytes.unsafe_set result (19) (char_base64 (x lsr 2));
  Bytes.unsafe_set result (20) (char_base64 (x land 0x03));
  Bytes.unsafe_to_string result

let pp_addr_partitioning_hash fmt (g:addr_partitioning) =
  let s = Format.asprintf "%a" !addr_partitioning_pp_chain g in
  let md5 = Digest.string s in
  let base64 = digest_to_base64 md5 in
  Format.pp_print_string fmt base64

let pp_addr_partitioning fmt ak =
  if !opt_hash_addr
  then pp_addr_partitioning_hash fmt ak
  else !addr_partitioning_pp_chain fmt ak



let compare_addr_partitioning a1 a2 =
  if a1 == a2 then 0 else !addr_partitioning_compare_chain a1 a2

let register_addr_partitioning (info: addr_partitioning TypeExt.info) =
  addr_partitioning_compare_chain := info.compare !addr_partitioning_compare_chain;
  addr_partitioning_pp_chain := info.print !addr_partitioning_pp_chain;
  ()


(** Heap addresses. *)
type addr = {
  addr_kind : addr_kind;   (** Kind of the address. *)
  addr_partitioning : addr_partitioning; (** Group of the address *)
  addr_mode : mode;        (** Assignment mode of address (string or weak) *)
}


let akind addr = addr.addr_kind

let pp_addr fmt a =
  Format.fprintf fmt "@@%a:%a:%s"
    pp_addr_kind a.addr_kind
    pp_addr_partitioning a.addr_partitioning
    (match a.addr_mode with WEAK -> "w" | STRONG -> "s")


let compare_addr a b =
  if a == b then 0
  else Compare.compose [
      (fun () -> compare_addr_kind a.addr_kind b.addr_kind);
      (fun () -> compare_addr_partitioning a.addr_partitioning b.addr_partitioning);
      (fun () -> compare_mode a.addr_mode b.addr_mode);
    ]


let addr_mode (a:addr) (omode: mode option) : mode =
  match omode with
  | None -> a.addr_mode
  | Some m -> m


(** Address variables *)
type var_kind +=
  | V_addr_attr of addr * string

let () =
  register_var {
    compare = (fun next v1 v2 ->
        match vkind v1, vkind v2 with
        | V_addr_attr (a1,attr1), V_addr_attr (a2,attr2) ->
          Compare.compose [
            (fun () -> compare_addr a1 a2);
            (fun () -> compare attr1 attr2)
          ]
        | _ -> next v1 v2
      );
    print = (fun next fmt v ->
        match vkind v with
        | V_addr_attr (addr, attr) -> Format.fprintf fmt "%a.%s" pp_addr addr attr
        | _ -> next fmt v
      )
  }

let mk_addr_attr addr attr typ =
  let name = Format.asprintf "%a.%s" pp_addr addr attr in
  mkv name (V_addr_attr (addr,attr)) ~mode:addr.addr_mode typ
