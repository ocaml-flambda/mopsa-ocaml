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

(** Management of memory blocks *)

open Mopsa
open Sig.Domain.Stateless
open Universal.Ast
open Ast
open Stubs.Ast
open Common.Base
open Common.Points_to


type typ += T_c_block of typ

let () = register_typ {
    compare = (fun next t1 t2 ->
        match t1, t2 with
        | T_c_block tt1, T_c_block tt2 -> compare_typ tt1 tt2
        | _ -> next t1 t2
      );
    print = (fun next fmt t ->
        match t with
        | T_c_block tt -> Format.fprintf fmt "block(%a)" pp_typ tt
        | _ -> next fmt t
      );
  }

let is_c_block_type = function T_c_block _ -> true | _ -> false

let base_to_block b = { b with etyp = T_c_block b.etyp }

let block_to_base bb =
  let t = match bb.etyp with T_c_block t -> t | _ -> assert false in
  { bb with etyp = t }


module Domain =
struct

  (** {2 Domain header *)
  (** ================ *)

  include GenStatelessDomainId(struct
      let name = "c.memory.blocks"
    end)

  let memory = mk_semantic "memory" ~domain:name
  let pointers = mk_semantic "pointer" ~domain:name

  let dependencies = [ memory; pointers ]

  let alarms = []


  (** {2 Transfer functions} *)
  (** ====================== *)

  let init _ _ flow = flow

  let exec_add b range man flow =
    man.post ~semantic:memory (mk_add b range) flow

  let exec_remove b range man flow =
    man.post ~semantic:memory (mk_remove b range) flow >>$ fun () flow ->
    man.post ~semantic:pointers (mk_invalidate b range) flow

  let exec_rename b1 b2 range man flow =
    man.post ~semantic:memory (mk_rename b1 b2 range) flow >>$ fun () flow ->
    let bb1 = base_to_block b1 in
    let bb2 = base_to_block b2 in
    man.post ~semantic:pointers (mk_rename bb1 bb2 range) flow

  let exec_forget b range man flow =
    man.post ~semantic:memory (mk_forget b range) flow

  let exec_expand b bl range man flow =
    man.post ~semantic:memory (mk_expand b bl range) flow >>$ fun () flow ->
    let bb = base_to_block b in
    let bbl = List.map base_to_block bl in
    man.post ~semantic:pointers (mk_expand bb bbl range) flow


  let exec_fold b bl range man flow =
    man.post ~semantic:memory (mk_fold b bl range) flow >>$ fun () flow ->
    let bb = base_to_block b in
    let bbl = List.map base_to_block bl in
    man.post ~semantic:pointers (mk_fold bb bbl range) flow

  let exec stmt man flow =
    match skind stmt with
    | S_add b when is_c_type b.etyp ->
      exec_add b stmt.srange man flow |>
      OptionExt.return

    | S_remove b when is_c_type b.etyp ->
      exec_remove b stmt.srange man flow |>
      OptionExt.return

    | S_rename(b1,b2) when is_c_type b1.etyp && is_c_type b2.etyp ->
      exec_rename b1 b2 stmt.srange man flow |>
      OptionExt.return

    | S_forget b when is_c_type b.etyp ->
      exec_forget b stmt.srange man flow |>
      OptionExt.return

    | S_expand(b,bl) when is_c_type b.etyp && List.for_all (fun b -> is_c_type b.etyp) bl  ->
      exec_expand b bl stmt.srange man flow |>
      OptionExt.return

    | S_fold(b,bl) when is_c_type b.etyp && List.for_all (fun b -> is_c_type b.etyp) bl  ->
      exec_fold b bl stmt.srange man flow |>
      OptionExt.return

    | _ -> None

  let eval exp man flow = None

  let ask query man flow = None

end

let () =
  register_stateless_domain (module Domain)
