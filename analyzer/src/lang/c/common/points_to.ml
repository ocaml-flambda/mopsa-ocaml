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

(** Common constructs for points-to evaluations. *)

open Mopsa
open Universal.Ast
open Base
open Alarms

(* Points-to results *)
(* ================= *)

type points_to =
  | P_fun of Ast.c_fundec
  | P_block of base (** base *) * expr (** offset *)
  | P_null
  | P_invalid
  | P_top

let pp_points_to fmt = function
  | P_fun f -> Format.fprintf fmt "(fp %s)" f.Ast.c_func_org_name
  | P_block(base, offset) -> Format.fprintf fmt "(%a, %a)" pp_base base pp_expr offset
  | P_null -> Format.pp_print_string fmt "NULL"
  | P_invalid -> Format.pp_print_string fmt "Invalid"
  | P_top -> Format.pp_print_string fmt "⊤"

let compare_points_to p1 p2 =
  match p1, p2 with
  | P_fun f1, P_fun f2 ->
    compare f1.Ast.c_func_unique_name f2.Ast.c_func_unique_name
  | P_block (b1, o1), P_block (b2, o2) ->
    Compare.compose [
      (fun () -> compare_base b1 b2);
      (fun () -> compare_expr o1 o2);
    ]
  | _, _ -> Pervasives.compare p1 p2



type expr_kind +=
  | E_c_points_to of points_to  (* Reply to a points-to evaluation *)


let mk_c_points_to pt range =
  mk_expr (E_c_points_to pt) range

let mk_c_points_to_bloc b o range =
  mk_c_points_to (P_block (b, o)) range

let mk_c_points_to_null range =
  mk_c_points_to P_null range

let mk_c_points_to_invalid range =
  mk_c_points_to P_invalid range

let mk_c_points_to_fun f range =
  mk_c_points_to (P_fun f) range

let mk_c_points_to_top range =
  mk_c_points_to P_top range


let () =
  register_expr_with_visitor {
    compare = (fun next e1 e2 ->
        match ekind e1, ekind e2 with
        | E_c_points_to p1, E_c_points_to p2 -> compare_points_to p1 p2

        | _ -> next e1 e2
      );
    print = (fun next fmt e ->
        match ekind e with
        | E_c_points_to p -> Format.fprintf fmt "⇝ %a" pp_points_to p
        | _ -> next fmt e
      );
    visit = (fun next e ->
        match ekind e with
        | E_c_points_to p -> leaf e (* FIXME: do we need to visit the offset expression? *)
        | _ -> next e
      )
  };
  ()

type zone +=
  | Z_c_points_to

let () =
  register_zone {
    zone = Z_c_points_to;
    zone_name = "C/Points-To";
    zone_subset = None;
    zone_eval = (fun exp ->
        match ekind exp with
        | E_c_points_to _ -> Keep
        | _ -> Process
      );
  }



let eval_pointed_base_offset ptr range (man:('a,'t,'s) Core.Sig.Stacked.Lowlevel.man) flow =
  man.eval ptr ~zone:(Zone.Z_c_low_level, Z_c_points_to) flow >>$ fun pt flow ->

  match ekind pt with
  | E_c_points_to P_null ->
    raise_c_alarm ANullDeref range ~bottom:true man.lattice flow |>
    Result.empty_singleton

  | E_c_points_to P_invalid ->
    raise_c_alarm AInvalidDeref range ~bottom:true man.lattice flow |>
    Result.empty_singleton

  | E_c_points_to (P_block (D _, offset)) ->
    raise_c_alarm AUseAfterFree range ~bottom:true man.lattice flow |>
    Result.empty_singleton

  | E_c_points_to (P_block (base, offset)) ->
    Result.singleton (Some (base, offset)) flow


  | E_c_points_to P_top ->
    Soundness.warn_at range "ignoring ⊤ pointer resolution";
    Result.singleton None flow

  | _ -> assert false
