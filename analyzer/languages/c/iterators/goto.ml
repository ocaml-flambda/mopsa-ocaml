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
open Sig.Abstraction.Stateless
open Ast
open Common.Scope_update

let name = "c.iterators.goto"

(*==========================================================================*)
(**                       {2 Command line options}                          *)
(*==========================================================================*)

let opt_goto_down : bool ref = ref false
(** Enable down iterations for goto *)

let () =
  register_domain_option name {
    key = "-goto-down";
    category = "Goto";
    doc = " perform a down iteration after goto stabilization";
    spec = ArgExt.Set opt_goto_down;
    default = "false";
  };



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

  let checks = []

  (** Initialization *)
  (** ============== *)

  let init _ _ flow =  flow

  let exec stmt man flow =
    match skind stmt with
    | S_c_goto (s,upd) ->
      (* Save T_cur env in T_goto s token, then set T_cur to bottom. *)
      update_scope upd stmt.srange man flow >>%? fun flow ->
      let cur = Flow.get T_cur man.lattice flow in
      let flow0 = Flow.add (T_goto s) cur man.lattice flow |>
                  Flow.remove T_cur
      in
      Post.return flow0 |>
      OptionExt.return

    | S_c_label s ->
      (* Moves flow in goto label inside current *)
      let fromlbl = Flow.get (T_goto s) man.lattice flow in
      let flow0 = Flow.add T_cur fromlbl man.lattice flow |>
                  Flow.remove (T_goto s)
      in
      Post.return flow0 |>
      OptionExt.return

    | S_c_goto_stab stmt' ->
      (* Stabilization statement for backward gotos *)
      let bottom = Flow.bottom_from flow in
      let nogotos, gotos = Flow.fold (fun (nogotos, gotos) k v ->
          match k with
          | T_goto _ -> (nogotos, Flow.add k v man.lattice gotos)
          | _       -> (Flow.add k v man.lattice nogotos, gotos)
        ) (bottom, bottom) flow in
      let init_report = Flow.get_report flow in
      let get_gotos f = Flow.filter
          (fun t e -> match t with | T_goto _ -> true | _ -> false)
          f
      in
      let drop_gotos f = Flow.filter
          (fun t e -> match t with | T_goto s -> false | _ -> true)
          f
      in
      let rec next f i wid_limit =
        man.exec stmt' f >>% fun f' ->
        let f1, f1' = get_gotos f, get_gotos f' in
        if Flow.subset man.lattice f1' f1 then
          if !opt_goto_down then
            down (drop_gotos f |> Flow.join man.lattice f1' |> Flow.set_report init_report)
          else
            Post.return f'
        else
          (* Join the input with the new goto flows *)
          let f2' = Flow.join man.lattice f f1' in
          if i >= wid_limit
          then next (Flow.widen man.lattice f f2') (i+1) wid_limit
          else next f2' (i+1) wid_limit
      and down f =
        man.exec stmt' f >>% fun f' ->
        Post.return f'
      in
      next nogotos 0 1 >>%? fun flow1 ->
      let flow1_minus_gotos = Flow.filter (fun k v ->
          match k with
          | T_goto s -> false | _ -> true
        ) flow1
      in
      let flow2 = Flow.join man.lattice gotos flow1_minus_gotos in
      Post.return flow2 |>
      OptionExt.return

    | _ -> None

  let eval man exp flow = None

  let ask _ _ _  = None

  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
