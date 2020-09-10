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

(** Profiler hook to track the amount of disjunctions *)

open Mopsa
open Hook
open Ast


let threshold = ref 8

module Hook =
struct


  (** {2 Hook header} *)
  (** *************** *)

  let name = "c.hooks.disj"

  let init ctx = ()


  (** {2 Events handlers} *)
  (** ******************* *)

  let on_before_exec zone stmt man flow =
    ()


  let max_nb = ref 1

  let on_after_exec zone stmt man flow post =
    let nb = Cases.cardinal post in
    let nb2 = Cases.fold_result (fun i _ f -> Flow.fold (fun i _ _ -> i + 1) i f) 0 post in
    if nb2 >= !threshold then (      
      max_nb := max !max_nb nb2;
      Format.printf "exec: %i / %i (max=%i): %a %a@." nb nb2 !max_nb pp_range (srange stmt) pp_stmt stmt
    )

  let on_before_eval zone exp man flow =
    ()


  let max_nb = ref 1
  
  let on_after_eval zone exp man flow eval =
    let nb = Cases.cardinal eval in
    if nb >= !threshold then (      
      max_nb := max !max_nb nb;
      Format.printf "eval: %i (max=%i): %a %a@." nb !max_nb pp_range (erange exp) pp_expr exp;
      ignore (Cases.map_result (fun e -> Format.printf "  %a@." pp_expr e; e) eval)
    )
  
  let on_finish man flow =
    ()
  
end

let () =
  register_stateless_hook (module Hook)
