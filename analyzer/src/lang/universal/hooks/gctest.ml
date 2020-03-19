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

(** Hook for displaying analysis logs as a tree *)

open Mopsa
open Format
open Core.All
open Sig.Domain.Manager



module Hook =
struct

  (** {2 Hook header} *)
  (** *************** *)

  let name = "gctest"

  let exec_zones = [Z_any]
  let eval_zones = [Z_any,Z_any]

  let init ctx = ()


  let on_before_exec zone stmt man flow = ()
  let on_after_exec zone stmt man post = ()
  let on_before_eval zone exp man flow = ()
  let on_after_eval zone exp man evl = ()

  let on_finish man flow =
    let alladdr = man.ask Heap.Recency.Q_allocated_addresses flow in
    let reachaddr = Heap.Recency.Pool.elements @@ man.ask Heap.Recency.Q_alive_addresses_aspset flow in
    let lall = List.length alladdr in
    let lreach = List.length reachaddr in
    Format.printf "[GCTEST] reach/all = %d / %d = %f@.unreachables = @[@.%a@]@." lreach lall (if lall = 0 then 0. else float_of_int lreach /. float_of_int lall) (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.") Ast.pp_addr) (List.filter (fun a -> not @@ List.mem a reachaddr) alladdr);
    Format.printf "Total gc time : %.3f@.Avg # collected addr: %d@.Max heap size: %d@." !Heap.Recency.gc_time (if !Heap.Recency.gc_nb_collections = 0 then 0 else !Heap.Recency.gc_nb_addr_collected / !Heap.Recency.gc_nb_collections) !Heap.Recency.gc_max_heap_size


end

let () =
  Core.Hook.register_stateless_hook (module Hook)
