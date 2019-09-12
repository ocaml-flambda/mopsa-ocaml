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

(** Simple packing strategy separating variables of distinct functions *)

open Mopsa
open Sig.Domain.Simplified
open Universal.Numeric.Packing.Strategy
open Ast

module Strategy(Domain:DOMAIN) =
struct

  let name = "static_c_scope"

  (** Packing key *)
  type key =
    | Globals (** Pack of global variables *)
    | Locals of c_fundec (** Pack of local variables of a function *)

  let compare k1 k2 =
    match k1, k2 with
    | Globals, Globals -> 0
    | Locals f1, Locals f2 -> compare f1.c_func_unique_name f2.c_func_unique_name
    | Globals, Locals _ -> 1
    | Locals _, Globals -> -1

  let print fmt = function
    | Globals -> Format.pp_print_string fmt "[globals]"
    | Locals f -> Format.pp_print_string fmt f.c_func_org_name

  module Map = MapExt.Make(struct type t = key let compare = compare end)

  let subset m1 m2 =
    Map.for_all2zo
      (fun _ a1 -> false)
      (fun _ a2 -> true)
      (fun _ a1 a2 -> Domain.subset a1 a2)
      m1 m2

  let join m1 m2 =
    Map.map2zo
      (fun _ a1 -> a1)
      (fun _ a2 -> a2)
      (fun _ a1 a2 -> Domain.join a1 a2)
      m1 m2

  let meet m1 m2 =
    Map.merge (fun _ a1 a2 ->
        match a1, a2 with
        | None, _ | _, None -> None
        | Some aa1, Some aa2 -> Some (Domain.meet aa1 aa2)
      ) m1 m2

  let widen ctx m1 m2 =
    Map.map2zo
      (fun _ a1 -> a1)
      (fun _ a2 -> a2)
      (fun _ a1 a2 -> Domain.widen ctx a1 a2)
      m1 m2

  let merge pre (m1,log1) (m2,log2) = join m1 m2

  let init prog = Map.singleton Globals (Domain.init prog)

  let exec stmt m =
    try
      Map.map (fun a ->
          Domain.exec stmt a |> Option.none_to_exn
        ) m |>
      Option.return
    with Option.Found_None -> None

end

let () =
  Universal.Numeric.Packing.Functor.register_strategy "static_c_scope" (module Strategy)
