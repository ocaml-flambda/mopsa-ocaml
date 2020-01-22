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

(** Constant strings abstraction *)

open Mopsa
open Ast
open Bot
open Core.Sig.Value.Lowlevel


module Value =
struct

  include Framework.Lattices.Powerset.Make
      (struct
        type t = string
        let compare = Pervasives.compare
        let print = Format.pp_print_string
      end)


  include GenValueId(struct
      type nonrec t = t
      let name = "universal.numeric.values.strings"
      let display = "strings"
    end)

  let zones = [Zone.Z_u_string]

  let mem_type = function
    | T_string -> true
    | _ -> false

  let constant t = function
    | C_string s ->
      singleton s

    | _ -> top

  let unop man t op v =
      match op with
      | _ -> assert false

  let binop man = lift_simplified_binop (fun op a1 a2 ->
      match op with
      | O_plus ->
        fold (fun s1 acc ->
            fold (fun s2 acc ->
                add (s1^s2) acc
              ) a2 acc) a1 empty
      | _  -> failwith "todo") man

  let filter man a b =
      failwith "todo"

  let bwd_unop _ _ _ _ _ = failwith "ni"
  let bwd_binop _ _ _ _ _  = failwith "ni"

  let compare _ _ _ _ _ _ = failwith "ni"

  (** {2 Query handlers} *)

  let ask : type r. ('a,t) man -> ('a,r) vquery -> r option =
    fun man q ->
    match q with
    (* TODO: query de values.py_string *)
    | _ -> None
end


let () =
  Core.Sig.Value.Lowlevel.register_value (module Value)
