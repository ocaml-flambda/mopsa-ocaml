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

(** Semantic dependencies *)

type semantic =
  | Any
  | One of { name : string;
             domain: string; }

let any_semantic = Any

let name_of_semantic = function
  | Any   -> Exceptions.panic "name_of_semantic called on any_semantic"
  | One s -> s.name

let domain_of_semantic = function
  | Any   -> Exceptions.panic "domain_of_semantic called on any_semantic"
  | One s ->  s.domain

let compare_semantic s1 s2 =
  match s1, s2 with
  | Any, Any -> 0
  | One ss1, One ss2 ->
    Compare.pair String.compare String.compare
      (ss1.name,ss1.domain)
      (ss2.name,ss2.domain)
  | _ -> compare s1 s2

let pp_semantic fmt = function
  | Any   -> Format.pp_print_string fmt "*"
  | One s -> Format.fprintf fmt "%s#%s" s.domain s.name

let mk_semantic ~name ~domain = One { domain; name }
