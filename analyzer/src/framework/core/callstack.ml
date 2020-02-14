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

(** Call stacks are represented as sequences of call sites
   (ranges). They are saved as annotations in flows. *)

open Location

type call = {
  call_fun:  string;
  call_site: range;
}

type cs = call list

let pp_call fmt c =
  Format.fprintf fmt "%s@%a"
    c.call_fun
    pp_range c.call_site

let pp_call_stack fmt (cs:cs) =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " → ")
    pp_call
    fmt cs

let print fmt (cs:cs) =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
    pp_call
    fmt cs

let compare_call c c' =
  if c == c' then 0
  else Compare.compose [
    (fun () -> compare c.call_fun c'.call_fun);
    (fun () -> compare_range c.call_site c'.call_site);
  ]

let compare cs cs' =
  Compare.list compare_call cs cs'

let empty : cs = []

let is_empty (cs:cs) =
  match cs with
  | [] -> true
  | _ -> false

let length (cs:cs) : int = List.length cs

let ctx_key =
  let module K = Context.GenUnitKey(
    struct
      type t = cs
      let print fmt cs =
        Format.fprintf fmt "Callstack: %a" print cs
    end
    )
  in
  K.key


let push f range cs =
  { call_fun = f; call_site = range} :: cs

exception EmptyCallstack

let pop cs =
  match cs with
  | [] -> raise EmptyCallstack
  | _ -> List.hd cs, List.tl cs

let top (cs:cs) : call =
  try List.hd cs
  with Failure _ -> raise EmptyCallstack
