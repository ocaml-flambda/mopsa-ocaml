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

(** Common definitions for value combiners *)

open Ast.All
open Core.Id
open Sig.Abstraction.Value

type _ id += V_empty : unit id
type _ id += V_pair  : 'a id * 'b id -> ('a*'b) id

module EmptyValue : VALUE =
struct
  type t = unit
  let id = V_empty
  let () =
    let open Eq in
    register_id {
      eq = (
        let f : type a. a id -> (a, t) eq option =
          function
          | V_empty -> Some Eq
          | _ -> None
        in
        f
      );
    }
  let name = ""
  let display = ""
  let zones = []
  let bottom = ()
  let top = ()
  let print fmt () = ()
  let is_bottom () = false
  let subset () () = true
  let join () () = ()
  let meet () () = ()
  let widen () () = ()
  let constant t c = None
  let cast man t e = None
  let unop op t () = ()
  let binop op t () () = ()
  let filter b t () = ()
  let bwd_unop op t () () = ()
  let bwd_binop op t () () () = ((),())
  let bwd_cast man t e () = ()
  let predicate op b t () = ()
  let compare op b t () () = ((),())
  let ask man q = None
end
