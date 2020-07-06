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
open Sig.Domain.Value

type _ id += V_empty : unit id
type _ id += V_pair  : 'a id * 'b id -> ('a*'b) id

let () = register_id {
    eq = (
      let f : type a b. witness -> a id -> b id -> (a,b) Eq.eq option = fun next id1 id2 ->
        match id1, id2 with
        | V_empty, V_empty -> Some Eq
        | V_pair(x1,y1), V_pair(x2,y2) ->
          begin match equal_id x1 x2 with
            | None -> None
            | Some Eq ->
              match equal_id y1 y2 with
              | None -> None
              | Some Eq -> Some Eq
          end
        | _ -> next.eq id1 id2
      in
      f
    );
  }

module EmptyValue : VALUE =
struct
  type t = unit
  let id = V_empty
  let name = ""
  let display = ""
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
