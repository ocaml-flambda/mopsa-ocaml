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

(** Collapse all partitions after the execution of each statement *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast


module Domain =
struct

  include GenStatelessDomainId(
    struct
      let name = "universal.partitioning.merge_after_stmt"
    end
    )

  let checks = []

  let init prog man flow = flow

  let eval exp man flow = None

  let ask query man flow = None

  let eval exp man flow = None

  let exec stmt man flow =
    let post = man.exec stmt flow ~route:(Below name) in
    let post' = Post.remove_duplicates man.lattice post in
    OptionExt.return post'

  let print_expr man flow printer exp = ()

end

let () =
  register_stateless_domain (module Domain)
