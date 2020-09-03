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

  let alarms = []

  let init prog man flow = flow
    
  let eval exp man flow = None

  let ask query man flow = None

  let eval exp man flow = None

  let exec stmt man flow =
    let post = man.exec stmt flow ~route:(Below name) in
    let post' =
      (* Collapse all partitions except NotHandled*)
      Cases.remove_duplicates
        (fun case case' ->
          match case,case' with
          | NotHandled,NotHandled -> 0
          | _,NotHandled -> 1
          | NotHandled,_ -> 2
          | _ -> 0
        ) man.lattice post >>% fun flow ->
      (* Since partitions can be tagged with [Some ()] or [None], we
         add this bind to ensure that final result is [Some ()] *)
      Post.return flow
    in
    OptionExt.return post'

end

let () =
  register_stateless_domain (module Domain)
