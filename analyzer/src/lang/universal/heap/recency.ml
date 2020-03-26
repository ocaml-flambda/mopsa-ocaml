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


(** Abstraction of the heap *)


open Mopsa
open Framework.Core.Sig.Stacked.Intermediate
open Ast
open Zone
open Policies


type _ query +=
  | Q_allocated_addresses : addr list query

let () =
  register_query {
    join = (
      let f : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_allocated_addresses -> a @ b
          | _ -> next.join_query query a b
      in f
    );
    meet = (
      let f : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_allocated_addresses -> assert false
          | _ -> next.meet_query query a b
      in f
    );
  }


(** {2 Domain definition} *)
(** ===================== *)

module Domain(Policy: POLICY) =
struct

  (** Domain header *)
  (** ============= *)

  module Pool = Framework.Lattices.Powerset.Make(
    struct
      type t = addr
      let compare = compare_addr
      let print = pp_addr
    end
    )

  type t = Pool.t

  include GenDomainId(struct
      type nonrec t = t
      let name = "universal.heap.recency" ^ "." ^ Policy.name
    end)

  let print fmt pool =
    Format.fprintf fmt "heap: @[%a@]@\n"
      Pool.print pool

  let bottom = Pool.bottom

  let top = Pool.top

  let alarms = []

  (** Lattice operators *)
  (** ================= *)

  let is_bottom _ = false

  let subset man ctx (a,s) (a',s') =
    Pool.subset a a', s, s'

  let join man ctx (a,s) (a',s') =
    Pool.join a a', s, s'

  let meet man ctx (a,s) (a',s') =
    Pool.meet a a', s, s'

  let widen man ctx (a,s) (a',s') =
    Pool.join a a', s, s', true

  let merge pre (a,log) (a',log') =
    assert false

  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {provides = [Z_u_heap]; uses = []};
    ieval = {provides = [Z_u_heap, Z_any]; uses = []};
  }


  (** Initialization *)
  (** ============== *)

  let init prog man flow =
    set_env T_cur Pool.empty man flow


  (** Post-conditions *)
  (** *************** *)

  let is_recent addr = addr.addr_mode = STRONG

  let is_old addr = addr.addr_mode = WEAK

  let exec zone stmt man flow =
    match skind stmt with
    (* 𝕊⟦ free(recent); ⟧ *)
    | S_free addr when is_recent addr ->
      let old = { addr with addr_mode = WEAK } in
      let pool = get_env T_cur man flow in
      (* Inform domains to remove addr *)
      let flow' = man.exec (mk_remove_addr addr stmt.srange) flow in
      if not (Pool.mem old pool) then
        (* only recent is present : remove it from the pool and return *)
        map_env T_cur (Pool.remove addr) man flow' |>
        Post.return |>
        OptionExt.return
      else
        (* old is present : expand it as the new recent *)
        man.exec (mk_expand_addr old [addr] stmt.srange) flow' |>
        Post.return |>
        OptionExt.return
      
    (* 𝕊⟦ free(old); ⟧ *)
    | S_free addr when is_old addr ->
      (* Inform domains to invalidate addr *)
      man.exec (mk_invalidate_addr addr stmt.srange) flow |>
      Post.return |>
      OptionExt.return


    | _ -> None


  (** Evaluations *)
  (** *********** *)

  let eval zone expr man flow =
    let range = erange expr in
    match ekind expr with
    | E_alloc_addr(addr_kind, STRONG) ->
      let pool = get_env T_cur man flow in

      let recent_addr = Policy.mk_addr addr_kind STRONG range flow in
      
      if not (Pool.mem recent_addr pool) then
        (* first allocation at this site: just add the address to the pool and return it *)
        map_env T_cur (Pool.add recent_addr) man flow |>
        Eval.singleton (mk_addr recent_addr range) |>
        OptionExt.return
      else
        let old_addr = Policy.mk_addr addr_kind WEAK range flow in
        if not (Pool.mem old_addr pool) then
          (* old address not present: rename the existing recent as old and return the new recent *)
          map_env T_cur (Pool.add old_addr) man flow |>
          man.exec (mk_rename_addr recent_addr old_addr range) |>
          Eval.singleton (mk_addr recent_addr range) |>
          OptionExt.return
        else
          (* old present : copy the content of the existing recent to old using `fold` statement *)
          man.exec (mk_fold_addr old_addr [recent_addr] range) flow |>
          Eval.singleton (mk_addr recent_addr range) |>
          OptionExt.return

    | E_alloc_addr(addr_kind, WEAK) ->
      let pool = get_env T_cur man flow in
      let weak_addr = Policy.mk_addr addr_kind WEAK range flow in

      let flow' =
        if Pool.mem weak_addr pool then
          flow
        else
          map_env T_cur (Pool.add weak_addr) man flow
      in
      Eval.singleton (mk_addr weak_addr range) flow' |>
      OptionExt.return


    | _ -> None

  (** Queries *)
  (** ******* *)

  let ask : type r. r query -> ('a, t, 's) man -> 'a flow -> r option =
    fun query man flow ->
    match query with
    | Q_allocated_addresses ->
      let pool = get_env T_cur man flow in
      Some (Pool.elements pool)
    | _ -> None

  let refine channel man flow = Channel.return flow

end




module Heap1 = Domain(StackRangePolicy)
module Heap2 = Domain(StackPolicy)
module Heap3 = Domain(AllPolicy)

let () =
  Framework.Core.Sig.Stacked.Intermediate.register_stack (module Heap1);
  Framework.Core.Sig.Stacked.Intermediate.register_stack (module Heap2);
  Framework.Core.Sig.Stacked.Intermediate.register_stack (module Heap3)
