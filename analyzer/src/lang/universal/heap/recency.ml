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
(* open Policies *)

type _ query +=
  | Q_allocated_addresses : addr list query
  | Q_select_allocated_addresses : (addr -> bool) -> addr list query

let () =
  register_query {
    join = (
      let f : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_allocated_addresses -> a @ b
          | Q_select_allocated_addresses _ ->
            (* is that ok? *)
            a @ b
          | _ -> next.join_query query a b
      in f
    );
    meet = (
      let f : type r. query_pool -> r query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_allocated_addresses ->
            assert false
          | Q_select_allocated_addresses _ ->
            (* is that ok? *)
            assert false
          | _ -> next.meet_query query a b
      in f
    );
  }

let name = "universal.heap.recency"

let opt_default_allocation_policy : string ref = ref "range_callstack"
let () = Policies.register_option opt_default_allocation_policy name "-default-alloc-pol" "default"

(** {2 Domain definition} *)
(** ===================== *)

module Domain =
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
      let name = name
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
    (* debug "widening@,a = %a@,a'= %a" Pool.print a Pool.print a';
     * (\* Search for strong addresses that belong only to a' and make them weak *\)
     * let aa = Pool.join a a' in
     * let diff = Pool.diff a' a |>
     *            Pool.filter (function ({ addr_mode } as addr) ->
     *                addr_mode = STRONG &&
     *                not (Pool.mem { addr with addr_mode = WEAK } aa)
     *              )
     * in
     * let range = mk_fresh_range () in
     * if Pool.is_empty diff
     * then aa, s, s', true
     * else
     *   let aa, s' = Pool.fold (fun addr (acc,s') ->
     *       debug "widening for address %a" pp_addr addr;
     *       let addr' = { addr with addr_mode = WEAK } in
     *       let acc = Pool.remove addr acc |>
     *                 Pool.add addr'
     *       in
     *       let s' = man.sexec (mk_rename (mk_addr addr range) (mk_addr addr' range) range) ctx s' in
     *       acc, s'
     *     ) diff (aa, s')
     *   in
     *   debug "aa=%a@\n" Pool.print aa;
     *   aa, s, s', true *)

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
    Policies.register_mk_addr (fun _ ak -> (Policies.of_string !opt_default_allocation_policy) ak);
    set_env T_cur Pool.empty man flow


  (** Post-conditions *)
  (** *************** *)

  let exec zone stmt man flow =
    match skind stmt with
    (* 𝕊⟦ free(addr); ⟧ *)
    | S_free_addr addr ->
      let flow' =
        if addr.addr_mode = WEAK then flow
        else map_env T_cur (Pool.remove addr) man flow
      in
      let stmt' = mk_remove (mk_addr addr stmt.srange) stmt.srange in
      man.exec stmt' flow' |>
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

      let recent_addr = Policies.mk_addr addr_kind STRONG range (Flow.get_unit_ctx flow) in

      (* Change the sub-domain *)
      let flow' =
        if not (Pool.mem recent_addr pool) then
          let () = debug "first allocation@\n" in
          (* First time we allocate at this site, so no change to the sub-domain. *)
          flow
        else
          (* Otherwise, we make the previous recent address as an old one *)
          let old_addr = Policies.mk_addr addr_kind WEAK range (Flow.get_unit_ctx flow) in
          debug "rename %a to %a" pp_addr recent_addr pp_addr old_addr;
          let nflow = map_env T_cur (Pool.add old_addr) man flow |>
                        man.exec (mk_rename (mk_addr recent_addr range) (mk_addr old_addr range) range) in
          if not (Pool.mem old_addr pool) then nflow
          else
            Flow.join man.lattice nflow (man.exec (mk_remove (mk_addr recent_addr range) range) flow)
      in

      (* Add the recent address *)
      map_env T_cur (Pool.add recent_addr) man flow' |>
      Eval.singleton (mk_addr recent_addr range) |>
      OptionExt.return

    | E_alloc_addr(addr_kind, WEAK) ->
      let pool = get_env T_cur man flow in
      let weak_addr = Policies.mk_addr addr_kind WEAK range (Flow.get_unit_ctx flow) in

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
    | Q_select_allocated_addresses f ->
      let pool = get_env T_cur man flow in
      Some (
        (* I guess a fold would be better than filter and elements *)
        Pool.elements @@ Pool.filter f pool
      )

    | _ -> None

  let refine channel man flow = Channel.return flow

end



let () =
  Framework.Core.Sig.Stacked.Intermediate.register_stack (module Domain)
