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

(** array access and structure member access transformation to pointer
   arithmetic*)

open Mopsa
open Ast
open Zone


(** {2 Domain definition} *)
(** ===================== *)

let name = "universal.heap.recency"
let debug fmt = Debug.debug ~channel:name fmt

module Domain =
struct

  (** Lattice definition *)
  (** ================== *)

  include Pool

  let is_bottom _ = false

  let print fmt pool =
    Format.fprintf fmt "heap: @[%a@]@\n"
      Pool.print pool

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_u_heap_recency : t domain
  let id = D_u_heap_recency
  let name = "universal.heap.recency"
  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_u_heap_recency -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  let widen = join

  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {export = [Z_u_heap]; import = []}
  let eval_interface = {export = [Z_u_heap, Z_any]; import = []}

  (** Initialization *)
  (** ============== *)

  let init prog man flow =
    Some (
      Flow.set_domain_cur empty man flow |>
      Flow.set_annot KAddr Equiv.empty |>
      Flow.without_callbacks
    )


  (** Post-conditions *)
  (** *************** *)

  let exec zone stmt man flow =
    match skind stmt with
    (* 𝕊⟦ free(addr); ⟧ *)
    | S_free_addr addr ->
      let flow' =
        if is_old addr flow then flow
        else Flow.map_domain_env T_cur (remove addr) man flow
      in
      let stmt' = mk_remove (mk_addr addr stmt.srange) stmt.srange in
      man.exec stmt' flow' |>
      Post.return

    | _ -> None


  (** Evaluations *)
  (** *********** *)

  let eval zone expr man flow =
    match ekind expr with
    | E_alloc_addr(addr_kind, STRONG) ->
      let pool = Flow.get_domain_cur man flow in

      let cs = Callstack.get flow in
      let range = erange expr in

      debug "strongly allocate %a in %a on call stack:@\n @[%a@]@\n%a@\nequiv %a@\n"
        pp_addr_kind addr_kind
        pp_range range
        Callstack.print cs
        print pool
        Equiv.print (Flow.get_annot KAddr flow);

      let recent_uid, flow = get_id_flow (addr_kind, cs, range, recent_flag) flow in
      let recent_addr = {addr_kind; addr_uid = recent_uid; addr_mode = STRONG} in

      (* Change the sub-domain *)
      let flow' =
        if not (Pool.mem recent_addr pool) then
          (* First time we allocate at this site, so no change to the sub-domain. *)
          flow
        else
          (* Otherwise, we make the previous recent address as an old one *)
          let old_uid, flow = get_id_flow (addr_kind, cs, range, old_flag) flow in
          let old_addr = {addr_kind; addr_uid = old_uid; addr_mode = WEAK} in
          Flow.map_domain_cur (add old_addr) man flow |>
          man.exec (mk_rename (mk_addr recent_addr range) (mk_addr old_addr range) range)
      in

      (* Add the recent address *)
      Flow.map_domain_cur (add recent_addr) man flow' |>
      Eval.singleton (mk_addr recent_addr range) |>
      Eval.return


    | E_alloc_addr(addr_kind, WEAK) ->
      let cs = Callstack.get flow in
      let range = erange expr in

      debug "weakly allocate %a in %a on call stack:@\n @[%a@]"
        pp_addr_kind addr_kind
        pp_range range
        Callstack.print cs;

      let old_uid, flow = get_id_flow (addr_kind, cs, range, old_flag) flow in
      let old_addr = {addr_kind; addr_uid = old_uid; addr_mode = WEAK} in
      Flow.map_domain_cur (add old_addr) man flow |>
      Eval.singleton (mk_addr old_addr range) |>
      Eval.return

    | _ -> None

  (** Queries *)
  (** ******* *)

  let ask _ _ _ = None

end

let () =
  Framework.Domain.register_domain (module Domain)
