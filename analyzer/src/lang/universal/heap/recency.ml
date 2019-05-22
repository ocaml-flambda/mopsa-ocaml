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
open Framework.Core.Sig.Domain.Intermediate
open Ast
open Zone


(** {2 Domain definition} *)
(** ===================== *)

module Domain(AddrInfo: Pool.ADDRINFO) =
struct

  (** Lattice definition *)
  (** ================== *)

  module Pool = Pool.Make(AddrInfo)

  include Pool

  let is_bottom _ = false

  let print fmt pool =
    Format.fprintf fmt "heap: @[%a@]@\n"
      Pool.print pool

  include GenDomainId(struct
      type typ = t
      let name = "universal.heap.recency" ^ "." ^ AddrInfo.name
    end)


  let widen ctx a b =
    let res = join a b in
    (* le soucis c'est que le widening peut pas appeler un rename global ce qui va pas être très modulaire, non ? *)
    debug "widen@\n@[%a@\n%a@\n= %a@]@\nlastb = %a@\n" print a print b print res pp_addr (Top.top_to_exn b |> Set.max_elt);
    res

  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {provides = [Z_u_heap]; uses = []};
    ieval = {provides = [Z_u_heap, Z_any]; uses = []};
  }


  (** Initialization *)
  (** ============== *)

  let init prog man flow =
    set_domain_env T_cur empty man flow |>
    Flow.set_ctx (Flow.get_ctx flow |> Context.add_unit Pool.ctx_key Equiv.empty)


  (** Post-conditions *)
  (** *************** *)

  let exec zone stmt man flow =
    match skind stmt with
    (* 𝕊⟦ free(addr); ⟧ *)
    | S_free_addr addr ->
      let flow' =
        if is_old addr flow then flow
        else map_domain_env T_cur (remove addr) man flow
      in
      let stmt' = mk_remove (mk_addr addr stmt.srange) stmt.srange in
      man.exec stmt' flow' |>
      Post.return |>
      Option.return

    | _ -> None


  (** Evaluations *)
  (** *********** *)

  let eval zone expr man flow =
    match ekind expr with
    | E_alloc_addr(addr_kind, STRONG) ->
      let pool = get_domain_env T_cur man flow in

      let cs = Callstack.get flow in
      let range = erange expr in

      let recent_uid, flow = get_id_flow (extract (addr_kind, cs, range, recent_flag)) flow in
      let recent_addr = {addr_kind; addr_uid = recent_uid; addr_mode = STRONG} in

      (* Change the sub-domain *)
      let flow' =
        if not (Pool.mem recent_addr pool) then
          let () = debug "first allocation@\n" in
          (* First time we allocate at this site, so no change to the sub-domain. *)
          flow
        else
          let () = debug "rename to perform@\n" in
          (* Otherwise, we make the previous recent address as an old one *)
          let old_uid, flow = get_id_flow (extract (addr_kind, cs, range, old_flag)) flow in
          let old_addr = {addr_kind; addr_uid = old_uid; addr_mode = WEAK} in
          map_domain_env T_cur (add old_addr) man flow |>
          man.exec (mk_rename (mk_addr recent_addr range) (mk_addr old_addr range) range)
      in

      (* Add the recent address *)
      map_domain_env T_cur (add recent_addr) man flow' |>
      Eval.singleton (mk_addr recent_addr range) |>
      Option.return


    | E_alloc_addr(addr_kind, WEAK) ->
      let cs = Callstack.get flow in
      let range = erange expr in

      debug "weakly allocate %a in %a on call stack:@\n @[%a@]"
        pp_addr_kind addr_kind
        pp_range range
        Callstack.print cs;

      let old_uid, flow = get_id_flow (extract (addr_kind, cs, range, old_flag)) flow in
      let old_addr = {addr_kind; addr_uid = old_uid; addr_mode = WEAK} in
      map_domain_env T_cur (add old_addr) man flow |>
      Eval.singleton (mk_addr old_addr range) |>
      Option.return

    | _ -> None

  (** Queries *)
  (** ******* *)

  let ask _ _ _ = None

  let refine channel man flow = Channel.return flow

end

module HeapRecency = Domain(Pool.AddrInfoRecency)
module HeapTypes   = Domain(Pool.AddrInfoTypes)
module HeapTypes2  = Domain(Pool.AddrInfoKindOnly)

let () =
  Framework.Core.Sig.Domain.Intermediate.register_domain (module HeapRecency);
  Framework.Core.Sig.Domain.Intermediate.register_domain (module HeapTypes);
  Framework.Core.Sig.Domain.Intermediate.register_domain (module HeapTypes2)
