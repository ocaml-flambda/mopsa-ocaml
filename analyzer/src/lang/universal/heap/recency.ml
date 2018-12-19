(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
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

module Domain : Framework.Domains.Stacked.S =
struct

  (** Lattice definition *)
  (** ================== *)

  include Pool

  let join annot subman (abs1, sub1) (abs2, sub2) =
    join annot abs1 abs2, sub1, sub2

  let meet annot subman (abs1, sub1) (abs2, sub2) =
    meet annot abs1 abs2, sub1, sub2

  let widen annot subman (abs1, sub1) (abs2, sub2) =
    widen annot abs1 abs2, true, sub1, sub2

  let subset subman (abs1, sub1) (abs2, sub2) =
    subset abs1 abs2, sub1, sub2

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

  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {export = []; import = []}
  let eval_interface = {export = [Z_u_heap, Z_any]; import = []}

  (** Initialization *)
  (** ============== *)

  let init prog man flow =
    Some (
      Flow.set_domain_cur empty man flow |>
      Flow.set_annot KAddr Equiv.empty
    )

  (** Post-conditions *)
  (** *************** *)

  let exec zone stmt man flow =
    None

  (** Evaluations *)
  (** *********** *)

  let eval zone expr man flow =
    match ekind expr with
    | E_alloc_addr(addr_kind) ->
      begin
        let pool = Flow.get_domain_cur man flow in

        let cs = Callstack.get flow in
        let range = erange expr in
        let recent_uid, flow = get_id_flow (cs, range, 0) flow in
        let old_uid, flow = get_id_flow (cs, range, 1) flow in

        let recent_addr = {addr_kind; addr_uid = recent_uid} in
        let old_addr = {addr_kind; addr_uid = old_uid} in

        (match Pool.mem_recent recent_addr pool, Pool.mem_old old_addr pool with
        | false, false ->
          (* First time we allocate at this site, so no change to the sub-domain. *)
          flow

        | true, false ->
          (* Only a previous strong address exists =>
             Rebase the previous strong address with strong updates. *)
          man.exec (mk_rebase_addr old_addr recent_addr STRONG range) flow

        | true, true ->
          (* Both strong and weak addresses exist =>
             Rebase the previous strong address with weak updates. *)
          man.exec (mk_rebase_addr old_addr recent_addr WEAK range) flow


        | false, true ->
          Exceptions.panic "? case")
        |> Flow.set_domain_cur (add_recent recent_addr pool) man
        |> Eval.singleton (mk_addr recent_addr (tag_range range "mk_recent_addr"))
        |> OptionExt.return
      end
    | _ -> None

  (** Queries *)
  (** ******* *)

  let ask _ _ _ = None

end

let () =
  Framework.Domains.Stacked.register_domain (module Domain)
