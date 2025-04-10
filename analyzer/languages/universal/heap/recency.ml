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
open Sig.Abstraction.Domain
open Ast

module Pool =
  Framework.Lattices.Powerset.Make
    (struct
      type t = addr
      let compare = compare_addr
      let print = unformat pp_addr
    end)

type ('a,_) query +=
  | Q_alive_addresses : ('a,addr list) query
  | Q_alive_addresses_aspset : ('a,Pool.t) query
  | Q_allocated_addresses_aspset : ('a,Pool.t) query

let () =
  register_query {
    join = (
      let f : type a r. query_pool -> (a,r) query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_alive_addresses -> List.sort_uniq compare_addr (a @ b)
          | Q_alive_addresses_aspset -> Pool.join a b
          | Q_allocated_addresses_aspset -> Pool.join a b
          | _ -> next.pool_join query a b
      in f
    );
    meet = (
      let f : type a r. query_pool -> (a,r) query -> r -> r -> r =
        fun next query a b ->
          match query with
          | Q_alive_addresses -> assert false
          | Q_alive_addresses_aspset -> Pool.meet a b
          | Q_allocated_addresses_aspset -> Pool.meet a b
          | _ -> next.pool_meet query a b
      in f
    );
  }

let name = "universal.heap.recency"

let opt_default_allocation_policy : string ref = ref "range_callstack"
let () = Policies.register_option opt_default_allocation_policy name "-default-alloc-pol" "by default"
           (fun _ ak -> (Policies.of_string !opt_default_allocation_policy) ak)

let gc_time = ref 0.
let gc_nb_collections = ref 0
let gc_nb_addr_collected = ref 0
let gc_max_heap_size = ref 0
(** {2 Domain definition} *)
(** ===================== *)

type stmt_kind +=
   | S_perform_gc

let () =
  register_stmt_with_visitor {
      compare = (fun next s1 s2 ->
        match skind s1, skind s2 with
        | _ -> next s1 s2);

      print = (fun default fmt stmt ->
        match skind stmt with
        | S_perform_gc -> Format.fprintf fmt "Abstract GC call"
        | _ -> default fmt stmt);

      visit = (fun default stmt ->
        match skind stmt with
        | S_perform_gc -> leaf stmt
        | _ -> default stmt);
    }



module Domain =
struct


  (** Domain header *)
  (** ============= *)

  type t = Pool.t

  include GenDomainId(struct
      type nonrec t = t
      let name = name
    end)

  let bottom = Pool.bottom

  let top = Pool.top

  let checks = []

  (** Lattice operators *)
  (** ================= *)

  let is_bottom _ = false

  let subset = Pool.subset

  let join = Pool.join

  let meet = Pool.meet

  let widen ctx = Pool.join

  let merge pre (a,e) (a',e') =
    assert false


  (** Initialization *)
  (** ============== *)

  let init prog man flow = set_env T_cur Pool.empty man flow |> Option.some


  (** Post-conditions *)
  (** *************** *)

  let is_recent addr = addr.addr_mode = STRONG

  let is_old addr = addr.addr_mode = WEAK

  let exec stmt man flow =
    let range = srange stmt in
    match skind stmt with
    (* 𝕊⟦ free(recent); ⟧ *)
    | S_free addr when is_recent addr ->
      let old = { addr with addr_mode = WEAK } in
      get_env T_cur man flow >>$? fun pool flow ->
      (* Inform domains to remove addr *)
      man.exec (mk_remove_addr addr stmt.srange) flow >>%? fun flow' ->
      if not (Pool.mem old pool) then
        (* only recent is present : remove it from the pool and return *)
        map_env T_cur (Pool.remove addr) man flow' |>
        OptionExt.return
      else
        (* old is present : expand it as the new recent *)
        man.exec (mk_expand_addr old [addr] stmt.srange) flow' |>
        OptionExt.return

    (* 𝕊⟦ free(old); ⟧ *)
    | S_free addr when is_old addr ->
       (* Inform domains to invalidate addr *)
       map_env T_cur (Pool.remove addr) man flow >>%? fun flow ->
       man.exec (mk_invalidate_addr addr stmt.srange) flow |>
       OptionExt.return

    | S_perform_gc ->
       let startt = Sys.time () in
       get_env T_cur man flow >>$? fun all flow ->
       let alive = ask_and_reduce man.ask Q_alive_addresses_aspset flow in
       let dead = Pool.diff all alive in
       debug "at %a, |dead| = %d@.dead = %a" pp_range range (Pool.cardinal dead) (format Pool.print) dead;
       let trange = tag_range range "agc" in
       (* let's free weak addresses first, and then the strong ones *)
       let dead_strong, dead_weak = Pool.partition (fun addr -> is_recent addr) dead in
       let post = Pool.fold (fun addr acc ->
                      debug "free %a" pp_addr addr;
                      acc >>% man.exec (mk_stmt (S_free addr) trange)) dead_weak (Post.return flow) in
       post >>% (fun flow ->
       let post = Pool.fold (fun addr acc ->
                      debug "free %a" pp_addr addr;
                      acc >>% man.exec (mk_stmt (S_free addr) trange)) dead_strong (Post.return flow) in
       let delta = Sys.time () -. startt in
       gc_time := !gc_time +. delta;
       incr gc_nb_collections;
       gc_nb_addr_collected := !gc_nb_addr_collected + (Pool.cardinal dead);
       gc_max_heap_size := max !gc_max_heap_size (Pool.cardinal all);
       post) |> OptionExt.return

    | _ -> None


  (** Evaluations *)
  (** *********** *)

  let eval expr man flow =
    let range = erange expr in
    match ekind expr with
    | E_alloc_addr(addr_kind, STRONG) ->
      get_env T_cur man flow >>$? fun pool flow ->

      let recent_addr = Policies.mk_addr addr_kind STRONG range (Flow.get_callstack flow) in

      if not (Pool.mem recent_addr pool) then
        (* first allocation at this site: just add the address to the pool and return it *)
        map_env T_cur (Pool.add recent_addr) man flow >>%? fun flow ->
        Eval.singleton (mk_addr recent_addr range) flow |>
        OptionExt.return
      else
        let old_addr = Policies.mk_addr addr_kind WEAK range (Flow.get_callstack flow) in
        if not (Pool.mem old_addr pool) then
          (* old address not present: rename the existing recent as old and return the new recent *)
          map_env T_cur (Pool.add old_addr) man flow >>%? fun flow ->
          man.exec (mk_rename_addr recent_addr old_addr range) flow >>%? fun flow ->
          Eval.singleton (mk_addr recent_addr range) flow |>
          OptionExt.return
        else
          (* old present : copy the content of the existing recent to old using `fold` statement *)
          man.exec (mk_fold_addr old_addr [recent_addr] range) flow >>%? fun flow ->
          Eval.singleton (mk_addr recent_addr range) flow |>
          OptionExt.return

    | E_alloc_addr(addr_kind, WEAK) ->
      get_env T_cur man flow >>$? fun pool flow ->
      let weak_addr = Policies.mk_addr addr_kind WEAK range (Flow.get_callstack flow) in

      let post =
        if Pool.mem weak_addr pool then
          Post.return flow
        else
          map_env T_cur (Pool.add weak_addr) man flow
      in
      post >>% Eval.singleton (mk_addr weak_addr range) |>
      OptionExt.return


    | _ -> None

  (** Queries *)
  (** ******* *)

  let ask : type r. ('a,r) query -> ('a, t) man -> 'a flow -> ('a, r) cases option =
    fun query man flow ->
    match query with
    | Q_allocated_addresses ->
      get_env T_cur man flow >>$? fun pool flow ->
      if Pool.is_top pool then Some (Cases.singleton [] flow)
      else Some (Cases.singleton (Pool.elements pool) flow)

    | Q_allocated_addresses_aspset ->
      Some (get_env T_cur man flow)

    | _ -> None


  (** Pretty printer *)
  (** ************** *)

  let print_state printer pool =
    pprint printer ~path:[Key "heap"]
      (pbox Pool.print pool)

  let print_expr man flow printer exp = ()


end


let () =
  register_standard_domain (module Domain)
