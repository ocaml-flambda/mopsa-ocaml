(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Recency abstraction of heap allocations. *)

open Framework.Ast
open Framework.Domains
open Framework.Domains.Fun
open Framework.Flow
open Framework.Manager
open Framework.Eval
open Ast

let name = "universal.heap.recency"
let debug fmt = Debug.debug ~channel:name fmt

(** Create a variable representing an attribute of an address. *)
let attribute_var addr attr vtyp =
  let () = Format.(fprintf str_formatter "%a.%s" Pp.pp_addr addr attr) in
  let name = Format.flush_str_formatter () in
  mkv name ~vtyp


module Make(Sub: Framework.Domains.Stateful.DOMAIN) =
struct

  type t = Pool.t * Sub.t

  (*==========================================================================*)
  (**                        {2 Sub-domain manager}                           *)
  (*==========================================================================*)

  (** Create a manager for the sub-domain that can access to the top-level
      transfer functions *)
  let sub_manager (man : ('a, t) manager) : ('a, Sub.t) manager = {
    man with
    ax = {
      get = (fun env -> snd @@ man.ax.get env);
      set = (fun sub env -> man.ax.set (fst @@ man.ax.get env, sub) env);
    }
  }

  (** Create a manager for the sub-domain with a local scope only. *)
  let rec local_sub_manager : (Sub.t, Sub.t) manager =
    let env_manager = Framework.Domains.Stateful.(mk_lattice_manager (module Sub : DOMAIN with type t = Sub.t)) in
    {
      env = env_manager;
      flow = Framework.Flow.lift_lattice_manager env_manager;
      exec = (fun ctx stmt flow -> match Sub.exec local_sub_manager ctx stmt flow with Some flow -> flow | None -> assert false);
      eval = (fun ctx exp flow -> assert false);
      ask = (fun ctx query flow -> assert false);
      ax = {
        get = (fun env -> env);
        set = (fun env' env -> env');
      }
    }

  let move_recent_addr_to_old man ctx recent_addr recent_attributes old_addr range flow =
    Pool.AttrSet.fold (fun attr acc ->
        let recent_var = attribute_var recent_addr attr T_any in
        let old_var = attribute_var old_addr attr  T_any in
        man.exec ctx (
          let range = tag_range range "recent.%s->old.%s" attr attr in
          mk_assign (mk_var old_var (tag_range range "lval")) (mk_var recent_var (tag_range range "")) range
        ) acc |>
        man.exec ctx (mk_remove_var recent_var (tag_range range "remove"))
      ) recent_attributes flow |>
    man.exec ctx (mk_rebase_addr recent_addr old_addr (tag_range range "rename addr"))


  (*==========================================================================*)
  (**                        {2 Lattice definition}                           *)
  (*==========================================================================*)


  let bottom = Pool.bottom, Sub.bottom
  let is_bottom (_, sub) = Sub.is_bottom sub

  let top = Pool.top, Sub.top
  let is_top (pool, sub) = Sub.is_top sub

  let leq (pool1, sub1) (pool2, sub2) =
    Pool.leq pool1 pool2 && Sub.leq sub1 sub2

  let join (pool1, sub1) (pool2, sub2) =
    (** FIXME: this join is imprecise. It may destroy information about weak addresses
        present in only one environments. It needs to take into consideration optional
        dimensions.
    *)
    Pool.join pool1 pool2, Sub.join sub1 sub2

  let meet (pool1, sub1) (pool2, sub2) =
    Pool.meet pool1 pool2, Sub.meet sub1 sub2

  let widening ctx (pool1, sub1) (pool2, sub2) =
    assert false

  let print fmt (pool, sub) =
    Format.fprintf fmt
      "pool:@\n  @[%a@]@\n%a"
      Pool.print pool
      Sub.print sub

  let get_pool (pool, _) = pool

  let set_pool pool (_, sub) = (pool, sub)

  (*==========================================================================*)
  (**                        {2 Transfer functions}                           *)
  (*==========================================================================*)

  let init man ctx prog flow =
    (* Initialize sub domain *)
    let ctx, flow = Sub.init (sub_manager man) ctx prog flow in

    (* After, put an empty pool in the returned cur flow *)
    ctx, man.flow.map (fun env -> function
        | TCur ->
          let (_, sub) = man.ax.get env in
          man.ax.set (Pool.empty, sub) env
        | _ -> env
      ) flow

  let exec man ctx stmt flow =
    match skind stmt with
    (* Assignment to an attribute. *)
    | S_assign({ekind = E_addr_attribute(addr, attr); etyp; erange}, rval, kind) ->
      let lval = mk_var (attribute_var addr attr etyp) erange in
      let kind = if Pool.is_weak addr then WEAK else kind in
      let stmt = mk_assign ~kind lval rval stmt.srange in
      map_domain_cur (fun (pool, sub) -> (Pool.add_attribute addr attr) pool, sub) man flow |>
      man.exec ctx stmt |>
      return

    (* Other statements are given to sub-domain *)
    | _ -> Sub.exec (sub_manager man) ctx stmt flow

  let eval (man: ('a, t) manager) ctx  exp (flow: 'a flow) =
    let range = erange exp in
    match ekind exp with
    (* Allocation of a head address *)
    | E_alloc_addr(addr_kind, addr_range) ->
      let pool = get_domain_cur man flow |>
                 get_pool
      in

      let recent_addr = {
        addr_kind;
        addr_range;
        addr_uid = Pool.recent_uid;
      }

      and old_addr = {
        addr_kind;
        addr_range;
        addr_uid = Pool.old_uid;
      }

      in

      let flow1 = match Pool.mem_recent recent_addr pool, Pool.mem_old old_addr pool with
        | false, false ->
          (* First time we allocate at this site, so no change to the sub-domain. *)
          flow

        | true, weak ->
          (* At least, a previous strong address exists. *)
          debug "old case, weak = %b" weak;
          let recent_attributes = Pool.find_recent recent_addr pool in
          let old_attributes = Pool.find_old old_addr pool |>
                               Pool.AttrSet.join recent_attributes
          in
          (* Move recent attributes to old ones. *)
          let flow1 = move_recent_addr_to_old man ctx recent_addr recent_attributes old_addr range flow in
          (* If the old address already exists, then perform a weak update *)
          let flow2 =
            if weak then man.flow.join flow flow1 else flow1
          in
          let pool = Pool.add_old old_addr old_attributes pool in
          let pool = Pool.add_recent recent_addr Pool.AttrSet.empty pool in
          map_domain_cur (set_pool pool) man flow2

        | false, true ->
          debug "? case";
          assert false
      in

      let flow2 = map_domain_cur (fun cur ->
          let pool = get_pool cur in
          let pool = Pool.add_recent recent_addr Pool.AttrSet.empty pool in
          set_pool pool cur
        ) man flow1
      in

      oeval_singleton (Some (mk_addr recent_addr range), flow2, [])


    (* Read-access to an attribute of a weak address *)
    | E_addr_attribute(addr, attr)
      when Pool.is_weak addr ->
      assert false

    (* Read-access to an attribute of a strong address *)
    | E_addr_attribute(addr, attr) ->
      let v = attribute_var addr attr exp.etyp in
      re_eval_singleton (man.eval ctx) (Some (mk_var v exp.erange), flow, [])

    | E_addr _ ->
      oeval_singleton (Some exp, flow, [])

    | _ ->
      Sub.eval (sub_manager man) ctx exp flow

  let ask man ctx query flow =
    Sub.ask (sub_manager man) ctx query flow


end


let setup () =
  register_domain name (module Make)
