(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Cell abstraction of low-level C memory operations. *)

open Framework.Ast
open Framework.Pp
open Framework.Domains.Global
open Framework.Manager
open Framework.Domains
open Framework.Flow
open Ast


let name = "c.memory.cell"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                              {2 Cells}                                  *)
(*==========================================================================*)

(** Memory cells. *)
type cell = {
  v: var; (** base variable *)
  o: int; (** offset *)
  t: typ; (** type *)
}

let pp_cell fmt c =
  Format.fprintf fmt "⟨%a,%d,%a⟩"
    pp_var c.v
    c.o
    pp_typ c.t

let compare_cell c c' =
  compare_composer [
    (fun () -> compare_var c.v c'.v);
    (fun () -> compare c.o c'.o);
    (fun () -> compare c.t c'.t); (* TODO replace this compare by a real comparison function over C_AST.type_qual*)
  ]

(** Annotate variables with cell information. *)
type var_kind +=
  | V_cell of cell

let () =
  register_var_compare (fun next v1 v2 ->
      match vkind v1, vkind v2 with
      | V_cell c1, V_cell c2 -> compare_cell c1 c2
      | _ -> next v1 v2
    )

module Make(ValAbs : DOMAIN) = struct

  (*==========================================================================*)
  (**                       {2 Lattice structure}                             *)
  (*==========================================================================*)

  module CS = Framework.Lattices.Top_set.Make(Framework.Utils.Var)

  type t = {
    cs : CS.t; (* set of cells *)
    a  : ValAbs.t (* abstract domain over variables *)
  }

  let print fmt x =
    Format.fprintf fmt "cells: @[%a@]@\n@[%a@]"
      CS.print x.cs
      ValAbs.print x.a

  (*==========================================================================*)
  (**                            {2 Managers}                                 *)
  (*==========================================================================*)

  let subman : ('a, t) manager -> ('a, ValAbs.t) manager =
    fun man ->
    {
      man with
      ax =
        {
          get = (fun x -> (man.ax.get x).a);
          set = (fun y x -> man.ax.set {(man.ax.get x) with a = y} x)
        }
    }

  let rec local_subman =
    let env_manager = Framework.Domains.Global.mk_lattice_manager (module ValAbs : DOMAIN with type t = ValAbs.t) in
    {
      env = env_manager;
      flow = Framework.Flow.lift_lattice_manager env_manager;
      exec = (fun stmt ctx flow -> match ValAbs.exec stmt local_subman ctx flow with Some flow -> flow | None -> assert false);
      eval = (fun exp ctx flow -> match ValAbs.eval exp local_subman ctx flow with Some evl -> evl | None -> eval_singleton (Some exp, flow, []) );
      ask = (fun query ctx flow -> assert false);
      ax = {
        get = (fun env -> env);
        set = (fun env' env -> env');
      }
    }


  let valabs_trivial_exec (stmt : stmt) (a : ValAbs.t) : ValAbs.t =
    debug "trivial exec %a in@ %a" Framework.Pp.pp_stmt stmt ValAbs.print a;
    let a' =
      set_domain_cur a local_subman local_subman.flow.bottom |>
      local_subman.exec stmt Framework.Context.empty |>
      local_subman.flow.get TCur
    in
    debug "res = %a" ValAbs.print a';
    a'

  (*==========================================================================*)
  (**                          {2 Unification}                                *)
  (*==========================================================================*)

  type pexp = Invalid

  type phi_exp =
      Nexp of expr option
    | Pexp of pexp

  (** [phi c u] collects constraints over [c] found in [u] *)
  let phi (c : cell) (u : t) range : phi_exp = assert false

  (** [add_cell c u] adds the cell [c] to the abstraction [u] *)
  let add_cell (c : cell) (u : t) range : t = assert false

  let unify (u : t) (u' : t) range : t * t = assert false


  (*==========================================================================*)
  (**                      {2 Lattice operators}                              *)
  (*==========================================================================*)


  let top = {cs = CS.top; a = ValAbs.top}

  let bottom = {cs = CS.top; a = ValAbs.bottom}

  let join (u : t) (u' : t) : t =
    debug "join:@\n u = @[%a@]@\n u' = @[%a@]" print u print u';
    if ValAbs.leq u.a ValAbs.bottom then
      u'
    else if ValAbs.leq u'.a ValAbs.bottom then
      u
    else
      let range = mk_fresh_range () in
      let u,u' = unify u u' range in
      {u with a = ValAbs.join u.a u'.a}

  let meet (u : t) (u' : t) : t =
    if ValAbs.leq ValAbs.top u.a then
      u'
    else if ValAbs.leq ValAbs.top u'.a then
      u
    else
      let range = mk_fresh_range () in
      let u,u' = unify u u' range in
      {u with a = ValAbs.join u.a u'.a}

  let widening (ctx : Framework.Context.context) (u : t) (u' : t) : t =
    if ValAbs.leq u.a ValAbs.bottom then
      u'
    else if ValAbs.leq u'.a ValAbs.bottom then
      u
    else
      let range = mk_fresh_range () in
      let u,u' = unify u u' range in
      {u with a = ValAbs.widening ctx u.a u'.a}

  let leq (u : t) (u' : t) : bool =
    if ValAbs.leq u.a ValAbs.bottom then
      true
    else if ValAbs.leq u'.a ValAbs.bottom then
      false
    else
      let range = mk_fresh_range () in
      let u,u' = unify u u' range in
      ValAbs.leq u.a u'.a


  let is_top x = ValAbs.is_top x.a

  let is_bottom x = ValAbs.is_bottom x.a


  (*==========================================================================*)
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)


  let init prog (man : ('a, t) manager) (flow : 'a flow) =
    let flow = ValAbs.init prog (subman man) flow in
    let u = get_domain_cur man flow in
    set_domain_cur {u with cs = CS.empty} man flow

  let exec (stmt : stmt) (man : ('a, t) manager) (ctx : Framework.Context.context) (flow : 'a flow)
    : 'a flow option =
    None

  let eval exp man ctx flow =
    None

  let ask request man ctx flow =
    ValAbs.ask request (subman man) ctx flow

end

let setup () =
  register_functor name (module Make)
