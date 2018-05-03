(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Smashing-based abstraction of C memory cells. *)

open Framework.Ast
open Framework.Pp
open Framework.Domains.Reduce_unify.Domain
open Framework.Manager
open Framework.Visitor
open Framework.Domains
open Framework.Alarm
open Framework.Flow
open Framework.Lattice
open Framework.Eval
open Framework.Exec
open Universal.Ast
open Ast
open Base
open Pointer

let name = "c.memory.cell.smashing"
let debug fmt = Debug.debug ~channel:name fmt


(** Smashed cell *)
type cell = {
  b : base;
  t : typ;
}

let pp_cell fmt c =
  Format.fprintf fmt "⟪%a,%a⟫"
    pp_base c.b
    pp_typ c.t

let compare_cell c c' =
  compare_composer [
    (fun () -> compare_base c.b c'.b);
    (fun () -> compare_typ c.t c'.t);
  ]

(** Annotate variables with cell information. *)
type var_kind +=
  | V_smash_cell of cell


(*==========================================================================*)
(**                       {2 Abstract domain}                               *)
(*==========================================================================*)


module Domain(SubDomain: Framework.Domains.Stateful.DOMAIN) = struct

  (*==========================================================================*)
  (**                       {2 Lattice structure}                             *)
  (*==========================================================================*)

  (** Set of cells variables. *)
  module CS = Framework.Lattices.Top_set.Make(struct
      type t = var
      let compare = compare_var
      let print fmt v = pp_var fmt v
    end)

  include CS

  let is_bottom x = false

  (** Pretty printer. *)
  let print fmt c =
    Format.fprintf fmt "smash cells: @[%a@]@\n"
      CS.print c

  let unify ctx a1 a2 = a1, a2

  (*==========================================================================*)
  (**                      {2 Sub-domain manager}                             *)
  (*==========================================================================*)

  let local_manager = mk_local_manager (module SubDomain : Framework.Domains.Stateful.DOMAIN with type t = SubDomain.t)

  let local_exec ctx stmt s =
    let flow = set_domain_cur s local_manager local_manager.flow.bottom in
    let flow' = local_manager.exec ctx stmt flow in
    get_domain_cur local_manager flow'


  (*==========================================================================*)
  (**                    {2 Cells Initialization}                             *)
  (*==========================================================================*)

  let init_manager man ctx =
    Init.{
      scalar = (fun v init is_global range flow -> return flow);
      array =  (fun a init is_global range flow -> return flow);
      strct =  (fun s init is_global range flow -> return flow);
    }

  let init man ctx prog flow =
    let flow = set_domain_cur empty man flow in
    match prog.prog_kind with
    | C_program(globals, _) ->
      let flow' = Init.fold_globals (init_manager man ctx) globals ctx flow in
      ctx, flow'

    | _ -> ctx, flow



  (*==========================================================================*)
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)

  let exec (man : ('a, t) manager) subman (ctx : Framework.Context.context) (stmt : stmt) (flow : 'a flow)
    : 'a rflow option =
    let range = stmt.srange in
    match skind stmt with
    | S_c_local_declaration(v, init) ->
      None

    | S_rename_var(v, v') ->
      assert false

    | S_remove_var v when is_c_int_type v.vtyp ->
      None

    | S_assign(lval, rval, mode) when is_c_int_type lval.etyp ->
      None

    | S_assign(lval, rval, smode) when is_c_record_type lval.etyp && is_c_record_type rval.etyp ->
      None

    | _ -> None


  let eval man subman ctx exp flow =
    match ekind exp with
    | E_var ({vkind = V_orig} as v) when is_c_type v.vtyp ->
      None

    | E_c_deref(p) ->
      None

    | E_c_arrow_access(p, i, f) ->
      None

    | E_c_array_subscript(arr, idx) ->
      None

    | E_c_member_access(r, idx, f) ->
      None

    | _ -> None


  and ask : type r. ('a, t) manager -> ('a, SubDomain.t) manager -> Framework.Context.context -> r Framework.Query.query -> 'a Framework.Flow.flow -> r option =
    fun man subman ctx query flow ->
    match query with
      | Query.QExtractVarBase {vkind = V_smash_cell c} ->
        let base_size =
          match c.b with
          | V v -> sizeof_type v.vtyp
          | A {addr_kind = Libs.Stdlib.A_c_static_malloc s} -> s
          | _ -> Framework.Exceptions.panic "smashing.ask: base %a not supported" pp_base c.b
        in
        let cell_size = sizeof_type c.t in
        let offset = mk_constant (C_int_interval (Z.zero, Z.(base_size - cell_size))) (mk_fresh_range ()) ~etyp:T_int in
        Some (c.b, offset)

      | _ -> None

  let refine man subman ctx channel flow = None


end

let setup () =
  register_domain name (module Domain);
  register_vkind_compare (fun next vk1 vk2 ->
      match vk1, vk2 with
      | V_smash_cell c1, V_smash_cell c2 -> compare_cell c1 c2
      | _ -> next vk1 vk2
    );
  register_pp_vkind (fun next fmt vk ->
      match vk with
      | V_smash_cell c -> pp_cell fmt c
      | _ -> next fmt vk
    )
