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

let annotate_var v =
  match v.vkind with
  | V_orig -> { v with vkind = V_smash_cell {b = V v; t = v.vtyp}}
  | V_smash_cell _ -> v
  | _ -> assert false

let cell_of_var v =
  match v.vkind with
  | V_smash_cell c -> c
  | _ -> assert false

let extract_cell v =
  match vkind v with
  | V_smash_cell c -> c
  | _ -> assert false


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


  let sub_exec subman ctx stmt flow =
    match SubDomain.exec subman ctx stmt flow with
    | Some flow -> flow
    | None -> assert false

  (*==========================================================================*)
  (**                       {2 Cell managements}                              *)
  (*==========================================================================*)

  let remove_overlapping_cells v range man subman ctx flow =
    let u = get_domain_cur man flow in
    let c = cell_of_var v in
    CS.fold (fun v' acc ->
        let c' = extract_cell v' in
        if compare_base c.b c'.b = 0 then
          sub_exec subman ctx (Universal.Ast.mk_remove_var v' range) acc
        else
          acc
      ) u flow


  (*==========================================================================*)
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)

  let rec exec man subman ctx stmt flow =
    let range = stmt.srange in
    match skind stmt with
    | S_c_local_declaration(v, init) ->
      None

    | S_rename_var(v, v') ->
      assert false

    | S_remove_var ({vkind = V_orig | V_smash_cell _} as v) when is_c_int_type v.vtyp ->
      let u = get_domain_cur man flow in
      let v' = annotate_var v in
      let u' = remove v' u in
      let stmt' = {stmt with skind = Universal.Ast.S_remove_var(v')} in
      let flow = set_domain_cur u' man flow in
      SubDomain.exec subman ctx stmt' flow |>
      oflow_compose (add_flow_mergers [mk_remove_var v' stmt.srange])


    | S_assign({ekind = E_var ({vkind = V_orig} as v)} as lval, rval, mode) when is_c_int_type v.vtyp ->
      let v' = annotate_var v in
      let stmt' = {stmt with skind = S_assign(mk_var v' lval.erange, rval, mode)} in
      SubDomain.exec subman ctx stmt' flow |>
      oflow_compose (add_flow_mergers [mk_remove_var v' stmt.srange])

    | S_assign(lval, rval, mode) when is_c_int_type lval.etyp ->
      eval_list [rval; lval] (man.eval ctx) flow |>
      eval_to_orexec (fun el flow ->
          match el with
          | [rval; {ekind = E_var ({vkind = V_smash_cell _} as v)} as lval] ->
            let stmt' = {stmt with skind = S_assign(lval, rval, WEAK)} in
            SubDomain.exec subman ctx stmt' flow |>
            oflow_compose (remove_overlapping_cells v stmt.srange man subman ctx) |>
            oflow_compose (add_flow_mergers [mk_remove_var v stmt.srange])

          | _ -> None
        ) (man.exec ctx) man.flow

    | _ -> None


  and eval man subman ctx exp flow =
    match ekind exp with
    | E_var ({vkind = V_orig} as v) when is_c_type v.vtyp ->
      let v = annotate_var v in
      re_eval_singleton (man.eval ctx) (Some (mk_var v exp.erange), flow, []) |>
      add_eval_mergers []

    | E_c_deref(p) ->
      man.eval ctx (mk_c_resolve_pointer p exp.erange) flow |>
      eval_compose (fun pe flow ->
          match ekind pe with
          | E_c_points_to(E_p_fun fundec) ->
            oeval_singleton (Some ({exp with ekind = E_c_function fundec}), flow, []) |>
            add_eval_mergers []

          | E_c_points_to(E_p_var (base, offset, t)) ->
            debug "E_p_var(%a, %a, %a)" pp_base base pp_expr offset pp_typ t;
            assert false

          | E_c_points_to(E_p_null) ->
            let flow = man.flow.add (Alarms.TNullDeref exp.erange) (man.flow.get TCur flow) flow |>
                       man.flow.set TCur man.env.Framework.Lattice.bottom
            in
            oeval_singleton (None, flow, [])


          | E_c_points_to(E_p_invalid) ->
            let flow = man.flow.add (Alarms.TInvalidDeref exp.erange) (man.flow.get TCur flow) flow |>
                       man.flow.set TCur man.env.Framework.Lattice.bottom
            in
            oeval_singleton (None, flow, [])

          | _ -> assert false
        )

    | E_c_arrow_access(p, i, f) ->
      None

    | E_c_array_subscript(arr, idx) ->
      None

    | E_c_member_access(r, idx, f) ->
      None

    | _ -> None


    (*==========================================================================*)
  (**                    {2 Cells Initialization}                             *)
  (*==========================================================================*)

  and init_manager man subman ctx =
    Init.{
      (* Initialization of arrays *)
      array =  (fun a init is_global range flow ->
          return flow
        );

      (* Initialization of structs *)
      strct =  (fun s init is_global range flow ->
          return flow
        );
    }

  let init man subman ctx prog flow =
    let flow = set_domain_cur empty man flow in
    match prog.prog_kind with
    | C_program(globals, _) ->
      let flow' = Init.fold_globals man ctx (init_manager man subman ctx) globals flow in
      ctx, flow'

    | _ -> ctx, flow





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
