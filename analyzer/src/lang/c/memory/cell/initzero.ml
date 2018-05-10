(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Memory initialization to zero predicate abstraction. *)

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
open Universal.Utils

let name = "c.memory.cell.initzero"
let debug fmt = Debug.debug ~channel:name fmt

type minmax =
  | Min | Max
let print_minmax fmt = function
  | Max -> Format.fprintf fmt "⇥"
  | Min -> Format.fprintf fmt "⇤"
let compare_minmax = compare

type bound =
  {
    b : base;
    m : minmax;
  }
let print_bound fmt c =
  Format.fprintf fmt "%a%a" print_minmax c.m pp_base c.b
let compare_bound c c' =
  compare_composer [
    (fun () -> compare_base c.b c'.b);
    (fun () -> compare_minmax c.m c'.m);
  ]

type var_kind +=
  | V_zero of bound

let annotate_var (v : var) (c : bound) = match v.vkind with
  | V_orig -> {v with vkind = V_zero c}
  | _ -> Debug.fail "tried to annotate var : %a with %a" Framework.Pp.pp_var v print_bound c

let vmin (v : var) =
  annotate_var v {b = V v ; m = Min}
let vmax (v : var) =
  annotate_var v {b = V v ; m = Max}

let is_origin (v : var) = match v.vkind with
  | V_orig -> true
  | _ -> false

let var_of_base b m =
  {vname = (let () = Format.fprintf Format.str_formatter "%a" print_bound {b ; m} in Format.flush_str_formatter ());
   vuid = base_uid b;
   vtyp = T_int;
   vkind = V_zero {b ; m}
  }

let incrs range v =
  mk_stmt (S_assign(
      mk_var v (tag_range range "v"),
      mk_binop
        (mk_var v (tag_range range "v"))
        (O_plus T_int)
        (mk_one (tag_range range "1"))
        (tag_range range "v+1"),
      STRONG
    )
    ) (tag_range range "v++")

let decrs range v =
  mk_stmt (S_assign(
      mk_var v (tag_range range "v"),
      mk_binop
        (mk_var v (tag_range range "v"))
        (O_minus T_int)
        (mk_one (tag_range range "1"))
        (tag_range range "v-1"),
      STRONG
    )
    ) (tag_range range "v--")

let diff range a b =
  mk_binop
    a
    (O_minus T_int)
    b
    (tag_range range "dvmod")

let mk_eq a b r : expr =
  mk_binop a O_eq b ~etyp:(T_int) r

(*==========================================================================*)
(**                       {2 Abstract domain}                               *)
(*==========================================================================*)


module Domain(SubDomain: Framework.Domains.Stateful.DOMAIN) = struct

  (*==========================================================================*)
  (**                       {2 Lattice structure}                             *)
  (*==========================================================================*)

  let is_bottom x = false
  let top = ()
  let bottom = ()
  let join = ()
  let meet = ()
  let meet = ()
  let widening = ()


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
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)

  let rec exec man subman ctx stmt flow =
    let range = stmt.srange in
    match skind stmt with
    | S_c_local_declaration(v, init) ->
      return_flow flow

    | S_rename_var(v , v') when is_origin v && is_origin v' ->
      let smin =
        mk_stmt
          (S_rename_var(vmin v, vmin v'))
          (tag_range range "rename_min")
      in
      let smax =
        mk_stmt
          (S_rename_var(vmax v, vmax v'))
          (tag_range range "rename_min")
      in
      let stmt' = mk_block [smin; smax] (tag_range range "rename_block") in
      SubDomain.exec subman ctx stmt' flow |>
      oflow_compose (add_flow_mergers [mk_remove_var (vmin v) (tag_range range "vmin merger");
                                       mk_remove_var (vmax v) (tag_range range "vmax merger");])

    | S_remove_var ({vkind = V_orig } as v) ->
      let v_min = vmin v in
      let v_max = vmax v in
      let smin = mk_stmt (Universal.Ast.S_remove_var v_min ) (tag_range range "remove_min") in
      let smax = mk_stmt (Universal.Ast.S_remove_var v_max ) (tag_range range "remove_max") in
      let stmt' = mk_block [smin; smax] (tag_range range "remove_block") in
      SubDomain.exec subman ctx stmt' flow |>
      oflow_compose (add_flow_mergers [mk_remove_var v_min (tag_range range "vmin merger");
                                       mk_remove_var v_max (tag_range range "vmax merger")])


    | S_assign({ekind = E_var ({vkind = V_orig} as v)} as lval, rval, mode) when is_c_int_type v.vtyp ->
      man.eval ctx rval flow |>
      eval_to_rexec (fun rval flow ->
          let p = mk_c_address_of lval (tag_range lval.erange "addrof") in
          man.eval ctx p flow |>
          eval_to_rexec (fun plval flow ->
              match ekind plval with
              | E_c_points_to(E_p_var (base, offset, t)) ->
                let vmin = var_of_base base Min in
                let vmax = var_of_base base Max in
                let abandon =
                  mk_block
                    [
                      mk_remove_var vmin (tag_range range "gu vmin");
                      mk_remove_var vmax (tag_range range "gu vmax")
                    ]
                    (tag_range range "gu block")
                in

                let diff_vmax_o = diff range (mk_var vmax (tag_range range "vmax-o")) offset in
                let diff_vmin_o = diff range (mk_var vmin (tag_range range "vmin-o")) offset in

                let c0 (* e2=0? *)=
                  mk_binop rval O_eq (mk_zero (tag_range range "0"))
                    (tag_range range "rv=0")
                in
                let c1 (* off(p) = vmax*) =
                  mk_eq diff_vmax_o (mk_int 0 (tag_range range "int"))
                    (tag_range range "off(p) = vmax")
                in
                let c2 (* off(p) = vmin -1 *) =
                  mk_eq diff_vmin_o (mk_int 1 (tag_range range "int"))
                    (tag_range range "off(p) = vmin -1")
                in
                let c3 (* off(p) = vmax -1*) =
                  mk_eq diff_vmax_o (mk_int 1 (tag_range range "int"))
                    (tag_range range "off(p) = vmax -1")
                in
                let c4 (* off(p) = vmin*) =
                  mk_eq diff_vmin_o (mk_int 1 (tag_range range "int"))
                    (tag_range range "off(p) = vmin")
                in
                let c5 (* off(p) > vmin*) =
                  mk_binop diff_vmin_o O_lt (mk_int 0 (tag_range range "int"))
                    (tag_range range "off(p) > vmin")
                in
                let c6 (* off(p) < vmax -1 *) =
                  mk_binop diff_vmax_o O_gt (mk_int 1 (tag_range range "int"))
                    (tag_range range "off(p) < vmax -1")
                in
                let c7 (* off(p) >= vmax *) =
                  mk_binop diff_vmax_o O_le (mk_int 0 (tag_range range "int"))
                    (tag_range range "off(p) >= vmax")
                in
                let c8 (* off(p) < vmin *) =
                  mk_binop diff_vmin_o O_gt (mk_int 0 (tag_range range "int"))
                    (tag_range range "off(p) < vmin")
                in
                switch_rexec
                  [([(c0, true); (c1, true)], (fun flow ->
                       man.exec ctx (incrs range vmax) flow |> add_flow_mergers [mk_remove_var vmax stmt.srange])
                    );
                   ([(c0, true); (c2, true)], (fun flow ->
                        man.exec ctx (decrs range vmin) flow |> add_flow_mergers [mk_remove_var vmin stmt.srange])
                   );
                   ([(c0, true); (c1, false); (c2, false)], (fun flow -> Framework.Domains.Reduce.Domain.return_flow_no_opt flow)
                   );
                   ([(c0, false); (c3, true)], (fun flow ->
                        man.exec ctx (decrs range vmax) flow |> add_flow_mergers [mk_remove_var vmax stmt.srange])
                   );
                   ([(c0, false); (c4, true)], (fun flow ->
                        man.exec ctx (incrs range vmin) flow |> add_flow_mergers [mk_remove_var vmin stmt.srange])
                   );
                   ([(c0, false); (c5, true); (c6, true)], (fun flow ->
                        man.exec ctx abandon flow |> add_flow_mergers [mk_remove_var vmin stmt.srange; mk_remove_var vmax stmt.srange])
                   );
                   ([(c0, false); (c7, true)], ( fun flow ->
                        Framework.Domains.Reduce.Domain.return_flow_no_opt flow)
                   );
                   ([(c0, false); (c8, true)], (fun flow ->
                        Framework.Domains.Reduce.Domain.return_flow_no_opt flow)
                   );
                  ]
                  man ctx flow

              | E_c_points_to(E_p_null) ->
                man.flow.add (Alarms.TNullDeref lval.erange) (man.flow.get TCur flow) flow |>
                man.flow.set TCur man.env.Framework.Lattice.bottom |>
                Framework.Domains.Reduce.Domain.return_flow_no_opt

              | E_c_points_to(E_p_invalid) ->
                man.flow.add (Alarms.TInvalidDeref lval.erange) (man.flow.get TCur flow) flow |>
                man.flow.set TCur man.env.Framework.Lattice.bottom |>
                Framework.Domains.Reduce.Domain.return_flow_no_opt
              | _ -> assert false
            ) (man.exec ctx) man.flow
        ) (man.exec ctx) man.flow
      |> fun x -> Some x
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
