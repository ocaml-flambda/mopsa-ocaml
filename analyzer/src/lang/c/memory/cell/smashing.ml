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

let var_of_new_cell c =
  { vname = (let () = Format.fprintf Format.str_formatter "%a" pp_cell c in Format.flush_str_formatter ());
    vuid = base_uid c.b;
    vtyp = c.t;
    vkind = V_smash_cell c;
  }


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

  let expand_relation subman ctx v range flow =
    let tmp = mktmp ~vkind:(v.vkind) ~vtyp:(v.vtyp) () in
    let stmt = mk_assign (mk_var tmp range) (mk_var v range) ~mode:EXPAND range in
    debug "expand stmt: %a" pp_stmt stmt;
    tmp, sub_exec subman ctx stmt flow

  (*==========================================================================*)
  (**                       {2 Cell managements}                              *)
  (*==========================================================================*)

  let mem_pred c = function {vkind = V_smash_cell c'} -> compare_cell c c' = 0 | _ -> false

  let exist_and_find_cell c cs =
    let exception Found of var * cell in
    try
      let () = CS.iter (fun v -> if mem_pred c v then raise (Found (v, c)) else ()) cs in
      None
    with
    | Found (v, c) -> Some (v, c)

  let var_of_cell (c: cell) cs =
    debug "var_of_cell %a in @[%a@]" pp_cell c print cs;
    match exist_and_find_cell c cs with
    | Some (v, _) -> debug "already exists"; v
    | None -> 
      debug "new cell"; 
      var_of_new_cell c
        
  let add_var ctx range (v : var) (u, s) =
    debug "add_var %a in %a" pp_var v print u;
    if CS.mem v u then u, s
    else if not (is_c_scalar_type v.vtyp) then u, s
    else if is_c_pointer_type v.vtyp then CS.add v u, s
    else
      let c = cell_of_var v in
      let (a, b) = rangeof c.t in
      let e = mk_z_interval a b range in
      let stmt = Universal.Ast.(mk_assume (mk_binop (mk_var v range) O_eq e range) range) in
      CS.add v u, local_exec ctx stmt s

  let remove_overlapping_cells v range man subman ctx flow =
    let u = get_domain_cur man flow in
    let u' = add v u in
    let flow' = set_domain_cur u' man flow in
    let c = cell_of_var v in
    let flow'', mergers = CS.fold (fun v' (flow, mergers) ->
        let c' = extract_cell v' in
        if compare_base c.b c'.b = 0 && compare_typ c.t c'.t <> 0 then
          let flow' = map_domain_cur (remove v') man flow |>
                      sub_exec subman ctx (Universal.Ast.mk_remove_var v' range)
          in
          flow', (mk_remove_var v' range) :: mergers
        else
          flow, mergers
      ) u (flow', [])
    in
    add_flow_mergers mergers flow''

  let eval_base_offset f err empty x0 base offset typ range man subman ctx flow =
    if man.flow.is_cur_bottom flow then empty ()
    else
      let cell_size = sizeof_type typ in
      
      let rec static_offset_case base_size =
      debug "static base case";
      match Universal.Utils.expr_to_z offset with
      | Some z when Z.geq z Z.zero && Z.leq (Z.add z cell_size) base_size  ->
        create_smash x0 flow

      | Some z ->
        debug "error, z = %a, cell_size = %a, base_size = %a" Z.pp_print z Z.pp_print cell_size Z.pp_print base_size;
        man.flow.add (Alarms.TOutOfBound range) (man.flow.get TCur flow) flow |>
        man.flow.set TCur man.env.bottom |>
        err x0

      | None ->
        dynamic_offset_case base_size

    and dynamic_offset_case base_size =
      debug "non-constant cell offset";
      (* Fast bound check with intervals *)
      let rec fast () =
        debug "trying fast check";
        let v = man.ask ctx (Universal.Numeric.Query.QIntInterval offset) flow in
        match v with
        | None -> assert false
        | Some itv ->
          try
            debug "offset interval = %a" Universal.Numeric.Values.Int.print itv;
            let l, u = Universal.Numeric.Values.Int.get_bounds itv in
            if Z.geq l Z.zero && Z.leq (Z.add u cell_size) base_size then
              create_smash x0 flow
            else if Z.lt u Z.zero || Z.gt (Z.add l cell_size) base_size then
              man.flow.add (Alarms.TOutOfBound range) (man.flow.get TCur flow) flow |>
              man.flow.set TCur man.env.bottom |>
              err x0
            else
              full ()
          with Universal.Numeric.Values.Int.Unbounded ->
            full ()


      (* Full bound check *)
      and full () =
        let safety_cond =
          mk_binop
            (mk_binop offset O_ge (mk_zero range) range)
            O_log_and
            (mk_binop (mk_binop offset math_plus (mk_z (sizeof_type typ) range) range) O_le (mk_z base_size range) range)
            range
        in
        let safe_case acc flow =
          create_smash acc flow
        in
        let error_case acc flow =
          man.flow.add (Alarms.TOutOfBound range) (man.flow.get TCur flow) flow |>
          man.flow.set TCur man.env.bottom |>
          err acc
        in
        if_flow
          (man.exec ctx (mk_assume safety_cond range))
          (man.exec ctx (mk_assume (mk_not safety_cond range) range))
          (safe_case x0)
          (error_case x0)
          (empty)
          (fun sflow eflow -> error_case (safe_case x0 sflow) eflow)
          man flow
      in
      (* Start with fast check *)
      fast ()

    and create_smash acc flow =
      let cs = get_domain_cur man flow in
      let v = var_of_cell {b = base; t = typ} cs in
      let s = get_domain_cur subman flow in
      let (cs', s') = add_var ctx range v (cs, s) in
      let flow' = set_domain_cur cs' man flow |>
                  set_domain_cur s' subman
      in
      f acc v flow'
    in

    match base with
    | V v -> static_offset_case (sizeof_type v.vtyp)
    | A {addr_kind = Libs.Stdlib.A_c_static_malloc s} -> static_offset_case s
    | _ -> Framework.Exceptions.panic "base %a not supported" pp_base base



  (*==========================================================================*)
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)

  let assign_var man subman ctx lval v rval mode range flow =
    let cs = get_domain_cur man flow in
    match v.vkind with
    | V_smash_cell c ->
      let v = var_of_cell c cs in
      let lval = {lval with ekind = E_var v} in
      (* Infer the mode of assignment *)
      let mode = match c.b with
        (* TODO: process the case of heap addresses *)
        | A a -> WEAK
        | V v ->
          (* In case of a base variable, we check that we are writing to the whole memory block *)
          if Z.equal (sizeof_type v.vtyp) (sizeof_type lval.etyp) then STRONG
          else WEAK
      in

      let stmt = mk_assign lval rval ~mode range in
      SubDomain.exec subman ctx stmt flow |>
      oflow_compose (
        fun flow ->
          if is_c_int_type v.vtyp then
            let rmin, rmax = rangeof v.vtyp in
            let cond = range_cond lval rmin rmax (erange lval) in
            let stmt' = (mk_assume cond (tag_range range "assume range")) in
            let () = debug "cell_smash assume %a" Framework.Pp.pp_stmt stmt' in
            match SubDomain.exec subman ctx stmt' flow with
            | Some flow -> flow
            | None -> assert false
          else
            flow
      ) |>
      oflow_compose (remove_overlapping_cells v stmt.srange man subman ctx) |>
      oflow_compose (append_flow_mergers [mk_remove_var v stmt.srange])

    | _ -> assert false

    
  let rec exec man subman ctx stmt flow =
    let range = stmt.srange in
    match skind stmt with
    | S_c_local_declaration({vkind = V_orig} as v, init) ->
      let v' = annotate_var v in
      Init.init_local (init_manager man subman ctx) v' init range flow |>
      return_flow

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

    | S_assign(lval, rval, mode) when is_c_scalar_type lval.etyp ->
      (* For the rval, we use the manager eval to simplify the expression *)
      man.eval ctx rval flow |>
      eval_to_orexec (fun rval flow ->
          (* However, the lval, we use our local eval so that the reduced product will not evict our simplifications *)
          eval man subman ctx lval flow |>
          oeval_to_orexec (fun (lval, _) flow ->
              match ekind lval with
              | E_var ({vkind = V_smash_cell _} as v)->
                assign_var man subman ctx lval v rval mode stmt.srange flow
              | _ -> None
            ) (man.exec ctx) man.flow
        ) (man.exec ctx) man.flow
    | _ -> None


  and eval man subman ctx exp flow =
    let range = exp.erange in
    match ekind exp with
    | E_var ({vkind = V_orig} as v) when is_c_type v.vtyp ->
      let v = annotate_var v in
      let u = get_domain_cur man flow in
      let s = get_domain_cur subman flow in
      let (u', s') = add_var ctx exp.erange v (u, s) in
      debug "new variable %a in %a" pp_var v print u';
      let flow' = set_domain_cur u' man flow |>
                  set_domain_cur s' subman
      in
      re_eval_singleton (man.eval ctx) (Some (mk_var v exp.erange), flow', []) |>
      add_eval_mergers []

    | E_c_deref(p) ->
      man.eval ctx (mk_c_resolve_pointer p range) flow |>
      eval_compose (fun pe flow ->
          match ekind pe with
          | E_c_points_to(E_p_fun fundec) ->
            oeval_singleton (Some ({exp with ekind = E_c_function fundec}), flow, []) |>
            add_eval_mergers []

          | E_c_points_to(E_p_var (base, offset, t)) ->
            debug "E_p_var(%a, %a, %a)" pp_base base pp_expr offset pp_typ t;
            eval_base_offset
              (fun acc v flow ->
                 let v', flow' = expand_relation subman ctx v range flow in
                 let exp' = {exp with ekind = E_var v'} in
                 (** FIXME: filter flow with (p == &v) *)
                 oeval_singleton (Some (exp', []), flow', [mk_remove_var v' range]) |>
                 oeval_join acc
              )
              (fun acc eflow -> oeval_singleton (None, eflow, []) |> oeval_join acc)
              (fun () -> oeval_singleton (None, flow, []))
              None base offset t exp.erange man subman ctx flow

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


    | E_c_array_subscript(arr, idx) ->
      man.eval ctx (mk_c_resolve_pointer arr exp.erange) flow |>
      eval_compose (fun pe flow ->
          match ekind pe with
          | E_c_points_to(E_p_fun fundec) ->
            Debug.fail "arrow access to function pointers"

          | E_c_points_to(E_p_var (base, offset, t)) ->
            man.eval ctx idx flow |>
            eval_compose (fun idx flow ->
                let offset' = mk_binop offset math_plus (mk_binop idx math_mult (mk_z (sizeof_type t) exp.erange) exp.erange) exp.erange in
                debug "offset' = %a" pp_expr offset';
                eval_base_offset
                  (fun acc v flow ->
                     let v', flow' = expand_relation subman ctx v range flow in
                     let exp' = {exp with ekind = E_var v'} in
                     oeval_singleton (Some (exp', []), flow', [mk_remove_var v' range]) |>
                     oeval_join acc
                  )
                  (fun acc eflow -> oeval_singleton (None, eflow, []) |> oeval_join acc)
                  (fun () -> oeval_singleton (None, flow, []))
                  None base offset' t exp.erange man subman ctx flow
              )

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
      man.eval ctx (mk_c_resolve_pointer p exp.erange) flow |>
      eval_compose (fun pe flow ->
          match ekind pe with
          | E_c_points_to(E_p_fun fundec) ->
            Debug.fail "arrow access to function pointers"

          | E_c_points_to(E_p_var (base, offset, t)) ->
            let record = match remove_typedef t with T_c_record r -> r | _ -> assert false in
            let field = List.nth record.c_record_fields i in
            let offset' = mk_binop offset math_plus (mk_int field.c_field_offset exp.erange) exp.erange in
            let t' = field.c_field_type in
            eval_base_offset
              (fun acc v flow ->
                 let v', flow' = expand_relation subman ctx v range flow in
                 let exp' = {exp with ekind = E_var v'} in
                 oeval_singleton (Some (exp', []), flow', [mk_remove_var v' range]) |>
                 oeval_join acc
              )
              (fun acc eflow -> oeval_singleton (None, eflow, []) |> oeval_join acc)
              (fun () -> oeval_singleton (None, flow, []))
              None base offset' t' exp.erange man subman ctx flow

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

    | E_c_member_access(r, i, f) ->
      let p = mk_c_address_of r exp.erange in
      man.eval ctx (mk_c_resolve_pointer p exp.erange) flow |>
      eval_compose (fun pe flow ->
          match ekind pe with
          | E_c_points_to(E_p_fun fundec) ->
            Debug.fail "arrow access to function pointers"

          | E_c_points_to(E_p_var (base, offset, t)) ->
            let record = match remove_typedef r.etyp with T_c_record r -> r | _ -> assert false in
            let field = List.nth record.c_record_fields i in
            let offset' = mk_binop offset math_plus (mk_int field.c_field_offset exp.erange) exp.erange in
            let t' = field.c_field_type in
            eval_base_offset
              (fun acc v flow ->
                 let v', flow' = expand_relation subman ctx v range flow in
                 let exp' = {exp with ekind = E_var v'} in
                 oeval_singleton (Some (exp', []), flow', [mk_remove_var v' range]) |>
                 oeval_join acc
              )
              (fun acc eflow -> oeval_singleton (None, eflow, []) |> oeval_join acc)
              (fun () -> oeval_singleton (None, flow, []))
              None base offset' t' exp.erange man subman ctx flow

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

    | _ -> None


  (*==========================================================================*)
  (**                    {2 Cells Initialization}                             *)
  (*==========================================================================*)

  and init_manager man subman ctx =
    Init.{
      (* Initialization of scalars *)
      scalar = (fun v e range flow ->
          match ekind v with
          | E_var v ->
            let v = annotate_var v in
            let u = get_domain_cur man flow in
            let s = get_domain_cur subman flow in
            let (u', s') = add_var ctx range v (u, s) in
            let flow' = set_domain_cur u' man flow |>
                        set_domain_cur s' subman
            in
            let stmt = mk_assign (mk_var v range) e range in
            sub_exec subman ctx stmt flow'

          | _ -> assert false
        );

      (* Initialization of arrays *)
      array =  (fun a is_global init_list range flow ->
          let v = match ekind a with E_var v -> v | _ -> assert false in
          let c = annotate_var v |> cell_of_var in
          let c' = {b = c.b; t = under_array_type a.etyp} in
          let v' = var_of_new_cell c' in
          let a' = mk_var v' range in
          let rec aux acc l =
            match l with
            | [] -> acc
            | init :: tl ->
              let flow = init_expr (init_manager man subman ctx) a' is_global init range flow in
              aux (man.flow.join acc flow) tl
          in
          aux man.flow.bottom init_list
        );

      (* Initialization of structs *)
      record =  (fun s is_global init_list range flow ->
          let v = match ekind s with E_var v -> v | _ -> assert false in
          let c = annotate_var v |> cell_of_var in
          let record = match remove_typedef c.t with T_c_record r -> r | _ -> assert false in
          match init_list with
          | Parts parts ->
            debug "init struct by parts";
            let rec aux i l acc =
              match l with
              | [] -> acc
              | init :: tl ->
                let field = List.nth record.c_record_fields i in
                let t' = field.c_field_type in
                let cf = {b = c.b; t = t'} in
                debug "field cell %a" pp_cell cf;
                let ef = var_of_new_cell cf in
                let flow' = init_expr (init_manager man subman ctx) (mk_var ef range) is_global init range flow in
                debug "flow' = @[%a@]" man.flow.print flow';
                let flow'' = man.flow.join acc flow' in
                aux (i + 1) tl flow''
            in
            aux 0 parts man.flow.bottom

          | Expr e ->
            debug "init struct with expression";
            record.c_record_fields |> List.fold_left (fun acc field ->
                let t' = field.c_field_type in
                let cf = {b = c.b; t = t'} in
                debug "field cell %a" pp_cell cf;
                let ef = var_of_new_cell cf in
                let init = C_init_expr (mk_c_member_access e field range) in
                let flow' = init_expr (init_manager man subman ctx) (mk_var ef range) is_global (Some init) range flow in
                debug "flow' = @[%a@]" man.flow.print flow';
                let flow'' = man.flow.join acc flow' in
                debug "flow'' = @[%a@]" man.flow.print flow'';
                flow''
              ) man.flow.bottom
        );
    }

  let init man subman ctx prog flow =
    let flow = set_domain_cur empty man flow in
    match prog.prog_kind with
    | C_program(globals, _) ->
      let flow' = Init.fold_globals ctx (init_manager man subman ctx) globals flow in
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
