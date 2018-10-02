(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Smashing-based abstraction of C memory cells. *)

open Framework.Essentials
open Universal.Ast
open Ast
open Base
open Cell
open Zone

module Domain =
struct


  (** Definition of smashed cells *)
  (** =========================== *)

  type cell +=
    | C_smash of base * typ

  let () =
    register_cell {
      extract = (fun next c ->
          match c with
          | C_smash (base, typ) ->
            let offset =
              match base with
              | V v -> mk_z_interval Z.zero (Z.sub (sizeof_type v.vtyp) (sizeof_type typ))
              | A a -> mk_top T_int (* FIXME: get size of the allocated memory block *)
              | S s -> mk_z_interval Z.zero (Z.of_int @@ String.length s)
            in
            base, offset, typ
          | _ -> next c
        );

      to_var = (fun next c ->
          match c with
          | C_smash(b,t) ->
            let vname =
              let () = Format.fprintf Format.str_formatter "{%a:⋆:%a}" pp_base b Pp.pp_c_type_short t in
              Format.flush_str_formatter ()
            in
            {
              vname;
              vuid = base_uid b;
              vtyp = t;
            }

          | _ -> next c
        );

      print = (fun next fmt c ->
          match c with
          | C_smash(b,t) -> Format.fprintf fmt "⟪%a:⋆:%a⟫" pp_base b pp_typ t
          | _ -> next fmt c
        );

      compare = (fun next c c' ->
          match c, c' with
          | C_smash(b,t), C_smash(b', t') ->
            Compare.compose [
              (fun () -> compare_base b b');
              (fun () -> compare_typ t t');
            ]
          | _ -> next c c'
        );
    }

  let var_to_cell v = C_smash (V v, v.vtyp)


  (** Lattice definition *)
  (** ================== *)

  (* An abstract element is the set of smashed cells previously realized *)
  include Framework.Lattices.Powerset.Make(Cell)

  let is_bottom _ = false

  let add c a =
    (* Do not add non-scalar cells *)
    if cell_type c |> is_c_scalar_type then
      add c a
    else
      a

  let widening = join

  let print fmt a =
    Format.fprintf fmt "smashed cells: @[%a@]@\n"
      print a


  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_cells_smashing : t domain

  let id = D_c_cells_smashing

  let name = "c.memory.cells.smashing"

  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_c_cells_smashing -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning interface *)
  (** ================ *)

  let exec_interface = {
    export = [Z_c]; (* We handle C assignments *)
    import = [Z_c_cell]; (* To forward simplified assignments to numeric/pointer domains *)
  }

  let eval_interface = {
    export = [Z_c_scalar, Z_c_cell]; (* We evaluate scalar C expressions into cells *)
    import = [
      Z_c, Z_c_scalar; (* To simplify lvals *)
      Z_c, Z_c_cell; (* To evaluate rhs expressions in assignments *)
      Z_c, Z_c_points_to_cell; (* To dereference pointer expressions *)
    ];
  }




  (** Initialization *)
  (** ============== *)

  let rec init_visitor man =
    let open Init_visitor in
    {
      (* Initialization of scalars *)
      scalar = (fun v e range flow ->
          man.eval ~zone:(Z_c, Z_c_cell) e flow |>
          Post.bind_flow man @@ fun e flow ->

          match ekind v with
          | E_var(v, mode) ->
            let c = var_to_cell v in
            let flow1 = Flow.map_domain_env T_cur (add c) man flow in

            let stmt = mk_assign (mk_cell c ~mode:mode range) e range in
            man.exec ~zone:(Z_c_cell) stmt flow1

          | E_c_cell(c, mode) ->
            let flow1 = Flow.map_domain_env T_cur (add c) man flow in

            let stmt = mk_assign (mk_cell c ~mode:mode range) e range in
            man.exec ~zone:(Z_c_cell) stmt flow1

          | _ -> assert false
        );

      (* Initialization of arrays *)
      array =  (fun a is_global init_list range flow ->
          let b, t, mode =
            match ekind a with
            | E_var (v, mode) -> V v, v.vtyp, mode
            | E_c_cell (C_smash (b, t), mode) -> b, t, mode
            | _ -> assert false
          in
          let a' = mk_cell (C_smash (b, under_array_type a.etyp)) ~mode range in
          let rec aux acc l =
            match l with
            | [] -> acc
            | init :: tl ->
              let flow = init_expr (init_visitor man) a' is_global init range flow in
              aux (Flow.join man acc flow) tl
          in
          aux (Flow.bottom (Flow.get_all_annot flow)) init_list
        );

      (* Initialization of structs *)
      record =  (fun s is_global init_list range flow ->
          let b, t, mode =
            match ekind s with
            | E_var (v, mode) -> V v, v.vtyp, mode
            | E_c_cell (C_smash (b, t), mode) -> b, t, mode
            | _ -> assert false
          in
          let record = match remove_typedef t with T_c_record r -> r | _ -> assert false in
          match init_list with
          | Parts parts ->
            let rec aux i l acc =
              match l with
              | [] -> acc
              | init :: tl ->
                let field = List.nth record.c_record_fields i in
                let t' = field.c_field_type in
                let cf = C_smash (b, t') in
                let ef = mk_cell cf range in
                let flow' = init_expr (init_visitor man) ef is_global init range flow in
                let flow'' = Flow.join man acc flow' in
                aux (i + 1) tl flow''
            in
            aux 0 parts (Flow.bottom (Flow.get_all_annot flow))

          | Expr e ->
            record.c_record_fields |> List.fold_left (fun acc field ->
                let t' = field.c_field_type in
                let cf = C_smash (b, t') in
                let ef = mk_cell cf range in
                let init = C_init_expr (mk_c_member_access e field range) in
                let flow' = init_expr (init_visitor man) ef is_global (Some init) range flow in
                Flow.join man acc flow'
              ) (Flow.bottom (Flow.get_all_annot flow))
        );

    }

  let init prog man flow =
    Some (
      Flow.set_domain_env T_cur empty man flow
    )


  (** Computation of post-conditions *)
  (** ============================== *)

  let rec exec zone stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_c_global_declaration(v, init) ->
      Init_visitor.init_global (init_visitor man) v init range flow |>
      Post.of_flow |>
      Option.return

    | S_c_local_declaration(v, init) ->
      Init_visitor.init_local (init_visitor man) v init range flow |>
      Post.of_flow |>
      Option.return

    | S_remove_var v ->
      let cl = cells_of_base (V v) man flow in
      let flow1 = Flow.map_domain_env T_cur (fun a ->
          List.fold_left (fun acc c -> remove c acc) a cl
        ) man flow
      in
      let block = List.map (fun c -> mk_remove_cell c stmt.srange) cl in
      man.exec ~zone:Z_c_cell (mk_block block stmt.srange) flow1 |>
      Post.of_flow |>
      Option.return

    | S_assign(lval, rval) when is_c_scalar_type lval.etyp ->
      man.eval ~zone:(Z_c, Z_c_cell) rval flow |>
      Post.bind_opt man @@ fun rval flow ->

      man.eval ~zone:(Zone.Z_c, Zone.Z_c_scalar) lval flow |>
      Post.bind_opt man @@ fun lval flow ->

      eval (Z_c, Z_c_cell) lval man flow |>
      Option.lift @@ Post.bind man @@ fun lval flow ->

      let c, mode = cell_of_expr lval in
      assign_cell c rval mode range man flow |>

      remove_overlappings c range man |>
      Post.add_merger (mk_remove_cell c range)


    | S_assume(e) ->
      man.eval ~zone:(Z_c, Z_c_cell) e flow |>
      Post.bind_opt man @@ fun e' flow ->

      let stmt' = {stmt with skind = S_assume e'} in
      man.exec ~zone:Z_c_cell stmt' flow |>

      Post.of_flow |>
      Option.return


    | _ -> None

  and assign_cell c e mode range man flow =
    (* Infer the mode of assignment *)
    let mode = match cell_base c with
      | A a -> WEAK (* TODO: process the case of heap addresses *)
      | V v ->
        (* In case of a base variable, we check that we are writing to the whole memory block *)
        if Z.equal (sizeof_type v.vtyp) (sizeof_type (cell_type c)) then mode
        else WEAK
      | S s -> mode
    in
    let lval = mk_cell c ~mode:mode range in
    let stmt = mk_assign lval e range in
    man.exec ~zone:(Z_c_cell) stmt flow


  and remove_overlappings c range man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let flow'', mergers = fold (fun c' (flow, mergers) ->
        if compare_base (cell_base c) (cell_base c') = 0 && compare_typ (cell_type c) (cell_type c') <> 0 then
          let flow' = Flow.map_domain_env T_cur (remove c') man flow |>
                      man.exec ~zone:Z_c_cell (mk_remove_cell c' range)
          in
          flow', (mk_remove_cell c' range) :: mergers
        else
          flow, mergers
      ) a (flow, [])
    in
    Post.of_flow flow'' |>
    Post.add_mergers mergers ~zone:Z_c_cell

  and cells_of_base b man flow =
    let a = Flow.get_domain_env T_cur man flow in
    fold (fun c acc ->
        let b' = cell_base c in
        if compare_base b b' = 0 then c :: acc
        else acc
      ) a []


  (** Evaluation of expressions *)
  (** ========================= *)

  and eval zone exp man flow =
    match ekind exp with
    | E_var(v, mode) when is_c_type v.vtyp ->
      let c = var_to_cell v in
      let flow1 = Flow.map_domain_env T_cur (add c) man flow in
      Eval.singleton (mk_cell c ~mode:mode (erange exp)) flow1 |>
      Option.return

    | E_c_deref p ->
      begin
        man.eval ~zone:(Z_c, Z_c_points_to_cell) p flow |> Eval.bind @@ fun pt flow ->
        match ekind pt with
        | E_c_points_to(P_fun fundec) ->
          Eval.singleton ({exp with ekind = E_c_function fundec}) flow

        | E_c_points_to(P_var (base, offset, t)) ->
          eval_base_offset
            ~safe:(fun c flow ->
                let flow1 = Flow.map_domain_env T_cur (add c) man flow in
                let exp' = mk_cell c exp.erange in
                Eval.singleton exp' flow1
            )
            ~outbound:(fun flow ->
                let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack flow in
                let alarm = mk_alarm Alarms.AOutOfBound exp.erange ~cs in
                let flow1 = Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man flow |>
                            Flow.set T_cur man.bottom man
                in
                Eval.empty_singleton flow1
              )
            base offset t exp.erange man flow

        | E_c_points_to(P_null) ->
          let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack flow in
          let alarm = mk_alarm Alarms.ANullDeref exp.erange ~cs in
          let flow1 = Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man flow |>
                      Flow.set T_cur man.bottom man
          in
          Eval.empty_singleton flow1

        | E_c_points_to(P_invalid) ->
          let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack flow in
          let alarm = mk_alarm Alarms.AInvalidDeref exp.erange ~cs in
          let flow1 = Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man flow |>
                      Flow.set T_cur man.bottom man
          in
          Eval.empty_singleton flow1

        | _ -> assert false

      end (* case of E_c_deref *) |>
      Option.return

    | _ -> None


  and eval_base_offset ~safe ~outbound base offset typ range man flow =
    let c = C_smash (base, typ) in
    let cell_size = sizeof_type typ in

    let rec doit () =
      match base with
      | V v -> static_offset_case (sizeof_type v.vtyp)
      (* | A {addr_kind = Libs.Stdlib.A_c_static_malloc s} -> static_offset_case s *)
      | _ -> Framework.Exceptions.panic "base %a not supported" pp_base base

    and static_offset_case base_size =
      debug "static base case";
      match Universal.Utils.expr_to_z offset with
      | Some z when Z.geq z Z.zero && Z.leq (Z.add z cell_size) base_size  ->
        safe c flow

      | Some z ->
        debug "error, z = %a, cell_size = %a, base_size = %a" Z.pp_print z Z.pp_print cell_size Z.pp_print base_size;
        outbound flow

      | None ->
        dynamic_offset_case base_size

    and dynamic_offset_case base_size =
      debug "non-constant cell offset";

      let rec doit2 () = fast_check ()

      (* Fast bound check with intervals *)
      and fast_check () =
        debug "trying fast check";
        let open Universal.Numeric.Values.Intervals.Value in
        let itv = man.ask (Q_interval offset) flow in
        if is_bounded itv then
          let l, u = bounds itv in
          if Z.geq l Z.zero && Z.leq (Z.add u cell_size) base_size then
            safe c flow
          else if Z.lt u Z.zero || Z.gt (Z.add l cell_size) base_size then
            outbound flow
          else
            full_check ()
        else
          full_check ()


      (* Full bound check *)
      and full_check () =
        let safety_cond =
          mk_binop
            (mk_binop offset O_ge (mk_zero range) range)
            O_log_and
            (mk_binop (mk_binop offset O_plus (mk_z (sizeof_type typ) range) range) O_le (mk_z base_size range) range)
            range
        in
        Eval.assume
          safety_cond
          ~fthen:(fun flow -> safe c flow)
          ~felse:(fun flow -> outbound flow)
          man flow
      in
      doit2 ()
    in
    doit ()



  (** Evaluation of expressions *)
  (** ========================= *)

  let ask query man flow = None


end

let () =
  Framework.Domain.register_domain (module Domain)
