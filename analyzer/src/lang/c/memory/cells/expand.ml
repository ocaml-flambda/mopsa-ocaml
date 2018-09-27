(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Expansion-based abstraction of C memory cells. *)

open Framework.Essentials
open Universal.Ast
open Ast
open Base
open Pointer
open Cell


let opt_expand = ref 1

let () =
  Framework.Options.register_option (
    "-cell-expand",
    Arg.Set_int opt_expand,
    " maximal number of expanded cells (default: 1)"
  )


module Domain = struct


  (** Definition of offset cells *)
  (** ========================== *)

  type ocell = {
    b: base;
    o: Z.t;
    t: typ;
  }

  let compare_ocell c c' = Compare.compose [
      (fun () -> compare_base c.b c'.b);
      (fun () -> Z.compare c.o c'.o);
      (fun () -> compare_typ c.t c'.t);
    ]

  let pp_ocell fmt c =
    Format.fprintf fmt "⟨%a,%a,%a⟩"
      pp_base c.b
      Z.pp_print c.o
      pp_typ c.t

  type cell +=
    | OffsetCell of ocell

  let () =
    register_cell
      {
        extract = (fun f c ->
            match c with
            | OffsetCell o -> (o.b, mk_z o.o, o.t)
            | _ -> f c
          );
        to_var = (fun next c ->
            match c with
            | OffsetCell o ->
              let vname =
                let () = Format.fprintf Format.str_formatter "{%a:%a:%a}" pp_base o.b Z.pp_print o.o Pp.pp_c_type_short o.t in
                Format.flush_str_formatter ()
              in
              {
                vname;
                vuid = base_uid o.b;
                vtyp = o.t;
              }
            | _ -> next c
          );
        compare = (fun f a b ->
            match a, b with
            | OffsetCell o, OffsetCell o' -> compare_ocell o o'
            | _ -> f a b
          );
        print = (fun f fmt x ->
            match x with
            | OffsetCell o -> Format.fprintf fmt "%a" pp_ocell o
            | _ -> f fmt x
          )
      }

  let mk_ocell (c: ocell) ?(mode = STRONG) range =
    mk_cell (OffsetCell c) ~mode:mode range

  let mk_remove_cell (c: ocell) range =
    mk_stmt
      (S_c_remove_cell (OffsetCell c))
      range

  let ch_addr_of_ocell o addr =
    {o with b = Base.A addr}



  (** Abstract element *)
  (** ================ *)

  (* We keep track of the set of previously realized cells. We use the
     Powerset generic lattice. *)

  include Framework.Lattices.Powerset.Make(struct
      type t = ocell
      let compare = compare_ocell
      let print = pp_ocell
    end)

  let is_bottom _ = false

  let print fmt c =
    Format.fprintf fmt "expand cells: @[%a@]@\n"
      print c


  (** Domain identification *)
  (** ===================== *)

  let name = "c.memory.cells.expand"

  type _ domain += D_c_cell_expand : t domain
  let id = D_c_cell_expand

  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_c_cell_expand -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Utility functions *)
  (** ================= *)

  let exist_and_find_cell f (cs:t) =
    apply (fun r ->
        let exception Found of ocell in
        try
          let () = Set.iter (fun c ->
              if f c then raise (Found(c))
            ) r in
          None
        with
        | Found (c) -> Some (c)
      )
      None cs

  let exist_and_find_cells f (cs:t) =
    apply (fun r ->
        Set.filter f r |>
        Set.elements
      )
      []
      cs


  (** Cell unification *)
  (** ================ *)

  type phi_exp =
    | Nexp of expr option
    | PInvalid

  (** [phi c u] collects constraints over cell [c] found in [u] *)
  let phi (c: ocell) (u : t) range : phi_exp =
    let open Universal.Ast in
    let cs = u in
    match exist_and_find_cell (fun c' -> compare_ocell c c' = 0) cs  with
    | Some c -> Nexp (Some (mk_ocell c range))
    | None ->
      begin
        match exist_and_find_cell
                (fun c' ->
                   is_c_int_type @@ c'.t && Z.equal (sizeof_type c'.t) (sizeof_type c.t) &&
                   compare_base c.b c'.b = 0 &&
                   Z.equal c.o c'.o) cs
        with
        | Some (c') -> Nexp (Some (wrap_expr (mk_ocell c' range) (int_rangeof c.t) range))
        | None ->
          begin
            match exist_and_find_cell ( fun c' ->
                let b = Z.sub c.o c'.o in
                Z.lt b (sizeof_type c'.t) && is_c_int_type c'.t && compare (remove_typedef c.t) (T_c_integer(C_unsigned_char)) = 0
              ) cs with
            | Some (c') ->
              let b = Z.sub c.o c'.o in
              let base = (Z.pow (Z.of_int 2) (8 * Z.to_int b))  in
              Nexp (Some (mk_binop (mk_binop (mk_ocell c' range) O_div (mk_z base range) range) O_mod (mk_int 256 range) range))
            | None ->
              begin
                let exception NotPossible in
                try
                  if is_c_int_type c.t then
                    begin
                      let t' = T_c_integer(C_unsigned_char) in
                      let n = Z.to_int (sizeof_type (c.t)) in
                      let rec aux i l =
                        if i < n then
                          let tobein = {b = c.b ; o = Z.add c.o (Z.of_int i); t = t'} in
                          match exist_and_find_cell (fun c' ->
                              compare_ocell c' tobein = 0
                            ) cs with
                          | Some (c') ->
                            aux (i+1) (c' :: l)
                          | None ->
                            raise NotPossible
                        else
                          List.rev l
                      in
                      let ll = aux 0 [] in
                      let _,e = List.fold_left (fun (time, res) x ->
                          let res' = mk_binop (mk_binop (mk_z time range) O_mult (mk_ocell x range) range) O_plus res range in
                          let time' = Z.mul time (Z.of_int 256) in
                          time',res'
                        ) (Z.of_int 1,(mk_int 0 range)) ll
                      in
                      Nexp (Some e)
                    end
                  else
                    raise NotPossible
                with
                | NotPossible ->
                  begin
                    if is_c_scalar_type c.t then
                      let a,b = rangeof c.t in
                      Nexp (Some ( mk_z_interval a b range))
                    else if is_c_pointer_type c.t then
                      PInvalid
                    else
                      Nexp None
                  end
              end
          end
      end

  (** [add_cons_cell_subman subman range c u s] adds a cell [c] to the
     abstraction [u] given a manager [subman] on the sub element of
     the stack [s] *)
  let add_cons_cell_subman (subman: ('b, 'b) man) range (c: ocell) u (s: 'b flow) =
    if mem c u then u, s
    else if not (is_c_scalar_type c.t) then u, s
    else if is_c_pointer_type (c.t) then
      add c u, s
    else
      match phi c u range with
      | Nexp (Some e) ->
        let stmt = Universal.Ast.(mk_assume (mk_binop (mk_ocell c range) O_eq e ~etyp:T_int range) range) in
        add c u, subman.exec stmt s
      | Nexp None ->
        add c u, s
      | PInvalid -> assert false

  (** [add_cons_cell v u] adds a variable [v] to the abstraction [u] *)
  let add_cons_cell man range (c: ocell) f =
    let u = Flow.get_domain_cur man f in
    if mem c u then f
    else if not (is_c_scalar_type c.t) then f
    else if is_c_pointer_type (c.t) then
      Flow.set_domain_cur (add c u) man f
    else
      match phi c u range with
      | Nexp (Some e) ->
        let stmt = Universal.Ast.(mk_assume (mk_binop (mk_ocell c range) O_eq e ~etyp:T_int range) range) in
        f |>
        Flow.set_domain_cur (add c u) man |>
        man.exec ~zone:(Z_c_cell) stmt
      | Nexp None ->
        Flow.set_domain_cur (add c u) man f
      | PInvalid -> assert false


  (** [unify u u'] finds non-common cells in [u] and [u'] and adds them. *)
  let unify (subman: ('b, 'b) man) ((u : t), (s: 'b flow)) ((u' : t), (s': 'b flow)) =
    let range = mk_fresh_range () in
    if is_empty u || is_empty u'
    then (u, s), (u', s')
    else
      let diff' = diff u u' in
      let diff = diff u' u in
      try
        fold (fun c (u, s) ->
            add_cons_cell_subman subman range c u  s
          ) diff (u, s)
        ,
        fold (fun c (u', s') ->
            add_cons_cell_subman subman range c u' s'
          ) diff' (u', s')
      with Top.Found_TOP ->
        (top, Flow.top (Flow.get_all_annot s)), (top, Flow.top (Flow.get_all_annot s'))

  let remove_overlapping_cells c range man flow =
    let u = Flow.get_domain_cur man flow in
    let u' = add c u in
    let flow' = Flow.set_domain_cur u' man flow in
    let flow'', to_remove = fold (fun c' (flow, to_remove) ->
        if compare_ocell c c' = 0 then
          (flow, to_remove)
        else
          let cell_range c = (c.o, Z.add c.o (sizeof_type c.t)) in
          let check_overlap (a1, b1) (a2, b2) =
            Z.lt (Z.max a1 a2) (Z.min b1 b2)
          in
          if compare_base c.b c'.b = 0 && check_overlap (cell_range c) (cell_range c') then
            let stmt = mk_remove_cell c' range in
            let flow' = Flow.map_domain_cur (remove c') man flow in
            (flow', stmt :: to_remove)
          else
            (flow, to_remove)
      ) u (flow', []) in
    (flow'', to_remove)

  let join annot (subman: ('b, 'b) man) (u , (s: 'b flow) ) (u', (s': 'b flow)) =
    let (u, s), (_, s') = unify subman (u, s) (u', s') in
    (u, s, s')

  let meet = join
  let widen = join

  let subset (subman: ('b, 'b) man) (u , (s: 'b flow) ) (u', (s': 'b flow)) =
    let (_, s), (_, s') = unify subman (u, s) (u', s') in
    (true, s, s')


  let exec_interface = {
    export = [Zone.Z_c];
    import = [Z_c_cell];
  }

  let eval_interface = {
    export = [Zone.Z_c_scalar, Z_c_cell];
    import = [
      (Zone.Z_c, Zone.Z_c_scalar);
      (Zone.Z_c, Z_c_cell);
      (Zone.Z_c, Z_c_points_to_cell)
    ];
  }

  (*==========================================================================*)
  (**                       {2 Cells expansion}                               *)
  (*==========================================================================*)

  let fold_cells f err empty x0 base offset typ range man flow =
    if Flow.is_cur_bottom man flow
    then empty ()
    else
      let cell_size = sizeof_type typ in
      let static_base_case base_size =
        debug "static base case";
        match Universal.Utils.expr_to_z offset with
        | Some z when Z.geq z Z.zero && Z.leq (Z.add z cell_size) base_size  ->
          let c = {b = base; o = z; t = typ} in
          let flow = add_cons_cell man range c flow in
          f x0 c flow
        | Some z ->
          debug "error, z = %a, cell_size = %a, base_size = %a" Z.pp_print z Z.pp_print cell_size Z.pp_print base_size;
          let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack flow in
          let alarm = mk_alarm Alarms.AOutOfBound range ~cs in
          Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man flow |>
          Flow.set T_cur man.bottom man |>
          err x0

        | None ->
          debug "non-constant cell offset";
          (* Create variables with offsets {min(l + k * step, u) | k >= 0} *)
          let fold_interval l u step acc flow =
            debug "fold interval  [%a, %a]:%a (%a)" Z.pp_print l Z.pp_print u Z.pp_print step Z.pp_print Z.((u - l + one) / step);
            if Z.(leq ((u - l + one) / step) (of_int !opt_expand)) then
              let rec iter x o =
                if Z.gt o u then x
                else
                  let c = {b = base; o; t = typ} in
                  let flow = man.exec ~zone:(Z_c_cell)
                      (mk_assume
                         (mk_binop offset O_eq
                            (mk_z o range)
                            range)
                         range) flow in
                  let flow = add_cons_cell man range c flow in
                  iter (f x c flow) (Z.add o step)
              in
              debug "iterating";
              iter acc l
            else
              Debug.fail "c.memory.expand fold_cells: to many cells to \
                          create"
              (* let v = var_of_xcell {b = base; t = typ} in
               * debug "using an xcell %a" pp_var v;
               * let s = Flow.get_domain_cur subman flow in
               * let (cs', s') = add_cell ctx range v (cs, s) in
               * let flow' = set_domain_cur cs' man flow |>
               *             set_domain_cur s' subman
               * in
               * f acc v flow' *)
          in

          (* Fast bound check with intervals *)
          (* FIXME: QIntStepInterval *)
          let open Universal.Numeric.Values.Intervals in
          let rec fast () =
            debug "trying fast check";
            let v = man.ask (Value.Q_interval offset) flow in
            let itv = v in
            let step = Z.one in
            if Value.is_bounded itv then
              begin
                debug "offset interval = %a" Value.print itv;
                let l, u = Universal.Numeric.Values.Intervals.Value.bounds itv in
                if Z.geq l Z.zero && Z.leq (Z.add u cell_size) base_size then
                  fold_interval l u step x0 flow
                else if Z.lt u Z.zero || Z.gt (Z.add l cell_size) base_size then
                  let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack flow in
                  let alarm = mk_alarm Alarms.AOutOfBound range ~cs in
                  Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man flow |>
                  Flow.set T_cur man.bottom man |>
                  err x0
                else
                  full ()
              end
            else
              full ()

          (* Full bound check *)
          (* FIXME: QIntStepInterval*)
          and full () =
            let safety_cond =
              mk_binop
                (mk_binop offset O_ge (mk_zero range) range)
                O_log_and
                (mk_binop (mk_binop offset O_plus (mk_z (sizeof_type typ) range) range) O_le (mk_z base_size range) range)
                range
            in
            let safe_case acc flow =
              let v = man.ask (Value.Q_interval offset) flow in
              let itv = v and step = Z.one in
              if Value.is_bounded itv then
                let l, u = Value.bounds itv in
                debug "interval = [%a, %a] mod %a" Z.pp_print l Z.pp_print u Z.pp_print step;
                fold_interval l u step acc flow
              else
                assert false
            in
            let error_case acc flow =
              let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack flow in
              let alarm = mk_alarm Alarms.AOutOfBound range ~cs in
              Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man flow |>
              Flow.set T_cur man.bottom man |>
              err acc
            in
	    Eval.assume
              safety_cond man
              ~fthen:(safe_case x0)
              ~felse:(error_case x0)
              ~fboth:(fun sflow eflow -> error_case (safe_case x0 sflow) eflow)
              ~fnone:(fun _ -> empty ())
              flow
          in
          (* Start with fast check *)
          fast ()
      in

      match base with
      | V v -> static_base_case (sizeof_type v.vtyp)
      | A {addr_kind = Libs.C_stdlib.A_c_static_malloc s} -> static_base_case s
      | _ -> Framework.Exceptions.panic "base %a not supported" pp_base base



  (*==========================================================================*)
  (**                      {2 Transfer functions}                             *)
  (*==========================================================================*)

  let assign_cell man c rval mode range flow =
    let lval = mk_ocell c ~mode:mode range in
    let stmt = mk_assign lval rval range in
    let () = debug "giving back stmt %a" pp_stmt stmt in
    let flow', to_remove = remove_overlapping_cells c stmt.srange man flow in
    let block =
      if is_c_int_type c.t then
        let rmin, rmax = rangeof c.t in
        let cond = range_cond lval rmin rmax (erange lval) in
        let stmt' = (mk_assume cond (tag_range range "assume range")) in
        [stmt; stmt']
      else
        [stmt]
    in
    let flow'' = man.exec ~zone:Z_c_cell (mk_block (to_remove @ block) range) flow' in
    (Post.add_mergers to_remove (Post.of_flow flow''))

  let rec exec zone stmt man flow =
    let range = stmt.srange in
    match skind stmt with
    | S_c_global_declaration(v, init) ->
      Init_visitor.init_global (init_visitor man) v init range flow |>
      Post.of_flow |>
      Option.return

    | S_c_local_declaration(v, init) ->
      Init_visitor.init_local (init_visitor man) v init range flow
      |> Post.of_flow
      |> Option.return

    | S_rename_var(v, v') ->
      assert false

    | S_rebase_addr(adr, adr', mode) ->
      begin
        let u = Flow.get_domain_cur man flow in
        let l = exist_and_find_cells (fun c -> compare_base c.b (Base.A adr) = 0) u in
        let u = List.fold_left (fun acc c -> remove c acc) u l in
        let assigns = List.map
            (fun c -> let c' = ch_addr_of_ocell c adr' in
              mk_assign
                (mk_ocell c' ~mode:mode (tag_range range "lval"))
                (mk_ocell c  (tag_range range "rval"))
                (tag_range range "assign")
            ) l
        in
        let mergers = List.map (fun c -> mk_remove_cell c (srange stmt)) l in
        Flow.set_domain_cur u man flow
        |> man.exec ~zone:Z_c_cell (mk_block assigns (tag_range range "block"))
        |> man.exec ~zone:Z_c_cell (mk_block mergers (tag_range range "mergers"))
        |> Post.of_flow
        |> Post.add_mergers mergers
        |> Option.return
      end

    | S_remove_var (v) when is_c_type v.vtyp ->
      let u = Flow.get_domain_cur man flow in
      let l = exist_and_find_cells (fun c -> compare_base c.b (Base.V v) = 0) u in
      let u' = List.fold_left (fun acc c -> remove c acc) u l in
      let mergers = List.map (fun c -> mk_remove_cell c stmt.srange) l in
      let to_exec_in_sub = mergers in
      let flow = Flow.set_domain_cur u' man flow in
      man.exec ~zone:Z_c_cell (mk_block to_exec_in_sub range) flow |>
      Post.of_flow |>
      Post.add_mergers mergers |>
      Option.return

    | S_assign(lval, rval) when is_c_scalar_type lval.etyp ->
      man.eval ~zone:(Zone.Z_c, Z_c_cell) rval flow |>
      Post.bind_opt man @@ fun rval flow ->

      man.eval ~zone:(Zone.Z_c, Zone.Z_c_scalar) lval flow |>
      Post.bind_opt man @@ fun lval flow ->

      eval (Zone.Z_c_scalar, Z_c_cell) lval man flow |>
      Option.lift @@ Post.bind man @@ fun lval flow ->

      begin
        match ekind lval with
        | E_c_cell(OffsetCell c, mode) ->
          assign_cell man c rval mode stmt.srange flow
        | _ -> assert false
      end

    | S_assume(e) ->
      man.eval ~zone:(Zone.Z_c, Z_c_cell) e flow |>
      Post.bind_opt man @@ fun e' flow ->

      let stmt' = {stmt with skind = S_assume e'} in
      man.exec ~zone:Z_c_cell stmt' flow |>

      Post.of_flow |>
      Option.return

    | _ -> None

  and eval zone exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_var (v, mode) when is_c_type v.vtyp ->
      begin
        debug "evaluating a scalar variable %a" pp_var v;
        let c = {b = V v; o = Z.zero; t = v.vtyp}  in
        let flow = add_cons_cell man range c flow in
        debug "new variable %a in %a" pp_var v (Flow.print man) flow;
        Eval.singleton {exp with ekind = E_c_cell (OffsetCell c, mode)} flow
      end |> Option.return

    | E_c_deref(p) ->
      begin
        man.eval ~zone:(Zone.Z_c, Z_c_points_to_cell) p flow |> Eval.bind @@ fun pe flow ->
        match ekind pe with
        | E_c_points_to(P_fun fundec) ->
          Eval.singleton {exp with ekind = E_c_function fundec} flow

        | E_c_points_to(P_var (base, offset, t)) ->
          debug "E_p_var(%a, %a, %a)" pp_base base pp_expr offset pp_typ t;
          fold_cells
            (fun acc c flow ->
               (** FIXME: filter flow with (p == &v) *)
               Eval.singleton (mk_ocell c range) flow |>
               Eval.join acc
            )
            (fun acc eflow -> Eval.empty_singleton eflow |> Eval.join acc)
            (fun () -> Eval.empty_singleton flow)
            Eval.empty base offset t exp.erange man flow

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
      end |> Option.return

    | _ -> None


  (*==========================================================================*)
  (**                    {2 Cells Initialization}                             *)
  (*==========================================================================*)

  and init_visitor man =
    Init_visitor.{
      (* Initialization of scalars *)
      scalar = (fun v e range flow ->
          match ekind v with
          | E_var(v, mode) ->
            man.eval ~zone:(Zone.Z_c, Z_c_cell) e flow |>
            Post.bind_flow man @@ fun e flow ->

            let c = {b = V v; o = Z.zero; t = v.vtyp} in
            let flow = add_cons_cell man range c flow in
            let stmt = mk_assign (mk_ocell c ~mode:mode range) e range in
            man.exec ~zone:Z_c_cell stmt flow
          | _ -> assert false
        );

      (* Initialization of arrays *)
      array =  (fun a is_global init_list range flow ->
          let c =
            match ekind a with
            | E_var(v, mode) -> {b = V v; o = Z.zero; t = v.vtyp}
            | E_c_cell (OffsetCell c, mode) -> c
            | _ -> assert false
          in
          let rec aux i l flow =
            if i = !opt_expand then flow
            else
              match l with
              | [] -> flow
              | init :: tl ->
                let t' = under_array_type c.t in
                let ci = {b = c.b; o = Z.(c.o + (Z.of_int i) * (sizeof_type t')); t = t'} in
                let flow' = init_expr (init_visitor man) (mk_ocell ci range) is_global init range flow in
                aux (i + 1) tl flow'
          in
          aux 0 init_list flow
        );

      (* Initialization of structs *)
      record =  (fun s is_global init_list range flow ->
          let c =
            match ekind s with
            | E_var (v, _) -> {b = V v; o = Z.zero; t = v.vtyp}
            | E_c_cell(OffsetCell c, _) -> c
            | _ -> assert false
          in
          let record = match remove_typedef c.t with T_c_record r -> r | _ -> assert false in
          match init_list with
          | Parts l ->
            let rec aux i l flow =
              match l with
              | [] -> flow
              | init :: tl ->
                let field = List.nth record.c_record_fields i in
                let t' = field.c_field_type in
                let cf = {b = c.b; o = Z.(c.o + (Z.of_int field.c_field_offset)); t = t'} in
                let flow' = init_expr (init_visitor man) (mk_ocell cf ~mode:STRONG range) is_global init range flow in
                aux (i + 1) tl flow'
            in
            aux 0 l flow

          | Expr e ->
            record.c_record_fields |> List.fold_left (fun acc field ->
                let t' = field.c_field_type in
                let cf = {b = c.b; o = Z.(c.o + (Z.of_int field.c_field_offset)); t = t'} in
                let init = C_init_expr (mk_c_member_access e field range) in
                init_expr (init_visitor man) (mk_ocell cf ~mode:STRONG range) is_global (Some init) range acc
              ) flow
        );
    }

  and init prog man flow =
    Some (
      Flow.set_domain_cur empty man flow
    )

  let ask _ _ _ = None

end


let () =
  Framework.Domains.Stacked.register_domain (module Domain);
