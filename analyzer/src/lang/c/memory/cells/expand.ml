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
open Zone
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

  let pp_ocell fmt c =
    Format.fprintf fmt "⟨%a,%a,%a⟩"
      pp_base c.b
      Z.pp_print c.o
      pp_typ c.t

  let compare_ocell c c' =
    Debug.debug "compare %a and %a" pp_ocell c pp_ocell c';
    Compare.compose [
      (fun () -> Debug.debug "base"; compare_base c.b c'.b);
      (fun () -> Debug.debug "offset"; Z.compare c.o c'.o);
      (fun () -> Debug.debug "typ"; compare_typ c.t c'.t);
    ]

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

  let get_overlappings c cs =
    exist_and_find_cells (fun c' ->
        compare_ocell c c' <> 0 &&
        (
          let cell_range c = (c.o, Z.add c.o (sizeof_type c.t)) in
          let check_overlap (a1, b1) (a2, b2) = Z.lt (Z.max a1 a2) (Z.min b1 b2) in
          compare_base c.b c'.b = 0 &&
          check_overlap (cell_range c) (cell_range c')
        )
      ) cs


  (** Cell unification *)
  (** ================ *)

  (** [phi c u] collects constraints over cell [c] found in [u] *)
  let phi (c: ocell) (u : t) range : expr option =
    let open Universal.Ast in
    let cs = u in
    match exist_and_find_cell (fun c' -> compare_ocell c c' = 0) cs  with
    | Some c -> Some (mk_ocell c range)
    | None ->
      begin
        match exist_and_find_cell
                (fun c' ->
                   is_c_int_type @@ c'.t && Z.equal (sizeof_type c'.t) (sizeof_type c.t) &&
                   compare_base c.b c'.b = 0 &&
                   Z.equal c.o c'.o) cs
        with
        | Some (c') -> Some (wrap_expr (mk_ocell c' range) (int_rangeof c.t) range)
        | None ->
          begin
            match exist_and_find_cell ( fun c' ->
                let b = Z.sub c.o c'.o in
                Z.lt b (sizeof_type c'.t) && is_c_int_type c'.t && compare (remove_typedef c.t) (T_c_integer(C_unsigned_char)) = 0
              ) cs with
            | Some (c') ->
              let b = Z.sub c.o c'.o in
              let base = (Z.pow (Z.of_int 2) (8 * Z.to_int b))  in
              Some (mk_binop (mk_binop (mk_ocell c' range) O_div (mk_z base range) range) O_mod (mk_int 256 range) range)
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
                      Some e
                    end
                  else
                    raise NotPossible
                with
                | NotPossible ->
                  match c.b with
                  | S s ->
                    Some (mk_int (String.get s (Z.to_int c.o) |> int_of_char) range)
                  | _ ->
                    begin
                      if is_c_scalar_type c.t then
                        let a,b = rangeof c.t in
                        Some ( mk_z_interval a b range)
                      else if is_c_pointer_type c.t then
                        assert false
                      else
                        None
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
      | Some e ->
        let stmt = Universal.Ast.(mk_assume (mk_binop (mk_ocell c range) O_eq e ~etyp:T_int range) range) in
        add c u, subman.exec stmt s
      | None ->
        add c u, s

  (** [add_cons_cell v u] adds a variable [v] to the abstraction [u] *)
  let add_cons_cell man range (c: ocell) f =
    let u = Flow.get_domain_cur man f in
    if mem c u then f
    else if not (is_c_scalar_type c.t) then f
    else if is_c_pointer_type (c.t) then
      Flow.set_domain_cur (add c u) man f
    else
      match phi c u range with
      | Some e ->
        let stmt = Universal.Ast.(mk_assume (mk_binop (mk_ocell c range) O_eq e ~etyp:T_int range) range) in
        f |>
        Flow.set_domain_cur (add c u) man |>
        man.exec ~zone:(Z_c_cell) stmt
      | None ->
        Flow.set_domain_cur (add c u) man f


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


  let subset (subman: ('b, 'b) man) (u , (s: 'b flow) ) (u', (s': 'b flow)) =
    let (_, s), (_, s') = unify subman (u, s) (u', s') in
    (true, s, s')

  let join annot (subman: ('b, 'b) man) (u , (s: 'b flow) ) (u', (s': 'b flow)) =
    let (u, s), (_, s') = unify subman (u, s) (u', s') in
    (u, s, s')

  let meet = join
  let widen = join


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


  (** Zoning interface *)
  (** ================ *)

  let exec_interface = {
    export = [Z_c];
    import = [Z_c_cell];
  }

  let eval_interface = {
    export = [Z_c_scalar, Z_c_cell];
    import = [
      (Z_c, Z_c_scalar);
      (Z_c, Universal.Zone.Z_u_num);
      (Z_c, Z_c_cell);
      (Z_c, Z_c_points_to_cell)
    ];
  }

  (** Initialization *)
  (** ============== *)

  let rec init_visitor man =
    Init_visitor.{
      (* Initialization of scalars *)
      scalar = (fun v e range flow ->
          let c =
            match ekind v with
            | E_var(v, mode) -> {b = V v; o = Z.zero; t = v.vtyp}
            | E_c_cell (OffsetCell c, mode) -> c
            | _ -> assert false
          in
          man.eval ~zone:(Z_c, Z_c_cell) e flow |>
          Post.bind_flow man @@ fun e flow ->

          let flow = add_cons_cell man range c flow in
          let stmt = mk_assign (mk_ocell c range) e range in
          man.exec ~zone:Z_c_cell stmt flow
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

  let init prog man flow =
    Some (
      Flow.set_domain_cur empty man flow
    )


  (** Evaluation of offsets *)
  (** ===================== *)

  (* Kinds of offset evaluations *)
  type offset =
    | O_concrete of Z.t
    | O_range of (Z.t -> bool)
    | O_out_of_bound

  let eval_cell_offset (offset:expr) cell_size base_size man flow : ('a, offset) evl =
    (* Try the static case *)
    match Universal.Utils.expr_to_z offset with
    | Some z ->
      if Z.geq z Z.zero &&
         Z.leq (Z.add z cell_size) base_size
      then Eval.singleton (O_concrete z) flow
      else Eval.singleton O_out_of_bound flow

    | None ->
      (* Evaluate the offset in Z_u_num and check the bounds *)
      man.eval ~zone:(Z_c, Universal.Zone.Z_u_num) offset flow |>
      Eval.bind @@ fun offset exp ->

      (* safety condition: offset ∈ [0, base_size - cell_size] *)
      let range = offset.erange in
      let cond =
        mk_in
          offset
          (mk_zero range)
          (mk_binop (mk_z base_size range) O_minus (mk_z cell_size range) range ~etyp:T_int)
          range
      in
      Eval.assume ~zone:Universal.Zone.Z_u_num cond
        ~fthen:(fun flow ->
            (* Compute the interval and create a finite number of cells *)
            let open Universal.Numeric.Values.Intervals in
            let v = man.ask (Value.Q_interval offset) flow in
            let itv = v and step = Z.one in

            (* Iterate in case of bounded interval *)
            if Value.is_bounded itv then
              let l, u = Value.bounds itv in
              let rec aux i o =
                if Z.gt o u
                then []
                else if i >= !opt_expand
                then
                  let flow' = man.exec ~zone:Universal.Zone.Z_u_num (mk_assume (mk_binop offset O_ge (mk_z o range) range) range) flow in
                  [Eval.singleton (O_range (fun o' -> Z.geq o' o && Z.leq o' u)) flow']
                else
                  let flow' = man.exec ~zone:Universal.Zone.Z_u_num (mk_assume (mk_binop offset O_eq (mk_z o range) range) range) flow in
                  Eval.singleton (O_concrete o) flow' :: aux (i + 1) (Z.add o step)
              in
              let evals = aux 0 l in
              Eval.join_list evals
            else
              Eval.singleton (O_range (fun o -> Value.mem o itv)) flow
          )
        ~felse:(fun flow -> Eval.singleton O_out_of_bound flow)
        man flow


  (** Evaluation of cells *)
  (** =================== *)

  type ecell =
    | C_cell of ocell * mode
    | C_range of base * (Z.t -> bool)
    | C_fun of c_fundec

  let eval_cell exp man flow =
    match ekind exp with
    | E_var (v, mode) when is_c_type v.vtyp ->
      let c = {b = V v; o = Z.zero; t = v.vtyp}  in
      let flow = add_cons_cell man exp.erange c flow in
      debug "new variable %a in %a" pp_var v (Flow.print man) flow;
      Eval.singleton (C_cell (c, mode)) flow |>
      OptionExt.return

    | E_c_deref(p) ->
      man.eval ~zone:(Zone.Z_c, Z_c_points_to_cell) p flow |>
      Eval.bind_opt @@ fun pe flow ->

      begin
        match ekind pe with
        | E_c_points_to(P_var (b, o, t)) ->
          begin
            eval_cell_offset o (sizeof_type t) (base_size b) man flow |>
            Eval.bind @@ fun o flow ->
            match o with
            | O_concrete o -> Eval.singleton (C_cell ({b; o; t}, STRONG)) flow
            | O_range pred -> Eval.singleton (C_range (b, pred)) flow
            | O_out_of_bound ->
               let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack flow in
               let alarm = mk_alarm Alarms.AOutOfBound exp.erange ~cs in
               let flow' = Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man flow |>
                           Flow.set T_cur man.bottom man
               in
               Eval.empty_singleton flow'
          end
          |>
          OptionExt.return

        | E_c_points_to(P_fun fundec) ->
          Eval.singleton (C_fun fundec) flow |>
          OptionExt.return

        | E_c_points_to(P_null) ->
          let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack flow in
          let alarm = mk_alarm Alarms.ANullDeref exp.erange ~cs in
          let flow1 = Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man flow |>
                      Flow.set T_cur man.bottom man
          in
          Eval.empty_singleton flow1 |>
          OptionExt.return

        | E_c_points_to(P_invalid) ->
          let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack flow in
          let alarm = mk_alarm Alarms.AInvalidDeref exp.erange ~cs in
          let flow1 = Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man flow |>
                      Flow.set T_cur man.bottom man
          in
          Eval.empty_singleton flow1 |>
          OptionExt.return


        | _ -> assert false
      end

      | _ -> None


  (** Evaluation of C expressions *)
  (** =========================== *)

  let eval zone exp man flow =
    eval_cell exp man flow |>
    OptionExt.lift @@ Eval.bind @@ fun c flow ->
    match c with
    | C_cell(c, mode) ->
      add_cons_cell man exp.erange c flow |>
      Eval.singleton (mk_ocell c ~mode exp.erange)

    | C_range(b, pred) ->
      let a, b = rangeof exp.etyp in
      Eval.singleton (mk_z_interval a b exp.erange) flow

    | C_fun f -> Eval.singleton {exp with ekind = E_c_function f} flow


  (** Computation of post-conditions *)
  (** ============================== *)

  let rec exec zone stmt man flow =
    match skind stmt with
    | S_c_global_declaration(v, init) ->
      Init_visitor.init_global (init_visitor man) v init stmt.srange flow |>
      Post.of_flow |>
      OptionExt.return

    | S_c_local_declaration(v, init) ->
      Init_visitor.init_local (init_visitor man) v init stmt.srange flow
      |> Post.of_flow
      |> OptionExt.return

    | S_rebase_addr(adr, adr', mode) ->
      begin
        let u = Flow.get_domain_cur man flow in
        let l = exist_and_find_cells (fun c -> compare_base c.b (Base.A adr) = 0) u in
        let u = List.fold_left (fun acc c -> remove c acc) u l in
        let assigns = List.map
            (fun c -> let c' = ch_addr_of_ocell c adr' in
              mk_assign
                (mk_ocell c' ~mode:mode (tag_range stmt.srange "lval"))
                (mk_ocell c  (tag_range stmt.srange "rval"))
                (tag_range stmt.srange "assign")
            ) l
        in
        let mergers = List.map (fun c -> mk_remove_cell c (srange stmt)) l in
        Flow.set_domain_cur u man flow
        |> man.exec ~zone:Z_c_cell (mk_block assigns (tag_range stmt.srange "block"))
        |> man.exec ~zone:Z_c_cell (mk_block mergers (tag_range stmt.srange "mergers"))
        |> Post.of_flow
        |> Post.add_mergers mergers
        |> OptionExt.return
      end

    | S_remove_var (v) when is_c_type v.vtyp ->
      let u = Flow.get_domain_cur man flow in
      let l = exist_and_find_cells (fun c -> compare_base c.b (Base.V v) = 0) u in
      let u' = List.fold_left (fun acc c -> remove c acc) u l in
      let mergers = List.map (fun c -> mk_remove_cell c stmt.srange) l in
      let to_exec_in_sub = mergers in
      let flow = Flow.set_domain_cur u' man flow in
      man.exec ~zone:Z_c_cell (mk_block to_exec_in_sub stmt.srange) flow |>
      Post.of_flow |>
      Post.add_mergers mergers |>
      OptionExt.return

    | S_assign(lval, rval) when is_c_scalar_type lval.etyp ->
      man.eval ~zone:(Z_c, Z_c_cell) rval flow |>
      Post.bind_opt man @@ fun rval flow ->

      man.eval ~zone:(Z_c, Z_c_scalar) lval flow |>
      Post.bind_opt man @@ fun lval flow ->

      eval_cell lval man flow |>
      OptionExt.lift @@ Post.bind man @@ fun c flow ->
      begin
        match c with
        | C_cell(c, mode) -> assign_cell c rval mode stmt.srange man flow

        | C_range(b, pred) -> remove_cells b pred stmt.srange man flow

        | C_fun f -> assert false
      end

    | S_assume(e) ->
      man.eval ~zone:(Z_c, Z_c_cell) e flow |>
      Post.bind_opt man @@ fun e' flow ->

      let stmt' = {stmt with skind = S_assume e'} in
      man.exec ~zone:Z_c_cell stmt' flow |>

      Post.of_flow |>
      OptionExt.return

    | _ -> None


  and assign_cell c rval mode range man flow =
    let lval = mk_ocell c ~mode:mode range in
    let flow' = Flow.map_domain_env T_cur (add c) man flow |>
                man.exec ~zone:Z_c_cell (mk_assign lval rval range)
    in

    let a = Flow.get_domain_env T_cur man flow' in
    let overlappings = get_overlappings c a in

    let a' =
      overlappings |>
      List.fold_left (fun a c' -> remove c' a) a
    in

    let flow'' = Flow.set_domain_env T_cur a' man flow' in

    let block = List.map (fun c' -> mk_remove_cell c' range) overlappings in

    man.exec ~zone:Z_c_cell (mk_block block range) flow'' |>
    Post.of_flow |>
    Post.add_mergers (mk_remove_cell c range :: block) ~zone:Z_c_cell


  and remove_cells base pred range man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let cells =
      a |>
      exist_and_find_cells
        (fun c -> compare_base c.b base = 0 &&
                  pred c.o
        )
    in
    let block = List.map (fun c' -> mk_remove_cell c' range) cells in

    man.exec ~zone:Z_c_cell (mk_block block range) flow |>
    Post.of_flow |>
    Post.add_mergers block ~zone:Z_c_cell


  let ask _ _ _ = None

end


let () =
  Framework.Domains.Stacked.register_domain (module Domain);
