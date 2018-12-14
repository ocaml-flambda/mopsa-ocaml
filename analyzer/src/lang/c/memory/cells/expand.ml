(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Expansion-based abstraction of C memory cells. *)

open Mopsa
open Universal.Ast
open Ast
open Zone
open Common.Base
open Common.Points_to
open Cell
open Stubs.Old
module Intervals = Universal.Numeric.Values.Intervals.Value

(** Maximal number of expanded cells when dereferencing a pointer *)
let opt_expand = ref 1

let () =
  Framework.Options.register_option (
    "-cell-expand",
    Arg.Set_int opt_expand,
    " maximal number of expanded cells (default: 1)"
  )

(** Zoning *)

type zone +=
  | Z_c_cell_expand

let () =
  register_zone {
    zone = Z_c_cell_expand;
    name = "C/Cell/Expand";
    subset = Some Z_c_cell;
    eval = (fun exp -> Framework.Zone.eval exp Z_c_cell);
  }


module Domain = struct

  (*
   * Abstract element
   * ================
   *
   * We keep track of the set of previously expanded cells using the generic 
   * Powerset lattice.
   *)


  include Framework.Lattices.Powerset.Make(Cell)

  let is_bottom _ = false

  let print fmt c =
    Format.fprintf fmt "expand cells: @[%a@]@\n"
      print c

  (** Get the integer offset of a cell *)
  let zoffset c =
    match c.o with
    | O_single n -> n
    | _ -> assert false


  let exist_and_find_cell (f:cell -> bool) (cs:t) =
    apply (fun r ->
        let exception Found of cell in
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
        compare_cell c c' <> 0 &&
        (
          let cell_range c =
            (zoffset c, Z.add (zoffset c) (sizeof_type c.t))
          in
          let check_overlap (a1, b1) (a2, b2) = Z.lt (Z.max a1 a2) (Z.min b1 b2) in
          old_apply2 compare_base c.b c'.b = 0 &&
          check_overlap (cell_range c) (cell_range c')
        )
      ) cs



  (** Cell unification *)
  (** ================ *)

  (** [phi c u] collects constraints over cell [c] found in [u] *)
  let phi (c: cell) (u : t) range : expr option =
    let open Universal.Ast in
    let cs = u in
    match exist_and_find_cell (fun c' -> compare_cell c c' = 0) cs  with
    | Some c -> Some (mk_c_cell c range)
    | None ->
      begin
        match exist_and_find_cell
                (fun c' ->
                   is_c_int_type c'.t && Z.equal (sizeof_type c'.t) (sizeof_type c.t) &&
                   old_apply2 compare_base c.b c'.b = 0 &&
                   Z.equal (zoffset c) (zoffset c')
                ) cs
        with
        | Some (c') -> Some (wrap_expr (mk_c_cell c' range) (int_rangeof c.t) range)
        | None ->
          begin
            match exist_and_find_cell ( fun c' ->
                let b = Z.sub (zoffset c) (zoffset c') in
                Z.lt b (sizeof_type c'.t) && is_c_int_type c'.t && compare_typ (remove_typedef_qual c.t) (T_c_integer(C_unsigned_char)) = 0
              ) cs with
            | Some (c') ->
              let b = Z.sub (zoffset c) (zoffset c') in
              let base = (Z.pow (Z.of_int 2) (8 * Z.to_int b))  in
              Some (mk_binop (mk_binop (mk_c_cell c' range) O_div (mk_z base range) range) O_mod (mk_int 256 range) range)
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
                          let tobein = {b = c.b ; o = O_single (Z.add (zoffset c) (Z.of_int i)); t = t'} in
                          match exist_and_find_cell (fun c' ->
                              compare_cell c' tobein = 0
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
                          let res' = mk_binop (mk_binop (mk_z time range) O_mult (mk_c_cell x range) range) O_plus res range in
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
                  match old_extract c.b with
                  | S s ->
                    Some (mk_int (String.get s (Z.to_int (zoffset c)) |> int_of_char) range)
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
  let add_cons_cell_subman (subman: ('b, 'b) man) range (c: cell) u (s: 'b flow) =
    if mem c u then u, s
    else if not (is_c_scalar_type c.t) then u, s
    else if is_c_pointer_type (c.t) then
      add c u, s
    else
      match phi c u range with
      | Some e ->
        let stmt = Universal.Ast.(mk_assume (mk_binop (mk_c_cell c range) O_eq e ~etyp:T_int range) range) in
        add c u, subman.exec stmt s
      | None ->
        add c u, s

  (** [add_cell c range man flow] adds a cell and its numerical constraints *)
  let add_cell c range man f  =
    let u = Flow.get_domain_cur man f in
    if mem c u then f
    else if not (is_c_scalar_type c.t) then f
    else if is_c_pointer_type (c.t) then
      Flow.set_domain_cur (add c u) man f
    else
      match phi c u range with
      | Some e ->
        let stmt = Universal.Ast.(mk_assume (mk_binop (mk_c_cell c range) O_eq e ~etyp:T_int range) range) in
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

  let meet annot (subman: ('b, 'b) man) (u , (s: 'b flow) ) (u', (s': 'b flow)) =
    let u = meet annot u u' in
    (u, s, s')

  let widen annot subman (u,s) (u', s') =
    let (u, s, s') = join annot subman (u,s) (u',s') in
    (u, true, s, s')



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
    export = [
      Z_c_low_level, Z_c_cell_expand
    ];
    import = [
      (Z_c_scalar, Universal.Zone.Z_u_num);
      (Z_c, Z_under Z_c_cell);
      (Z_c, Z_c_cell_expand);
      (Z_c, Z_c_points_to);
      (Z_c_cell, Z_c_points_to)
    ];
  }


  (** Initialization *)
  (** ============== *)

  let rec init_visitor man =
    Common.Init_visitor.{
      (* Initialization of scalars *)
      scalar = (fun v e range flow ->
          let c =
            match ekind v with
            | E_var(v, mode) -> mkcell (V v) (O_single Z.zero) v.vtyp
            | E_c_cell (c, mode) -> c
            | _ -> assert false
          in
          man.eval ~zone:(Z_c, Z_under Z_c_cell) e flow |>
          Post.bind_flow man @@ fun e flow ->

          let flow = add_cell c range man flow in
          let stmt = mk_assign (mk_c_cell c range) e range in
          man.exec ~zone:Z_c_cell stmt flow
        );

      (* Initialization of arrays *)
      array =  (fun a is_global init_list range flow ->
          let c =
            match ekind a with
            | E_var(v, mode) -> mkcell (V v) (O_single Z.zero) v.vtyp
            | E_c_cell (c, mode) -> c
            | _ -> assert false
          in
          let rec aux i l flow =
            if i = !opt_expand then flow
            else
              match l with
              | [] -> flow
              | init :: tl ->
                let t' = under_array_type c.t in
                let ci = {b = c.b; o = O_single Z.((zoffset c) + (Z.of_int i) * (sizeof_type t')); t = t'} in
                let flow' = init_expr (init_visitor man) (mk_c_cell ci range) is_global init range flow in
                aux (i + 1) tl flow'
          in
          aux 0 init_list flow
        );

      (* Initialization of structs *)
      record =  (fun s is_global init_list range flow ->
          let c =
            match ekind s with
            | E_var (v, _) -> mkcell (V v) (O_single Z.zero) (v.vtyp)
            | E_c_cell(c, _) -> c
            | _ -> assert false
          in
          let record = match remove_typedef_qual c.t with T_c_record r -> r | _ -> assert false in
          match init_list with
          | Parts l ->
            let rec aux i l flow =
              match l with
              | [] -> flow
              | init :: tl ->
                let field = List.nth record.c_record_fields i in
                let t' = field.c_field_type in
                let cf = {b = c.b; o = O_single Z.((zoffset c) + (Z.of_int field.c_field_offset)); t = t'} in
                let flow' = init_expr (init_visitor man) (mk_c_cell cf ~mode:STRONG range) is_global init range flow in
                aux (i + 1) tl flow'
            in
            aux 0 l flow

          | Expr e -> panic "expand: record assignment not supported"
        );
    }

  let init prog man flow =
    Some (
      Flow.set_domain_cur empty man flow
    )


  (** Evaluation of C expressions *)
  (** =========================== *)

  (** Evaluate an offset expression into an offset evaluation *)
  let eval_cell_offset (offset:expr) cell_size base_size man flow : ('a, offset) evl =
    (* Try the static case *)
    match expr_to_z offset with
    | Some z ->
      if Z.geq z Z.zero &&
         Z.leq (Z.add z cell_size) base_size
      then Eval.singleton (O_single z) flow
      else Eval.singleton O_out_of_bound flow

    | None ->
      (* Evaluate the offset in Z_u_num and check the bounds *)
      man.eval ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) offset flow |>
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
                  [Eval.singleton (O_region (Intervals.of_constant () (C_int_interval (o, u)))) flow']
                else
                  let flow' = man.exec ~zone:Universal.Zone.Z_u_num (mk_assume (mk_binop offset O_eq (mk_z o range) range) range) flow in
                  Eval.singleton (O_single o) flow' :: aux (i + 1) (Z.add o step)
              in
              let evals = aux 0 l in
              Eval.join_list evals
            else
              Eval.singleton (O_region itv) flow
          )
        ~felse:(fun flow -> Eval.singleton O_out_of_bound flow)
        man flow


  (** Evaluation of cells *)
  (** =================== *)

  (** Evaluate a base and an non-quantified offset expression into a cell *)
  let eval_non_quantified_cell b o t range man flow : ('a, cell) evl =
    eval_cell_offset o (sizeof_type t) (base_size b) man flow |>
    Eval.bind @@ fun o flow ->
    match o with
    | O_single _ | O_region _ ->
      Eval.singleton (mkcell b o t) flow

    | O_out_of_bound ->
      let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack flow in
      let alarm = mk_alarm Alarms.AOutOfBound range ~cs in
      let flow' = Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man flow |>
                  Flow.set T_cur man.bottom man
      in
      Eval.empty_singleton flow'

  (** Evaluate a base and a quantified offset expression into a cell *)
  let eval_quantified_cell b o t range man flow : ('a, cell) evl =
    panic_at range "expand: evaluation of quantified cells not supported"

  (** Evaluate a scalar lval into a cell *)
  let eval_scalar_cell exp man flow : ('a, cell) evl =
    match ekind exp with
    | E_var (v, mode) when is_c_scalar_type v.vtyp ->
      let c = mkcell (V v) (O_single Z.zero) v.vtyp  in
      Eval.singleton c flow

    | E_c_deref(p) ->
      man.eval ~zone:(Z_c, Z_c_cell_expand) p flow |>
      Eval.bind @@ fun p flow ->

      let t = under_type p.etyp in

      man.eval ~zone:(Z_c_cell, Z_c_points_to) p flow |>
      Eval.bind @@ fun pe flow ->

      begin match ekind pe with
        | E_c_points_to(P_block (b, o)) when Stubs.Ast.is_expr_quantified o ->
          eval_quantified_cell b o t p.erange man flow

        | E_c_points_to(P_block (b, o)) ->
          eval_non_quantified_cell b o t p.erange man flow

        | E_c_points_to(P_null) ->
          let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack flow in
          let alarm = mk_alarm Alarms.ANullDeref p.erange ~cs in
          let flow1 = Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man flow |>
                      Flow.set T_cur man.bottom man
          in
          Eval.empty_singleton flow1

        | E_c_points_to(P_invalid) ->
          let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack flow in
          let alarm = mk_alarm Alarms.AInvalidDeref p.erange ~cs in
          let flow1 = Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man flow |>
                      Flow.set T_cur man.bottom man
          in
          Eval.empty_singleton flow1

        | _ -> assert false
      end

    | _ -> debug "%a" pp_expr exp; assert false


  (** Entry-point of evaluations *)
  let eval zone exp man flow =
    match ekind exp with
    (* 𝔼⟦ v ⟧ *)
    | E_var _

    (* 𝔼⟦ *p ⟧ *)
    | E_c_deref _ when is_c_scalar_type exp.etyp ->
      eval_scalar_cell exp man flow |>
      Eval.bind_return @@ fun c flow ->

      begin match c.o with
        | O_single _ ->
          let flow = add_cell c exp.erange man flow in
          Eval.singleton (mk_c_cell c exp.erange) flow

        | O_region _ ->
          let l, u = rangeof c.t in
          Eval.singleton (mk_z_interval l u exp.erange) flow

        | _ -> assert false
      end

    (* 𝔼⟦ *p ⟧ when p is function pointer*)
    | E_c_deref p when is_c_function_type exp.etyp ->
      man.eval ~zone:(Z_c, Z_c_cell_expand) p flow |> Eval.bind_return @@ fun pp flow ->
      man.eval ~zone:(Z_c_cell, Z_c_points_to) pp flow |> Eval.bind @@ fun pe flow ->

      begin match ekind pe with
        | E_c_points_to(P_fun f) ->
          Eval.singleton { exp with ekind = E_c_function f } flow

        | _ -> panic_at exp.erange "expand: unsupported function pointer %a" pp_expr p
      end

    (* 𝔼⟦ size(p) ⟧ *)
    | Stubs.Ast.E_stub_builtin_call({ content = SIZE }, p) ->
      man.eval ~zone:(Zone.Z_c, Z_c_points_to) p flow |>
      Eval.bind_opt @@ fun pe flow ->

      begin match ekind pe with
        | E_c_points_to(P_block (b, o)) ->
          let t = under_type p.etyp in
          Eval.singleton (mk_z (Z.div (base_size b) (Z.max Z.one (sizeof_type t))) exp.erange) flow |>
          OptionExt.return

        | _ -> panic_at exp.erange "cells.expand: size(%a) not supported" pp_expr exp
      end

    (* 𝔼⟦ old(e) ⟧ *)
    | Stubs.Ast.E_stub_builtin_call({content = OLD }, e) ->
      panic_at exp.erange "old not supported"

    | _ -> None



  (** Computation of post-conditions *)
  (** ============================== *)

  (** Assign an rval to a cell *)
  let assign_cell c rval range man flow =
    let lval = mk_c_cell c range in
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

    let block = List.map (fun c' -> mk_c_remove_cell c' range) overlappings in

    man.exec ~zone:Z_c_cell (mk_block block range) flow'' |>
    Post.of_flow |>
    Post.add_mergers (mk_c_remove_cell c range :: block) ~zone:Z_c_cell


  (** Remove cells identified by an offset membership predicate *)
  let remove_cells b pred range man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let cells =
      a |>
      exist_and_find_cells
        (fun c ->
           old_apply2 compare_base c.b b = 0 &&
           pred (zoffset c)
        )
    in
    let block = List.map (fun c' -> mk_c_remove_cell c' range) cells in

    man.exec ~zone:Z_c_cell (mk_block block range) flow |>
    Post.of_flow |>
    Post.add_mergers block ~zone:Z_c_cell


  (** Interpret stub assignments. All cells within
      container [c] and having an offset in the range [offsets] are put
      to ⊤, and old copies created.
  *)
  let stub_assign_cell c offsets t range man flow = panic "prepare_cells_for_stub_assigns not implemented"

  (** Remove old copies of a stub assigned cell *)
  let remove_old_cells c range man flow = panic "remove_old_cells not implemented"

  (** Entry point of post-condition computation *)
  let rec exec zone stmt man flow =
    match skind stmt with
    (* 𝕊⟦ t v = e; ⟧ when v is a global variable *)
    | S_c_global_declaration(v, init) ->
      Common.Init_visitor.init_global (init_visitor man) v init stmt.srange flow |>
      Post.return

    (* 𝕊⟦ t v = e; ⟧ when v is a local variable *)
    | S_c_local_declaration(v, init) ->
      Common.Init_visitor.init_local (init_visitor man) v init stmt.srange flow |>
      Post.return

    (* 𝕊⟦ add v ⟧ *)
    | S_add { ekind = E_var (v, _) } when is_c_type v.vtyp ->
      eval_scalar_cell (mk_var v stmt.srange) man flow |>
      Post.bind_return man @@ fun c flow ->

      man.exec ~zone:Z_c_cell (mk_c_add_cell c stmt.srange) flow |>
      Post.of_flow |>
      Post.add_merger (mk_c_add_cell c stmt.srange)

    (* 𝕊⟦ rebase adr -> adr' ⟧ *)
    | S_rebase_addr(adr, adr', mode) ->
      panic_at stmt.srange "expand: S_rebase_addr not supported"

    (* 𝕊⟦ remove v ⟧ *)
    | S_remove { ekind = E_var (v, _) } when is_c_type v.vtyp ->
      let u = Flow.get_domain_cur man flow in
      let l = exist_and_find_cells (fun c -> old_apply compare_base c.b (V v) = 0) u in
      let u' = List.fold_left (fun acc c -> remove c acc) u l in
      let mergers = List.map (fun c -> mk_c_remove_cell c stmt.srange) l in
      let to_exec_in_sub = mergers in
      let flow = Flow.set_domain_cur u' man flow in
      man.exec ~zone:Z_c_cell (mk_block to_exec_in_sub stmt.srange) flow |>
      Post.of_flow |>
      Post.add_mergers mergers |>
      OptionExt.return

    (* 𝕊⟦ lval = rval ⟧ *)
    | S_assign(lval, rval) when is_c_scalar_type lval.etyp ->
      man.eval ~zone:(Z_c, Z_under Z_c_cell) rval flow |>
      Post.bind_opt man @@ fun rval flow ->

      man.eval ~zone:(Z_c, Z_c_low_level) lval flow |>
      Post.bind_opt man @@ fun lval flow ->

      eval_scalar_cell lval man flow |>
      Post.bind_return man @@ fun c flow ->

      begin match c.o with
        | O_single _ -> assign_cell c rval stmt.srange man flow
        | O_region itv -> remove_cells c.b (fun o -> Intervals.mem o itv) stmt.srange man flow
        | _ -> panic_at stmt.srange "expand: lval cell %a not supported in assignments" pp_cell c
      end

    (* 𝕊⟦ assume ?e ⟧ *)
    | S_assume(e) ->
      man.eval ~zone:(Z_c, Z_under Z_c_cell) e flow |>
      Post.bind_opt man @@ fun e' flow ->

      let stmt' = {stmt with skind = S_assume e'} in
      man.exec ~zone:Z_c_cell stmt' flow |>

      Post.return

    (* 𝕊⟦ assigns a; ⟧ *)
    | Stubs.Ast.S_stub_assigns _ ->
      panic_at stmt.srange "expand: stub assigns directive not supported"

    | _ -> None


  (** Query handlers *)
  (** ============== *)


  let ask _ _ _ = None

end


let () =
  Framework.Domains.Stacked.register_domain (module Domain);
