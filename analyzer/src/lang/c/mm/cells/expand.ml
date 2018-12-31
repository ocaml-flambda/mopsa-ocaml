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
open Stubs.Ast
open Ast
open Zone
open Common.Base
open Common.Points_to
open Cell
module Itv = Universal.Numeric.Values.Intervals.Value

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

  (* An abstract element is the set of previously expanded cells,
     represented as a powerset lattice.  *)

  module Cells = Framework.Lattices.Powerset.Make(PrimedCell)

  include Cells

  let is_bottom _ = false

  let print fmt c =
    Format.fprintf fmt "expand cells: @[%a@]@\n"
      print c

  (** Get the integer offset of a cell *)
  let zoffset c =
    match c.o with
    | O_single n -> n
    | _ -> assert false


  let exist_and_find_cell (f:cell primed-> bool) (cs:t) =
    apply (fun r ->
        let exception Found of cell primed in
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
        PrimedCell.compare c c' <> 0 &&
        (
          let cell_range c =
            (primed_apply cell_zoffset c, Z.add (primed_apply cell_zoffset c) (sizeof_type (primed_apply cell_typ c)))
          in
          let check_overlap (a1, b1) (a2, b2) = Z.lt (Z.max a1 a2) (Z.min b1 b2) in
          compare_base (primed_apply cell_base c) (primed_apply cell_base c') = 0 &&
          check_overlap (cell_range c) (cell_range c')
        )
      ) cs



  (** Cell unification *)
  (** ================ *)

  (** [phi c u] collects constraints over cell [c] found in [u] *)
  let phi (c: cell primed) (u : t) range : expr option =
    let open Universal.Ast in
    let cs = u in
    match
      exist_and_find_cell (fun c' -> PrimedCell.compare c c' = 0) cs
    with
    | Some c ->
      debug "case 1";
      Some (PrimedCell.to_expr c (primed_apply cell_mode c) range)

    | None ->
      match exist_and_find_cell
              (fun c' ->
                 is_similarly_primed c c' &&
                 is_c_int_type (primed_apply cell_typ c') &&
                 Z.equal (sizeof_type (primed_apply cell_typ c')) (sizeof_type (primed_apply cell_typ c)) &&
                 compare_base (primed_apply cell_base c) (primed_apply cell_base c') = 0 &&
                 Z.equal (primed_apply cell_zoffset c) (primed_apply cell_zoffset c')
              ) cs
      with
      | Some (c') ->
        debug "case 2";
        Some (wrap_expr
                (PrimedCell.to_expr c' (primed_apply cell_mode c') range)
                (int_rangeof (primed_apply cell_typ c))
                range
             )
      | None ->
        match
          exist_and_find_cell ( fun c' ->
              let b = Z.sub (primed_apply cell_zoffset c) (primed_apply cell_zoffset c') in
              is_similarly_primed c c' &&
              Z.lt b (sizeof_type (primed_apply cell_typ c')) &&
              is_c_int_type (primed_apply cell_typ c') &&
              compare_typ (remove_typedef_qual (primed_apply cell_typ c)) (T_c_integer(C_unsigned_char)) = 0
            ) cs
        with
        | Some (c') ->
          debug "case 3";
          let b = Z.sub (primed_apply cell_zoffset c) (primed_apply cell_zoffset c') in
          let base = (Z.pow (Z.of_int 2) (8 * Z.to_int b))  in
          Some (_mod
                  (div (PrimedCell.to_expr c' (primed_apply cell_mode c') range) (mk_z base range) range)
                  (mk_int 256 range)
                  range
               )

        | None ->
          let exception NotPossible in
          try
            if is_c_int_type (primed_apply cell_typ c) then
              let t' = T_c_integer(C_unsigned_char) in
              let n = Z.to_int (sizeof_type ((primed_apply cell_typ c))) in
              let rec aux i l =
                if i < n then
                  let tobein = primed_lift (fun cc -> {b = cc.b ; o = O_single (Z.add (primed_apply cell_zoffset c) (Z.of_int i)); t = t'}) c in
                  match
                    exist_and_find_cell (fun c' ->
                        PrimedCell.compare c' tobein = 0
                      ) cs
                  with
                  | Some (c') -> aux (i+1) (c' :: l)
                  | None -> raise NotPossible
                else
                  List.rev l
              in
              let ll = aux 0 [] in
              let _,e = List.fold_left (fun (time, res) x ->
                  let res' =
                    add
                      (mul (mk_z time range) (PrimedCell.to_expr x (primed_apply cell_mode x) range) range)
                      res
                      range
                  in
                  let time' = Z.mul time (Z.of_int 256) in
                  time',res'
                ) (Z.of_int 1,(mk_int 0 range)) ll
              in
              debug "case 4";
              Some e
            else
              raise NotPossible
          with
          | NotPossible ->
            match primed_apply cell_base c with
            | S s ->
              debug "case 5";
              Some (mk_int (String.get s (Z.to_int (primed_apply cell_zoffset c)) |> int_of_char) range)

            | _ ->
              if is_c_scalar_type (primed_apply cell_typ c) then
                let () = debug "case 6" in
                let a,b = rangeof (primed_apply cell_typ c) in
                Some (mk_z_interval a b range)
              else if is_c_pointer_type (primed_apply cell_typ c) then
                assert false
              else
                let () = debug "case 7" in
                None

  (** [add_cons_cell_subman subman range c u s] adds a cell [c] to the
      abstraction [u] given a manager [subman] on the sub element of
      the stack [s] *)
  let add_cons_cell_subman (subman: ('b, 'b) man) range (c: cell primed) u (s: 'b flow) =
    if mem c u then u, s
    else if not (is_c_scalar_type (primed_apply cell_typ c)) then u, s
    else if is_c_pointer_type ((primed_apply cell_typ c)) then
      add c u, s
    else
      match phi c u range with
      | Some e ->
        let stmt = Universal.Ast.(mk_assume (mk_binop (PrimedCell.to_expr c (primed_apply cell_mode c) range) O_eq e ~etyp:T_int range) range) in
        add c u, subman.exec stmt s
      | None ->
        add c u, s

  (** [add_cell c range man flow] adds a cell and its numerical constraints *)
  let add_cell c range man f  =
    let u = Flow.get_domain_cur man f in
    if mem c u then f
    else if not (is_c_scalar_type (primed_apply cell_typ c)) then f
    else if is_c_pointer_type ((primed_apply cell_typ c)) then
      Flow.set_domain_cur (add c u) man f
    else
      match phi c u range with
      | Some e ->
        let stmt = Universal.Ast.(mk_assume (mk_binop (PrimedCell.to_expr c (primed_apply cell_mode c) range) O_eq e ~etyp:T_int range) range) in
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
      (Z_c_low_level, Z_c_cell_expand);
      (Z_c_scalar, Universal.Zone.Z_u_num);
      (Z_c, Universal.Zone.Z_u_num);
      (Z_c, Z_under Z_c_cell);
      (Z_c, Z_c_cell_expand);
      (Z_c, Z_c_points_to);
      (Z_c, Z_c_scalar);
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
            | E_var(v, mode) -> {b = V v; o = O_single Z.zero; t = v.vtyp}
            | E_c_cell (c, mode) -> c
            | _ -> assert false
          in
          man.eval ~zone:(Z_c, Z_under Z_c_cell) e flow |>
          Post.bind_flow man @@ fun e flow ->

          let flow = add_cell (unprimed c) range man flow in
          let stmt = mk_assign (mk_c_cell c range) e range in
          man.exec ~zone:Z_c_cell stmt flow
        );

      (* Initialization of arrays *)
      array =  (fun a is_global init_list range flow ->
          let c =
            match ekind a with
            | E_var(v, mode) -> {b = V v; o = O_single Z.zero; t = v.vtyp}
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
            | E_var (v, _) -> {b = V v; o = O_single Z.zero; t = v.vtyp}
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
    match expr_to_z offset, expr_to_z base_size with
    | Some z, Some base_size ->
      if Z.geq z Z.zero &&
         Z.leq (Z.add z cell_size) base_size
      then Eval.singleton (O_single z) flow
      else Eval.singleton O_out_of_bound flow

    | _ ->
      (* Evaluate the offset and base_size in Z_u_num and check the bounds *)
      man.eval ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) offset flow |>
      Eval.bind @@ fun offset exp ->

      man.eval ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) base_size flow |>
      Eval.bind @@ fun base_size exp ->

      (* safety condition: offset ∈ [0, base_size - cell_size] *)
      let range = offset.erange in
      let cond =
        mk_in
          offset
          (mk_zero range)
          (sub base_size (mk_z cell_size range) range ~typ:T_int)
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
                  [Eval.singleton (O_region (Itv.of_constant () (C_int_interval (o, u)))) flow']
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
  let eval_non_quantified_cell b o t ~is_primed range man flow : ('a, cell primed) evl =
    eval_base_size b range man flow |>
    Eval.bind @@ fun base_size flow ->

    eval_cell_offset o (sizeof_type t) base_size man flow |>
    Eval.bind @@ fun o flow ->

    match o with
    | O_single _ | O_region _ ->
      let c = {b;o;t} in
      let pc = if is_primed then primed c else unprimed c in
      Eval.singleton pc flow

    | O_out_of_bound ->
      let flow' = raise_alarm Alarms.AOutOfBound range ~bottom:true man flow in
      Eval.empty_singleton flow'



  (** Evaluate a base and a quantified offset expression into a cell

      Existentially quantified variables are replaced by un-quantified
      variables, since cell evaluation returns a disjunction.

       However, for universally quantified variables, we discritize a
      number of samples within the under-approximation of the
      range. For each sample, we replace the variable by its litteral
      value, and we compute a conjuction of the resulting evaluations
  *)
  let eval_quantified_cell b o t ~is_primed range man flow : ('a, cell primed) evl =
    debug "eval_quantified_cell: %a %a" pp_base b pp_expr o;
    (* Replace ∃ variables with unquantified variables and get the
       list of ∀ variables *)
    let forall_vars, o' =
      Visitor.fold_map_expr
        (fun acc exp ->
           match ekind exp with
           | E_stub_quantified(EXISTS, var, set) ->
             Keep (acc, { exp with ekind = E_var(var, STRONG) })

           | E_stub_quantified(FORALL, var, S_interval(l, u)) ->
             Keep ((var, l, u) :: acc, exp)

           | _ -> VisitParts (acc, exp)
        )
        (fun acc stmt -> VisitParts (acc, stmt))
        [] o
    in

    (* Compute the under-approximation space of the range of ∀ variables *)
    let exception NoUnderApprox in
    try
      let space = List.map (fun (var, l, u) ->
          let evl1 = man.eval ~zone:(Z_c, Universal.Zone.Z_u_num) l flow in
          let itv1 = Eval.substitute (fun l flow ->
              man.ask (Itv.Q_interval l) flow
            ) (Itv.join ()) (Itv.meet ()) Itv.bottom evl1
          in

          let evl2 = man.eval ~zone:(Z_c, Universal.Zone.Z_u_num) u flow in
          let itv2 = Eval.substitute (fun u flow ->
              man.ask (Itv.Q_interval u) flow
            ) (Itv.join ()) (Itv.meet ()) Itv.bottom evl2
          in

          debug "%a in [ %a, %a ]" pp_var var Itv.print itv1 Itv.print itv2;
          if Itv.is_bounded itv1 && Itv.is_bounded itv2 then
            let (a,b) = Itv.bounds itv1 in
            let (c,d) = Itv.bounds itv2 in
            (var, b, c)
          else
            raise (NoUnderApprox)
        ) forall_vars
      in

      (* Compute some samples from the space of under-approximations *)
      let rec samples space =
        match space with
        | [] -> [[]]
        | (var, l, u) :: tl ->
          let after = samples tl in

          (** Iterate on values in [i, u] and add them to the result vectors *)
          let rec iter_on_space_dim ret i =
            if Z.gt i u then ret
            else iter_on_space_dim (add_sample i after ret) (Z.succ i)

          (** and a sample value i to the result *)
          and add_sample i after ret =
            if List.length ret == !opt_expand then
              ret
            else
              match after with
              | [] -> ret
              | hd :: tl ->
                add_sample i tl (((var, i) :: hd)  :: ret)
          in

          iter_on_space_dim [] l
      in
      (* Each sample vector is an evaluation of the values of
         quantified variables.  So, for each vector, we substitute the
         quantified variable with the its value.  We then compute the
         cell for this sample.  At the end, we meet all
         evaluations. *)
      let conj =
        samples space |>
        List.map (fun sample ->
            let o'' = Visitor.map_expr
                (fun e ->
                   match ekind e with
                   | E_stub_quantified(FORALL, var, _) ->
                     let _, i = List.find (fun (var', _) -> compare_var var var' = 0) sample in
                     Keep { e with ekind = E_constant (C_int i) }

                   | _ -> VisitParts e
                )
                (fun s -> VisitParts s)
                o'
            in
            debug "offset sample: %a" pp_expr o'';
            eval_non_quantified_cell b o'' ~is_primed t range man flow
          )
      in

      (* We need to add evaluated cells, so that they will not be lost after meet *)
      let cells = conj |> List.fold_left (fun acc evl ->
          let c = Eval.substitute
            (fun c _ -> singleton c)
            (Cells.join ()) (Cells.meet ()) (Cells.empty)
            evl
          in
          Cells.join () acc c
        ) Cells.empty
      in
      let conj = List.map (fun evl ->
          Eval.map (fun c flow ->
              c, Flow.map_domain_env T_cur (Cells.join () cells) man flow
            ) evl
        ) conj
      in
      Eval.meet_list conj
    with NoUnderApprox ->
      (* The under-approximation is empty => return an empty evaluation *)
      warn "no under approxiamtion";
      Eval.empty_singleton flow




  (** Evaluate a scalar lval into a cell *)
  let rec eval_scalar_cell exp man flow : ('a, cell primed) evl =
    match ekind exp with
    | var when PrimedVar.match_expr exp &&
               is_c_scalar_type exp.etyp ->
      let v = PrimedVar.from_expr exp in
      let c = { b = V (unprime v); o = O_single Z.zero; t = primed_apply vtyp v } in
      let pc = if is_primed v then primed c else unprimed c in
      Eval.singleton pc flow

    | deref when match_primed_expr (function { ekind = E_c_deref _ } -> true | _ -> false) exp ->
      let is_primed = is_primed_expr exp in
      let p = primed_expr_apply (function { ekind = E_c_deref p } -> p | _ -> assert false) exp in
      let t = under_type p.etyp in

      man.eval ~zone:(Z_c, Z_c_points_to) ~via:Z_c_cell_expand p flow |>
      Eval.bind @@ fun pe flow ->

      begin match ekind pe with
        | E_c_points_to(P_block (b, o)) when is_expr_quantified o ->
          eval_quantified_cell b o t ~is_primed p.erange man flow

        | E_c_points_to(P_block (b, o)) ->
          eval_non_quantified_cell b o t ~is_primed p.erange man flow

        | E_c_points_to(P_null) ->
          let flow' = raise_alarm Alarms.ANullDeref p.erange ~bottom:true man flow in
          Eval.empty_singleton flow'

        | E_c_points_to(P_invalid) ->
          let flow' = raise_alarm Alarms.AInvalidDeref p.erange ~bottom:true man flow in
          Eval.empty_singleton flow'

        | _ -> panic_at exp.erange "eval_scalar_cell: invalid pointer %a" pp_expr p;
      end

    | _ -> debug "%a" pp_expr exp; assert false



  (** Entry-point of evaluations *)
  let eval zone exp man flow =
    match ekind exp with
    (* 𝔼⟦ v | *p | v' | ( *p )' ⟧ *)
    | lval when is_c_scalar_type exp.etyp &&
                (
                  match_primed_expr (function
                      | { ekind = E_var _ | E_c_deref _ } -> true
                      | _ -> false
                    ) exp
                )
      ->
      eval_scalar_cell exp man flow |>
      Eval.bind_return @@ fun c flow ->

      begin match primed_apply cell_offset c with
        | O_single _ ->
          let flow = add_cell c exp.erange man flow in
          Eval.singleton (PrimedCell.to_expr c (primed_apply cell_mode c) exp.erange) flow

        | O_region _ ->
          let l, u = rangeof (primed_apply cell_typ c) in
          Eval.singleton (mk_z_interval l u ~typ:exp.etyp exp.erange) flow

        | _ -> assert false
      end

    (* 𝔼⟦ *p ⟧ when p is function pointer*)
    | E_c_deref p when is_c_function_type exp.etyp ->
      man.eval ~zone:(Z_c, Z_c_points_to) ~via:Z_c_cell_expand p flow |>
      Eval.bind_return @@ fun pe flow ->

      begin match ekind pe with
        | E_c_points_to(P_fun f) ->
          Eval.singleton { exp with ekind = E_c_function f } flow

        | _ -> panic_at exp.erange "expand: unsupported function pointer %a" pp_expr p
      end

    (* 𝔼⟦ size(p) ⟧ *)
    | E_stub_builtin_call(SIZE, p) ->
      man.eval ~zone:(Zone.Z_c, Z_c_points_to) p flow |>
      Eval.bind_return @@ fun pe flow ->

      begin match ekind pe with
        | E_c_points_to(P_block (b, o)) ->
          eval_base_size b exp.erange man flow |>
          Eval.bind @@ fun base_size flow ->

          let t = under_type p.etyp in
          let exp' = div base_size (of_z (Z.max Z.one (sizeof_type t)) exp.erange) exp.erange in
          Eval.singleton exp' flow

        | _ -> panic_at exp.erange "cells.expand: size(%a) not supported" pp_expr exp
      end

    (* 𝔼⟦ valid(p) ⟧ *)
    | E_stub_builtin_call( PTR_VALID, p) ->
      man.eval ~zone:(Z_c_low_level, Z_c_cell_expand) p flow |>
      Eval.bind_return @@ fun p flow ->
      let exp' = { exp with ekind = E_stub_builtin_call( PTR_VALID, p) } in
      Eval.singleton exp' flow

    | _ -> None



  (** Computation of post-conditions *)
  (** ============================== *)

  (** Assign an rval to a cell *)
  let assign_cell c rval range man flow =
    let lval = PrimedCell.to_expr c (primed_apply cell_mode c) range in
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

    let block = List.map (fun c' -> mk_remove (PrimedCell.to_expr c' (primed_apply cell_mode c') range) range) overlappings in

    man.exec ~zone:Z_c_cell (mk_block block range) flow''



  (** Remove cells identified by a membership predicate *)
  let remove_cells pred range man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let cells = exist_and_find_cells pred a in
    let block = List.map (fun c' -> mk_remove (PrimedCell.to_expr c' (primed_apply cell_mode c') range) range) cells in
    man.exec ~zone:Z_c_cell (mk_block block range) flow



  (** Rename an old cell into a new one *)
  let rename_cell cold cnew range man flow =
    debug "rename %a into %a" PrimedCell.print cold PrimedCell.print cnew;
    let flow' =
      (* Add the old cell in case it has not been accessed before so
         that its constraints are added in the sub domain *)
      add_cell cold range man flow |>
      (* Remove the old cell and add the new one *)
      Flow.map_domain_env T_cur (fun a ->
          remove cold a |>
          add cnew
        ) man
    in
    let stmt' = mk_rename
        (PrimedCell.to_expr cold (primed_apply cell_mode cold) range)
        (PrimedCell.to_expr cnew (primed_apply cell_mode cnew) range)
        range
    in
    man.exec ~zone:Z_c_cell stmt' flow'



  (** Rename primed cells that have been declared as assigned in a stub *)
  let rename_stub_primed_cells pt offsets t range man flow =
    let base, offset =
      match ekind pt with
      | E_c_points_to (P_block(b, o)) -> b, o
      | _ -> assert false
    in

    let a = Flow.get_domain_env T_cur man flow in

    let cells = filter (fun c ->
        compare_base base (primed_apply cell_base c) = 0
      ) a
    in

    debug "cells = %a" print cells;

    (* For primed cells, just rename to an unprimed cell *)
    let flow = fold (fun c flow ->
        if is_primed c
        then rename_cell c (unprimed (unprime c)) range man flow
        else flow
      ) cells flow
    in

    (* Compute the interval of the assigned cells *)
    let itv =

      (* Return the expression of an offset bound *)
      let mk_offset_bound before bound t =
        let elem_size = sizeof_type t in
        Universal.Ast.add before (
          mul bound (mk_z elem_size range) range ~typ:T_int
        ) range ~typ:T_int
      in

      (* Create the expressions for the offset bounds *)
      let l, u =
        let rec doit accl accu t =
          function
          | [] -> accl, accu
          | [(l, u)] ->
            (mk_offset_bound accl l t), (mk_offset_bound accu u t)
          | (l, u) :: tl ->
            doit (mk_offset_bound accl l t) (mk_offset_bound accu u t) (under_type t) tl

        in
        doit offset offset t offsets
      in
      debug "l = %a, u = %a" pp_expr l pp_expr u;
      (* Compute the interval of the bounds *)
      let evl1 = man.eval ~zone:(Z_c, Universal.Zone.Z_u_num) l flow in
      let itv1 = Eval.substitute (fun l flow ->
          man.ask (Itv.Q_interval l) flow
        ) (Itv.join ()) (Itv.meet ()) Itv.bottom evl1
      in

      let evl2 = man.eval ~zone:(Z_c, Universal.Zone.Z_u_num) u flow in
      let itv2 = Eval.substitute (fun u flow ->
          man.ask (Itv.Q_interval u) flow
        ) (Itv.join ()) (Itv.meet ()) Itv.bottom evl2
      in

      Itv.join () itv1 itv2
    in
    debug "itv = %a" Itv.print itv;
    (* Remove remaining cells that have an offset within the assigned interval *)
    remove_cells (fun c ->
        mem c cells &&
        not (mem (primed (unprime c)) cells) &&
        Itv.mem (primed_apply cell_zoffset c) itv
      ) range man flow


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

      add_cell c stmt.srange man flow |>
      man.exec ~zone:Z_c_cell (mk_add (PrimedCell.to_expr c (primed_apply cell_mode c) stmt.srange) stmt.srange) |>

      Post.of_flow |>
      Post.add_merger (mk_add (PrimedCell.to_expr c (primed_apply cell_mode c) stmt.srange) stmt.srange)

    (* 𝕊⟦ remove v ⟧ *)
    | S_remove { ekind = E_var (v, _) } when is_c_type v.vtyp ->
      let u = Flow.get_domain_cur man flow in
      let l = exist_and_find_cells (fun c -> compare_base (primed_apply cell_base c) (V v) = 0) u in
      let u' = List.fold_left (fun acc c -> remove c acc) u l in
      let mergers = List.map (fun c -> mk_remove (PrimedCell.to_expr c (primed_apply cell_mode c) stmt.srange) stmt.srange) l in
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
      Post.bind_opt man @@ fun c flow ->

      let flow =
        match primed_apply cell_offset c with
        | O_single _ ->
          assign_cell c rval stmt.srange man flow

        | O_region itv ->
          remove_cells (fun c' ->
              compare_base (primed_apply cell_base c) (primed_apply cell_base c') = 0 &&
              Itv.mem (primed_apply cell_zoffset c') itv
            ) stmt.srange man flow

        | _ -> panic_at stmt.srange "expand: lval cell %a not supported in assignments" PrimedCell.print c
      in

      Post.return flow

    (* 𝕊⟦ assume ?e ⟧ *)
    | S_assume(e) ->
      man.eval ~zone:(Z_c, Z_under Z_c_cell) e flow |>
      Post.bind_opt man @@ fun e' flow ->

      let stmt' = {stmt with skind = S_assume e'} in
      man.exec ~zone:Z_c_cell stmt' flow |>

      Post.return

    (* 𝕊⟦ rename(old, new) ⟧ *)
    | S_rename(eold, enew) when is_c_scalar_type eold.etyp &&
                                is_c_scalar_type enew.etyp
      ->
      man.eval ~zone:(Z_c, Z_under Z_c_cell) eold flow |>
      Post.bind_opt man @@ fun eold flow ->

      let cold = PrimedCell.from_expr eold in

      man.eval ~zone:(Z_c, Z_under Z_c_cell) enew flow |>
      Post.bind_opt man @@ fun enew flow ->

      let cnew = PrimedCell.from_expr enew in

      rename_cell cold cnew stmt.srange man flow |>
      Post.return

    | S_stub_rename_primed(p, offsets) ->
      man.eval ~zone:(Z_c, Z_c_cell_expand) p flow |>
      Post.bind_opt man @@ fun p flow ->

      man.eval ~zone:(Z_c_cell, Z_c_points_to) p flow |>
      Post.bind_opt man @@ fun pe flow ->

      rename_stub_primed_cells pe offsets (under_type p.etyp) stmt.srange man flow  |>
      Post.return

    | _ -> None


  (** Query handlers *)
  (** ============== *)


  let ask _ _ _ = None

end


let () =
  Framework.Domains.Stacked.register_domain (module Domain);
