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


(** {2 Zoning of expanded cells} *)
(** **************************** *)

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

  (** {2 Abstract elements} *)
  (** ===================== *)

  (** This domain maintains the set of previously created cells and
      the bases of dimensions that are present in the underlying
      partial maps.

      The set of bases is necessary for unification because the domain
      may not keep all cells previously created.
  *)

  (** Set of previously created cells. *)
  module Cells = Framework.Lattices.Powerset.Make(Cell)

  (** Bases of underlying dimensions *)
  module Bases = Framework.Lattices.Powerset.Make(Base)

  (** Abstract element *)
  type t = {
    cells: Cells.t;
    bases: Bases.t;
  }

  (** Bottom values are not useful here *)
  let bottom = {
    cells = Cells.bottom;
    bases = Bases.bottom;
  }

  let is_bottom _ = false

  let top = {
    cells = Cells.top;
    bases = Bases.top;
  }

  (** Empty support *)
  let empty = {
    cells = Cells.empty;
    bases = Bases.empty;
  }

  (** Pretty printer *)
  let print fmt a =
    Format.fprintf fmt "bases: @[%a@]@\ncells: @[%a@]@\n"
      Bases.print a.bases
      Cells.print a.cells


  (** {2 Domain identification} *)
  (** ========================= *)

  let name = "c.memory.cells.expand"
  let debug fmt = Debug.debug ~channel:name fmt

  type _ domain += D_c_cell_expand : t domain
  let id = D_c_cell_expand

  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_c_cell_expand -> Some Eq
    | _ -> None


  (** Command-line options *)
  (** ******************** *)

  (** Maximal number of expanded cells when dereferencing a pointer *)
  let opt_expand = ref 1

  let () =
    register_domain_option name {
      key = "-cell-expand";
      doc = " maximal number of expanded cells";
      spec = Arg.Set_int opt_expand;
      default = "1";
    }


  
  (** {2 Zoning interface} *)
  (** ==================== *)

  let exec_interface = {
    export = [Z_c];
    import = [Z_c_cell];
  }

  let eval_interface = {
    export = [
      Z_c_low_level, Z_c_cell_expand
    ];
    import = [
      (Z_c_low_level, Z_c_cell_expand);     (* for lvals *)
      (Z_c, Z_c_scalar);                    (* for base size *)
      (Z_c_scalar, Universal.Zone.Z_u_num); (* for offsets *)
      (Z_c, Universal.Zone.Z_u_num);        (* for quantified vars *)
      (Z_c, Z_under Z_c_cell);              (* for rvals *)
      (Z_c, Z_c_points_to);                 (* for pointers *)
    ];
  }


  (** {2 Utility functions for cells} *)
  (** =============================== *)

  (** [find_cell_opt f a] finds the cell in [a.cells] verifying
      predicate [f]. None is returned if such cells is not found. *)
  let find_cell_opt (f:cell->bool) (a:t) =
    Cells.apply (fun r ->
        let exception Found of cell in
        try
          let () = Cells.Set.iter (fun c ->
              if f c then raise (Found(c))
            ) r in
          None
        with
        | Found (c) -> Some (c)
      )
      None a.cells

  (** [find_cells f a] returns the list of cells in [a.cells] verifying
      predicate [f]. *)
  let find_cells f (a:t) =
    Cells.apply (fun r ->
        Cells.Set.filter f r |>
        Cells.Set.elements
      )
      []
      a.cells

  (** [get_overlappings c a] returns the list of cells in [a.cells]
      that overlap with [c]. *)
  let get_overlappings c a =
    find_cells (fun c' ->
        compare_cell c c' <> 0 &&
        (
          let cell_range c =
            (cell_zoffset c, Z.add (cell_zoffset c) (sizeof_type (cell_typ c)))
          in
          let check_overlap (a1, b1) (a2, b2) = Z.lt (Z.max a1 a2) (Z.min b1 b2) in
          compare_base (cell_base c) (cell_base c') = 0 &&
          check_overlap (cell_range c) (cell_range c')
        )
      ) a


  (** {2 Unification of cells} *)
  (** ======================== *)

  (** [phi c a range] returns a constraint expression over cell [c] found in [a] *)
  let phi (c:cell) (a:t) range : expr option =
    match find_cell_opt (fun c' -> compare_cell c c' = 0) a with
    | Some c ->
      debug "case 1";
      Some (mk_c_cell c range)

    | None ->
      match find_cell_opt
              (fun c' ->
                 is_c_int_type (cell_typ c') &&
                 Z.equal (sizeof_type (cell_typ c')) (sizeof_type (cell_typ c)) &&
                 compare_base (cell_base c) (cell_base c') = 0 &&
                 Z.equal (cell_zoffset c) (cell_zoffset c') &&
                 c.p = c'.p
              ) a
      with
      | Some (c') ->
        debug "case 2";
        Some (wrap_expr
                (mk_c_cell c' range)
                (int_rangeof (cell_typ c))
                range
             )
      | None ->
        match
          find_cell_opt ( fun c' ->
              let b = Z.sub (cell_zoffset c) (cell_zoffset c') in
              compare_base (cell_base c) (cell_base c') = 0 &&
              Z.lt b (sizeof_type (cell_typ c')) &&
              is_c_int_type (cell_typ c') &&
              compare_typ (remove_typedef_qual (cell_typ c)) (T_c_integer(C_unsigned_char)) = 0 &&
              c.p = c'.p
            ) a
        with
        | Some (c') ->
          debug "case 3";
          let b = Z.sub (cell_zoffset c) (cell_zoffset c') in
          let base = (Z.pow (Z.of_int 2) (8 * Z.to_int b))  in
          Some (_mod
                  (div (mk_c_cell c' range) (mk_z base range) range)
                  (mk_int 256 range)
                  range
               )

        | None ->
          let exception NotPossible in
          try
            if is_c_int_type (cell_typ c) then
              let t' = T_c_integer(C_unsigned_char) in
              let n = Z.to_int (sizeof_type ((cell_typ c))) in
              let rec aux i l =
                if i < n then
                  let tobein = (fun cc ->
                      {
                        b = cc.b;
                        o = O_single (Z.add (cell_zoffset c) (Z.of_int i));
                        t = t';
                        p = cc.p;
                      }
                    ) c
                  in
                  match find_cell_opt (fun c' -> compare_cell c' tobein = 0) a with
                  | Some (c') -> aux (i+1) (c' :: l)
                  | None -> raise NotPossible
                else
                  List.rev l
              in
              let ll = aux 0 [] in
              let _,e = List.fold_left (fun (time, res) x ->
                  let res' =
                    add
                      (mul (mk_z time range) (mk_c_cell x range) range)
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
            match cell_base c with
            | S s ->
              debug "case 5 %a" pp_cell c;
              let o = cell_zoffset c in
              let len = String.length s in
              if Z.equal o (Z.of_int len) then
                Some (mk_zero range)
              else
                Some (mk_int (String.get s (Z.to_int o) |> int_of_char) range)

            | _ ->
              if is_c_int_type (cell_typ c) then
                let () = debug "case 6" in
                let a,b = rangeof (cell_typ c) in
                Some (mk_z_interval a b range)
              else if is_c_float_type (cell_typ c) then
                let () = debug "case 7" in
                let prec = get_c_float_precision (cell_typ c) in
                Some (mk_top (T_float prec) range)
              else if is_c_pointer_type (cell_typ c) then
                panic_at range ~loc:__LOC__ "phi called on a pointer cell %a" pp_cell c
              else
                let () = debug "case 8" in
                None

  (** [constrain_cell c a range man f] add numerical constraints of [c] in [a] to flow [f] *)
  let constrain_cell c a range man f  =
    if Cells.mem c a.cells then f else
    if not (is_c_scalar_type (cell_typ c)) then f else
    if not (Bases.mem (cell_base c) a.bases) then f
    else
      let f' = man.exec ~zone:Z_c_cell (mk_add (mk_c_cell c range) range) f in
      if is_c_pointer_type ((cell_typ c)) then f'
      else
        match phi c a range with
        | Some e ->
          let stmt = mk_assume (mk_binop (mk_c_cell c range) O_eq e ~etyp:u8 range) range in
          man.exec ~zone:(Z_c_cell) stmt f'

        | None ->
          f'

  (** [add_cell c range man flow] adds a cell [c] and its numerical constraints to [flow] *)
  let add_cell c range man flow =
    debug "add_cell %a" pp_cell c;
    let a = Flow.get_domain_env T_cur man flow in
    let a' = {
      a with bases = Bases.add (cell_base c) a.bases;
    }
    in
    let flow' = constrain_cell c a' range man flow in
    let a'' = {
      a' with cells = Cells.add c a'.cells
    }
    in
    Flow.set_domain_env T_cur a'' man flow'

  (** [add_cells cells range man flow] adds a set of cells and their constraints to the [flow] *)
  let add_cells cells range man flow =
    Cells.fold (fun c flow ->
        add_cell c range man flow
      ) cells flow

  (** [add_base base man flow] adds a new base to the support *)
  let add_base base man flow =
    Flow.map_domain_env T_cur (fun a ->
        { a with bases = Bases.add base a.bases }
      ) man flow

  (** [unify a a'] finds non-common cells in [a] and [a'] and adds them. *)
  let unify (subman: ('b, 'b) man) (a:t) (s: 'b flow) (a':t) (s': 'b flow) =
    let range = mk_fresh_range () in
    if Cells.is_empty a.cells then s, s' else
    if Cells.is_empty a'.cells then s, s'
    else
      try
        let diff' = Cells.diff a.cells a'.cells in
        let diff = Cells.diff a'.cells a.cells in
        Cells.fold (fun c s ->
            constrain_cell c a range subman s
          ) diff s
        ,
        Cells.fold (fun c s' ->
            constrain_cell c a' range subman s'
          ) diff' s'
      with Top.Found_TOP ->
        Flow.top (Flow.get_all_annot s), Flow.top (Flow.get_all_annot s')


  let subset (subman: ('b, 'b) man) (u , (s: 'b flow) ) (u', (s': 'b flow)) =
    let  s, s' = unify subman u s u' s' in
    (true, s, s')

  let join annot (subman: ('b, 'b) man) (u , (s: 'b flow) ) (u', (s': 'b flow)) =
    let s, s' = unify subman u s u' s' in
    let a = {
      cells = Cells.join () u.cells u'.cells;
      bases = Bases.join () u.bases u'.bases;
    }
    in
    (a, s, s')

  let meet annot (subman: ('b, 'b) man) (u , (s: 'b flow) ) (u', (s': 'b flow)) =
    join annot subman (u, s) (u', s')

  let widen annot subman (u,s) (u', s') =
    let (u, s, s') = join annot subman (u,s) (u',s') in
    (u, true, s, s')



  (** {2 Domain initialization} *)
  (** ========================= *)

  let rec init_visitor man =
    Common.Init_visitor.{
      (* Initialization of scalars *)
      scalar = (fun v e range flow ->
          let c =
            match ekind v with
            | E_var(v, mode) -> {b = V v; o = O_single Z.zero; t = v.vtyp; p = false;}
            | E_c_cell (c, mode) -> c
            | _ -> assert false
          in
          let flow = add_cell c range man flow in
          match e with
          | Some e ->
            man.eval ~zone:(Z_c, Z_under Z_c_cell) e flow |>
            Post.bind_flow man @@ fun e flow ->

            let stmt = mk_assign (mk_c_cell c range) e range in
            man.exec ~zone:Z_c_cell stmt flow

          | None ->
            flow
        );

      (* Initialization of arrays *)
      array =  (fun a is_global init_list range flow ->
          let c =
            match ekind a with
            | E_var(v, mode) -> {b = V v; o = O_single Z.zero; t = v.vtyp; p = false;}
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
                let ci = {b = c.b; o = O_single Z.((cell_zoffset c) + (Z.of_int i) * (sizeof_type t')); t = t'; p = false;} in
                let flow' = init_expr (init_visitor man) (mk_c_cell ci range) is_global init range flow in
                aux (i + 1) tl flow'
          in
          aux 0 init_list flow
        );

      (* Initialization of structs *)
      record =  (fun s is_global init_list range flow ->
          let c =
            match ekind s with
            | E_var (v, _) -> {b = V v; o = O_single Z.zero; t = v.vtyp; p = false;}
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
                let cf = {b = c.b; o = O_single Z.((cell_zoffset c) + (Z.of_int field.c_field_offset)); t = t'; p = false;} in
                let flow' = init_expr (init_visitor man) (mk_c_cell cf ~mode:STRONG range) is_global init range flow in
                aux (i + 1) tl flow'
            in
            aux 0 l flow

          | Expr e -> panic "expand: record assignment not supported"
        );
    }

  let init prog man flow =
    Some (
      Flow.set_domain_cur empty man flow |>
      Flow.without_callbacks
    )


  (** {2 Evaluation of offsets} *)
  (** ========================= *)

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


  (** {2 Evaluation of cells} *)
  (** ======================= *)

  (** Evaluate a base and an non-quantified offset expression into a cell *)
  let eval_cell b o t range man flow : ('a, cell) evl =
    eval_base_size b range man flow |>
    Eval.bind @@ fun base_size flow ->

    eval_cell_offset o (sizeof_type t) base_size man flow |>
    Eval.bind @@ fun o flow ->

    match o with
    | O_single _ | O_region _ ->
      Eval.singleton {b; o; t; p = false} flow

    | O_out_of_bound ->
      let flow' = raise_alarm Alarms.AOutOfBound range ~bottom:true man flow in
      Eval.empty_singleton flow'


  (** {2 Evaluation of quantified cells} *)
  (** ================================== *)

  (** Return ∀ variables present in an expression *)
  let get_forall_vars e =
    Visitor.fold_expr
        (fun acc exp ->
           match ekind exp with
           | E_stub_quantified(EXISTS, var, set) ->
             panic_at exp.erange ~loc:__LOC__
               "%a not translated into %a"
               pp_expr exp pp_var var

           | E_stub_quantified(FORALL, var, S_interval(l, u)) ->
             Keep ((var, l, u) :: acc)

           | _ -> VisitParts (acc)
        )
        (fun acc stmt -> VisitParts (acc))
        [] e

  (** Compute the interval of a C expression *)
  let compute_bound e man flow =
    let evl = man.eval ~zone:(Z_c, Universal.Zone.Z_u_num) e flow in
    Eval.substitute (fun ee flow ->
        man.ask (Itv.Q_interval ee) flow
      ) (Itv.join ()) (Itv.meet ()) Itv.bottom evl

  (** Under-approximate an interval range *)
  let get_variation_under_approx itv1 itv2 =
    if Itv.is_bounded itv1 && Itv.is_bounded itv2 then
      let (a,b) = Itv.bounds itv1 in
      let (c,d) = Itv.bounds itv2 in
      if Z.leq b c then Itv.of_z b c
      else Itv.bottom
    else
      Itv.bottom

  (** Over-approximate an interval range *)
  let get_variation_over_approx itv1 itv2 =
    Itv.join () itv1 itv2

  (** Compute the variation space of a ∀ variable *)
  let compute_variation v l u man flow =
    let itv1 = compute_bound l man flow in
    let itv2 = compute_bound u man flow in
    let under = get_variation_under_approx itv1 itv2 in
    let over  = get_variation_over_approx itv1 itv2 in
    under, over

  let is_variation_empty (v, under, over) =
    Itv.is_bottom over

  let is_variation_feasible (v, under, over) =
    not (Itv.is_bottom under)

  let rec get_samples space =
    match space with
    | [] -> [[]]
    | (var, under, upper) :: tl ->
      let l, u = Itv.bounds under in
      let after = get_samples tl in

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

  (** Evaluate a base and a quantified offset expression into a cell

      Only ∀ variables are processed, since ∃ variables are evaluated
      into classic variables.

      We take a finite number of samples within the under-approximation
      of the variable range. For each sample, we replace the variable
      by its literal value, and we compute a conjunction of the resulting
      evaluations.
  *)
  let eval_quantified_cell b o t range man flow : ('a, cell) evl =
    debug "eval_quantified_cell: %a %a" pp_base b pp_expr o;
    (* Get the list of ∀ variables *)
    let forall_vars = get_forall_vars o in

    (* Compute the variation space of ∀ variables *)
    let space = List.map (fun (v, l, u) ->
        let under, upper = compute_variation v l u man flow in
        v, under, upper
      ) forall_vars
    in

    (* Check consistency *)
    if List.exists (fun s ->
        is_variation_empty s ||
        not (is_variation_feasible s)
      ) space
    then
      Eval.empty_singleton flow
    else
      (* Compute some samples from the space of under-approximations *)
      let samples = get_samples space in
      (* Each sample vector is an evaluation of the values of
         quantified variables. *)
      let conj =
        samples |>
        List.map (fun sample ->
            (* For each vector, we substitute the quantified variable
               with the its value. *)
            let o' = Visitor.map_expr
                (fun e ->
                   match ekind e with
                   | E_stub_quantified(FORALL, var, _) ->
                     let _, i = List.find (fun (var', _) ->
                         compare_var var var' = 0
                       ) sample
                     in
                     Keep { e with ekind = E_constant (C_int i) }

                   | _ -> VisitParts e
                )
                (fun s -> VisitParts s)
                o
            in
            (* Compute the cell for this sample. *)
            debug "offset sample: %a" pp_expr o';
            eval_cell b o' t range man flow
          )
      in
      (* At the end, meet all evaluations. *)
      Eval.meet_list conj




  (** Evaluate a scalar lval into a cell *)
  let rec eval_scalar_cell exp man flow : ('a, cell) evl =
    match ekind exp with
    | E_var (v, _) when is_c_scalar_type v.vtyp ->
      let c = { b = V v; o = O_single Z.zero; t = vtyp v; p = false; } in
      Eval.singleton c flow

    | E_c_deref p ->
      let t = under_type p.etyp in

      man.eval ~zone:(Z_c, Z_c_points_to) ~via:Z_c_cell_expand p flow |>
      Eval.bind @@ fun pe flow ->

      begin match ekind pe with
        | E_c_points_to(P_block (Common.Base.Z, o)) ->
          panic_at exp.erange ~loc:__LOC__ "dereference of absolute pointers not supported"

        | E_c_points_to(P_block (b, o)) when is_expr_quantified o ->
          eval_quantified_cell b o t p.erange man flow

        | E_c_points_to(P_block (b, o)) ->
          eval_cell b o t p.erange man flow

        | E_c_points_to(P_null) ->
          let flow' = raise_alarm Alarms.ANullDeref p.erange ~bottom:true man flow in
          Eval.empty_singleton flow'

        | E_c_points_to(P_invalid) ->
          let flow' = raise_alarm Alarms.AInvalidDeref p.erange ~bottom:true man flow in
          Eval.empty_singleton flow'

        | _ -> panic_at exp.erange "eval_scalar_cell: invalid pointer %a" pp_expr p;
      end

    | _ -> panic_at exp.erange ~loc:__LOC__ "eval_scalar_cell called on a non-scalar expression %a" pp_expr exp



  (** Entry-point of evaluations *)
  let eval zone exp man flow =
    match ekind exp with
    (* 𝔼⟦ v | *p ⟧ *)
    | E_var _
    | E_c_deref _
      when is_c_scalar_type exp.etyp
      ->
      eval_scalar_cell exp man flow |>
      Eval.bind_return @@ fun c flow ->
      debug "scalar cell evaluated into %a" pp_cell c;
      begin match cell_offset c with
        | O_single _ ->
          let flow = add_cell c exp.erange man flow in
          Eval.singleton (mk_c_cell c exp.erange) flow

        | O_region _ ->
          let l, u = rangeof (cell_typ c) in
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

    | E_stub_primed (e) ->
      eval_scalar_cell e man flow |>
      Eval.bind_return @@ fun c flow ->
      let c' = { c with p = true } in
      debug "primed scalar cell evaluated into %a" pp_cell c';
      begin match cell_offset c with
        | O_single _ ->
          let flow = add_cell c' exp.erange man flow in
          Eval.singleton (mk_c_cell c' exp.erange) flow

        | O_region _ ->
          let l, u = rangeof (cell_typ c) in
          Eval.singleton (mk_z_interval l u ~typ:exp.etyp exp.erange) flow

        | _ -> assert false
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
          let exp' =
            (* Pointer to void => return size in bytes *)
            if t = T_c_void then base_size
            else div base_size (of_z (sizeof_type t) exp.erange) exp.erange
          in
          Eval.singleton exp' flow

        | _ -> panic_at exp.erange "cells.expand: size(%a) not supported" pp_expr exp
      end

    (* 𝔼⟦ valid(p) ⟧ *)
    | E_stub_builtin_call( PTR_VALID, p) ->
      man.eval ~zone:(Z_c_low_level, Z_c_cell_expand) p flow |>
      Eval.bind_return @@ fun p flow ->
      let exp' = { exp with ekind = E_stub_builtin_call( PTR_VALID, p) } in
      Eval.singleton exp' flow

    (* 𝔼⟦ ∃v ⟧ *)
    | E_stub_quantified(EXISTS, var, set) when var.vtyp |> is_c_scalar_type ->
      let c = { b = V var; o = O_single Z.zero; t = var.vtyp; p = false } in
      Eval.singleton (mk_c_cell c exp.erange) flow |>
      Eval.return

    | _ -> None



  (** Computation of post-conditions *)
  (** ============================== *)

  (** Assign an rval to a cell *)
  let assign_cell c rval range man flow =
    let lval = mk_c_cell c range in
    let flow' = Flow.map_domain_env T_cur (fun a -> { a with cells = Cells.add c a.cells}) man flow |>
                man.exec ~zone:Z_c_cell (mk_assign lval rval range)
    in

    let a = Flow.get_domain_env T_cur man flow' in
    let overlappings = get_overlappings c a in

    let a' =
      overlappings |>
      List.fold_left (fun a c' -> {a with cells = Cells.remove c' a.cells}) a
    in

    let flow'' = Flow.set_domain_env T_cur a' man flow' in

    let block = List.map (fun c' -> mk_remove (mk_c_cell c' range) range) overlappings in

    man.exec ~zone:Z_c_cell (mk_block block range) flow''



  (** Remove cells identified by a membership predicate *)
  let remove_cells pred range man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let cells = find_cells pred a in
    let block = List.map (fun c' -> mk_remove (mk_c_cell c' range) range) cells in
    man.exec ~zone:Z_c_cell (mk_block block range) flow



  (** Rename an old cell into a new one *)
  let rename_cell cold cnew range man flow =
    debug "rename %a into %a" pp_cell cold pp_cell cnew;
    let flow' =
      (* Add the old cell in case it has not been accessed before so
         that its constraints are added in the sub domain *)
      add_cell cold range man flow |>
      (* Remove the old cell and add the new one *)
      Flow.map_domain_env T_cur (fun a ->
          { a with cells = Cells.remove cold a.cells |>
                           Cells.add cnew
          }
        ) man
    in
    let stmt' = mk_rename
        (mk_c_cell cold range)
        (mk_c_cell cnew range)
        range
    in
    man.exec ~zone:Z_c_cell stmt' flow'


  (** Rename primed cells that have been declared in `assigns` stub section *)
  let rename_primed_cells target offsets range man flow =
    match offsets with
    | [] ->
      (* target should be a scalar lval *)
      eval_scalar_cell target man flow |>
      Post.bind_flow man @@ fun c flow ->
      rename_cell { c with p = true } c range man flow

    | _ ->
      (* target is pointer, so resolve it and compute the affected offsets *)
      man.eval ~zone:(Z_c, Z_c_points_to) target flow |>
      Post.bind_flow man @@ fun pt flow ->
      let base, offset =
        match ekind pt with
        | E_c_points_to (P_block(b, o)) -> b, o
        | _ -> assert false
      in

      let a = Flow.get_domain_env T_cur man flow in

      (* Get cells with the same base *)
      let same_base_cells = Cells.filter (fun c ->
          compare_base base (cell_base c) = 0
        ) a.cells
      in

      (* For primed cells, just rename to an unprimed cell *)
      let flow = Cells.fold (fun c flow ->
          if c.p
          then rename_cell c { c with p = false } range man flow
          else flow
        ) same_base_cells flow
      in

      (* Create the bound expressions of the offsets *)
      let l, u =
        let rec doit accl accu t =
          function
          | [] -> accl, accu
          | [(l, u)] ->
            (mk_offset_bound accl l t), (mk_offset_bound accu u t)
          | (l, u) :: tl ->
            doit (mk_offset_bound accl l t) (mk_offset_bound accu u t) (under_type t) tl

        (* Utility function that returns the expression of an offset bound *)
        and mk_offset_bound before bound t =
          let elem_size = sizeof_type t in
          Universal.Ast.add before (
            mul bound (mk_z elem_size range) range ~typ:T_int
          ) range ~typ:T_int
        in
        doit offset offset (under_type target.etyp) offsets
      in
      debug "l = %a, u = %a" pp_expr l pp_expr u;

      (* Compute the interval of the bounds *)
      let itv1 = compute_bound l man flow in
      let itv2 = compute_bound u man flow in      

      (* Compute the interval of the assigned cells *)
      let itv = Itv.join () itv1 itv2 in

      (* Remove remaining cells that have an offset within the assigned interval *)
      remove_cells (fun c ->
          Cells.mem c same_base_cells &&
          not (Cells.mem {c with p = true} same_base_cells) &&
          Itv.mem (cell_zoffset c) itv
        ) range man flow


  (** Entry point of post-condition computation *)
  let exec zone stmt man flow =
    match skind stmt with
    (* 𝕊⟦ t v = e; ⟧ when v is a global variable *)
    | S_c_declaration({vkind = V_c {var_scope = Variable_extern
                                               | Variable_global
                                               | Variable_file_static _
                                               | Variable_func_static _;
                                     var_init = init}} as v)
      ->
      add_base (V v) man flow |>
      Common.Init_visitor.init_global (init_visitor man) v init stmt.srange |>
      Post.return

    (* 𝕊⟦ t v = e; ⟧ when v is a local variable *)
    | S_c_declaration({vkind = V_c {var_scope = Variable_local _; var_init = init}} as v) ->
      add_base (V v) man flow |>
      Common.Init_visitor.init_local (init_visitor man) v init stmt.srange |>
      Post.return

    (* 𝕊⟦ add v ⟧ when v is a scalar *)
    | S_add { ekind = E_var (v, _) } when is_c_scalar_type v.vtyp ->
      eval_scalar_cell (mk_var v stmt.srange) man flow |>
      Post.bind_return man @@ fun c flow ->

      add_cell c stmt.srange man flow |>
      man.exec ~zone:Z_c_cell (mk_add (mk_c_cell c stmt.srange) stmt.srange) |>

      Post.of_flow |>
      Post.add_merger (mk_add (mk_c_cell c stmt.srange) stmt.srange)

    (* 𝕊⟦ add v ⟧ when v is not a scalar *)
    | S_add { ekind = E_var (v, _) } when not (is_c_scalar_type v.vtyp) ->
      add_base (V v) man flow |>
      Post.return

    (* 𝕊⟦ remove v ⟧ *)
    | S_remove { ekind = E_var (v, _) } when is_c_type v.vtyp ->
      let u = Flow.get_domain_cur man flow in
      let l = find_cells (fun c -> compare_base (cell_base c) (V v) = 0) u in
      let u = List.fold_left (fun u c -> { u with cells = Cells.remove c u.cells }) u l in
      let u = { u with bases = Bases.remove (V v) u.bases } in
      let mergers = List.map (fun c -> mk_remove (mk_c_cell c stmt.srange) stmt.srange) l in
      let to_exec_in_sub = mergers in
      let flow = Flow.set_domain_cur u man flow in
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
        match cell_offset c with
        | O_single _ ->
          assign_cell c rval stmt.srange man flow

        | O_region itv ->
          remove_cells (fun c' ->
              compare_base (cell_base c) (cell_base c') = 0 &&
              Itv.mem (cell_zoffset c') itv
            ) stmt.srange man flow

        | _ -> panic_at stmt.srange "expand: lval cell %a not supported in assignments" pp_cell c
      in

      Post.return flow

    (* 𝕊⟦ assume ?e ⟧ *)
    | S_assume(e) ->
      man.eval ~zone:(Z_c, Z_under Z_c_cell) e flow |>
      Post.bind_opt man @@ fun e' flow ->

      let stmt' = {stmt with skind = S_assume e'} in
      man.exec ~zone:Z_c_cell stmt' flow |>

      Post.return

    | S_stub_rename_primed(p, offsets) ->
      rename_primed_cells p offsets stmt.srange man flow  |>
      Post.return

    (* 𝕊⟦ rename(@1, @2) ⟧ *)
    | S_rename({ ekind = E_addr addr1 }, { ekind = E_addr addr2 }) ->
      (* For each cell in base @1, create a similar one in base @2 and
         copy its content *)
      let a = Flow.get_domain_env T_cur man flow in
      let flow = Cells.fold (fun c flow ->
          let base = cell_base c in
          if compare_base base (A addr1) != 0 then
            flow
          else
            (* create a similar cell in @2 *)
            let c' = {
              b = A addr2;
              o = cell_offset c;
              t = cell_typ c;
              p = is_cell_primed c;
            }
            in
            rename_cell c c' stmt.srange man flow
        ) a.cells flow
      in
      Post.return flow

    | _ -> None


  (** Query handlers *)
  (** ============== *)


  let ask _ _ _ = None

end


let () =
  Framework.Domains.Stacked.register_domain (module Domain);
