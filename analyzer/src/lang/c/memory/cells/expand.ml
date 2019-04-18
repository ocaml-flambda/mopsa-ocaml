(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
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
module Itv = Universal.Numeric.Values.Intervals.Integer.Value


(** {2 Zoning of expanded cells} *)
(** **************************** *)

type zone +=
  | Z_c_cell_expand

let () =
  register_zone {
    zone = Z_c_cell_expand;
    zone_name = "C/Cell/Expand";
    zone_subset = Some Z_c_cell;
    zone_eval = (fun exp -> Core.Zone.eval_template exp Z_c_cell);
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

  include GenDomainId(struct
      type typ = t
      let name = "c.memory.cells.expand"
    end)


  (** Command-line options *)
  (** ******************** *)

  (** Maximal number of expanded cells when dereferencing a pointer *)
  let opt_expand = ref 1

  let () =
    register_domain_option name {
      key = "-cell-expand";
      category = "C";
      doc = " maximal number of expanded cells";
      spec = ArgExt.Set_int opt_expand;
      default = "1";
    }


  (** {2 Zoning interface} *)
  (** ==================== *)

  let interface = {
    iexec = {
      provides = [Z_c];
      uses = [Z_c_cell];
    };

    ieval = {
      provides = [Z_c_low_level, Z_c_cell_expand];
      uses = [
        (Z_c_low_level, Z_c_cell_expand);     (* for lvals *)
        (Z_c, Z_c_scalar);                    (* for base size *)
        (Z_c_scalar, Universal.Zone.Z_u_num); (* for offsets *)
        (Z_c, Universal.Zone.Z_u_num);        (* for quantified vars *)
        (Z_c, Z_under Z_c_cell);              (* for rvals *)
        (Z_c, Z_c_points_to);                 (* for pointers *)
        (Z_c_cell, Z_c_scalar);               (* for Q_print_var query *)
        (Z_c_scalar, Z_c_points_to);          (* for Q_print_var query *)
      ];
    }
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
        Some (wrap_expr
                (mk_c_cell c' range)
                (int_rangeof (cell_typ c))
                range
             )
      | None ->
        match
          find_cell_opt ( fun c' ->
              let b = Z.sub (cell_zoffset c) (cell_zoffset c') in
              Z.geq b Z.zero &&
              compare_base (cell_base c) (cell_base c') = 0 &&
              Z.lt b (sizeof_type (cell_typ c')) &&
              is_c_int_type (cell_typ c') &&
              compare_typ (remove_typedef_qual (cell_typ c)) (T_c_integer(C_unsigned_char)) = 0 &&
              c.p = c'.p
            ) a
        with
        | Some (c') ->
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
              Some e
            else
              raise NotPossible
          with
          | NotPossible ->
            match cell_base c with
            | S s ->
              let o = cell_zoffset c in
              let len = String.length s in
              if Z.equal o (Z.of_int len) then
                Some (mk_zero range)
              else
                Some (mk_int (String.get s (Z.to_int o) |> int_of_char) range)

            | _ ->
              if is_c_int_type (cell_typ c) then
                let a,b = rangeof (cell_typ c) in
                Some (mk_z_interval a b range)
              else if is_c_float_type (cell_typ c) then
                let prec = get_c_float_precision (cell_typ c) in
                Some (mk_top (T_float prec) range)
              else if is_c_pointer_type (cell_typ c) then
                panic_at range ~loc:__LOC__ "phi called on a pointer cell %a" pp_cell c
              else
                None

  (** [constrain_cell c a range man f] add numerical constraints of [c] in [a] to flow [f] *)
  let constrain_cell_in_sub c a range (man:'s sman) (s:'s) : 's  =
    if Cells.mem c a.cells ||
       not (is_c_scalar_type (cell_typ c)) ||
       not (Bases.mem (cell_base c) a.bases)
    then s
    else
      let s' = man.sexec ~zone:Z_c_cell (mk_add (mk_c_cell c range) range) s in
      if is_c_pointer_type ((cell_typ c)) then s'
      else
        match phi c a range with
        | Some e ->
          let stmt = mk_assume (mk_binop (mk_c_cell c range) O_eq e ~etyp:u8 range) range in
          man.sexec ~zone:(Z_c_cell) stmt s'

        | None ->
          s'

  let constrain_cell_in_flow c a range man flow =
    if Cells.mem c a.cells ||
       not (is_c_scalar_type (cell_typ c)) ||
       not (Bases.mem (cell_base c) a.bases)
    then flow
    else
      let flow' = man.exec ~zone:Z_c_cell (mk_add (mk_c_cell c range) range) flow in
      if is_c_pointer_type ((cell_typ c)) then flow'
      else
        match phi c a range with
        | Some e ->
          let stmt = mk_assume (mk_binop (mk_c_cell c range) O_eq e ~etyp:u8 range) range in
          man.exec ~zone:(Z_c_cell) stmt flow'

        | None ->
          flow'

  (** [add_cell c range man flow] adds a cell [c] and its numerical constraints to [flow] *)
  let add_cell c range (man:('a,t) man) flow =
    let a = get_domain_env T_cur man flow in
    let a' = {
      a with bases = Bases.add (cell_base c) a.bases;
    }
    in
    let flow' = constrain_cell_in_flow c a' range man flow in
    let a'' = {
      a' with cells = Cells.add c a'.cells
    }
    in
    set_domain_env T_cur a'' man flow'

  (** [add_cells cells range man flow] adds a set of cells and their constraints to the [flow] *)
  let add_cells cells range man flow =
    Cells.fold (fun c flow ->
        add_cell c range man flow
      ) cells flow

  (** [add_base base man flow] adds a new base to the support *)
  let add_base base man stman flow =
    map_domain_env T_cur (fun a ->
        { a with bases = Bases.add base a.bases }
      ) man flow

  (** [unify a a'] finds non-common cells in [a] and [a'] and adds them. *)
  let unify (sbman: 's sman) (a:t) (s: 's) (a':t) (s': 's) =
    let range = mk_fresh_range () in
    if Cells.is_empty a.cells then s, s' else
    if Cells.is_empty a'.cells then s, s'
    else
      try
        let diff' = Cells.diff a.cells a'.cells in
        let diff = Cells.diff a'.cells a.cells in
        Cells.fold (fun c s ->
            constrain_cell_in_sub c a range sbman s
          ) diff s
        ,
        Cells.fold (fun c s' ->
            constrain_cell_in_sub c a' range sbman s'
          ) diff' s'
      with Top.Found_TOP ->
        s, s'


  let subset (sbman: 's sman) (u,s) (u',s') =
    let  s, s' = unify sbman u s u' s' in
    (true, s, s')

  let join (sbman: 's sman) ((u:t),(s:'s)) ((u':t),(s':'s)) =
    let s, s' = unify sbman u s u' s' in
    let a = {
      cells = Cells.join u.cells u'.cells;
      bases = Bases.join u.bases u'.bases;
    }
    in
    (a, s, s')

  let meet (sbman: 's sman) ((u:t),(s:'s)) ((u':t),(s':'s)) =
    join sbman (u, s) (u', s')

  let widen (sbman: 's sman) ctx ((u:t),(s:'s)) ((u':t),(s':'s)) =
    let (u, s, s') = join sbman  (u,s) (u',s') in
    (u, s, s', true)

  let merge pre (post1,log1) (post2,log2) =
    assert false


  (** {2 Domain initialization} *)
  (** ========================= *)

  let rec init_visitor man stman =
    Common.Init_visitor.{
      (* Initialization of scalars *)
      scalar = (fun v e range flow ->
          let c =
            match ekind v with
            | E_var(v, mode) -> {b = V v; o = O_single Z.zero; t = remove_qual v.vtyp; p = false;}
            | E_c_cell (c, mode) -> c
            | _ -> assert false
          in
          let flow = add_cell c range man flow in
          match e with
          | Some e ->
            man.eval ~zone:(Z_c, Z_under Z_c_cell) e flow |>
            exec_eval man @@ fun e flow ->

            let stmt = mk_assign (mk_c_cell c range) e range in
            man.exec ~zone:Z_c_cell stmt flow

          | None -> flow
        );

      (* Initialization of arrays *)
      array =  (fun a is_global init_list range flow ->
          let c =
            match ekind a with
            | E_var(v, mode) -> {b = V v; o = O_single Z.zero; t = remove_qual v.vtyp; p = false;}
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
                let ci = {b = c.b; o = O_single Z.((cell_zoffset c) + (Z.of_int i) * (sizeof_type t')); t = remove_qual t'; p = false;} in
                let flow' = init_expr (init_visitor man stman) (mk_c_cell ci range) is_global init range flow in
                aux (i + 1) tl flow'
          in
          aux 0 init_list flow
        );

      (* Initialization of structs *)
      record =  (fun s is_global init_list range flow ->
          let c =
            match ekind s with
            | E_var (v, _) -> {b = V v; o = O_single Z.zero; t = remove_qual v.vtyp; p = false;}
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
                let cf = {b = c.b; o = O_single Z.((cell_zoffset c) + (Z.of_int field.c_field_offset)); t = remove_qual t'; p = false;} in
                let flow' = init_expr (init_visitor man stman) (mk_c_cell cf ~mode:STRONG range) is_global init range flow in
                aux (i + 1) tl flow'
            in
            aux 0 l flow

          | Expr e -> panic "expand: record assignment not supported"
        );
    }

  let init prog man flow =
    set_domain_env T_cur empty man flow


  (** {2 Evaluation of offsets} *)
  (** ========================= *)

  (** Evaluate an offset expression into an offset evaluation *)
  let eval_cell_offset (offset:expr) cell_size base_size man flow : (offset,'a) eval =
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
      assume_eval ~zone:Universal.Zone.Z_u_num cond
        ~fthen:(fun flow ->
            (* Compute the interval and create a finite number of cells *)
            let v = man.ask (Itv.Q_interval offset) flow in
            let itv = v and step = Z.one in

            (* Iterate in case of bounded interval *)
            if Itv.is_bounded itv then
              let l, u = Itv.bounds itv in
              let rec aux i o =
                if Z.gt o u
                then []
                else if i >= !opt_expand
                then
                  let flow' = man.exec ~zone:Universal.Zone.Z_u_num (mk_assume (mk_binop offset O_ge (mk_z o range) range) range) flow in
                  [Eval.singleton (O_region (Itv.of_constant (C_int_interval (o, u)))) flow']
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
  let eval_cell b o t range man flow : (cell,'a) eval =
    eval_base_size b range man flow |>
    Eval.bind @@ fun base_size flow ->

    eval_cell_offset o (sizeof_type t) base_size man flow |>
    Eval.bind @@ fun o flow ->

    match o with
    | O_single _ | O_region _ ->
      Eval.singleton {b; o; t = remove_qual t; p = false} flow

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
    Eval.apply
      (fun ee flow ->
        man.ask (Itv.Q_interval ee) flow
      )
      Itv.join Itv.meet Itv.bottom
      evl

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
    Itv.join itv1 itv2

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
  let eval_quantified_cell b o t range man flow : (cell,'a) eval =
    (* Get the list of ∀ variables *)
    let forall_vars = get_forall_vars o in

    (* Compute the variation space of ∀ variables *)
    let space = List.map (fun (v, l, u) ->
        let under, upper = compute_variation v l u man flow in
        v, under, upper
      ) forall_vars
    in

    (* Check consistency *)
    if List.exists is_variation_empty space then
      Eval.empty_singleton flow
    else
    if List.exists (fun s -> not (is_variation_feasible s)) space then
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
            eval_cell b o' t range man flow
          )
      in
      (* At the end, meet all evaluations. *)
      Eval.meet_list conj




  (** Evaluate a scalar lval into an optional cell *)
  let eval_scalar_cell_opt exp man flow : (cell option,'a) eval =
    match ekind exp with
    | E_var (v, _) when is_c_scalar_type v.vtyp ->
      let c = { b = V v; o = O_single Z.zero; t = remove_qual v.vtyp; p = false; } in
      Eval.singleton (Some c) flow

    | E_c_deref p ->
      let t = under_type p.etyp in

      man.eval ~zone:(Z_c, Z_c_points_to) ~via:Z_c_cell_expand p flow |>
      Eval.bind @@ fun pe flow ->

      begin match ekind pe with
        | E_c_points_to(P_block (Common.Base.Z, o)) ->
          panic_at exp.erange ~loc:__LOC__ "dereference of absolute pointers not supported"

        | E_c_points_to(P_block (b, o)) when is_expr_quantified o ->
          eval_quantified_cell b o t p.erange man flow |>
          Eval.bind @@ fun c flow ->
          Eval.singleton (Some c) flow

        | E_c_points_to(P_block (b, o)) ->
          eval_cell b o t p.erange man flow |>
          Eval.bind @@ fun c flow ->
          Eval.singleton (Some c) flow

        | E_c_points_to(P_null) ->
          let flow' = raise_alarm Alarms.ANullDeref p.erange ~bottom:true man flow in
          Eval.empty_singleton flow'

        | E_c_points_to(P_invalid) ->
          let flow' = raise_alarm Alarms.AInvalidDeref p.erange ~bottom:true man flow in
          Eval.empty_singleton flow'

        | E_c_points_to(P_top) ->
          Eval.singleton None flow

        | _ ->
          panic_at exp.erange "eval_scalar_cell: invalid pointer %a" pp_expr p
      end

    | _ -> panic_at exp.erange ~loc:__LOC__ "eval_scalar_cell called on a non-scalar expression %a" pp_expr exp


  (** Evaluate a scalar lval into a cell *)
  let eval_scalar_cell exp man flow : (cell,'a) eval =
    eval_scalar_cell_opt exp man flow |>
    Eval.bind @@ fun c flow ->
    match c with
    | Some cc -> Eval.singleton cc flow
    | None -> panic_at exp.erange ~loc:__LOC__ "%a can't be evaluated into a cell" pp_expr exp



  let cell_singleton c range man flow =
    match cell_offset c with
    | O_single _ ->
      let flow = add_cell c range man flow in
      Eval.singleton (mk_c_cell c range) flow

    | O_region _ ->
      Eval.singleton (mk_top c.t range) flow

    | O_out_of_bound ->
      assert false


  (** Entry-point of evaluations *)
  let eval zone exp (man:('a,t) man) flow =
    match ekind exp with
    (* 𝔼⟦ v ⟧ *)
    | E_var (v, _) when is_c_scalar_type v.vtyp ->
      let c = { b = V v; o = O_single Z.zero; t = remove_qual v.vtyp; p = false; } in
      cell_singleton c exp.erange man flow |>
      Option.return

    (* 𝔼⟦ *p ⟧ *)
    | E_c_deref p
      when is_c_scalar_type exp.etyp ||
           is_c_function_type exp.etyp
      ->
      let t = under_type p.etyp in

      man.eval ~zone:(Z_c, Z_c_points_to) ~via:Z_c_cell_expand p flow |>
      Eval.bind_some @@ fun pe flow ->

      begin match ekind pe with
        | E_c_points_to(P_block (Common.Base.Z, o)) ->
          panic_at exp.erange ~loc:__LOC__ "dereference of absolute pointers not supported"

        | E_c_points_to(P_block (b, o)) when is_expr_quantified o ->
          eval_quantified_cell b o t p.erange man flow |>
          Eval.bind @@ fun c flow ->
          cell_singleton c exp.erange man flow

        | E_c_points_to(P_block (b, o)) ->
          eval_cell b o t p.erange man flow  |>
          Eval.bind @@ fun c flow ->
          cell_singleton c exp.erange man flow

        | E_c_points_to(P_null) ->
          raise_alarm Alarms.ANullDeref p.erange ~bottom:true man flow |>
          Eval.empty_singleton

        | E_c_points_to(P_invalid) ->
          raise_alarm Alarms.AInvalidDeref p.erange ~bottom:true man flow |>
          Eval.empty_singleton

        | E_c_points_to(P_top) ->
          Eval.singleton (mk_top exp.etyp exp.erange) flow

        | E_c_points_to(P_fun f) ->
          Eval.singleton { exp with ekind = E_c_function f } flow

        | _ -> assert false
      end


    | E_stub_primed (e) ->
      eval_scalar_cell_opt e man flow |>
      Eval.bind_some @@ fun c flow ->
      begin match c with
        | None -> Eval.singleton (mk_top e.etyp exp.erange) flow
        | Some c ->
          let c' = { c with p = true } in
          match cell_offset c with
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
      Eval.bind_some @@ fun pe flow ->

      begin match ekind pe with
        | E_c_points_to(P_block (b, _)) ->
          (* When p points to a block b, return sizeof(b) *)
          eval_base_size b exp.erange man flow |>
          Eval.bind @@ fun base_size flow ->

          let t = under_type p.etyp in
          let exp' =
            (* Pointer to void => return size in bytes *)
            if is_c_void_type t then base_size
            else div base_size (of_z (sizeof_type t) exp.erange) exp.erange
          in
          Eval.singleton exp' flow

        | E_c_points_to P_top ->
          (* When we have no information on pointer p, return TOP *)
          let exp' = mk_top exp.etyp exp.erange in
          Eval.singleton exp' flow

        | _ -> panic_at exp.erange "cells.expand: size(%a) not supported" pp_expr exp
      end

    (* 𝔼⟦ valid(p) ⟧ *)
    | E_stub_builtin_call(PTR_VALID, p) ->
      man.eval ~zone:(Z_c_low_level, Z_c_cell_expand) p flow |>
      Eval.bind_some @@ fun p flow ->
      let exp' = { exp with ekind = E_stub_builtin_call( PTR_VALID, p) } in
      Eval.singleton exp' flow

    (* 𝔼⟦ ∃v ⟧ *)
    | E_stub_quantified(EXISTS, var, set) when var.vtyp |> is_c_scalar_type ->
      let c = { b = V var; o = O_single Z.zero; t = remove_qual var.vtyp; p = false } in
      Eval.singleton (mk_c_cell c exp.erange) flow |>
      Option.return

    | _ -> None



  (** Computation of post-conditions *)
  (** ============================== *)

  (** Assign an rval to a cell *)
  let assign_cell c rval range man stman flow =
    let lval = mk_c_cell c range in
    let flow' = map_domain_env T_cur (fun a -> { a with cells = Cells.add c a.cells}) man flow |>
                man.exec ~zone:Z_c_cell (mk_assign lval rval range)
    in

    let a = get_domain_env T_cur man flow' in
    let overlappings = get_overlappings c a in

    let a' =
      overlappings |>
      List.fold_left (fun a c' -> {a with cells = Cells.remove c' a.cells}) a
    in

    let flow'' = set_domain_env T_cur a' man flow' in

    let block = List.map (fun c' -> mk_remove (mk_c_cell c' range) range) overlappings in
    man.exec ~zone:Z_c_cell (mk_block block range) flow''


  (** Remove cells identified by a membership predicate *)
  let remove_cells pred range man stman flow =
    let a = get_domain_env T_cur man flow in
    let cells = Cells.filter pred a.cells in
    let flow = set_domain_env T_cur { a with cells = Cells.diff a.cells cells } man flow in
    let block = List.map (fun c' -> mk_remove (mk_c_cell c' range) range) (Cells.elements cells) in
    man.exec ~zone:Z_c_cell (mk_block block range) flow

  (** Remove a cell *)
  let remove_cell c range man stman flow =
    let flow = map_domain_env T_cur (fun a ->
        {a with cells = Cells.remove c a.cells}
      ) man flow
    in
    let stmt = mk_remove (mk_c_cell c range) range in
    man.exec ~zone:Z_c_cell stmt flow


  (** Rename an old cell into a new one *)
  let rename_cell old_cell new_cell range man stman flow =
    let flow' =
      (* Add the old cell in case it has not been accessed before so
         that its constraints are added in the sub domain *)
      add_cell old_cell range man flow |>
      (* Remove the old cell and add the new one *)
      map_domain_env T_cur (fun a ->
          { a with cells = Cells.remove old_cell a.cells |>
                           Cells.add new_cell
          }
        ) man
    in
    let stmt' = mk_rename
        (mk_c_cell old_cell range)
        (mk_c_cell new_cell range)
        range
    in
    man.exec ~zone:Z_c_cell stmt' flow'

  (** Rename bases and their cells *)
  let rename_base base1 base2 range man stman flow =
    let a = get_domain_env T_cur man flow in
    (* Cells of base1 *)
    let cells1 = Cells.filter (fun c ->
        compare_base (cell_base c) base1 = 0
      ) a.cells
    in

    (* Cell renaming *)
    let to_base2 c = { c with b = base2 } in

    (* Content copy, depends on the presence of base2 *)
    let copy =
      if not (Bases.mem base2 a.bases) then
      (* If base2 is not already present => rename the cells *)
        fun c flow -> rename_cell c (to_base2 c) range man stman flow
      else
        (* Otherwise, assign with weak update *)
        fun c flow -> assign_cell (to_base2 c) (mk_c_cell c range) range man stman flow |>
                      remove_cell c range man stman
    in

    (* Apply copy function *)
    let flow = Cells.fold copy cells1 flow in

    (* Remove base1 and add base2 *)
    let flow = map_domain_env T_cur (fun a ->
        {
          a with
          bases = Bases.remove base1 a.bases |>
                  Bases.add base2;
        }
      ) man flow
    in
    flow

  (** Rename primed cells that have been declared in `assigns` stub section *)
  let rename_primed_cells target offsets range man stman flow =
    match offsets with
    | [] ->
      (* target should be a scalar lval *)
      eval_scalar_cell target man flow |>
      post_eval man @@ fun c flow ->
      rename_cell { c with p = true } c range man stman flow |>
      Post.return

    | _ ->
      (* target is pointer, so resolve it and compute the affected offsets *)
      man.eval ~zone:(Z_c, Z_c_points_to) target flow |>
      post_eval man @@ fun pt flow ->
      match ekind pt with
      | E_c_points_to (P_block(base, offset)) ->
        let a = get_domain_env T_cur man flow in

        (* Get cells with the same base *)
        let same_base_cells = Cells.filter (fun c ->
            compare_base base (cell_base c) = 0
          ) a.cells
        in

        (* For primed cells, just rename to an unprimed cell *)
        let flow = Cells.fold (fun c flow ->
            if c.p
            then rename_cell c { c with p = false } range man stman flow
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

        (* Compute the interval of the bounds *)
        let itv1 = compute_bound l man flow in
        let itv2 = compute_bound u man flow in

        (* Compute the interval of the assigned cells *)
        let itv = Itv.join itv1 itv2 in

        (* Remove remaining cells that have an offset within the assigned interval *)
        remove_cells (fun c ->
            Cells.mem c same_base_cells &&
            not (Cells.mem {c with p = true} same_base_cells) &&
            Itv.mem (cell_zoffset c) itv
          ) range man stman flow |>
        Post.return

      | E_c_points_to(P_top) ->
        Post.return flow

      | _ -> assert false



  (** Entry point of post-condition computation *)
  let exec zone stmt man stman flow =
    match skind stmt with
    (* 𝕊⟦ t v = e; ⟧ when v is a global variable *)
    | S_c_declaration({vkind = V_c {var_scope = Variable_extern
                                               | Variable_global
                                               | Variable_file_static _
                                               | Variable_func_static _;
                                     var_init = init}} as v)
      ->
      add_base (V v) man stman flow |>
      Common.Init_visitor.init_global (init_visitor man stman) v init stmt.srange |>
      Post.return |> Option.return

    (* 𝕊⟦ t v = e; ⟧ when v is a local variable *)
    | S_c_declaration({vkind = V_c {var_scope = Variable_local _; var_init = init}} as v) ->
      add_base (V v) man stman flow |>
      Common.Init_visitor.init_local (init_visitor man stman) v init stmt.srange |>
      Post.return |> Option.return

    (* 𝕊⟦ add v ⟧ when v is a scalar *)
    | S_add { ekind = E_var (v, _) } when is_c_scalar_type v.vtyp ->
      eval_scalar_cell (mk_var v stmt.srange) man flow |>
      Option.return |> Option.lift @@ post_eval man @@
      fun c flow ->

      add_cell c stmt.srange man flow |>
      man.exec ~zone:Z_c_cell (mk_add (mk_c_cell c stmt.srange) stmt.srange) |>
      Post.return

    (* 𝕊⟦ add v ⟧ when v is not a scalar *)
    | S_add { ekind = E_var (v, _) } when not (is_c_scalar_type v.vtyp) ->
      add_base (V v) man stman flow |>
      Post.return |> Option.return

    (* 𝕊⟦ remove v ⟧ *)
    | S_remove { ekind = E_var (v, _) } when is_c_type v.vtyp ->
      let u = get_domain_env T_cur man flow in
      let l = find_cells (fun c -> compare_base (cell_base c) (V v) = 0) u in
      let u = List.fold_left (fun u c -> { u with cells = Cells.remove c u.cells }) u l in
      let u = { u with bases = Bases.remove (V v) u.bases } in
      let to_exec_in_sub = List.map (fun c -> mk_remove (mk_c_cell c stmt.srange) stmt.srange) l in
      let flow = set_domain_env T_cur u man flow in
      man.exec ~zone:Z_c_cell (mk_block to_exec_in_sub stmt.srange) flow |>
      Post.return |> Option.return

    (* 𝕊⟦ lval = rval ⟧ *)
    | S_assign(lval, rval) when is_c_scalar_type lval.etyp ->
      man.eval ~zone:(Z_c, Z_under Z_c_cell) rval flow |>
      Option.return |> Option.lift @@ post_eval man @@ fun rval flow ->

      man.eval ~zone:(Z_c, Z_c_low_level) lval flow |>
      post_eval man @@ fun lval flow ->

      eval_scalar_cell lval man flow |>
      post_eval man @@ fun c flow ->

      let flow =
        match cell_offset c with
        | O_single _ ->
          assign_cell c rval stmt.srange man stman flow

        | O_region itv ->
          remove_cells (fun c' ->
              compare_base (cell_base c) (cell_base c') = 0 &&
              Itv.mem (cell_zoffset c') itv
            ) stmt.srange man stman flow

        | _ -> panic_at stmt.srange "expand: lval cell %a not supported in assignments" pp_cell c
      in

      Post.return flow

    (* 𝕊⟦ assume ?e ⟧ *)
    | S_assume(e) ->
      man.eval ~zone:(Z_c, Z_under Z_c_cell) e flow |>
      Option.return |> Option.lift @@ post_eval man @@ fun e' flow ->

      let stmt' = {stmt with skind = S_assume e'} in
      man.exec ~zone:Z_c_cell stmt' flow |>

      Post.return

    (* 𝕊⟦ rename(v1, v2) ⟧ *)
    | S_rename({ ekind = E_var (v1, _) }, { ekind = E_var (v2, _) }) ->
      rename_base (V v1) (V v2) stmt.srange man stman flow |>
      Post.return |> Option.return

    (* 𝕊⟦ rename(addr1, addr2) ⟧ *)
    | S_rename({ ekind = E_addr addr1 }, { ekind = E_addr addr2 }) ->
      rename_base (A addr1) (A addr2) stmt.srange man stman flow |>
      man.exec ~zone:Z_c_scalar stmt |>
      Post.return |> Option.return

    (* 𝕊⟦ rename primed p[a0, b0][a1, b1]... ⟧ *)
    | S_stub_rename_primed(p, offsets) ->
      rename_primed_cells p offsets stmt.srange man stman flow |>
      Option.return

    | _ -> None


  (** Query handlers *)
  (** ============== *)

  let ask : type r. r Query.query -> ('a, t) man -> 'a flow -> r option = fun query man flow ->
    match query with
    | Framework.Engines.Interactive.Q_print_var ->
      Some (
        fun fmt v ->
          let a = get_domain_env T_cur man flow in
          (* Get the cells in variable v *)
          let cells = find_cells (fun c ->
              match cell_base c with
              | V vv -> vv.org_vname = v
              | _ -> false
            ) a
          in

          (* Process each cell depending on its type *)
          let range = mk_fresh_range () in
          let ret = ref [] in
          let open Format in
          cells |> List.iter (fun c ->
              (* Evaluate cell c into a scalar *)
              man.eval
                (mk_c_cell c range) ~zone:(Z_c_cell, Z_c_scalar) flow
              |>
              Eval.iter (fun e flow ->
                  if e.etyp |> is_c_num_type then
                    (* For numeric cells, get the interval and print it *)
                    man.eval e ~zone:(Z_c_scalar, Universal.Zone.Z_u_num) flow |>
                    Eval.iter (fun ee flow ->
                        let itv = man.ask (Itv.Q_interval ee) flow in
                        ret := (fun fmt -> fprintf fmt "%a = %a" pp_expr e Itv.print itv) :: !ret
                      )
                  else
                  if e.etyp |> is_c_pointer_type then
                    (* For pointer cells, get pointed bases and offsets *)
                    man.eval e ~zone:(Z_c_scalar, Z_c_points_to) flow |>
                    Eval.iter (fun p flow ->
                        match ekind p with
                        | E_c_points_to(P_block(base, offset)) ->
                          let itv = man.ask (Itv.Q_interval offset) flow in
                          ret := (fun fmt ->
                              fprintf fmt "%a ⇝ %a%a" pp_expr e pp_base base Itv.print itv
                            ) :: !ret


                        | E_c_points_to pp ->
                          ret := (fun fmt ->
                              fprintf fmt "%a ⇝ %a" pp_expr e pp_points_to pp
                            ) :: !ret

                        | _ -> assert false
                      )
                )
            );
          fprintf fmt "@[<v>%a@]"
            (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
               (fun fmt pp -> pp fmt)
            ) !ret
      )

    | _ -> None


  let refine channel man sman flow = Channel.return flow

end


let () =
  Framework.Core.Sig.Stacked.Intermediate.register_stack (module Domain);
