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
module Intervals = Universal.Numeric.Values.Intervals.Value

(** Maximal number of expanded cells when dereferencing a pointer *)
let opt_expand = ref 1

let () =
  Framework.Options.register_option (
    "-cell-expand",
    Arg.Set_int opt_expand,
    " maximal number of expanded cells (default: 1)"
  )


module Domain = struct


  (** Definition of offset cells
      ==========================

      Offset cells represent contiguous memory regions within some
      base.  The position of the region is given by an integer offset,
      and its size is given by a type.
  *)

  type cell +=
    | C_offset of {
        b: base;
        o: Z.t;
        t: typ;
      }

  let () =
    register_cell
      {
        extract = (fun f c ->
            match c with
            | C_offset o -> (o.b, mk_z o.o, o.t)
            | _ -> f c
          );
        to_var = (fun next c ->
            match c with
            | C_offset o ->
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
            | C_offset o, C_offset o' ->
              Compare.compose [
                (fun () -> compare_base o.b o'.b);
                (fun () -> Z.compare o.o o'.o);
                (fun () -> compare_typ o.t o'.t);
              ]

            | _ -> f a b
          );
        print = (fun f fmt x ->
            match x with
            | C_offset o ->
              Format.fprintf fmt "⟨%a,%a,%a⟩"
                pp_base o.b
                Z.pp_print o.o
                pp_typ o.t

            | _ -> f fmt x
          )
      }

  let mk_offset_cell b o t ?(mode = STRONG) range =
    mk_cell (C_offset {b; o; t}) ~mode:mode range


  let rec base c =
    match c with
    | C_offset cc -> cc.b
    | C_old cc -> base cc
    | _ -> assert false

  let rec offset c =
    match c with
    | C_offset cc -> cc.o
    | C_old cc -> offset cc
    | _ -> assert false

  let rec typ c =
    match c with
    | C_offset cc -> cc.t
    | C_old cc -> typ cc
    | _ -> assert false


  let rec ch_addr_of_cell c addr =
    match c with
    | C_offset cc -> C_offset {cc with b = Base.A addr}
    | C_old cc -> ch_addr_of_cell cc addr
    | _ -> assert false

  (** Abstract element
      ================

      We keep track of the set of previously expanded cells. For that,
      we use the Powerset generic lattice.
  *)

  include Framework.Lattices.Powerset.Make(struct
      type t = cell
      let compare = compare_cell
      let print = pp_cell
    end)

  let is_bottom _ = false

  let print fmt c =
    Format.fprintf fmt "expand cells: @[%a@]@\n"
      print c


  let exist_and_find_cell f (cs:t) =
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
          let cell_range c = (offset c, Z.add (offset c) (sizeof_type (typ c))) in
          let check_overlap (a1, b1) (a2, b2) = Z.lt (Z.max a1 a2) (Z.min b1 b2) in
          compare_base (base c) (base c') = 0 &&
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
    | Some c -> Some (mk_cell c range)
    | None ->
      begin
        match exist_and_find_cell
                (fun c' ->
                   is_c_int_type @@ (typ c') && Z.equal (sizeof_type (typ c')) (sizeof_type (typ c)) &&
                   compare_base (base c) (base c') = 0 &&
                   Z.equal (offset c) (offset c')) cs
        with
        | Some (c') -> Some (wrap_expr (mk_cell c' range) (int_rangeof (typ c)) range)
        | None ->
          begin
            match exist_and_find_cell ( fun c' ->
                let b = Z.sub (offset c) (offset c') in
                Z.lt b (sizeof_type (typ c')) && is_c_int_type (typ c') && compare (remove_typedef_qual (typ c)) (T_c_integer(C_unsigned_char)) = 0
              ) cs with
            | Some (c') ->
              let b = Z.sub (offset c) (offset c') in
              let base = (Z.pow (Z.of_int 2) (8 * Z.to_int b))  in
              Some (mk_binop (mk_binop (mk_cell c' range) O_div (mk_z base range) range) O_mod (mk_int 256 range) range)
            | None ->
              begin
                let exception NotPossible in
                try
                  if is_c_int_type (typ c) then
                    begin
                      let t' = T_c_integer(C_unsigned_char) in
                      let n = Z.to_int (sizeof_type ((typ c))) in
                      let rec aux i l =
                        if i < n then
                          let tobein = C_offset {b = (base c) ; o = Z.add (offset c) (Z.of_int i); t = t'} in
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
                          let res' = mk_binop (mk_binop (mk_z time range) O_mult (mk_cell x range) range) O_plus res range in
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
                  match (base c) with
                  | S s ->
                    Some (mk_int (String.get s (Z.to_int (offset c)) |> int_of_char) range)
                  | _ ->
                    begin
                      if is_c_scalar_type (typ c) then
                        let a,b = rangeof (typ c) in
                        Some ( mk_z_interval a b range)
                      else if is_c_pointer_type (typ c) then
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
    else if not (is_c_scalar_type (typ c)) then u, s
    else if is_c_pointer_type ((typ c)) then
      add c u, s
    else
      match phi c u range with
      | Some e ->
        let stmt = Universal.Ast.(mk_assume (mk_binop (mk_cell c range) O_eq e ~etyp:T_int range) range) in
        add c u, subman.exec stmt s
      | None ->
        add c u, s

  (** [add_cons_cell v u] adds a variable [v] to the abstraction [u] *)
  let add_cons_cell man range (c: cell) f =
    let u = Flow.get_domain_cur man f in
    if mem c u then f
    else if not (is_c_scalar_type (typ c)) then f
    else if is_c_pointer_type ((typ c)) then
      Flow.set_domain_cur (add c u) man f
    else
      match phi c u range with
      | Some e ->
        let stmt = Universal.Ast.(mk_assume (mk_binop (mk_cell c range) O_eq e ~etyp:T_int range) range) in
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
      Z_c_scalar, Z_c_cell;
      Z_c, Z_c_scalar
    ];
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
            | E_var(v, mode) -> C_offset {b = V v; o = Z.zero; t = v.vtyp}
            | E_c_cell (c, mode) -> c
            | _ -> assert false
          in
          man.eval ~zone:(Z_c, Z_c_cell) e flow |>
          Post.bind_flow man @@ fun e flow ->

          let flow = add_cons_cell man range c flow in
          let stmt = mk_assign (mk_cell c range) e range in
          man.exec ~zone:Z_c_cell stmt flow
        );

      (* Initialization of arrays *)
      array =  (fun a is_global init_list range flow ->
          let c =
            match ekind a with
            | E_var(v, mode) -> C_offset {b = V v; o = Z.zero; t = v.vtyp}
            | E_c_cell (c, mode) -> c
            | _ -> assert false
          in
          let rec aux i l flow =
            if i = !opt_expand then flow
            else
              match l with
              | [] -> flow
              | init :: tl ->
                let t' = under_array_type (typ c) in
                let ci = C_offset {b = (base c); o = Z.((offset c) + (Z.of_int i) * (sizeof_type t')); t = t'} in
                let flow' = init_expr (init_visitor man) (mk_cell ci range) is_global init range flow in
                aux (i + 1) tl flow'
          in
          aux 0 init_list flow
        );

      (* Initialization of structs *)
      record =  (fun s is_global init_list range flow ->
          let c =
            match ekind s with
            | E_var (v, _) -> C_offset {b = V v; o = Z.zero; t = v.vtyp}
            | E_c_cell(c, _) -> c
            | _ -> assert false
          in
          let record = match remove_typedef_qual (typ c) with T_c_record r -> r | _ -> assert false in
          match init_list with
          | Parts l ->
            let rec aux i l flow =
              match l with
              | [] -> flow
              | init :: tl ->
                let field = List.nth record.c_record_fields i in
                let t' = field.c_field_type in
                let cf = C_offset {b = (base c); o = Z.((offset c) + (Z.of_int field.c_field_offset)); t = t'} in
                let flow' = init_expr (init_visitor man) (mk_cell cf ~mode:STRONG range) is_global init range flow in
                aux (i + 1) tl flow'
            in
            aux 0 l flow

          | Expr e ->
            record.c_record_fields |> List.fold_left (fun acc field ->
                let t' = field.c_field_type in
                let cf = C_offset {b = (base c); o = Z.((offset c) + (Z.of_int field.c_field_offset)); t = t'} in
                let init = C_init_expr (mk_c_member_access e field range) in
                init_expr (init_visitor man) (mk_cell cf ~mode:STRONG range) is_global (Some init) range acc
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
    | O_concrete of Z.t         (* specific offset with some numerical value *)
    | O_interval of Intervals.t (* interval of offsets *)
    | O_out_of_bound            (* invalid offset *)


  (** Evaluate an offset expression into an offset evaluation *)
  let eval_cell_offset_cases (offset:expr) cell_size base_size man flow : ('a, (offset * 'a flow) list) evl =
    let singleton e flow =
      Eval.singleton [e, flow] flow
    in
    (* Try the static case *)
    match Universal.Utils.expr_to_z offset with
    | Some z ->
      if Z.geq z Z.zero &&
         Z.leq (Z.add z cell_size) base_size
      then singleton (O_concrete z) flow
      else singleton O_out_of_bound flow

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
                  [O_interval (Intervals.of_constant () (C_int_interval (o, u))), flow']
                else
                  let flow' = man.exec ~zone:Universal.Zone.Z_u_num (mk_assume (mk_binop offset O_eq (mk_z o range) range) range) flow in
                  (O_concrete o, flow') :: aux (i + 1) (Z.add o step)
              in
              let evals = aux 0 l in
              Eval.singleton (evals) flow
            else
              singleton (O_interval itv) flow
          )
        ~felse:(fun flow -> singleton O_out_of_bound flow)
        man flow


  (** Evaluation of cells *)
  (** =================== *)

  (** Kinds of evaluated cells *)
  type ecell =
    | C_cell of cell * mode                         (* specific cell with an integer offset *)
    | C_interval_offset of base * Intervals.t * typ (* set of cells with an interval offset *)
    | C_fun of c_fundec                             (* functions *)

  let eval_cell_cases b o t range man flow =
    eval_cell_offset_cases o (sizeof_type t) (base_size b) man flow |>
    Eval.bind @@ fun ol flow ->
    let cl = List.map (fun (o, flow) ->
        match o with
        | O_concrete o -> Some (C_cell (C_offset {b; o; t}, STRONG)), flow
        | O_interval itv -> Some (C_interval_offset (b, itv, t)), flow
        | O_out_of_bound ->
          let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack flow in
          let alarm = mk_alarm Alarms.AOutOfBound range ~cs in
          let flow' = Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man flow |>
                      Flow.set T_cur man.bottom man
          in
          None, flow'
      ) ol
    in
    Eval.singleton cl flow


  (** Evaluate a quantified cell *)
  let eval_quantified_cell b o t range man flow =
    let open Stubs.Ast in
    let q, vl, o' = unquantify_expr o in
    eval_cell_cases b o' t range man flow |>
    Eval.bind @@ fun l flow ->
    let evl = List.map (fun (c, flow) ->
        match c with
        | Some cc -> Eval.singleton cc flow
        | None -> Eval.empty_singleton flow
      ) l
    in
    let evl' =
      match q with
      | EXISTS -> Eval.join_list evl
      | FORALL -> Eval.meet_list evl
      | MIXED  -> panic_at range "cell: evaluation of offsets with mixed quantifiers not supported"
    in
    let cleaners = List.map (fun v -> mk_remove_var v range) vl in
    Eval.add_cleaners cleaners evl'


  (** Evaluate a non-quantified cell *)
  let eval_non_quantified_cell b o t range man flow =
    eval_cell_cases b o t range man flow |>
    Eval.bind @@ fun cl flow ->
    let cl' = cl |> List.map(function
        | Some c, flow -> Eval.singleton c flow
        | None, flow -> Eval.empty_singleton flow
      )
    in
    Eval.join_list cl'


  let eval_pointed_cell p man flow =
    man.eval ~zone:(Zone.Z_c, Z_c_points_to_cell) p flow |>
    Eval.bind @@ fun pe flow ->

    match ekind pe with
    | E_c_points_to(P_var (b, o, t)) when Stubs.Ast.is_expr_quantified o ->
      eval_quantified_cell b o t p.erange man flow

    | E_c_points_to(P_var (b, o, t)) ->
      eval_non_quantified_cell b o t p.erange man flow

    | E_c_points_to(P_fun fundec) ->
      Eval.singleton (C_fun fundec) flow

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

  (** Evaluate an lval into a cell *)
  let rec eval_lval exp man flow =
    match ekind exp with
    | E_var (v, mode) when is_c_type v.vtyp ->
      let c = C_offset {b = V v; o = Z.zero; t = v.vtyp}  in
      let flow = add_cons_cell man exp.erange c flow in
      debug "new variable %a" pp_var v;
      Eval.singleton (C_cell (c, mode)) flow |>
      OptionExt.return

    | E_c_deref(p) ->
      eval_pointed_cell p man flow |>
      OptionExt.return

    | _ -> None


  (** Evaluation of C expressions *)
  (** =========================== *)

  let eval zone exp man flow =
    match ekind exp with
    | E_var _ | E_c_deref _ ->
      eval_lval exp man flow |>
      OptionExt.lift @@ Eval.bind @@ fun c flow ->
      begin match c with
        | C_cell(c, mode) ->
          add_cons_cell man exp.erange c flow |>
          Eval.singleton (mk_cell c ~mode exp.erange)

        | C_interval_offset(b, itv, t) ->
          let a, b = rangeof t in
          Eval.singleton (mk_z_interval a b exp.erange) flow

        | C_fun f -> Eval.singleton {exp with ekind = E_c_function f} flow
      end

    | Stubs.Ast.E_stub_builtin_call({ content = SIZE }, p) ->
      man.eval ~zone:(Zone.Z_c, Z_c_points_to_cell) p flow |>
      Eval.bind_opt @@ fun pe flow ->
      begin match ekind pe with
        | E_c_points_to(P_var (b, o, t)) ->
          Eval.singleton (mk_z (Z.div (base_size b) (Z.max Z.one (sizeof_type t))) exp.erange) flow |>
          OptionExt.return

        | _ -> panic_at exp.erange "cells.expand: size(%a) not supported" pp_expr exp
      end

    | Stubs.Ast.E_stub_builtin_call({content = OLD }, e) ->
      man.eval ~zone:(Z_c, Z_c_scalar) e flow |>
      Eval.bind_opt @@ fun e flow ->

      eval_lval e man flow |>
      OptionExt.lift @@ Eval.bind @@ fun c flow ->
      begin match c with
        | C_cell(c, mode) ->
          add_cons_cell man exp.erange c flow |>
          Eval.singleton (mk_old_cell c ~mode exp.erange)

        | C_interval_offset(b, itv, t) ->
          let a, b = rangeof t in
          Eval.singleton (mk_z_interval a b exp.erange) flow

        | _ -> panic_at exp.erange "old on functions not supported"
      end


    | _ -> None


  (** Computation of post-conditions *)
  (** ============================== *)

  (** Assign an rval to a cell *)
  let assign_cell c rval mode range man flow =
    let lval = mk_cell c ~mode:mode range in
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


  (** Remove cells identified by an offset membership predicate *)
  let remove_cells b pred range man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let cells =
      a |>
      exist_and_find_cells
        (fun c -> compare_base (base c) b = 0 &&
                  pred (offset c)
        )
    in
    let block = List.map (fun c' -> mk_remove_cell c' range) cells in

    man.exec ~zone:Z_c_cell (mk_block block range) flow |>
    Post.of_flow |>
    Post.add_mergers block ~zone:Z_c_cell


  (** Prepare the assignment of a cell in a stub. All cells within
      container [c] and having an offset in the range [offsets] are put
      to ⊤, and old copies created.
  *)
  let prepare_cells_for_stub_assigns c offsets t range man flow =
    (* Get info about the container cell [c] *)
    let cb, co, ct =
      match c with
      | C_cell(c, _) -> (base c), (Intervals.of_constant () (C_int (offset c))), (typ c)
      | C_interval_offset(b, o, t) -> b, o, t
      | _ -> panic "prepare_cells_for_stub_assigns: invalid cell argument"
    in

    (* Compute the offset interval of assigned cell *)
    let offset_itv =
      match offsets with
      | None -> co
      | Some(l, u) ->
        (* Compute the intervals of the bound expressions *)
        let itv1 = man.ask (Intervals.Q_interval l) flow in
        let itv2 = man.ask (Intervals.Q_interval u) flow in

        (* Compute the interval containing the two intervals *)
        let itv = Intervals.join () itv1 itv2 in

        (* Multiply by the size of the assigned cell *)
        Intervals.binop () O_mult itv (Intervals.of_constant () (C_int (sizeof_type t))) |>
        Channel.without_channel |>

        (* Add the container offset *)
        Intervals.binop () O_plus co |>
        Channel.without_channel
    in

    (* Get assigned cells *)
    let cells =
      Flow.get_domain_env T_cur man flow |>
      exist_and_find_cells
        (fun c -> compare_base (base c) cb = 0 &&
                  Intervals.mem (offset c) offset_itv
        )
    in

    (* Remove assigned cells and create old ones *)
    let flow = List.fold_left (fun flow c ->
        let old = C_old c in
        Flow.map_domain_cur (add old) man flow |>
        man.exec ~zone:Z_c_cell (mk_c_add_cell old range) |>
        man.exec ~zone:Z_c_cell (mk_cell_expand c [old] range) |>
        man.exec ~zone:Z_c_cell (mk_remove_cell c range)
      ) flow cells
    in

    (* Create a list of mergers *)
    let mergers =
      List.fold_left (fun acc c ->
          let old = C_old c in
          (mk_remove_cell old range) ::
          (mk_remove_cell c range) ::
          acc
      ) [] cells
    in

    Post.of_flow flow |>
    Post.add_mergers mergers ~zone:Z_c_cell

  let remove_old_cells c range man flow =
    let b =
      match c with
      | C_cell (c, _) -> (base c)
      | C_interval_offset(b, _, _) -> b
      | _ -> assert false
    in
    let cells =
      Flow.get_domain_env T_cur man flow |>
      exist_and_find_cells
        (fun cc ->
           match cc with
           | C_old ccc ->
             compare_base b (base ccc) = 0
           | _ -> false
        )
    in

    let flow = List.fold_left (fun flow old ->
        Flow.map_domain_cur (remove old) man flow |>
        man.exec ~zone:Z_c_cell (mk_remove_cell old range)
      ) flow cells
    in

    (* Create a list of mergers *)
    let mergers =
      List.fold_left (fun acc old ->
          (mk_remove_cell old range) ::
          acc
        ) [] cells
    in

    Post.of_flow flow |>
    Post.add_mergers mergers ~zone:Z_c_cell


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

    | S_add_var (v) when is_c_type v.vtyp ->
      eval_lval (mk_var v stmt.srange) man flow |>
      OptionExt.lift @@ Post.bind man @@ fun ce flow ->
      let c = match ce with
        | C_cell (c, _) -> c
        | _ -> assert false
      in
      man.exec ~zone:Z_c_cell (mk_c_add_cell c stmt.srange) flow |>
      Post.of_flow |>
      Post.add_merger (mk_c_add_cell c stmt.srange)

    | S_rebase_addr(adr, adr', mode) ->
      begin
        let u = Flow.get_domain_cur man flow in
        let l = exist_and_find_cells (fun c -> compare_base (base c) (Base.A adr) = 0) u in
        let u = List.fold_left (fun acc c -> remove c acc) u l in
        let assigns = List.map
            (fun c ->
               let c' = ch_addr_of_cell c adr' in
               mk_assign
                 (mk_cell c' ~mode:mode (tag_range stmt.srange "lval"))
                 (mk_cell c  (tag_range stmt.srange "rval"))
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
      let l = exist_and_find_cells (fun c -> compare_base (base c) (Base.V v) = 0) u in
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

      eval_lval lval man flow |>
      OptionExt.lift @@ Post.bind man @@ fun c flow ->
      begin
        match c with
        | C_cell(c, mode) -> assign_cell c rval mode stmt.srange man flow

        | C_interval_offset(b, itv, _) -> remove_cells b (fun o -> Intervals.mem o itv) stmt.srange man flow

        | C_fun f -> assert false
      end

    | S_assume(e) ->
      man.eval ~zone:(Z_c, Z_c_cell) e flow |>
      Post.bind_opt man @@ fun e' flow ->

      let stmt' = {stmt with skind = S_assume e'} in
      man.exec ~zone:Z_c_cell stmt' flow |>

      Post.of_flow |>
      OptionExt.return

    | Stubs.Ast.S_stub_assigns(x, None) ->
      let t = x.etyp in
      man.eval ~zone:(Z_c, Z_c_scalar) x flow |>
      Post.bind_opt man @@ fun x flow ->

      eval_lval x man flow |>
      OptionExt.lift @@ Post.bind man @@ fun x flow ->

      prepare_cells_for_stub_assigns x None t stmt.srange man flow

    | Stubs.Ast.S_stub_assigns(x, Some (o1, o2)) ->
      let t = under_type x.etyp in

      Some (
        eval_pointed_cell x man flow |>
        Post.bind man @@ fun x flow ->

        man.eval ~zone:(Z_c, Universal.Zone.Z_u_num) o1 flow |>
        Post.bind man @@ fun o1 flow ->

        man.eval ~zone:(Z_c, Universal.Zone.Z_u_num) o2 flow |>
        Post.bind man @@ fun o2 flow ->

        prepare_cells_for_stub_assigns x (Some (o1, o2)) t stmt.srange man flow
      )

    | Stubs.Ast.S_stub_remove_old(x, None) ->
      man.eval ~zone:(Z_c, Z_c_scalar) x flow |>
      Post.bind_opt man @@ fun x flow ->
      eval_lval x man flow |>
      OptionExt.lift @@ Post.bind man @@ fun x flow ->

      remove_old_cells x stmt.srange man flow

    | Stubs.Ast.S_stub_remove_old(x, Some _) ->
      Some (
        eval_pointed_cell x man flow |>
        Post.bind man @@ fun x flow ->

        remove_old_cells x stmt.srange man flow
      )

    | _ -> None



  let ask _ _ _ = None

end


let () =
  Framework.Domains.Stacked.register_domain (module Domain);
