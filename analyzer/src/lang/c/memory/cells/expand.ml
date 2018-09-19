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

let name = "c.memory.cell.expand"
let debug fmt = Debug.debug ~channel:name fmt

let opt_max_expand = ref 2

(*==========================================================================*)
(**                              {2 Cells}                                  *)
(*==========================================================================*)

(** Memory cells. *)
type ocell = {
  b: base;
  o: Z.t;
  t: typ;
}

let ch_addr_of_ocell o addr =
  {o with b = Base.A addr}

type cell +=
  | OffsetCell of ocell

let pp_ocell fmt (c: ocell) =
  Format.fprintf fmt "⟨%a,%a,%a⟩"
    pp_base c.b
    Z.pp_print c.o
    pp_typ c.t

let compare_ocell (c: ocell) (c': ocell) =
  Compare.compose [
    (fun () -> compare_base c.b c'.b);
    (fun () -> Z.compare c.o c'.o);
    (fun () -> compare_typ c.t c'.t);
  ]

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
              let () = Format.fprintf Format.str_formatter "%a" pp_ocell o in
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
  (* mk_expr ~etyp:(c.t) (E_c_cell (OffsetCell c)) range *)

let mk_remove_cell (c: ocell) range =
  mk_stmt
    (S_c_remove_cell (OffsetCell c))
    range

let mk_ocell_of_var v t =
  {b = Base.V v; o = Z.zero ; t = t}
(* FIXME : offset should not be 0 *)

let base_of_ocell (c: ocell) =
  c.b

(* let var_of_new_ocell c =
 *   { vname = (let () = Format.fprintf Format.str_formatter "%a" pp_ocell c in Format.flush_str_formatter ());
 *     vuid = 0;
 *     vtyp = c.t;
 *   } *)


(*==========================================================================*)
(**                       {2 Abstract domain}                               *)
(*==========================================================================*)


module Domain (* : Framework.Domains.Stacked.S *) = struct


  (*==========================================================================*)
  (**                       {2 Lattice structure}                             *)
  (*==========================================================================*)

  (** Set of cells variables. *)

  (* module Bind = Equiv.Make(struct type t = ocell let compare = compare_ocell let print = pp_ocell end)(Var) *)

  module OCell = struct
    type t = ocell
    let compare = compare_ocell
    let print = pp_ocell
  end

  module CellSet = Set.Make(OCell)
  let pp_cellset fmt cs =
    Format.fprintf fmt "{%a}"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         pp_ocell
      )
      (CellSet.elements cs)

  type t = CellSet.t Bot.with_bot

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_cell_expand : t domain
  let id = D_c_cell_expand
  let name = "c.cell.expand"
  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_c_cell_expand -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  let top = Bot.Nb CellSet.empty
  let bottom = Bot.BOT

  let is_top u = Bot.bot_dfl1 false CellSet.is_empty u
  let is_bot u = Bot.bot_dfl1 true (fun _ -> false) u
  let mem_cell c = Bot.bot_dfl1 false (CellSet.mem c)
  let add_cell c = Bot.bot_lift1 (CellSet.add c)
  let rm_cell c = Bot.bot_lift1 (CellSet.remove c)

  let is_bottom (u: t) = match u with | Bot.BOT -> true | Bot.Nb _ -> false

  let diff u v =
    let u = Bot.bot_to_exn u in
    let v = Bot.bot_to_exn v in
    CellSet.diff u v

  let fold f acc_bot acc_nbot =
    Bot.bot_dfl1 acc_bot (fun x -> CellSet.fold f x acc_nbot)

  (** Pretty printer. *)
  let print fmt c =
    Format.fprintf fmt "expand cells: @[%a@]@\n"
      (Bot.bot_fprint pp_cellset) c

  let exist_and_find_cell f cs =
    match cs with
    | Bot.BOT  -> None
    | Bot.Nb r ->
      begin
        let exception Found of ocell in
        try
          let () = CellSet.iter (fun c ->
              if f c then raise (Found(c))
            ) r in
          None
        with
        | Found (c) -> Some (c)
      end

  let exist_and_find_cells f cs =
    match cs with
    | Bot.BOT -> []
    | Bot.Nb r ->
      begin
        CellSet.filter f r |>
        CellSet.elements
      end

  (*==========================================================================*)
  (**                          {2 Unification}                                *)
  (*==========================================================================*)

  type pexp = Invalid

  type phi_exp =
      Nexp of expr option
    | Pexp of pexp

  (** [phi c u] collects constraints over cell [c] found in [u] *)
  let phi (c: ocell) (u : t) range : phi_exp =
    let open Universal.Ast in
    let cs = u in
    match exist_and_find_cell (fun c' -> compare_ocell c c' = 0) cs  with
    | Some c -> Nexp (Some (mk_ocell c range))
    | None ->
      begin
        match exist_and_find_cell (fun c' -> is_c_int_type @@ c'.t && Z.equal (sizeof_type c'.t) (sizeof_type c.t) && compare_base c.b c'.b = 0 && Z.equal c.o c'.o) cs with
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
                      Pexp Invalid
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
    if mem_cell c u then u, s
    else if not (is_c_scalar_type c.t) then u, s
    else if is_c_pointer_type (c.t) then
      add_cell c u, s
    else
      match phi c u range with
      | Nexp (Some e) ->
        let stmt = Universal.Ast.(mk_assume (mk_binop (mk_ocell c range) O_eq e ~etyp:T_int range) range) in
        add_cell c u, subman.exec stmt s
      | Nexp None ->
        add_cell c u, s
      | Pexp Invalid -> assert false

  (** [add_cons_cell v u] adds a variable [v] to the abstraction [u] *)
  let add_cons_cell man range (c: ocell) f =
    let u = Flow.get_domain_cur man f in
    if mem_cell c u then f
    else if not (is_c_scalar_type c.t) then f
    else if is_c_pointer_type (c.t) then
      Flow.set_domain_cur (add_cell c u) man f
    else
      match phi c u range with
      | Nexp (Some e) ->
        let stmt = Universal.Ast.(mk_assume (mk_binop (mk_ocell c range) O_eq e ~etyp:T_int range) range) in
        f |>
        Flow.set_domain_cur (add_cell c u) man |>
        man.exec ~zone:(Z_c_cell) stmt
      | Nexp None ->
        Flow.set_domain_cur (add_cell c u) man f
      | Pexp Invalid -> assert false


  (** [unify u u'] finds non-common cells in [u] and [u'] and adds them. *)
  let unify (subman: ('b, 'b) man) ((u : t), (s: 'b flow)) ((u' : t), (s': 'b flow)) =
    let range = mk_fresh_range () in
    if is_bot u || is_bot u' then (u, s), (u', s')
    else
      let diff' = diff u u' in
      let diff = diff u' u in
      CellSet.fold (fun c (u, s) ->
          add_cons_cell_subman subman range c u  s
        ) diff (u, s),
      CellSet.fold (fun c (u', s') ->
          add_cons_cell_subman subman range c u' s'
        ) diff' (u', s')

  let remove_overlapping_cells c range man flow =
    let u = Flow.get_domain_cur man flow in
    let u' = add_cell c u in
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
            let flow' = Flow.map_domain_cur (rm_cell c') man flow in
            (flow', stmt :: to_remove)
          else
            (flow, to_remove)
      ) (flow', []) (flow', []) u in
    (flow'', to_remove)

  let join annot (subman: ('b, 'b) man) (u , (s: 'b flow) ) (u', (s': 'b flow)) =
    let (u, s), (_, s') = unify subman (u, s) (u', s') in
    (u, s, s')

  let meet = join
  let widen = join

  let subset (subman: ('b, 'b) man) (u , (s: 'b flow) ) (u', (s': 'b flow)) =
    let (_, s), (_, s') = unify subman (u, s) (u', s') in
    (true, s, s')

  let print = Bot.bot_fprint pp_cellset

  let exec_interface = {
    export = [Zone.Z_c];
    import = [Z_c_cell];
  }
  let eval_interface = {
    export = [Zone.Z_c_scalar, Z_c_cell];
    import = [
      (Zone.Z_c, Zone.Z_c_scalar);
      (Zone.Z_c, Z_c_points_to)
    ];
  }

  (*==========================================================================*)
  (**                       {2 Cells expansion}                               *)
  (*==========================================================================*)

  let fold_cells f err empty x0 base offset typ range man flow =
    if Flow.is_cur_bottom man flow then empty ()
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
            if Z.(leq ((u - l + one) / step) (of_int !opt_max_expand)) then
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
    let rmin, rmax = rangeof c.t in
    let cond = range_cond lval rmin rmax (erange lval) in
    let stmt' = (mk_assume cond (tag_range range "assume range")) in
    let flow'' = man.exec ~zone:Z_c_cell (mk_block (to_remove @ [stmt; stmt']) range) flow' in
    (Post.add_mergers to_remove (Post.of_flow flow''))

  let rec exec zone stmt man flow =
    let range = stmt.srange in
    match skind stmt with
    | S_c_local_declaration(v, init) ->
      Init_visitor.init_local (init_manager man) v init range flow
      |> Post.of_flow
      |> Option.return

    | S_rename_var(v, v') ->
      assert false

    | S_rebase_addr(adr, adr', mode) ->
      begin
        let u = Flow.get_domain_cur man flow in
        let l = exist_and_find_cells (fun c -> compare_base (base_of_ocell c) (Base.A adr) = 0) u in
        let u = List.fold_left (fun acc c -> rm_cell c acc) u l in
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
      let l = exist_and_find_cells (fun c -> compare_base (base_of_ocell c) (Base.V v) = 0) u in
      let u' = List.fold_left (fun acc c -> rm_cell c acc) u l in
      let mergers = List.map (fun c -> mk_remove_cell c stmt.srange) l in
      let to_exec_in_sub = mergers in
      let flow = Flow.set_domain_cur u' man flow in
      man.exec ~zone:Z_c_cell (mk_block to_exec_in_sub range) flow |>
      Post.of_flow |>
      Post.add_mergers mergers |>
      Option.return

    | S_assign(lval, rval) when is_c_scalar_type lval.etyp ->
      begin
        man.eval ~zone:(Zone.Z_c, Z_c_cell) rval flow
        |> Post.bind man @@ fun rval flow ->
        man.eval ~zone:(Zone.Z_c, Zone.Z_c_scalar) lval flow
        |> Post.bind man @@ fun lval flow ->
        eval (Zone.Z_c_scalar, Z_c_cell) lval man flow
        |> Eval.default_opt lval flow
        |> Post.bind man @@ fun lval flow ->
        match ekind lval with
        | E_c_cell(OffsetCell c, mode) ->
          assign_cell man c rval mode stmt.srange flow
        | _ -> assert false
      end |> Option.return
    | _ -> None

  and eval zone exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_c_cell(OffsetCell c, _) ->
      Eval.singleton exp flow |> Option.return

    | E_var (v, mode) when is_c_type v.vtyp ->
      begin
        debug "evaluating a scalar variable %a" pp_var v;
        let c = mk_ocell_of_var v v.vtyp  in
        let flow = add_cons_cell man range c flow in
        debug "new variable %a in %a" pp_var v (Flow.print man) flow;
        Eval.singleton {exp with ekind = E_c_cell (OffsetCell c, mode)} flow
      end |> Option.return

    | E_c_deref(p) ->
      begin
        man.eval ~zone:(Zone.Z_c, Z_c_points_to) p flow |> Eval.bind @@ fun pe flow ->
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

  and init_manager man =
    Init_visitor.{
      (* Initialization of scalars *)
      scalar = (fun v e range flow ->
          match ekind v with
          | E_var(v, mode) ->
            let c = mk_ocell_of_var v v.vtyp in
            let flow = add_cons_cell man range c flow in
            let stmt = mk_assign (mk_var v ~mode:mode range) e range in
            man.exec stmt flow
          | _ -> assert false
        );

      (* Initialization of arrays *)
      array =  (fun a is_global init_list range flow ->
          match ekind a with
          | E_var(a, mode) ->
            let rec aux i l flow =
              if i = !opt_max_expand then flow
              else
                match l with
                | [] -> flow
                | init :: tl ->
                  let t' = under_array_type a.vtyp in
                  let ci = {b = Base.V a; o = Z.(zero + (Z.of_int i) * (sizeof_type t')); t = t'} in
                  let flow' = init_expr (init_manager man) (mk_ocell ci ~mode:mode range) is_global init range flow in
                  aux (i + 1) tl flow'
            in
            aux 0 init_list flow
          | _ -> assert false
        );

      (* Initialization of structs *)
      record =  (fun s is_global init_list range flow ->
          let s = match ekind s with E_var(s, mode) -> s | _ -> assert false in
          let c = mk_ocell_of_var s s.vtyp in
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
                let flow' = init_expr (init_manager man) (mk_ocell cf ~mode:STRONG range) is_global init range flow in
                aux (i + 1) tl flow'
            in
            aux 0 l flow

          | Expr e ->
            record.c_record_fields |> List.fold_left (fun acc field ->
                let t' = field.c_field_type in
                let cf = {b = c.b; o = Z.(c.o + (Z.of_int field.c_field_offset)); t = t'} in
                let init = C_init_expr (mk_c_member_access e field range) in
                init_expr (init_manager man) (mk_ocell cf ~mode:STRONG range) is_global (Some init) range acc
              ) flow
        );
    }

  and init prog man flow =
    let flow = Flow.set_domain_cur top man flow in
    match prog.prog_kind with
    | C_program(globals, _) ->
      Some (Init_visitor.init_globals (init_manager man) globals flow)
    | _ ->
      None

  let ask _ _ _ = None

end


let () =
  Framework.Domains.Stacked.register_domain (module Domain);
  (* register_domain name (module Domain); *)
  Framework.Options.register_option (
    "-cell-max-expand",
    Arg.Set_int opt_max_expand,
    " maximal number of expanded cells (default: 1)"
  )
