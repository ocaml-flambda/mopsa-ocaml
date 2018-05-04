(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Expansion-based abstraction of C memory cells. *)

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

type xcell = {
  b: base;
  t: typ;
}

type cell =
  | OffsetCell of ocell
  | AnyCell of xcell

let pp_ocell fmt (c: ocell) =
  Format.fprintf fmt "⟨%a,%a,%a⟩"
    pp_base c.b
    Z.pp_print c.o
    pp_typ c.t

let pp_xcell fmt (c: xcell) =
  Format.fprintf fmt "⟨%a,?,%a⟩"
    pp_base c.b
    pp_typ c.t

let pp_cell fmt c =
  match c with
  | OffsetCell c -> pp_ocell fmt c
  | AnyCell c -> pp_xcell fmt c

let compare_ocell (c: ocell) (c': ocell) =
  compare_composer [
    (fun () -> compare_base c.b c'.b);
    (fun () -> Z.compare c.o c'.o);
    (fun () -> compare_typ c.t c'.t);
  ]

let compare_xcell (c: xcell) (c': xcell) =
  compare_composer [
    (fun () -> compare_base c.b c'.b);
    (fun () -> compare_typ c.t c'.t);
  ]

let compare_cell (c: cell) (c': cell) =
  match c, c' with
  | OffsetCell c, OffsetCell c' -> compare_ocell c c'
  | AnyCell c, AnyCell c' -> compare_xcell c c'
  | _ -> compare c c'


(** Annotate variables with cell information. *)
type var_kind +=
  | V_expand_cell of cell

let annotate_var_kind v =
  match v.vkind with
  | V_orig -> { v with vkind = V_expand_cell (OffsetCell{b = V v; o = Z.zero; t = v.vtyp})}
  | V_expand_cell _ -> v
  | _ -> assert false

let cell_of_var v =
  match v.vkind with
  | V_expand_cell c -> c
  | _ -> assert false

let type_of_cell = function
  | OffsetCell c -> c.t
  | AnyCell c -> c.t

let base_of_cell = function
  | OffsetCell c -> c.b
  | AnyCell c -> c.b


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
      let print  = pp_var
    end)

  include CS

  let is_bottom x = false

  (** Pretty printer. *)
  let print fmt c =
    Format.fprintf fmt "expand cells: @[%a@]@\n"
      CS.print c

  let mem_pred c = function {vkind = V_expand_cell c'} -> compare_cell c c' = 0 | _ -> false

  let exist_and_find_cell f cs =
    let exception Found of var * ocell in
    try
      let () = CS.iter (function ({vkind = V_expand_cell (OffsetCell c)} as v) -> if f c then raise (Found (v, c)) else () | _ -> ()) cs in
      None
    with
    | Found (v, c) -> Some (v, c)

  let var_of_ocell (c: ocell) cs =
    match exist_and_find_cell (fun c' -> compare c c' = 0) cs with
    | Some (v, _) -> v
    | None ->
      { vname = (let () = Format.fprintf Format.str_formatter "%a" pp_ocell c in Format.flush_str_formatter ());
        vuid = 0;
        vtyp = c.t;
        vkind = V_expand_cell (OffsetCell c);
      }

  let var_of_xcell (c: xcell) =
    { vname = (let () = Format.fprintf Format.str_formatter "%a" pp_xcell c in Format.flush_str_formatter ());
      vuid = 0;
      vtyp = c.t;
      vkind = V_expand_cell (AnyCell c);
    }



  (*==========================================================================*)
  (**                          {2 Unification}                                *)
  (*==========================================================================*)

  type pexp = Invalid

  type phi_exp =
      Nexp of expr option
    | Pexp of pexp

  (** [phi v u] collects constraints over cell var [v] found in [u] *)
  let phi (v : var) (u : t) range : phi_exp =
    let open Universal.Ast in
    let cs = u in
    let c = cell_of_var v in
    match c with
    | AnyCell c ->
      let a,b = rangeof c.t in
      Nexp (Some ( mk_z_interval a b range))
    | OffsetCell c ->
      match exist_and_find_cell (fun c' -> compare_ocell c c' = 0) cs  with
      | Some (v', _) -> Nexp (Some (mk_var v' range))
      | None ->
        begin
          match exist_and_find_cell (fun c' -> is_c_int_type @@ c'.t && Z.equal (sizeof_type c'.t) (sizeof_type c.t) && compare_base c.b c'.b = 0 && Z.equal c.o c'.o) cs with
          | Some (v', c') -> Nexp (Some (wrap v' (int_rangeof c.t) range))
          | None ->
            begin
              match exist_and_find_cell ( fun c' ->
                  let b = Z.sub c.o c'.o in
                  Z.lt b (sizeof_type c'.t) && is_c_int_type c'.t && compare (remove_typedef c.t) (T_c_integer(C_unsigned_char)) = 0
                ) cs with
              | Some (v', c') ->
                let b = Z.sub c.o c'.o in
                let base = (Z.pow (Z.of_int 2) (8 * Z.to_int b))  in
                Nexp (Some (mk_binop (mk_binop (mk_var v' range) math_div (mk_z base range) range) math_mod (mk_int 256 range) range))
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
                            | Some (v', c') ->
                              aux (i+1) (v' :: l)
                            | None ->
                              raise NotPossible
                          else
                            List.rev l
                        in
                        let ll = aux 0 [] in
                        let _,e = List.fold_left (fun (time,res) x ->
                            let res' = mk_binop (mk_binop (mk_z time range) math_mult (mk_var x range) range) math_plus res range in
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

  let local_manager = mk_local_manager (module SubDomain : Framework.Domains.Stateful.DOMAIN with type t = SubDomain.t)

  let local_exec ctx stmt s =
    let flow = set_domain_cur s local_manager local_manager.flow.bottom in
    let flow' = local_manager.exec ctx stmt flow in
    get_domain_cur local_manager flow'

  let sub_exec subman ctx stmt flow =
    match SubDomain.exec subman ctx stmt flow with
    | Some flow -> flow
    | None -> assert false

  (** [add_var v u] adds a variable [v] to the abstraction [u] *)
  let add_var ctx range (v : var) (u, s) =
    if CS.mem v u then u, s
    else if not (is_c_scalar_type v.vtyp) then u, s
    else if is_c_pointer_type v.vtyp then CS.add v u, s
    else
      let c = cell_of_var v in
      match c with
      | AnyCell c ->
        let (a, b) = rangeof c.t in
        let e = mk_z_interval a b range in
        let stmt = Universal.Ast.(mk_assume (mk_binop (mk_var v range) O_eq e range) range) in
        u, local_exec ctx stmt s

      | OffsetCell _ ->
        match phi v u range with
        | Nexp (Some e) ->
          let stmt = Universal.Ast.(mk_assume (mk_binop (mk_var v range) O_eq e range) range) in
          CS.add v u, local_exec ctx stmt s

        | Nexp None -> CS.add v u, s

        | Pexp Invalid -> assert false


  (** [unify u u'] finds non-common cells in [u] and [u'] and adds them. *)
  let unify ctx (u, s) (u', s') =
    let range = mk_fresh_range () in
    if is_top u || is_top u' then (u, s), (u', s')
    else
      let t = Timing.start () in
      let diff' = CS.diff u u' in
      debug "diff' done in %.4fs" (Timing.stop t);
      let t = Timing.start () in
      let diff = CS.diff u' u in
      debug "diff done in %.4fs" (Timing.stop t);
      CS.fold (fun v acc ->
          add_var ctx range v acc
        ) diff (u, s),
      CS.fold (fun v acc ->
          add_var ctx range v acc
        ) diff' (u', s')

  let extract_cell v =
    match vkind v with
    | V_expand_cell c -> c
    | _ -> assert false

  let extract_ocell v =
    match extract_cell v with
    | OffsetCell c -> c
    | _ -> assert false

  let remove_overlapping_cells v range man subman ctx flow =
    let u = get_domain_cur man flow in
    let c = cell_of_var v in
    match c with
    | AnyCell c ->
      CS.fold (fun v' acc ->
          let c' = extract_ocell v' in
          if compare_base c.b c'.b = 0 then
            map_domain_cur (remove v') man acc |>
            sub_exec subman ctx (Universal.Ast.mk_remove_var v' range)
          else
            acc
        ) u flow

    | OffsetCell c ->
      let u' = add v u in
      let flow' = set_domain_cur u' man flow in
      CS.fold (fun v' acc ->
          if compare_var v v' = 0 then
            acc
          else
            let c' = extract_ocell v' in
            let cell_range c = (c.o, Z.add c.o (sizeof_type c.t)) in
            let check_overlap (a1, b1) (a2, b2) =
              Z.lt (Z.max a1 a2) (Z.min b1 b2)
            in
            if compare_base c.b c'.b = 0 && check_overlap (cell_range c) (cell_range c') then
              map_domain_cur (remove v') man acc |>
              sub_exec subman ctx (Universal.Ast.mk_remove_var v' range)
            else
              acc
        ) u flow'



  (*==========================================================================*)
  (**                       {2 Cells expansion}                               *)
  (*==========================================================================*)

  let fold_cells f err empty x0 base offset typ range man subman ctx flow =
    let cs = get_domain_cur man flow in
    let cell_size = sizeof_type typ in

    let static_base_case base_size =
      debug "static base case";
      match Universal.Utils.expr_to_z offset with
      | Some z when Z.geq z Z.zero && Z.leq (Z.add z cell_size) base_size  ->
        let v = var_of_ocell {b = base; o = z; t = typ} cs in
        let s = get_domain_cur subman flow in
        let (cs', s') = add_var ctx range v (cs, s) in
        let flow' = set_domain_cur cs' man flow |>
                    set_domain_cur s' subman
        in
        f x0 v flow'

      | Some z ->
        debug "error, z = %a, cell_size = %a, base_size = %a" Z.pp_print z Z.pp_print cell_size Z.pp_print base_size;
        man.flow.add (Alarms.TOutOfBound range) (man.flow.get TCur flow) flow |>
        man.flow.set TCur man.env.bottom |>
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
                let v = var_of_ocell {b = base; o; t = typ} cs in
                let flow = man.exec ctx (mk_assume (mk_binop offset O_eq (mk_z o range) range) range) flow in
                let s = get_domain_cur subman flow in
                let (cs', s') = add_var ctx range v (cs, s) in
                let flow' = set_domain_cur cs' man flow |>
                            set_domain_cur s' subman
                in
                iter (f x v flow') (Z.add o step)
            in
            debug "iterating";
            iter acc l
          else
            let v = var_of_xcell {b = base; t = typ} in
            debug "using an xcell %a" pp_var v;
            let s = get_domain_cur subman flow in
            let (cs', s') = add_var ctx range v (cs, s) in
            let flow' = set_domain_cur cs' man flow |>
                        set_domain_cur s' subman
            in
            f acc v flow'
        in

        (* Fast bound check with intervals *)
        let rec fast () =
          debug "trying fast check";
          let v = man.ask ctx (Universal.Numeric.Query.QIntStepInterval offset) flow in
          match v with
          | None -> assert false
          | Some (itv, step) ->
            try
              debug "offset interval = %a" Universal.Numeric.Values.Int.print itv;
              let l, u = Universal.Numeric.Values.Int.get_bounds itv in
              if Z.geq l Z.zero && Z.leq (Z.add u cell_size) base_size then
                fold_interval l u step x0 flow
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
            let v = man.ask ctx (Universal.Numeric.Query.QIntStepInterval offset) flow in
            match v with
            | None -> assert false
            | Some (itv, step) ->
              try
                let l, u = Universal.Numeric.Values.Int.get_bounds itv in
                debug "interval = [%a, %a] mod %a" Z.pp_print l Z.pp_print u Z.pp_print step;
                fold_interval l u step acc flow
              with Universal.Numeric.Values.Int.Unbounded ->
                assert false
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
    in

    match base with
    | V v -> static_base_case (sizeof_type v.vtyp)
    | A {addr_kind = Libs.Stdlib.A_c_static_malloc s} -> static_base_case s
    | _ -> Framework.Exceptions.panic "base %a not supported" pp_base base



  (*==========================================================================*)
  (**                      {2 Transfer functions}                             *)
  (*==========================================================================*)

  let rec exec man subman ctx stmt flow =
    let range = stmt.srange in
    match skind stmt with
    | S_c_local_declaration(v, init) ->
      Init.init_local man ctx (init_manager man subman ctx) v init range flow |>
      return_flow

    | S_rename_var(v, v') ->
      assert false

    | S_remove_var ({vkind = V_orig | V_expand_cell _} as v) when is_c_int_type v.vtyp ->
      let u = get_domain_cur man flow in
      let v' = annotate_var_kind v in
      let u' = remove v' u in
      let stmt' = {stmt with skind = Universal.Ast.S_remove_var(v')} in
      let flow = set_domain_cur u' man flow in
      SubDomain.exec subman ctx stmt' flow |>
      oflow_compose (add_flow_mergers [mk_remove_var v' stmt.srange])

    | S_assign(lval, rval, mode) when is_c_scalar_type lval.etyp ->
      eval_list [rval; lval] (man.eval ctx) flow |>
      eval_to_orexec (fun el flow ->
          match el with
          | [rval; {ekind = E_var ({vkind = V_expand_cell (OffsetCell c)} as v)} as lval] ->
            let stmt' = {stmt with skind = S_assign(lval, rval, mode)} in
            SubDomain.exec subman ctx stmt' flow |>
            oflow_compose (remove_overlapping_cells v stmt.srange man subman ctx) |>
            oflow_compose (
              fun flow ->
                if is_c_int_type c.t then
                  let rmin, rmax = rangeof c.t in
                  let cond = range_cond lval rmin rmax (erange lval) in
                  let stmt'' = (mk_assume cond (tag_range range "assume range")) in
                  let () = debug "cell_expand assume %a" Framework.Pp.pp_stmt stmt'' in
                  match SubDomain.exec subman ctx stmt'' flow with
                  | Some flow -> flow
                  | None -> assert false
                else
                  flow
            ) |>
            oflow_compose (add_flow_mergers [mk_remove_var v stmt.srange])

          | [rval; {ekind = E_var ({vkind = V_expand_cell (AnyCell _)} as v)}] ->
            remove_overlapping_cells v stmt.srange man subman ctx flow |>
            return_flow

          | _ -> None
        ) (man.exec ctx) man.flow

    | _ -> None


  and eval man subman ctx exp flow =
    match ekind exp with
    | E_var ({vkind = V_orig} as v) when is_c_type v.vtyp ->
      debug "evaluating a scalar variable %a" pp_var v;
      let u = get_domain_cur man flow in
      let s = get_domain_cur subman flow in
      let v = annotate_var_kind v in
      let (u', s') = add_var ctx exp.erange v (u, s) in
      debug "new variable %a in %a" pp_var v print u';
      let flow'' = set_domain_cur u' man flow |>
                   set_domain_cur s' subman
      in
      re_eval_singleton (man.eval ctx) (Some (mk_var v exp.erange), flow'', []) |>
      add_eval_mergers []

    | E_c_deref(p) ->
      man.eval ctx (mk_c_resolve_pointer p exp.erange) flow |>
      eval_compose (fun pe flow ->
          match ekind pe with
          | E_c_points_to(E_p_fun fundec) ->
            oeval_singleton (Some ({exp with ekind = E_c_function fundec}), flow, []) |>
            add_eval_mergers []

          | E_c_points_to(E_p_var (base, offset, t)) ->
            debug "E_p_var(%a, %a, %a)" pp_base base pp_expr offset pp_typ t;
            fold_cells
              (fun acc v flow ->
                 debug "var case";
                 let exp' = {exp with ekind = E_var v} in
                 (** FIXME: filter flow with (p == &v) *)
                 oeval_singleton (Some (exp', []), flow, []) |>
                 oeval_join acc
              )
              (fun acc eflow -> debug "error case:@\n acc = %a@\n eflow = @[%a@]" (pp_oevals (fun fmt (e, _) -> pp_expr fmt e)) acc man.flow.print eflow; oeval_singleton (None, eflow, []) |> oeval_join acc)
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
            fold_cells
              (fun acc v flow ->
                 let exp' = {exp with ekind = E_var v} in
                 oeval_singleton (Some (exp', []), flow, []) |>
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


    | E_c_array_subscript(arr, idx) ->
      debug "array subscript to deref";
      let exp' = {exp with ekind = E_c_deref(mk_binop arr math_plus idx exp.erange ~etyp: arr.etyp)} in
      re_eval_singleton (man.eval ctx) (Some exp', flow, []) |>
      add_eval_mergers []

    | E_c_member_access(r, idx, f) ->
      let exp' = {exp with ekind = E_c_arrow_access(mk_c_address_of r r.erange, idx, f)} in
      re_eval_singleton (man.eval ctx) (Some exp', flow, []) |>
      add_eval_mergers []

    | _ -> None


  (*==========================================================================*)
  (**                    {2 Cells Initialization}                             *)
  (*==========================================================================*)

  and init_manager man subman ctx =
    Init.{
      (* Initialization of arrays *)
      array =  (fun a init is_global range flow ->
          match init with
          (* Local uninitialized arrays are kept ⟙ *)
          | None when not is_global -> return flow

          (* Otherwise: *)
          | None                   (* a. when the array is global and uninitialized *)
          | Some (C_init_list _)   (* b. when the array is initialized with a list of expressions *)
          | Some (Ast.C_init_expr {ekind = E_constant(C_c_string _)}) (* c. or when it consists in a string initialization *)
            ->
            (* just initialize a limited number of cells *)
            deeper (Some !opt_max_expand)

          | _ ->
            Framework.Exceptions.panic "Array initialization not supported"

        );

      (* Initialization of structs *)
      strct =  (fun s init is_global range flow ->
          match init with
          | None when not is_global -> return flow

          | None
          | Some (C_init_list _) ->
            deeper None

          | Some (C_init_expr e) ->
            man.exec ctx (mk_assign s e range) flow |>
            return

          | _ -> Framework.Exceptions.panic "Struct initialization not supported"
        );
    }

  and init man subman ctx prog flow =
    let flow = set_domain_cur empty man flow in
    match prog.prog_kind with
    | C_program(globals, _) ->
      let flow' = Init.fold_globals man ctx (init_manager man subman ctx) globals flow in
      ctx, flow'

    | _ -> ctx, flow


  let ask : type r. ('a, t) manager -> ('a, SubDomain.t) manager -> Framework.Context.context -> r Framework.Query.query -> 'a Framework.Flow.flow -> r option =
    fun man subman ctx query flow ->
    match query with
      | Query.QExtractVarBase {vkind = V_expand_cell (OffsetCell c)} ->
        Some (c.b, mk_z c.o (mk_fresh_range ()))

      | Query.QExtractVarBase {vkind = V_expand_cell (AnyCell c)} ->
        let range = mk_fresh_range () in
        let cell_size = sizeof_type c.t in
        let base_size = sizeof_type c.t in
        Some (c.b, mk_z_interval Z.zero Z.(base_size - cell_size) range)

      | _ -> None

  let refine man subman ctx channel flow = None



end

let setup () =
  register_domain name (module Domain);
  register_vkind_compare (fun next vk1 vk2 ->
      match vk1, vk2 with
      | V_expand_cell c1, V_expand_cell c2 -> compare_cell c1 c2
      | _ -> next vk1 vk2
    );
  register_pp_vkind (fun next fmt vk ->
      match vk with
      | V_expand_cell c -> pp_cell fmt c
      | _ -> next fmt vk
    );
  Framework.Options.register (
    "-cell-max-expand",
    Arg.Set_int opt_max_expand,
    " maximal number of expanded cells (default: 1)"
  )
