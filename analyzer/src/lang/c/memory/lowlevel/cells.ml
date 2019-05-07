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

(** Cell-based memory abstraction.

    This is an implementation of the cell abstraction presented in [1,2].
    Memory blocks are decomposed into collections of scalar fields, called
    "cells". A cell <b,o,t> is identified by a base memory block b where it
    resides, an integer offset o ∈ ℕ and a scalar type t.

    [1] A. Miné. Field-sensitive value analysis of embedded {C} programs with
    union types and pointer arithmetics. LCTES 2006.

    [2] A. Miné. Static analysis by abstract interpretation of concurrent
    programs. HDR thesis, École normale supérieure. 2013.

*)


open Mopsa
open Core.Sig.Stacked.Intermediate
open Universal.Ast
open Ast
open Universal.Zone
open Zone
open Common.Base
open Common.Points_to
module Itv = Universal.Numeric.Values.Intervals.Integer.Value


module Domain =
struct

  (** {2 Domain header} *)
  (** ***************** *)

  (** Memory cell *)
  type cell = {
    base   : Base.t;
    offset : Z.t;
    typ    : typ;
    primed : bool;
  }


  (** Total order of cells *)
  let compare_cell c1 c2 =
    Compare.compose [
      (fun () -> compare_base c1.base c2.base);
      (fun () -> Z.compare c1.offset c2.offset);
      (fun () -> compare_typ c1.typ c2.typ);
      (fun () -> compare c1.primed c2.primed);
    ]


  (** Pretty printer of cells *)
  let pp_cell fmt c =
    Format.fprintf fmt "⟨%a,%a,%a⟩%a"
      pp_base c.base
      Z.pp_print c.offset
      Pp.pp_c_type_short (remove_qual c.typ)
      (fun fmt () -> if c.primed then Format.pp_print_string fmt "'" else ()) ()


  (** Set of memory cells *)
  module CellSet = Framework.Lattices.Powerset.Make(struct
      type t = cell
      let compare = compare_cell
      let print = pp_cell
    end)


  (** Set of bases. Needed during unification to determine whether a
      missing cell belongs to an optional base *)
  module BaseSet = Framework.Lattices.Powerset.Make(Base)


  (** Abstract state *)
  type t = {
    cells: CellSet.t;
    bases: BaseSet.t;
  }

  let bottom = {
    cells = CellSet.bottom;
    bases = BaseSet.bottom;
  }

  let top = {
    cells = CellSet.top;
    bases = BaseSet.top;
  }

  let print fmt a =
    Format.fprintf fmt "cells: @[%a@]@\n"
      CellSet.print a.cells


  (** Domain identifier *)
  include GenDomainId(struct
      type typ = t
      let name = "c.memory.lowlevel.cells"
    end)


  (** Zone interface *)
  let interface = {
    iexec = {
      provides = [Z_c_low_level];
      uses = [Z_c_scalar];
    };

    ieval = {
      provides = [Z_c_low_level, Z_c_scalar];
      uses = [
        (Z_c_low_level, Z_c_scalar);
        (Z_c_scalar, Z_u_num);
        (Z_c_low_level, Z_u_num);
        (Z_c_low_level, Z_c_points_to);
      ];
    }
  }


  (** {2 Command-line options} *)
  (** ************************ *)

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


  (** {2 Utility functions for cells} *)
  (** =============================== *)

  (** [find_cell_opt f a] finds the cell in [a.cells] verifying
      predicate [f]. None is returned if such cells is not found. *)
  let find_cell_opt (f:cell->bool) (a:t) =
    CellSet.apply (fun r ->
        let exception Found of cell in
        try
          let () = CellSet.Set.iter (fun c ->
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
    CellSet.apply (fun r ->
        CellSet.Set.filter f r |>
        CellSet.Set.elements
      )
      []
      a.cells

  (** [get_overlappings c a] returns the list of cells in [a.cells]
      that overlap with [c]. *)
  let get_overlappings c (a:t) =
    find_cells (fun c' ->
        compare_cell c c' <> 0 &&
        (
          let cell_range c = (c.offset, Z.add c.offset (sizeof_type c.typ))
          in
          let check_overlap (a1, b1) (a2, b2) = Z.lt (Z.max a1 a2) (Z.min b1 b2) in
          compare_base (c.base) (c'.base) = 0 &&
          check_overlap (cell_range c) (cell_range c')
        )
      ) a

  let mk_cell base offset ?(primed=false) typ =
    { base; offset; typ = remove_typedef_qual typ; primed }

  (** {2 Cell-variable binding} *)
  (** ************************* *)

  (** The bindings between cells and their associated scalar variables
      are kept in a flow-insensitive context *)

  module CellEquiv = Equiv.Make
      (struct
        type t = cell
        let compare = compare_cell
        let print = pp_cell
      end)
      (Var)

  let ctx_key =
    let module C = Context.GenUnitKey(
      struct
        type t = CellEquiv.t
        let print fmt m =
          Format.fprintf fmt "Cells<=>vars: @[%a@]" CellEquiv.print m
      end
      )
    in
    C.key

  let get_ctx ctx =
    try Context.ufind ctx_key ctx
    with _ -> CellEquiv.empty

  let set_ctx bindings ctx =
    Context.uadd ctx_key bindings ctx

  let init_ctx ctx =
    set_ctx CellEquiv.empty ctx


  (** Create a scalar variable corresponding to a cell *)
  let mk_cell_var (c:cell) ctx : var * Context.uctx =
    let bindings = get_ctx ctx in
    (* Check if the binding already exists *)
    try CellEquiv.find_l c bindings, ctx
    with Not_found ->
      (* Create a new variable *)
      let () = Format.fprintf Format.str_formatter
          "{%a,%a,%a}%a"
          pp_base c.base
          Z.pp_print c.offset
          Pp.pp_c_type_short (remove_qual c.typ)
          (fun fmt () -> if c.primed then Format.pp_print_string fmt "'" else ()) ()
      in
      let name = Format.flush_str_formatter () in
      let v = mkfresh (fun uid ->
          name, name ^ ":" ^ (string_of_int uid)
        ) c.typ ()
      in
      v, set_ctx (CellEquiv.add (c,v) bindings) ctx

  let mk_cell_var_with_flow c flow =
    let ctx = Flow.get_unit_ctx flow in
    let v, ctx' = mk_cell_var c ctx in
    let flow' = Flow.set_unit_ctx ctx' flow in
    v, flow'


  (** {2 Unification of cells} *)
  (** ======================== *)

  (** [phi c a range] returns a constraint expression over cell [c] found in [a] *)
  let phi (c:cell) (a:t) ctx range : expr option * Context.uctx =
    match find_cell_opt (fun c' -> compare_cell c c' = 0) a with
    | Some c ->
      let v, ctx = mk_cell_var c ctx in
      Some (mk_var v range), ctx

    | None ->
      match find_cell_opt
              (fun c' ->
                 is_c_int_type c'.typ &&
                 Z.equal (sizeof_type c'.typ) (sizeof_type c.typ) &&
                 compare_base c.base (c'.base) = 0 &&
                 Z.equal c.offset c'.offset &&
                 c.primed = c'.primed
              ) a
      with
      | Some (c') ->
        let v, ctx= mk_cell_var c' ctx in
        Some (wrap_expr
                (mk_var v range)
                (int_rangeof c.typ)
                range
             ),
        ctx

      | None ->
        match
          find_cell_opt ( fun c' ->
              let b = Z.sub c.offset c'.offset in
              Z.geq b Z.zero &&
              compare_base c.base (c'.base) = 0 &&
              Z.lt b (sizeof_type c'.typ) &&
              is_c_int_type c'.typ &&
              compare_typ (remove_typedef_qual c.typ) (T_c_integer(C_unsigned_char)) = 0 &&
              c.primed = c'.primed
            ) a
        with
        | Some (c') ->
          let b = Z.sub c.offset c'.offset in
          let base = (Z.pow (Z.of_int 2) (8 * Z.to_int b))  in
          let v, ctx = mk_cell_var c' ctx in
          Some (_mod
                  (div (mk_var v range) (mk_z base range) range)
                  (mk_int 256 range)
                  range
               ),
          ctx

        | None ->
          let exception NotPossible in
          try
            if is_c_int_type c.typ then
              let t' = T_c_integer(C_unsigned_char) in
              let n = Z.to_int (sizeof_type (c.typ)) in
              let rec aux i l =
                if i < n then
                  let tobein = (fun cc ->
                      {
                        base = cc.base;
                        offset = Z.add c.offset (Z.of_int i);
                        typ = t';
                        primed = cc.primed;
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
              let _,e,ctx = List.fold_left (fun (time, res, ctx) x ->
                  let v, ctx = mk_cell_var x ctx in
                  let res' =
                    add
                      (mul (mk_z time range) (mk_var v range) range)
                      res
                      range
                  in
                  let time' = Z.mul time (Z.of_int 256) in
                  time',res',ctx
                ) (Z.of_int 1,(mk_int 0 range),ctx) ll
              in
              Some e,ctx
            else
              raise NotPossible
          with
          | NotPossible ->
            match c.base with
            | S s ->
              let len = String.length s in
              if Z.equal c.offset (Z.of_int len) then
                Some (mk_zero range), ctx
              else
                Some (mk_int (String.get s (Z.to_int c.offset) |> int_of_char) range), ctx

            | _ ->
              if is_c_int_type c.typ then
                let a,b = rangeof c.typ in
                Some (mk_z_interval a b range), ctx
              else if is_c_float_type c.typ then
                let prec = get_c_float_precision c.typ in
                Some (mk_top (T_float prec) range), ctx
              else if is_c_pointer_type c.typ then
                panic_at range ~loc:__LOC__ "phi called on a pointer cell %a" pp_cell c
              else
                None, ctx

  (** Add a cell in the underlying domain *)
  let add_cell c a range man ctx s =
    if CellSet.mem c a.cells ||
       not (is_c_scalar_type c.typ) ||
       not (BaseSet.mem c.base a.bases)
    then s
    else
      let v, _ = mk_cell_var c ctx in
      let s' = man.sexec ~zone:Z_c_scalar (mk_add (mk_var v range) range) ctx s in
      if is_c_pointer_type c.typ then s'
      else
        match phi c a ctx range with
        | Some e, _ ->
          let stmt = mk_assume (mk_binop (mk_var v range) O_eq e ~etyp:u8 range) range in
          man.sexec ~zone:Z_c_scalar stmt ctx s'

        | None, _ ->
          s'


  (** [unify a a'] finds non-common cells in [a] and [a'] and adds them. *)
  let unify man ctx (a,s) (a',s') =
    let range = mk_fresh_range () in
    if CellSet.is_empty a.cells  then s, s' else
    if CellSet.is_empty a'.cells then s, s'
    else
      try
        let diff' = CellSet.diff a.cells a'.cells in
        let diff = CellSet.diff a'.cells a.cells in
        CellSet.fold (fun c s ->
            add_cell c a range man ctx s
          ) diff s
        ,
        CellSet.fold (fun c s' ->
            add_cell c a' range man ctx s'
          ) diff' s'
      with Top.Found_TOP ->
        s, s'


  (** {2 Lattice operators} *)
  (** ********************* *)

  let is_bottom _ = false

  let subset man ctx (a,s) (a',s') =
    let s, s' = unify man ctx (a, s) (a', s') in
    (true, s, s')

  let join man ctx (a,s) (a',s') =
    let s, s' = unify man ctx (a,s) (a',s') in
    let a = {
      cells = CellSet.join a.cells a'.cells;
      bases = BaseSet.join a.bases a'.bases;
    }
    in
    (a, s, s')

  let meet man ctx (a,s) (a',s') =
    join man ctx (a,s) (a',s')

  let widen man ctx (a,s) (a',s') =
    let (a, s, s') = join man ctx (a,s) (a',s') in
    (a, s, s', true)

  let merge pre (a,log) (a',log') =
    assert false


  (** {2 Cell expansion} *)
  (** ****************** *)

  (** Expand a pointer dereference into a finite set of cells. Return
      None if pointer is ⊤ *)
  let expand p range man flow : (cell option, 'a) eval =
    (* Get the base and offset of the pointed block *)
    man.eval ~zone:(Z_c_low_level, Z_c_points_to) p flow |>
    Eval.bind @@ fun pt flow ->

    match ekind pt with
    | E_c_points_to P_top ->
      Eval.singleton None flow

    | E_c_points_to P_null ->
      let flow = raise_alarm Alarms.ANullDeref range ~bottom:true man.lattice flow in
      Eval.empty_singleton flow

    | E_c_points_to P_invalid ->
      let flow = raise_alarm Alarms.AInvalidDeref range ~bottom:true man.lattice flow in
      Eval.empty_singleton flow

    | E_c_points_to (P_fun f) ->
      panic_at range "%a can not be dereferenced to a cell; it points to function %s"
        pp_expr p
        f.c_func_org_name

    | E_c_points_to P_block (base, offset) ->
      (* Get the size of the base *)
      eval_base_size base range man flow |>
      Eval.bind @@ fun size flow ->

      (* Convert the size and the offset to numeric *)
      man.eval ~zone:(Z_c_scalar,Z_u_num) size flow |>
      Eval.bind @@ fun size flow ->

      man.eval ~zone:(Z_c_low_level,Z_u_num) offset flow |>
      Eval.bind @@ fun offset flow ->

      (* Check the bounds: offset ∈ [0, size - |typ|] *)
      let typ = under_pointer_type p.etyp in
      let elm = sizeof_type typ in
      let cond = mk_in offset (mk_zero range)
          (sub size (mk_z elm range) range ~typ:T_int)
          range
      in
      assume_eval ~zone:Z_u_num cond
        ~fthen:(fun flow ->
            (* Compute the interval and create a finite number of cells *)
            let itv = man.ask (Itv.Q_interval offset) flow in
            let step = Z.one in

            let l, u = Itv.bounds_opt itv in

            (* l >= 0 *)
            let l =
              match l with
              | None -> Z.zero
              | Some l -> Z.max l Z.zero
            in

            (* u <= l + !opt_expand *)
            let u =
              match u with
              | None -> Z.add l (Z.of_int !opt_expand)
              | Some u -> Z.min u (Z.add l (Z.of_int !opt_expand))
            in

            (* Iterate over [l, u] *)
            let rec aux i o =
              if Z.gt o u
              then []
              else
                let flow' = man.exec ~zone:Z_u_num (mk_assume (mk_binop offset O_eq (mk_z o range) range) range) flow in
                let c = mk_cell base o typ in
                Eval.singleton (Some c) flow' :: aux (i + 1) (Z.add o step)
            in
            let evals = aux 0 l in
            Eval.join_list evals
          )
        ~felse:(fun flow ->
            let flow = raise_alarm Alarms.AOutOfBound range ~bottom:true man.lattice flow in
            Eval.empty_singleton flow
          )
        man flow



    | _ -> assert false




  (** {2 Initial state} *)
  (** ***************** *)

  let init prog man flow =
    set_domain_env T_cur { cells = CellSet.empty; bases = BaseSet.empty } man flow |>
    Flow.set_unit_ctx (
      Flow.get_unit_ctx flow |>
      init_ctx
    )


  (** {2 Abstract transformers} *)
  (** ************************* *)

  (** 𝕊⟦ type v = init; ⟧  *)
  let declare v range man flow =
    (* Since we are in the Z_low_level zone, we assume that init has
       been translated by a structured domain into a flatten
       initialization *)
    let vinfo, flat_init =
      match v.vkind with
      | V_c ({ var_init = Some (C_init_flat l) } as vinfo) -> vinfo, l
      | _ -> assert false
    in

    (* Add the base *)
    let flow = map_domain_env T_cur (fun a ->
        { a with bases = BaseSet.add (V v) a.bases }
      ) man flow
    in

    (* Initialize cells, but expand at most !opt_expand cells, as
       defined by the option -cell-expand *)
    let rec aux o i l flow =
      if i = !opt_expand
      then Post.return flow
      else
        let c, init, tl, o' =
          match l with
          | C_flat_expr e :: tl ->
            let c = mk_cell (V v) o e.etyp in
            let init = Some (C_init_expr e) in
            c, init, tl, Z.add o (sizeof_type e.etyp)

          | C_flat_none(n) :: tl when is_c_scalar_type v.vtyp &&
                                      Z.equal n (sizeof_type v.vtyp)
            ->
            let c = mk_cell (V v) o v.vtyp in
            let init = None in
            c, init, tl, Z.add o n

          | _ -> assert false
        in
        (* Evaluate the initialization into a scalar expression *)
        (
          match init with
          | None -> Eval.singleton None flow

          | Some (C_init_expr e) ->
            man.eval ~zone:(Z_c_low_level,Z_c_scalar) e flow |>
            Eval.bind @@ fun e flow ->
            Eval.singleton (Some (C_init_expr e)) flow

          | _ -> assert false
        )
        |>
        post_eval man @@ fun init flow ->

        (* Add the cell *)
        let flow = map_domain_env T_cur (fun a ->
            { a with cells = CellSet.add c a.cells }
          ) man flow
        in

        (* Initialize the associated variable *)
        let v, flow = mk_cell_var_with_flow c flow in
        let v = { v with vkind = V_c { vinfo with var_init = init } } in
        let stmt = mk_c_declaration v range in
        man.exec_sub ~zone:Z_c_scalar stmt flow |>

        Post.bind @@ fun flow ->
        aux o' (i + 1) tl flow
    in
    aux Z.zero 0 flat_init flow

  (** 𝕊⟦ *p = e; ⟧ *)
  let assign p e mode range man flow =
    (* Expand *p into cells *)
    expand p range man flow |>
    post_eval man @@ fun c flow ->

    match c with
    | None ->
      (* ⊤ pointer !!! *)
      panic_at range "lval *%a can not be expanded" pp_expr p

    | Some c ->
      let v, flow = mk_cell_var_with_flow c flow in

      man.eval ~zone:(Z_c_low_level,Z_c_scalar) e flow |>
      post_eval man @@ fun e flow ->

      let stmt = mk_assign (mk_var v ~mode range) e range in
      man.exec_sub ~zone:Z_c_scalar stmt flow


  (** 𝕊⟦ ?e ⟧ *)
  let assume e range man flow =
    assert false

  let exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration v ->
      declare v stmt.srange man flow |>
      Option.return

    | S_assign(({ekind = E_var(var, mode)} as lval), e) ->
      assign (mk_c_address_of lval lval.erange) e mode stmt.srange man flow |>
      Option.return

    | S_assign(({ekind = E_c_deref(p)}), e) ->
      assign p e STRONG stmt.srange man flow |>
      Option.return

    | S_assume(e) ->
      assume e stmt.srange man flow |>
      Option.return

    | _ -> None


  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (** 𝔼⟦ *p ⟧ *)
  let deref p mode range man flow =
    (* Expand *p into cells *)
    expand p range man flow |>
    Eval.bind @@ fun c flow ->

    match c with
    | None ->
      (* ⊤ pointer => use the whole value interval *)
      Eval.singleton (mk_top (under_pointer_type p.etyp) range) flow

    | Some c ->
      let v, flow = mk_cell_var_with_flow c flow in
      Eval.singleton (mk_var v ~mode range) flow



  let eval zone exp man flow =
    match ekind exp with
    | E_var (v,mode) when is_c_scalar_type v.vtyp ->
      let c = mk_cell (V v) Z.zero v.vtyp in
      let vv, flow = mk_cell_var_with_flow c flow in
      Eval.singleton (mk_var vv ~mode exp.erange) flow |>
      Option.return

    | E_c_deref p ->
      deref p STRONG exp.erange man flow |>
      Option.return

    | _ -> None


  (** {2 Communication handlers} *)
  (** ************************** *)

  let ask query man flow = None


  let refine channel man flow =
    assert false

end

let () =
  Core.Sig.Stacked.Intermediate.register_stack (module Domain)
