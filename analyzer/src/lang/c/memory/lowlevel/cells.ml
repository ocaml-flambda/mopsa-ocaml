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
open Stubs.Ast
open Ast
open Universal.Zone
open Zone
open Common.Base
open Common.Points_to
open Common.Alarms
module Itv = Universal.Numeric.Values.Intervals.Integer.Value


module Domain =
struct


  (** {2 Memory cells} *)
  (** **************** *)


  (** Type of a cell *)
  type cell_typ =
    | Numeric of typ
    | Pointer


  (** Cells *)
  type cell = {
    base   : Base.t;
    offset : Z.t;
    typ    : cell_typ;
  }


  (** Total order of cell types *)
  let compare_cell_typ t1 t2 =
    match t1, t2 with
    | Numeric tt1, Numeric tt2 -> compare_typ tt1 tt2
    | Pointer, Pointer -> 0
    | _ -> compare t1 t2


  (** Total order of cells *)
  let compare_cell c1 c2 =
    Compare.compose [
      (fun () -> compare_base c1.base c2.base);
      (fun () -> Z.compare c1.offset c2.offset);
      (fun () -> compare_cell_typ c1.typ c2.typ);
    ]


  (** Pretty printer of cell types *)
  let pp_cell_typ fmt t =
    match t with
    | Numeric tt -> Pp.pp_c_type_short fmt tt
    | Pointer -> Format.fprintf fmt "ptr"


  (** Pretty printer of cells *)
  let pp_cell fmt c =
    Format.fprintf fmt "⟨%a,%a,%a⟩"
      pp_base c.base
      Z.pp_print c.offset
      pp_cell_typ c.typ


  (** Create a cell *)
  let mk_cell base offset typ =
    {
      base;
      offset;
      typ =
        if is_c_num_type typ then Numeric (remove_typedef_qual typ)
        else if is_c_pointer_type typ then Pointer
        else panic "cell can not be created with type %a" pp_typ typ;
    }


  (** Return the type of a cell *)
  let cell_type c =
    match c.typ with
    | Numeric t -> t
    | Pointer -> T_c_pointer T_c_void


  (** Check if a cell is numeric *)
  let is_numeric_cell c =
    match c.typ with
    | Numeric _ -> true
    | Pointer   -> false


  (** Check if a cell is an integer cell *)
  let is_int_cell c =
    match c.typ with
    | Numeric t -> is_c_int_type t
    | Pointer   -> false


  (** Check if a cell is a float cell *)
  let is_float_cell c =
    match c.typ with
    | Numeric t -> is_c_float_type t
    | Pointer   -> false


  (** Check if a cell is a pointer *)
  let is_pointer_cell c =
    match c.typ with
    | Numeric _ -> false
    | Pointer   -> true


  (** Size of a cell in bytes *)
  let sizeof_cell c = sizeof_type (cell_type c)


  (** Value range of an integer cell *)
  let rangeof_int_cell c =
    assert(is_int_cell c);
    rangeof (cell_type c)



  (** {2 Cell variables} *)
  (** ****************** *)

  type var_kind +=
    | V_c_cell of cell


  let () =
    register_var {

      print = (fun next fmt v ->
          match v.vkind with
          | V_c_cell c -> pp_cell fmt c
          | _ -> next fmt v
        );

      compare = (fun next v1 v2 ->
          match v1.vkind, v2.vkind with
          | V_c_cell c1, V_c_cell c2 -> compare_cell c1 c2
          | _ -> next v1 v2
        );
    }


  (** Construct a unique name associated to a cell *)
  let mk_cell_uniq_name c =
    match c.base.base_kind with
    | Var { vkind = V_c_cell c } ->
        panic "recursive creation of cell %a" pp_cell c

    | _ ->
      let () =
        Format.fprintf Format.str_formatter "⟨%s,%a,%a⟩"
          (base_uniq_name c.base)
          Z.pp_print c.offset
          pp_cell_typ c.typ
      in
      Format.flush_str_formatter ()


  (** Create a variable from a cell *)
  let mk_cell_var c : var =
    let name = mk_cell_uniq_name c in
    mkv name (V_c_cell c) (cell_type c) ~mode:(base_mode c.base)


  (** Create a variable from a numeric cell *)
  let mk_numeric_cell_var_expr c ?(mode=None) range : expr =
    assert(is_numeric_cell c);
    let v = mk_cell_var c in
    mk_var v ~mode range


  (** Create a variable from a pointer cell *)
  let mk_pointer_cell_var_expr c ?(mode=None) typ range : expr =
    assert(is_pointer_cell c);
    let v = mk_cell_var c in
    if is_c_void_type (under_type typ)
    then mk_var v ~mode range
    else mk_c_cast (mk_var v ~mode range) typ range



  (** {2 Domain header} *)
  (** ***************** *)


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
      type nonrec t = t
      let name = "c.memory.lowlevel.cells"
    end)


  (** Zone interface *)
  let interface = {
    iexec = {
      provides = [Z_c_low_level];
      uses = [
        Z_c;
        Z_c_scalar
      ];
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

  let alarms = [A_c_out_of_bound_cls; A_c_null_deref_cls; A_c_use_after_free_cls; A_c_invalid_deref_cls]


  (** {2 Command-line options} *)
  (** ************************ *)

  (** Maximal number of expanded cells when dereferencing a pointer *)
  let opt_deref_expand = ref 1

  let () =
    register_domain_option name {
      key = "-cell-deref-expand";
      category = "C";
      doc = " maximal number of expanded cells when dereferencing a pointer";
      spec = ArgExt.Set_int opt_deref_expand;
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

  (** Return the list of cells in [a.cells] - other than [c] - that
      overlap with [c]. *)
  let get_cell_overlappings c (a:t) =
    let check_overlap c1 c2 =
      let (a1,b1) = (c1.offset, Z.add c1.offset (sizeof_cell c1)) in
      let (a2,b2) = (c2.offset, Z.add c2.offset (sizeof_cell c2)) in
      Z.lt (Z.max a1 a2) (Z.min b1 b2)
    in
    find_cells (fun c' ->
        compare_cell c c' <> 0 &&
        compare_base (c.base) (c'.base) = 0 &&
        (
          check_overlap c c'
        )
      ) a

  (** Return the list of cells in [a.cells] in [base] that overlap in
      offset interval [itv]. *)
  let get_region_overlappings base itv (a:t) =
    find_cells (fun c ->
        compare_base (c.base) base = 0 &&
        (
          let itv' = Itv.of_z c.offset (Z.add c.offset (Z.pred (sizeof_cell c))) in
          not (Itv.meet itv itv' |> Itv.is_bottom)
        )
      ) a



  (** {2 Unification of cells} *)
  (** ======================== *)

  (** [phi c a range] returns a constraint expression over cell [c] found in [a] *)
  let phi (c:cell) (a:t) range : expr option =
    match find_cell_opt (fun c' -> compare_cell c c' = 0) a with
    | Some c ->
      None

    | None ->
      if not (is_c_int_type @@ cell_type c) then None else
      match find_cell_opt
              (fun c' ->
                 is_int_cell c' &&
                 Z.equal (sizeof_cell c') (sizeof_cell c) &&
                 compare_base c.base (c'.base) = 0 &&
                 Z.equal c.offset c'.offset
              ) a
      with
      | Some (c') ->
        let v = mk_numeric_cell_var_expr c' range in
        Some (wrap_expr v (rangeof_int_cell c) range)

      | None ->
        match
          find_cell_opt ( fun c' ->
              let b = Z.sub c.offset c'.offset in
              Z.geq b Z.zero &&
              compare_base c.base (c'.base) = 0 &&
              Z.lt b (sizeof_cell c') &&
              is_int_cell c' &&
              compare_typ (cell_type c) (T_c_integer(C_unsigned_char)) = 0
            ) a
        with
        | Some (c') ->
          let b = Z.sub c.offset c'.offset in
          let base = (Z.pow (Z.of_int 2) (8 * Z.to_int b))  in
          let v = mk_numeric_cell_var_expr c' range in
          Some (
            (_mod
               (div v (mk_z base range) range)
               (mk_int 256 range)
               range
            )
          )

        | None ->
          let exception NotPossible in
          try
            if is_int_cell c then
              let t' = T_c_integer(C_unsigned_char) in
              let n = Z.to_int (sizeof_cell c) in
              let rec aux i l =
                if i < n then
                  let tobein = (fun cc ->
                      {
                        base = cc.base;
                        offset = Z.add c.offset (Z.of_int i);
                        typ = Numeric t';
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
                  let v = mk_numeric_cell_var_expr x range in
                  let res' =
                    add
                      (mul (mk_z time range) v range)
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
            match c.base.base_kind with
            | String s ->
              let len = String.length s in
              if Z.equal c.offset (Z.of_int len) then
                Some (mk_zero range)
              else
                Some (mk_int (String.get s (Z.to_int c.offset) |> int_of_char) range)

            | _ ->
              if is_int_cell c then
                let a,b = rangeof_int_cell c in
                Some (mk_z_interval a b range)
              else if is_float_cell c then
                let prec = get_c_float_precision (cell_type c) in
                Some (mk_top (T_float prec) range)
              else if is_pointer_cell c then
                panic_at range ~loc:__LOC__ "phi called on a pointer cell %a" pp_cell c
              else
                None

  (** Add a cell in the underlying domain using the simplified manager *)
  let add_cell_simplified c a range man ctx s =
    if CellSet.mem c a.cells ||
       (* not (is_c_scalar_type c.typ) || *)
       not (BaseSet.mem c.base a.bases)
    then s
    else
      let v = mk_cell_var c in
      let s' = man.sexec ~zone:Z_c_scalar (mk_add_var v range) ctx s in
      if is_pointer_cell c
      then s'
      else
        match phi c a range with
        | Some e ->
          let stmt = mk_assume (mk_binop (mk_var v range) O_eq e ~etyp:u8 range) range in
          man.sexec ~zone:Z_c_scalar stmt ctx s'

        | None ->
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
            add_cell_simplified c a range man ctx s
          ) diff s
        ,
        CellSet.fold (fun c s' ->
            add_cell_simplified c a' range man ctx s'
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

  (** Possible results of a cell expansion *)
  type expansion =
    | Cell of cell * mode option
    | Region of base * Itv.t
    | Top


  let is_interesting_base = function
    | { base_valid = true; base_kind = Var _ }  -> true
    | { base_valid = true; base_kind = Addr _ } -> true

    | _ -> false


  let eval_pointed_base_offset ptr range man flow =
    man.eval ptr ~zone:(Zone.Z_c_low_level, Z_c_points_to) flow >>$ fun pt flow ->

    match ekind pt with
    | E_c_points_to P_null ->
      raise_c_null_deref_alarm ptr range man flow |>
      Cases.empty_singleton

    | E_c_points_to P_invalid ->
      raise_c_invalid_deref_alarm ptr range man flow |>
      Cases.empty_singleton

    | E_c_points_to (P_block ({ base_kind = Addr _; base_valid = false; base_invalidation_range = Some r }, offset, _)) ->
      raise_c_use_after_free_alarm ptr r range man flow |>
      Cases.empty_singleton

    | E_c_points_to (P_block ({ base_kind = Var v; base_valid = false; base_invalidation_range = Some r }, offset, _)) ->
      raise_c_dangling_deref_alarm ptr v r range man flow |>
      Cases.empty_singleton

    | E_c_points_to (P_block (base, offset, mode)) ->
      Cases.singleton (Some (base, offset, mode)) flow

    | E_c_points_to P_top ->
      Cases.singleton None flow

    | _ -> assert false


  (** Expand a pointer dereference into a cell. *)
  let expand p range man flow : ('a, expansion) cases =
    eval_pointed_base_offset p range man flow >>$ fun pp flow ->
    match pp with
    | None ->
      Soundness.warn_at range "ignoring ⊤ pointer %a" pp_expr p;
      Cases.singleton Top flow

    | Some (base,offset,mode) ->
      let typ = under_type p.etyp |> void_to_char in
      let elm = sizeof_type typ in

      (* Get the size of the base *)
      eval_base_size base range man flow >>$ fun size flow ->

      (* Convert the size and the offset to numeric *)
      man.eval ~zone:(Z_c_scalar,Z_u_num) size flow >>$ fun size flow ->
      man.eval ~zone:(Z_c_scalar,Z_u_num) offset flow >>$ fun offset flow ->

      (* Try static check *)
      match expr_to_z size, expr_to_z offset with
      | Some s, Some o ->
        if Z.gt elm s then
          let flow = raise_c_out_bound_alarm ~base ~offset ~size range man flow in
          Cases.empty_singleton flow
        else
        if Z.leq Z.zero o &&
           Z.leq o (Z.sub s elm)
        then
          let c = mk_cell base o typ in
          Cases.singleton (Cell (c,mode)) flow
        else
          let flow = raise_c_out_bound_alarm ~base ~offset ~size range man flow in
          Cases.empty_singleton flow

      | _ ->

        (* Check the bounds: offset ∈ [0, size - |typ|] *)
        let cond = mk_in offset (mk_zero range)
            (sub size (mk_z elm range) range ~typ:T_int)
            range
        in
        assume ~zone:Z_u_num cond
          ~fthen:(fun flow ->
              (* Expand only interesting bases *)
              if not @@ is_interesting_base base
              then Cases.singleton Top flow
              else
                (* Compute the interval and create a finite number of cells *)
                let itv, (stride,_) = man.ask (Universal.Numeric.Common.Q_int_congr_interval offset) flow in
                let step = if Z.equal stride Z.zero then Z.one else stride in

                let l, u = Itv.bounds_opt itv in

                let l =
                  match l with
                  | None -> Z.zero
                  | Some l -> Z.max l Z.zero
                in

                let u =
                  match u, expr_to_z size with
                  | None, Some size -> Z.sub size elm
                  | Some u, Some size -> Z.min u (Z.sub size elm)
                  | Some u, None -> u
                  | None, None ->
                    (* No bound found for the offset and the size is not constant, so
                       get an upper bound of the size.
                    *)
                    let size_itv = man.ask (Universal.Numeric.Common.mk_int_interval_query size) flow in
                    let ll, uu = Itv.bounds_opt size_itv in
                    match uu with
                    | Some size -> Z.sub size elm
                    | None ->
                      (* We are in trouble: the size is not bounded!
                         So we assume that it does not exceed the range of unsigned long, usually used for size_t
                      *)
                      let _, uuu = rangeof ul in
                      Soundness.warn_at range
                        "size of %a is unbounded and is assumed to %a"
                        pp_base base
                        Z.pp_print uuu
                      ;
                      Z.sub uuu elm
                in

                let nb = Z.div (Z.sub u l) step in
                if nb > Z.of_int !opt_deref_expand then
                  (* top *)
                  let region = Region (base, Itv.of_z l u) in
                  let flow = man.exec ~zone:Z_u_num (mk_assume (mk_binop offset O_ge (mk_z l range) range) range) flow in
                  if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom
                  then Cases.empty_singleton flow
                  else Cases.singleton region flow
                else
                (* Iterate over [l, u] *)
                  let rec aux o =
                    let flow = man.exec ~zone:Z_u_num (mk_assume (mk_binop offset O_eq (mk_z o range) range) range) flow in
                    if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom
                    then aux (Z.add o step)
                    else
                      let c = mk_cell base o typ in
                      Cases.singleton (Cell (c,mode)) flow :: aux (Z.add o step)
                  in
                  let evals = aux l in
                  Cases.join_list ~empty:(fun () -> Cases.empty_singleton flow) evals
                    (*
                (* Iterate over [l, u] *)
                let rec aux i o =
                  if i = !opt_deref_expand
                  then
                    if Z.gt o u
                    then []
                    else
                      let region = Region (base, Itv.of_z o u) in
                      let flow = man.exec ~zone:Z_u_num (mk_assume (mk_binop offset O_ge (mk_z o range) range) range) flow in
                      if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom
                      then []
                      else [Cases.singleton region flow]
                  else
                    let flow = man.exec ~zone:Z_u_num (mk_assume (mk_binop offset O_eq (mk_z o range) range) range) flow in
                    if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom
                    then aux (i + 1) (Z.add o step)
                    else
                      let c = mk_cell base o typ in
                      Cases.singleton (Cell (c,mode)) flow :: aux (i + 1) (Z.add o step)
                in
                let evals = aux 0 l in
                Cases.join_list ~empty:(fun () -> Cases.empty_singleton flow) evals
*)
            )
          ~felse:(fun flow ->
              let flow = raise_c_out_bound_alarm ~base ~offset ~size range man flow in
              Cases.empty_singleton flow
            )
          man flow


  let add_base b man flow =
    let a = get_env T_cur man flow in
    let aa = { a with bases = BaseSet.add b a.bases } in
    set_env T_cur aa man flow


  (** Add a cell and its constraints *)
  let add_cell c range man flow =
    let flow = add_base c.base man flow in
    let a = get_env T_cur man flow in

    if CellSet.mem c a.cells (* || not (is_c_scalar_type c.typ) *)
    then Post.return flow
    else
      let v = mk_cell_var c in
      man.post ~zone:Z_c_scalar (mk_add_var v range) flow |>
      Post.bind @@ fun flow ->

      if is_pointer_cell c
      then
        set_env T_cur { a with cells = CellSet.add c a.cells } man flow |>
        Post.return
      else
        begin
          match phi c a range with
          | Some e ->
            let stmt = mk_assume (mk_binop (mk_var v range) O_eq e ~etyp:u8 range) range in
            man.post ~zone:Z_c_scalar stmt flow

          | None -> Post.return flow
        end
        |>
        Post.bind @@ fun flow ->
        set_env T_cur { a with cells = CellSet.add c a.cells } man flow |>
        Post.return

  (* Remove a cell and its associated scalar variable *)
  let remove_cell c range man flow =
    let flow = map_env T_cur (fun a ->
        { a with cells = CellSet.remove c a.cells }
      ) man flow
    in
    let v = mk_cell_var c in
    let stmt = mk_remove_var v range in
    man.post ~zone:Z_c_scalar stmt flow


  (** Remove cells overlapping with cell [c] *)
  let remove_cell_overlappings c range man flow =
    let a = get_env T_cur man flow in
    let overlappings = get_cell_overlappings c a in

    List.fold_left (fun acc c' ->
        Post.bind (remove_cell c' range man) acc
      ) (Post.return flow) overlappings


  (** Remove cells overlapping with cell [c] *)
  let remove_region_overlappings base itv range man flow =
    let a = get_env T_cur man flow in
    let overlappings = get_region_overlappings base itv a in

    List.fold_left (fun acc c' ->
        Post.bind (remove_cell c' range man) acc
      ) (Post.return flow) overlappings


  let rename_cell c1 c2 range man flow =
    let v1 = mk_cell_var c1 in
    let v2 = mk_cell_var c2 in
    let flow = map_env T_cur (fun a ->
        { a with cells = CellSet.remove c1 a.cells |>
                         CellSet.add c2 }
      ) man flow in
    let stmt = mk_rename_var v1 v2 range in
    man.post ~zone:Z_c_scalar stmt flow


  let assign_cell c e mode range man flow =
    let flow = map_env T_cur (fun a ->
        { a with cells = CellSet.add c a.cells }
      ) man flow
    in

    let v = mk_cell_var c in
    let vv = mk_var v ~mode range in
    let stmt = mk_assign vv e range in
    man.post ~zone:Z_c_scalar stmt flow >>= fun _ flow ->
    remove_cell_overlappings c range man flow


  let assign_region base itv range man flow =
    remove_region_overlappings base itv range man flow


  (** Remove all cells already realized *)
  let remove_all_cells range man flow =
    let a = get_env T_cur man flow in

    if CellSet.is_top a.cells
    then Post.return flow

    else CellSet.fold (fun c acc ->
        Post.bind (remove_cell c range man) acc
      ) a.cells (Post.return flow)


  let expand_cell c cl range man flow =
    (* Add cells in cl to the state *)
    let flow = map_env T_cur (fun a ->
        { a with cells = CellSet.union (CellSet.of_list cl) a.cells }
      ) man flow in
    (* Expand cell variables *)
    let v = mk_cell_var c in
    let vl = List.map mk_cell_var cl in
    let stmt = mk_expand_var v vl range in
    man.post stmt ~zone:Z_c_scalar flow


  let forget_cell c range man flow =
    let v = mk_cell_var c in
    let stmt = mk_forget_var v range in
    man.post stmt ~zone:Z_c_scalar flow


  (** {2 Initial state} *)
  (** ***************** *)

  let init prog man flow =
    set_env T_cur { cells = CellSet.empty; bases = BaseSet.empty } man flow




  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (** 𝔼⟦ *p ⟧ where p is a pointer to a scalar *)
  let eval_deref_scalar_pointer p range man flow =
    (* Expand *p into cells *)
    expand p range man flow >>$ fun expansion flow ->
    let t = under_type p.etyp in
    match expansion with
    | Top ->
      Eval.singleton (mk_top (void_to_char t) range) flow

    | Region _ ->
      Eval.singleton (mk_top (void_to_char t) range) flow

    | Cell (c,mode) ->
      add_cell c range man flow >>= fun _ flow ->
      let v =
        if is_pointer_cell c then
          mk_pointer_cell_var_expr c ~mode t range
        else
          mk_numeric_cell_var_expr c ~mode range
      in
      Eval.singleton v flow


  (* 𝔼⟦ *p ⟧ where p is a pointer to a function *)
  let eval_deref_function_pointer p range man flow =
    man.eval ~zone:(Z_c_low_level,Z_c_points_to) p flow |>
    Eval.bind @@ fun pt flow ->

    match ekind pt with
    | E_c_points_to (P_fun f) ->
      Eval.singleton (mk_expr (E_c_function f) ~etyp:(under_type p.etyp) range) flow

    | _ -> panic_at range
             "deref_function_pointer: pointer %a points to a non-function object %a"
             pp_expr p
             pp_expr pt


  (** 𝔼⟦ &lval ⟧ *)
  let eval_address_of lval range man flow =
    match ekind @@ remove_casts lval with
    | E_var _ ->
      Eval.singleton (mk_c_address_of lval range) flow

    | E_c_deref p ->
      man.eval ~zone:(Z_c_low_level,Z_c_scalar) p flow

    | _ ->
      panic_at range ~loc:__LOC__
        "evaluation of &%a not supported"
        pp_expr lval


  (** 𝔼⟦ *(p + ∀i) ⟧ *)
  let eval_deref_quantified p range man flow =
    let typ = under_type p.etyp |> void_to_char in
    eval_pointed_base_offset p range man flow >>$ fun pp flow ->

    match pp with
    | None ->
      (* Valid pointer but unknown offset *)
      Soundness.warn_at range "ignoring ⊤ pointer %a" pp_expr p;
      Eval.singleton (mk_top typ range) flow

    | Some (base,offset,mode) ->
      eval_base_size base range man flow >>$ fun size flow ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) size flow >>$ fun size flow ->

      let min, max = Common.Quantified_offset.bound offset in
      man.eval ~zone:(Z_c, Z_u_num) min flow >>$ fun min flow ->
      man.eval ~zone:(Z_c, Z_u_num) max flow >>$ fun max flow ->

      let limit = sub size (mk_z (sizeof_type typ) range) range in

      (* Safety condition: [min, max] ⊆ [0, size - |elm|] *)
      assume (
        mk_binop
          (mk_in min (mk_zero range) limit range)
          O_log_and
          (mk_in max (mk_zero range) limit range)
          range
      )
        ~fthen:(fun flow ->
            Eval.singleton (mk_top typ range) flow
          )
        ~felse:(fun flow ->
            raise_c_out_bound_quantified_alarm ~base ~min ~max ~size range man flow |>
            Eval.empty_singleton
          )
        ~zone:Z_u_num man flow



  let eval zone exp man flow =
    match ekind exp with
    | E_var (v,_) when is_c_scalar_type v.vtyp ->
      eval_deref_scalar_pointer (mk_c_address_of exp exp.erange) exp.erange man flow |>
      OptionExt.return

    | E_c_deref p when under_type p.etyp |> void_to_char |> is_c_scalar_type &&
                       not (is_pointer_offset_forall_quantified p)
      ->
      eval_deref_scalar_pointer p exp.erange man flow |>
      OptionExt.return


    | E_c_deref p when under_type p.etyp |> is_c_function_type &&
                       not (is_pointer_offset_forall_quantified p)
      ->
      eval_deref_function_pointer p exp.erange man flow |>
      OptionExt.return

    | E_c_address_of lval ->
      eval_address_of lval exp.erange man flow |>
      OptionExt.return


    | E_c_deref p when is_pointer_offset_forall_quantified p ->
      eval_deref_quantified p exp.erange man flow |>
      OptionExt.return


    | E_stub_builtin_call((VALID_PTR | VALID_FLOAT) as f, e) ->
      man.eval ~zone:(Z_c_low_level,Z_c_scalar) e flow >>$? fun e flow ->
      Eval.singleton (mk_expr (E_stub_builtin_call(f, e)) ~etyp:exp.etyp exp.erange) flow |>
      OptionExt.return

    | E_stub_quantified(EXISTS, v, _) ->
      let e = mk_var v exp.erange in
      man.eval ~zone:(Z_c_low_level,Z_c_scalar) e flow |>
      OptionExt.return

    | _ -> None



  (** {2 Abstract transformers} *)
  (** ************************* *)

  (** 𝕊⟦ type v; ⟧  *)
  let exec_declare v scope range man flow =
    (* Add v to the bases *)
    let base = mk_var_base v in
    let flow = map_env T_cur (fun a ->
        { a with bases = BaseSet.add base a.bases }
      ) man flow
    in
    (* If v is a scalar variable, add it to the scalar domain *)
    if is_c_scalar_type v.vtyp then
      let c = mk_cell base Z.zero v.vtyp in
      let vv = mk_cell_var c in
      map_env T_cur (fun a ->
        { a with cells = CellSet.add c a.cells }
      ) man flow |>
      man.post ~zone:Z_c_scalar (mk_c_declaration vv None scope range)
    else
      Post.return flow


  (** 𝕊⟦ x = e; ⟧ *)
  let exec_assign x e range man flow =
    let p = match ekind x with
      | E_var _     -> mk_c_address_of x range
      | E_c_deref p -> p
      | _           -> assert false
    in
    (* Expand *p into cells *)
    expand p range man flow >>$ fun expansion flow ->
    match expansion with
    | Top ->
      Soundness.warn_at range "ignoring assignment to ⊤ pointer *%a = %a;"
        pp_expr p
        pp_expr e
      ;
      Post.return flow

    | Cell ({ base },_) when is_base_readonly base ->
      let flow = raise_c_read_only_modification_alarm base range man flow in
      Post.return flow

    | Cell (c,mode) ->
      man.eval ~zone:(Z_c_low_level,Z_c_scalar) e flow >>$ fun e flow ->
      assign_cell c e mode range man flow

    | Region (base,itv) when is_c_num_type e.etyp ->
      man.eval ~zone:(Z_c_low_level,Z_u_num) e flow >>$ fun e flow ->
      assign_region base itv range man flow

    | Region (base,itv)  ->
      man.eval ~zone:(Z_c_low_level,Z_c_scalar) e flow >>$ fun e flow ->
      assign_region base itv range man flow


  (** 𝕊⟦ ?e ⟧ *)
  let exec_assume e range man flow =
    man.eval ~zone:(Z_c_low_level,Z_c_scalar) e flow >>$ fun e flow ->
    let stmt = mk_assume e range in
    man.post ~zone:Z_c_scalar stmt flow


  let exec_add b range man flow =
    match b with
    | { base_kind = Var v; base_valid = true; } when is_c_scalar_type v.vtyp ->
      let c = mk_cell b Z.zero v.vtyp in
      add_cell c range man flow

    | _ ->
      add_base b man flow |>
      Post.return




  (* 𝕊⟦ remove v ⟧ *)
  let exec_remove b range man flow =
    let a = get_env T_cur man flow in
    let flow = set_env T_cur { a with bases = BaseSet.remove b a.bases } man flow in
    let cells = find_cells (fun c -> compare_base c.base b = 0) a in
    List.fold_left (fun acc c ->
        Post.bind (remove_cell c range man) acc
      ) (Post.return flow) cells


  (** Rename bases and their cells *)
  let exec_rename base1 base2 range man flow =
    let a = get_env T_cur man flow in

    (* Remove base1 and add base2 *)
    let a = { a with
              bases = BaseSet.remove base1 a.bases |>
                      BaseSet.add base2; }
    in
    let flow = set_env T_cur a man flow in

    (* Cells of base1 *)
    let cells1 = CellSet.filter (fun c ->
        compare_base c.base base1 = 0
      ) a.cells
    in

    (* Cells of base2 *)
    let cells2 = CellSet.filter (fun c ->
        compare_base c.base base2 = 0
      ) a.cells
    in

    (* Remove cells of base2 *)
    let post = CellSet.fold (fun c2 acc -> Post.bind (remove_cell c2 range man) acc) cells2 (Post.return flow) in

    (* Rename cells in base1 by rebasing to base2 *)
    CellSet.fold (fun c1 acc ->
        let c2 = { c1 with base = base2 } in
        Post.bind (rename_cell c1 c2 range man) acc
      ) cells1 post


  (** Expand a base into a set of bases *)
  let exec_expand b bl range man flow =
    let a = get_env T_cur man flow in
    (* Add the list of bases bl to abstract state a *)
    let a = { a with bases = BaseSet.union a.bases (BaseSet.of_list bl) } in
    (* Get the cells of base b *)
    let cells = find_cells (function { base = b' } -> compare_base b b' = 0) a in
    (* Expand each cell *)
    List.fold_left (fun acc c ->
        (* Create the corresponding expanded cells *)
        let cl = List.map (fun bb -> { c with base = bb } ) bl in
        Post.bind (expand_cell c cl range man) acc
      ) (Post.return flow) cells


  (** Compute the interval of an offset *)
  let offset_interval offset range man flow : Itv.t =
    let interval e =
      let evl = man.eval e ~zone:(Z_c, Z_u_num) flow in
      Eval.apply (fun ee flow -> man.ask (Universal.Numeric.Common.mk_int_interval_query ee) flow) Itv.join Itv.meet Itv.bottom evl
    in
    if is_expr_forall_quantified offset then
      let min, max = Common.Quantified_offset.bound offset in
      let min_itv = interval min in
      let max_itv = interval max in
      Itv.join min_itv max_itv
    else
      interval offset


  (** Forget the value of an lval *)
  let exec_forget lval range man flow =
    (* Get the pointed base *)
    let ptr = match ekind lval with
      | E_var _   -> mk_c_address_of lval range
      | E_c_deref(p) -> p
      | _ -> assert false
    in
    man.eval ptr ~zone:(Z_c_low_level,Z_c_points_to) flow >>$ fun p flow ->
    match ekind p with
    | E_c_points_to(P_block(base,offset,mode)) ->
      (* Compute the interval of the offset *)
      let itv = offset_interval offset range man flow in
      (* Add the size of the pointed cells *)
      let size = sizeof_type (under_type ptr.etyp |> void_to_char) in
      let itv = Bot.bot_lift2 ItvUtils.IntItv.add itv (Itv.of_z Z.zero (Z.pred size)) in
      (* Forget all affected cells *)
      let a = get_env T_cur man flow in
      let cells = find_cells (fun c ->
          compare_base c.base base = 0 &&
          Itv.mem c.offset itv
        ) a
      in
      List.fold_left (fun acc c -> Post.bind (forget_cell c range man) acc) (Post.return flow) cells

    | _ -> Post.return flow


  let exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration (v,init,scope) ->
      exec_declare v scope stmt.srange man flow |>
      OptionExt.return

    | S_assign(x, e) when is_c_scalar_type x.etyp ->
      exec_assign x e stmt.srange man flow |>
      OptionExt.return


    | S_assume(e) ->
      exec_assume e stmt.srange man flow |>
      OptionExt.return


    | S_add e when is_base_expr e ->
      exec_add (expr_to_base e) stmt.srange man flow |>
      OptionExt.return


    | S_remove e when is_base_expr e ->
      exec_remove (expr_to_base e) stmt.srange man flow |>
      OptionExt.return


    | S_rename(e1,e2) when is_base_expr e1 && is_base_expr e2 ->
      exec_rename (expr_to_base e1) (expr_to_base e2) stmt.srange man flow |>
      OptionExt.return

    | S_expand(e,el) when is_base_expr e &&
                          List.for_all is_base_expr el ->
      exec_expand (expr_to_base e) (List.map expr_to_base el) stmt.srange man flow |>
      OptionExt.return

    | S_forget(e) ->
      exec_forget e stmt.srange man flow |>
      OptionExt.return

    | S_stub_assigns _ ->
      Post.return flow |>
      OptionExt.return


    | _ -> None


  (** {2 Communication handlers} *)
  (** ************************** *)

  let ask query man flow = None


  let refine channel man flow =
    assert false

end

let () =
  Core.Sig.Stacked.Intermediate.register_stack (module Domain)
