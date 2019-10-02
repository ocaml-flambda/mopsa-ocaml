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
open Alarms
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
    primed : bool;
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
      (fun () -> compare c1.primed c2.primed);
    ]


  (** Pretty printer of cell types *)
  let pp_cell_typ fmt t =
    match t with
    | Numeric tt -> Pp.pp_c_type_short fmt tt
    | Pointer -> Format.fprintf fmt "ptr"


  (** Pretty printer of cells *)
  let pp_cell fmt c =
    Format.fprintf fmt "⟨%a,%a,%a⟩%s"
      pp_base c.base
      Z.pp_print c.offset
      pp_cell_typ c.typ
      (if c.primed then "'" else "")


  (** Create a cell *)
  let mk_cell base offset ?(primed=false) typ =
    {
      base;
      offset;
      primed;
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


  (** Construct the variable name associated to a cell *)
  let mk_cell_var_name c =
    let () = match c.base with
      | V { vkind = V_c_cell c } ->
        panic "recursive creation of cell %a" pp_cell c

      | V v ->
        Format.fprintf Format.str_formatter "⟨%s,%a,%a⟩%s"
          v.vname
          Z.pp_print c.offset
          pp_cell_typ c.typ
          (if c.primed then "'" else "")

      | _ -> pp_cell Format.str_formatter c
    in
    Format.flush_str_formatter ()


  (** Create a variable from a cell *)
  let mk_cell_var c : var =
    let name = mk_cell_var_name c in
    mkv name (V_c_cell c) (cell_type c)
  

  (** Create a variable from a numeric cell *)
  let mk_numeric_cell_var_expr c range : expr =
    assert(is_numeric_cell c);
    let v = mk_cell_var c in
    mk_var v ~mode:(base_mode c.base) range


  (** Create a variable from a pointer cell *)
  let mk_pointer_cell_var_expr c typ range : expr =
    assert(is_pointer_cell c);
    let v = mk_cell_var c in
    if is_c_void_type (under_type typ)
    then mk_var v ~mode:(base_mode c.base) range
    else mk_c_cast (mk_var v ~mode:(base_mode c.base) range) typ range


    
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
      match find_cell_opt
              (fun c' ->
                 is_int_cell c' &&
                 Z.equal (sizeof_cell c') (sizeof_cell c) &&
                 compare_base c.base (c'.base) = 0 &&
                 Z.equal c.offset c'.offset &&
                 c.primed = c'.primed
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
              compare_typ (cell_type c) (T_c_integer(C_unsigned_char)) = 0 &&
              c.primed = c'.primed
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
            match c.base with
            | S s ->
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
    | Cell of cell
    | Region of base * Itv.t
    | Top


  let is_interesting_base = function
    | V _
    | A _ -> true

    | _ -> false


  (** Expand a pointer dereference into a cell. *)
  let expand p range man flow : ('a, expansion) result =
    eval_pointed_base_offset p range man flow >>$ fun pp flow ->
    match pp with
    | None ->
      (* Valid pointer but unknown offset *)
      raise_c_alarm AOutOfBound range ~bottom:false man.lattice flow |>
      Result.singleton Top

    | Some (base,offset) ->
      let typ = under_pointer_type p.etyp |> void_to_char in
      let elm = sizeof_type typ in

      (* Get the size of the base *)
      eval_base_size base range man flow >>$ fun size flow ->

      (* Convert the size and the offset to numeric *)
      man.eval ~zone:(Z_c_scalar,Z_u_num) size flow >>$ fun size flow ->

      man.eval ~zone:(Z_c_scalar,Z_u_num) offset flow >>$ fun offset flow ->

      (* Try static check *)
      match expr_to_z size, expr_to_z offset with
      | Some size, Some offset ->
        if Z.gt elm size then
          panic_at range
            "%a points to a cell of size %a, which is greater than the size %a of its base %a"
            pp_expr p
            Z.pp_print elm
            Z.pp_print size
            pp_base base
        ;
        if Z.leq Z.zero offset &&
           Z.leq offset (Z.sub size elm)
        then
          let c = mk_cell base offset typ in
          Result.singleton (Cell c) flow
        else
          let flow = raise_c_alarm AOutOfBound range ~bottom:true man.lattice flow in
          Result.empty_singleton flow

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
              then Result.singleton Top flow
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

                (* Iterate over [l, u] *)
                let rec aux i o =
                  if i = !opt_expand
                  then
                    if Z.gt o u
                    then []
                    else
                      let region = Region (base, Itv.of_z o u) in
                      let flow = man.exec ~zone:Z_u_num (mk_assume (mk_binop offset O_ge (mk_z o range) range) range) flow in
                      if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom
                      then []
                      else [Result.singleton region flow]
                  else
                    let flow = man.exec ~zone:Z_u_num (mk_assume (mk_binop offset O_eq (mk_z o range) range) range) flow in
                    if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom
                    then aux (i + 1) (Z.add o step)
                    else
                      let c = mk_cell base o typ in
                      Result.singleton (Cell c) flow :: aux (i + 1) (Z.add o step)
                in
                let evals = aux 0 l in
                Result.join_list ~empty:(Result.empty_singleton flow) evals
            )
          ~felse:(fun flow ->
              let flow = raise_c_alarm AOutOfBound range ~bottom:true man.lattice flow in
              Result.empty_singleton flow
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


  (** Rename a cell and its associated scalar variable *)
  let rename_cell old_cell new_cell range man flow =
    (* Add the old cell in case it has not been accessed before so
       that its constraints are added in the sub domain
    *)
    add_cell old_cell range man flow |>
    Post.bind @@ fun flow ->

    (* Remove the old cell and add the new one *)
    let flow =
      map_env T_cur (fun a ->
          { a with cells = CellSet.remove old_cell a.cells |>
                           CellSet.add new_cell
          }
        ) man flow
    in

    let oldv = mk_cell_var old_cell in
    let newv = mk_cell_var new_cell in
    let stmt = mk_rename_var oldv newv range in
    man.post ~zone:Z_c_scalar stmt flow





  (** Compute the interval of a C expression *)
  let compute_bound ?(zone=(Z_c_low_level, Z_u_num)) e man flow =
    let evl = man.eval ~zone e flow in
    Eval.apply
      (fun ee flow ->
         man.ask (Universal.Numeric.Common.mk_int_interval_query ee) flow
      )
      Itv.join Itv.meet Itv.bottom
      evl


  (** Remove all cells already realized *)
  let remove_all_cells range man flow =
    let a = get_env T_cur man flow in

    if CellSet.is_top a.cells
    then Post.return flow

    else CellSet.fold (fun c acc ->
        Post.bind (remove_cell c range man) acc
      ) a.cells (Post.return flow)


  (** {2 Initial state} *)
  (** ***************** *)

  let init prog man flow =
    set_env T_cur { cells = CellSet.empty; bases = BaseSet.empty } man flow




  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (** 𝔼⟦ *p ⟧ where p is a pointer to a scalar *)
  let eval_deref_scalar_pointer p primed range man flow =
    (* Expand *p into cells *)
    expand p range man flow >>$ fun expansion flow ->
    let t = under_type p.etyp in
    match expansion with
    | Top | Region _ ->
      (* ⊤ pointer or expand threshold exceeded => use the whole value interval *)
      Eval.singleton (mk_top (void_to_char t) range) flow

    | Cell c ->
      let c = { c with primed } in
      add_cell c range man flow >>= fun _ flow ->
      let v =
        if is_pointer_cell c then
          mk_pointer_cell_var_expr c t range
        else
          mk_numeric_cell_var_expr c range
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
    let elm = under_type p.etyp |> void_to_char in
    eval_pointed_base_offset p range man flow >>$ fun pp flow ->

    match pp with
    | None ->
      (* Valid pointer but unknown offset *)
      raise_c_alarm AOutOfBound range ~bottom:false man.lattice flow |>
      Eval.singleton (mk_top elm range)

    | Some (base,offset) ->
      eval_base_size base range man flow >>$ fun size flow ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) size flow >>$ fun size flow ->

      let min, max = Common.Quantified_offset.bound offset in
      man.eval ~zone:(Z_c, Z_u_num) min flow >>$ fun min flow ->
      man.eval ~zone:(Z_c, Z_u_num) max flow >>$ fun max flow ->

      let limit = sub size (mk_z (sizeof_type elm) range) range in

      (* Safety condition: [min, max] ⊆ [0, size - |elm|] *)
      assume (
        mk_binop
          (mk_in min (mk_zero range) limit range)
          O_log_and
          (mk_in max (mk_zero range) limit range)
          range
      )
        ~fthen:(fun flow ->
            Eval.singleton (mk_top elm range) flow
          )
        ~felse:(fun flow ->
            debug "out-of-bound in %a" (Flow.print man.lattice.print) flow;
            raise_c_alarm AOutOfBound range ~bottom:true man.lattice flow |>
            Eval.empty_singleton
          )
        ~zone:Z_u_num man flow



  let eval zone exp man flow =
    match ekind exp with
    | E_var (v,STRONG) when is_c_scalar_type v.vtyp ->
      eval_deref_scalar_pointer (mk_c_address_of exp exp.erange) false exp.erange man flow |>
      Option.return

    | E_c_deref p when under_type p.etyp |> void_to_char |> is_c_scalar_type &&
                       not (is_pointer_offset_quantified p)
      ->
      eval_deref_scalar_pointer p false exp.erange man flow |>
      Option.return


    | E_c_deref p when under_type p.etyp |> is_c_function_type &&
                       not (is_pointer_offset_quantified p)
      ->
      eval_deref_function_pointer p exp.erange man flow |>
      Option.return

    | E_c_address_of lval ->
      eval_address_of lval exp.erange man flow |>
      Option.return

    | E_stub_primed lval when not (is_lval_offset_quantified lval) ->
      eval_deref_scalar_pointer (mk_c_address_of lval exp.erange) true exp.erange man flow |>
      Option.return

    | E_c_deref p when is_pointer_offset_quantified p ->
      eval_deref_quantified p exp.erange man flow |>
      Option.return

    | E_stub_primed e when is_lval_offset_quantified exp -> 
      eval_deref_quantified (mk_c_address_of e exp.erange) exp.erange man flow |>
      Option.return


    | E_stub_builtin_call((VALID_PTR | VALID_FLOAT) as f, e) ->
      man.eval ~zone:(Z_c_low_level,Z_c_scalar) e flow >>$? fun e flow ->
      Eval.singleton (mk_expr (E_stub_builtin_call(f, e)) ~etyp:exp.etyp exp.erange) flow |>
      Option.return

    | _ -> None



  (** {2 Abstract transformers} *)
  (** ************************* *)

  (** 𝕊⟦ type v = init; ⟧  *)
  let exec_declare v init scope range man flow =
    (* Since we are in the Z_low_level zone, we assume that init has
       been translated by a structured domain into a flatten
       initialization *)
    let flat_init =
      match init with
      | Some (C_init_flat l) -> l
      | _ -> assert false
    in

    (* Add the base *)
    let flow = map_env T_cur (fun a ->
        { a with bases = BaseSet.add (V v) a.bases }
      ) man flow
    in


    (* Initialize cells, but expand at most !opt_expand cells, as
       defined by the option -cell-expand *)
    let rec aux o i l flow =
      if i = !opt_expand || List.length l = 0
      then Post.return flow
      else
        let c, init, tl, o' =
          match l with
          | C_flat_expr (e,t) :: tl ->
            let c = mk_cell (V v) o t in
            let init = Some (C_init_expr e) in
            c, init, tl, Z.add o (sizeof_type t)

          | C_flat_none(n,t) :: tl ->
            let c = mk_cell (V v) o t in
            let init = None in
            let tl' = if Z.equal n Z.one then tl else C_flat_none(Z.pred n,t) :: tl in
            c, init, tl', Z.add o (sizeof_type t)

          | C_flat_fill(e,t,n) :: tl ->
            let c = mk_cell (V v) o t in
            let init = Some (C_init_expr e) in
            let tl' = if Z.equal n Z.one then tl else C_flat_fill(e,t,Z.pred n) :: tl in
            c, init, tl', Z.add o (sizeof_type t)


          | l -> panic "?? %a" Pp.pp_c_init (C_init_flat l)
        in
        (* Evaluate the initialization into a scalar expression *)
        (
          match init, is_c_global_scope scope with
          | None, _ -> Result.singleton None flow

          | _, true -> Result.singleton init flow

          | Some (C_init_expr e), false ->
            man.eval ~zone:(Z_c_low_level,Z_c_scalar) e flow >>$ fun e flow ->
            Result.singleton (Some (C_init_expr e)) flow

          | _ -> assert false
        )
        |>
        bind_some @@ fun init flow ->

        (* Add the cell *)
        let flow = map_env T_cur (fun a ->
            { a with cells = CellSet.add c a.cells }
          ) man flow
        in

        (* Initialize the associated variable *)
        let v = mk_cell_var c in
        let stmt = mk_c_declaration v init scope range in
        man.post ~zone:Z_c_scalar stmt flow |>

        Post.bind @@ fun flow ->
        aux o' (i + 1) tl flow
    in
    aux Z.zero 0 flat_init flow


  (** 𝕊⟦ *p = e; ⟧ *)
  let exec_assign p e mode range man flow =
    (* Expand *p into cells *)
    expand p range man flow >>$ fun expansion flow ->
    match expansion with
    | Top ->
      Soundness.warn_at range "ignoring assignment to undetermined lval *%a = %a;"
        pp_expr p
        pp_expr e
      ;
      Post.return flow

    | Cell { base } when is_base_readonly base ->
      let flow = raise_c_alarm AReadOnlyModification ~bottom:true range man.lattice flow in
      Post.return flow

    | Cell c ->
      man.eval ~zone:(Z_c_low_level,Z_c_scalar) e flow >>$ fun e flow ->
      assign_cell c e mode range man flow

    | Region (base,itv) ->
      assign_region base itv range man flow


  (** 𝕊⟦ ?e ⟧ *)
  let exec_assume e range man flow =
    man.eval ~zone:(Z_c_low_level,Z_c_scalar) e flow >>$ fun e flow ->
    let stmt = mk_assume e range in
    man.post ~zone:Z_c_scalar stmt flow


  let exec_add b range man flow =
    match b with
    | V v when is_c_scalar_type v.vtyp ->
      let c = mk_cell (V v) Z.zero v.vtyp in
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
    debug "rename %a into %a" pp_base base1 pp_base base2;
    let a = get_env T_cur man flow in

    (* Cell renaming function *)
    let to_base2 c = { c with base = base2 } in

    (* Cells of base1 *)
    let cells1 = CellSet.filter (fun c ->
        let x = compare_base c.base base1 in
        if x = 0 then debug "%a should be renamed %a" pp_cell c pp_cell (to_base2 c);
        x = 0
      ) a.cells
    in


    (* Cell copy function, depends on the presence of base2 *)
    let copy =
      if not (BaseSet.mem base2 a.bases) then
        (* If base2 is not already present => rename the cells *)
        fun c flow ->
          rename_cell c (to_base2 c) range man flow
      else
        (* Otherwise, assign with weak update *)
        fun c flow ->
          debug "copy %a to %a" pp_cell c pp_cell (to_base2 c);
          let v = mk_cell_var c in
          assign_cell (to_base2 c) (mk_var v range) WEAK range man flow |>
          Post.bind @@ remove_cell c range man
    in

    (* Apply copy function *)
    CellSet.fold (fun c acc -> Post.bind (copy c) acc) cells1 (Post.return flow) |>
    Post.bind @@ fun flow ->

    (* Remove base1 and add base2 *)
    let flow = map_env T_cur (fun a ->
        {
          a with
          bases = BaseSet.remove base1 a.bases |>
                  BaseSet.add base2;
        }
      ) man flow
    in

    (* Forward the rename statement to scalar domains in case of addresses *)
    match base1, base2 with
    | A addr1, A addr2 ->
      man.post ~zone:Z_c_scalar (mk_rename (mk_addr addr1 range) (mk_addr addr2 range) range) flow

    | _ ->
      Post.return flow


  (* 𝕊⟦ rename target[i1][i2]...[in]' into target[i1][i2]...[in],
     ∀ i1 ∈ [l1,u1], ..., in ∈ [ln,un] ⟧
  *)
  let exec_rename_primed target bounds range man flow =
    match bounds with
    | [] when not (is_c_scalar_type target.etyp) ->
      panic_at range
        "non-scalar %a' can not be unprimed without specifying the assigned offsets"
        pp_expr target

    | [] ->
      (* target should be a scalar lval *)
      begin
        expand (mk_c_address_of target range) range man flow >>$ fun expansion flow ->
        match expansion with
        | Cell c ->
          rename_cell { c with primed = true } c range man flow

        | _ -> assert false
      end

    | _ :: _ ->
      (* target is pointer, so resolve it and compute the affected offsets *)
      man.eval ~zone:(Z_c_low_level, Z_c_points_to) target flow >>$ fun pt flow ->
      match ekind pt with
      | E_c_points_to P_valid | E_c_points_to P_null | E_c_points_to P_invalid ->
        Post.return flow

      | E_c_points_to (P_block(base, offset)) ->
        (* Get cells with the same base *)
        let a = get_env T_cur man flow in
        let same_base_cells = CellSet.filter (fun c ->
            compare_base base c.base = 0
          ) a.cells
        in

        (* Compute the offset interval *)
        let itv =
          (* First, get the flattened expressions of the lower and upper bounds *)
          let l, u =
            let rec doit accl accu t =
              function
              | [] -> accl, accu
              | [(l, u)] ->
                (mk_offset_bound accl l t), (mk_offset_bound accu u t)
              | (l, u) :: tl ->
                doit (mk_offset_bound accl l t) (mk_offset_bound accu u t) (under_type t |> void_to_char) tl

            (* Utility function that returns the expression of an offset bound *)
            and mk_offset_bound before bound t =
              let elem_size = sizeof_type t in
              add before (
                mul bound (mk_z elem_size range) range ~typ:T_int
              ) range ~typ:T_int
            in
            doit offset offset (under_type target.etyp |> void_to_char) bounds
          in

          (* Compute the interval of the bounds *)
          let itv1 = compute_bound l man flow in
          let itv2 = compute_bound u man flow in

          (* Compute the interval of the assigned cells *)
          Itv.join itv1 itv2
        in


        (* Search for primed cells that reside withing the assigned offsets and rename them *)
        CellSet.fold (fun c acc ->
            if not (Itv.mem c.offset itv)
            then acc
            else if c.primed then
              (* Primed cells are unprimed by renaming them *)
              Post.bind (rename_cell c { c with primed = false } range man) acc
            else if not (CellSet.mem { c with primed = true } same_base_cells) then
              (* Remove unprimed cells that have no primed version *)
              Post.bind (remove_cell c range man) acc
            else
              acc
          ) same_base_cells (Post.return flow)

      | _ -> assert false



  let exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration (v,init,scope) ->
      exec_declare v init scope stmt.srange man flow |>
      Option.return

    | S_assign(({ekind = E_var(v, STRONG)} as lval), e) when is_c_scalar_type v.vtyp ->
      Some (
        let c = mk_cell (V v) Z.zero v.vtyp in
        let flow = map_env T_cur (fun a -> { a with cells = CellSet.add c a.cells }) man flow in

        let v = mk_cell_var c in
        man.eval ~zone:(Z_c_low_level,Z_c_scalar) e flow >>$ fun e flow ->
        let stmt = mk_assign (mk_var v lval.erange) e stmt.srange in
        man.post ~zone:Z_c_scalar stmt flow |>

        Post.bind @@ remove_cell_overlappings c stmt.srange man
      )

    | S_assign(({ekind = E_c_deref(p)}), e) when is_c_scalar_type @@ under_type p.etyp ->
      exec_assign p e STRONG stmt.srange man flow |>
      Option.return


    | S_assume(e) ->
      exec_assume e stmt.srange man flow |>
      Option.return


    | S_add { ekind = E_var (v, _) } ->
      exec_add (V v) stmt.srange man flow |>
      Option.return


    | S_add { ekind = E_addr addr } ->
      exec_add (A addr) stmt.srange man flow |>
      Option.return

    | S_remove { ekind = E_var (v, _) } when is_c_type v.vtyp ->
      exec_remove (V v) stmt.srange man flow |>
      Option.return

    | S_rename({ ekind = E_var (v1, _) }, { ekind = E_var (v2, _) }) ->
      exec_rename (V v1) (V v2) stmt.srange man flow |>
      Option.return

    | S_rename({ ekind = E_addr addr1 }, { ekind = E_addr addr2 }) ->
      exec_rename (A addr1) (A addr2) stmt.srange man flow |>
      Option.return

    | S_stub_assigns _ ->
      Post.return flow |>
      Option.return

    | S_stub_rename_primed(lval, bounds) ->
      exec_rename_primed lval bounds stmt.srange man flow |>
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
