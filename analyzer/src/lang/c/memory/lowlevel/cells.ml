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

  (** Add a cell in the sub-tree abstract state *)
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

end

let () =
  Core.Sig.Stacked.Intermediate.register_stack (module Domain)
