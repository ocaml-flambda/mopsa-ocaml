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
open Sig.Abstraction.Stacked
open Universal.Ast
open Stubs.Ast
open Ast
open Common.Base
open Common.Points_to
open Common.Alarms
module Itv = Universal.Numeric.Values.Intervals.Integer.Value
open Universal.Numeric.Common


module Domain =
struct

  let name = "c.memory.lowlevel.cells"

  
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
    match c.base.base_kind with
    (* Don't create new variables for cells representing scalar variables *)
    | Var v when is_c_scalar_type v.vtyp &&
                 Z.(c.offset = zero) &&
                 (c.typ = Pointer || compare_typ (remove_typedef_qual v.vtyp) (cell_type c) = 0)
      -> v
    | _ ->
      let name = mk_cell_uniq_name c in
      mkv name (V_c_cell c) (cell_type c) ~mode:(base_mode c.base) ~semantic:"C/Scalar"


  (** Create a variable from a numeric cell *)
  let mk_numeric_cell_var_expr c ?(mode=None) range : expr =
    assert(is_numeric_cell c);
    let v = mk_cell_var c in
    mk_var v ~mode range


  (** Create a variable from a pointer cell *)
  let mk_pointer_cell_var_expr c ?(mode=None) typ range : expr =
    assert(is_pointer_cell c);
    let v = mk_cell_var c in
    if compare_typ (remove_typedef_qual v.vtyp) (remove_typedef_qual typ) = 0
    then mk_var v ~mode range
    else mk_c_cast (mk_var v ~mode range) typ range



  (** {2 Cell sets} *)
  (** ************* *)

  (** Cell sets are partitionned by base and then by offset to
      improve the efficiency.
  *)

  module Cell = struct
      type t = cell
      let compare = compare_cell
      let print = pp_cell
    end

  module Off = struct
    type t = Z.t
    let compare = Z.compare
    let print = Z.pp_print
  end

  module Cells = Framework.Lattices.Powerset.Make(Cell)

  module OffCells = Framework.Lattices.Pointwise.Make(Off)(Cells)

  module CellSet = Framework.Lattices.Pointwise.Make(Base)(OffCells)


  (* Max size of cells, used to get a safe offset range to look for
     overlapping cells
  *)
  let max_sizeof_cell = Z.of_int 16


  let cell_set_add (c:cell) (m:CellSet.t) : CellSet.t =
    (* check that max_sizeof_cell is safe, to be sure *)
    assert (sizeof_cell c <= max_sizeof_cell);
    CellSet.apply c.base (OffCells.apply c.offset (Cells.add c)) m

  let cell_set_remove (c:cell) (m:CellSet.t) : CellSet.t =
    CellSet.apply c.base (OffCells.apply c.offset (Cells.remove c)) m

  let cell_set_mem (c:cell) (m:CellSet.t) : bool =
    CellSet.find c.base m |> OffCells.find c.offset |> Cells.mem c

  (** Folds only cells at base [b] and with offsets between [lo] and [hi],
      included.
  *)
  let cell_set_fold_range
      (f:cell -> 'a -> 'a)
      (b:Base.t) (lo:Z.t) (hi:Z.t) (m:CellSet.t)
      (acc:'a) : 'a =
    OffCells.fold_slice
      (fun o s acc -> Cells.fold (fun c acc -> f c acc) s acc)
      (CellSet.find b m) lo hi acc

  (** Returns the cells satisfying [f] in the [lo], [hi] offset range. *)
  let cell_set_filter_range
      (f:cell -> bool)
      (b:Base.t) (lo:Z.t) (hi:Z.t) (m:CellSet.t) : cell list =
    cell_set_fold_range (fun (c:cell) (l:cell list) -> if f c then c::l else l) b lo hi m []

  (** Returns the cells satisfying [f] only considering cells that overlap
      an offset range [lo], [hi] included.
  *)
  let cell_set_filter_overlapping_range
      (f:cell -> bool)
      (b:Base.t) (lo:Z.t) (hi:Z.t) (m:CellSet.t) : cell list =
    cell_set_filter_range
      (fun c -> c.offset  <= hi && lo < Z.add c.offset (sizeof_cell c) && f c)
      b (Z.sub lo max_sizeof_cell) hi m

  (** Returns the cells satisfying [f] only considering cells that overlap
      [c] (including [c] itself, if it is in the map).
  *)
  let cell_set_filter_overlapping_cell_range
      (f:cell -> bool) (c:cell) (m:CellSet.t) : cell list =
    cell_set_filter_overlapping_range
      f c.base c.offset (Z.pred (Z.add c.offset (sizeof_cell c))) m

  (** Returns all the cells that overlap [c] but are different from [c]. *)
  let cell_set_find_overlapping_cell (c:cell) (m:CellSet.t) : cell list =
    cell_set_filter_overlapping_cell_range
      (fun c' -> compare_cell c c' <> 0) c m

  (** Returns all the cells that overlap an interval (bounds included). *)
  let cell_set_find_overlapping_itv (b:Base.t) (i:Itv.t) (m:CellSet.t) : cell list =
    match i with
    | Bot.BOT -> []
    | Bot.Nb (lo,hi) ->
      (* get interval bounds as Z.t *)
      let lo = match lo with Finite f -> f | _ -> Z.zero in
      let hi = match hi with
        | Finite f -> f
        | _ -> match OffCells.max_binding (CellSet.find b m) with
          | Some (f,_) -> f
          | _ -> Z.zero
      in
      cell_set_filter_overlapping_range (fun _ -> true) b lo hi m

  (** All cells in a base. *)
  let cell_set_find_base (b:Base.t) (m:CellSet.t) : cell list =
    OffCells.fold
      (fun _ s l -> Cells.fold (fun c l -> c :: l) s l)
      (CellSet.find b m) []



  (** {2 Domain header} *)
  (** ***************** *)


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
      let name = name
    end)

  let scalar = Semantic "C/Scalar"

  let alarms = [ A_c_out_of_bound;
                 A_c_null_deref;
                 A_c_use_after_free;
                 A_c_invalid_deref ]


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

  (** Flag to activate the on-demand smashing *)
  let opt_smash = ref false
  let () =
    register_domain_option name {
      key = "-cell-smash";
      category = "C";
      doc = " activate the on-demand smashing when the expansion threshold is reached";
      spec = ArgExt.Set opt_smash;
      default = "";
    }


  (** {2 Unification of cells} *)
  (** ======================== *)

  (** [phi c a range] returns a constraint expression over cell [c] found in [a] *)
  let phi (c:cell) (a:t) range : expr option =
    if cell_set_mem c a.cells then None

    else if not (is_c_int_type @@ cell_type c) then None

    else
      match
        cell_set_filter_range
          (fun c' ->
             is_int_cell c' &&
             Z.equal (sizeof_cell c') (sizeof_cell c)
          )
          c.base c.offset c.offset a.cells
      with
      | c'::_ ->
        let v = mk_numeric_cell_var_expr c' range in
        Some (wrap_expr v (rangeof_int_cell c) range)

      | [] ->
        match
          compare_typ (cell_type c |> remove_typedef_qual) u8 = 0,
          cell_set_filter_overlapping_cell_range
            (fun c' ->
               let b = Z.sub c.offset c'.offset in
               Z.geq b Z.zero &&
               Z.lt b (sizeof_cell c') &&
               is_int_cell c'
            )
            c a.cells
        with
        | true, c'::_ ->
          let b = Z.sub c.offset c'.offset in
          let base = (Z.pow (Z.of_int 2) (8 * Z.to_int b))  in
          let v = mk_numeric_cell_var_expr c' range in
          Some (
            (_mod_
               (div v (mk_z base range) range)
               (mk_int 256 range)
               range
            )
          )

        | _ ->
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
                  if cell_set_mem tobein a.cells
                  then aux (i+1) (tobein :: l)
                  else raise NotPossible
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
          | NotPossible -> None
    


  (** Add a cell and its constraints *)
  let add_cell c range man flow =
    let a = get_env T_cur man flow in
    if cell_set_mem c a.cells
    then Post.return flow
    else
      let flow = set_env T_cur { a with cells = cell_set_add c a.cells } man flow in
      let v = mk_cell_var c in
      man.exec ~route:scalar (mk_add_var v range) flow >>% fun flow ->
      if is_pointer_cell c then
        Post.return flow
      else
        match phi c a range with
        | Some e ->
          let stmt = mk_assume (mk_binop (mk_var v range) O_eq e ~etyp:u8 range) range in
          man.exec stmt flow

        | None -> Post.return flow


  (** Range used for tagging unification statements *)
  let unify_range = tag_range (mk_fresh_range ()) "cell-unification"


  (** [unify a a'] finds non-common cells in [a] and [a'] and adds them. *)
  let unify man sman ctx (a,s) (a',s') =
    CellSet.fold2zo
      (fun b m1 acc -> acc)
      (fun b m2 acc -> acc)
      (fun b m1 m2 acc ->
         OffCells.fold2zo
           (fun _ s1 (acc1,acc2) ->
              acc1,
              Cells.fold
                (fun c s ->
                   sub_env_exec (add_cell c unify_range man) ctx man sman a' s |> snd )
                s1 acc2
           )
           (fun _ s2 (acc1,acc2) ->
             Cells.fold
               (fun c s ->
                  sub_env_exec (add_cell c unify_range man) ctx man sman a s |> snd )
               s2 acc1,
             acc2
           )
           (fun _ s1 s2 acc ->
              Cells.fold
                (fun c s ->
                   sub_env_exec (add_cell c unify_range man) ctx man sman a s |> snd )
                (Cells.diff s2 s1) (fst acc)
              ,
              Cells.fold
                (fun c s ->
                   sub_env_exec (add_cell c unify_range man) ctx man sman a' s |> snd )
                (Cells.diff s1 s2) (snd acc)

           )
           m1 m2 acc
      )
      a.cells a'.cells (s,s')


  (** {2 Lattice operators} *)
  (** ********************* *)

  let is_bottom _ = false

  let subset man sman ctx (a,s) (a',s') =
    let s, s' = unify man sman ctx (a, s) (a', s') in
    (true, s, s')

  let join man sman ctx (a,s) (a',s') =
    let s, s' = unify man sman ctx (a,s) (a',s') in
    let a = {
      cells = CellSet.join a.cells a'.cells;
      bases = BaseSet.join a.bases a'.bases;
    }
    in
    (a, s, s')

  let meet = join

  let widen man sman ctx (a,s) (a',s') =
    let (a, s, s') = join man sman ctx (a,s) (a',s') in
    (a, s, s', true)

  let merge pre (a,log) (a',log') =
    assert false


  (** {2 Cell expansion} *)
  (** ****************** *)

  (** Possible results of a cell expansion *)
  type expansion =
    | Cell of cell * mode option
    | Region of base * Z.t (** offset lower bound *) * Z.t (** offset higher bound *) * Z.t (** offset step *)
    | Top


  let rec is_interesting_base = function
    | { base_valid = true; base_kind = Var {vkind = Cstubs.Aux_vars.V_c_primed_base base}; } -> is_interesting_base base
    | { base_valid = true; base_kind = Var _ }  -> true
    | { base_valid = true; base_kind = Addr _ } -> true
    | _ -> false


  let eval_pointed_base_offset ptr range man flow =
    resolve_pointer ptr man flow >>$ fun pt flow ->
    match pt with
    | P_null ->
      raise_c_null_deref_alarm ptr man flow |>
      Cases.empty_singleton

    | P_invalid ->
      raise_c_invalid_deref_alarm ptr man flow |>
      Cases.empty_singleton

    | P_block ({ base_kind = Addr _; base_valid = false; base_invalidation_range = Some r }, offset, _) ->
      raise_c_use_after_free_alarm ptr r man flow |>
      Cases.empty_singleton

    | P_block ({ base_kind = Var v; base_valid = false; base_invalidation_range = Some r }, offset, _) ->
      raise_c_dangling_deref_alarm ptr v r man flow |>
      Cases.empty_singleton

    | P_block (base, offset, mode) ->
      Cases.singleton (Some (base, offset, mode)) flow

    | P_top ->
      Cases.singleton None flow

    | P_fun _ ->
      assert false

  (** Expand a pointer dereference into a cell. *)
  let expand p range man flow : ('a, expansion) cases =
    eval_pointed_base_offset p range man flow >>$ fun pp flow ->
    match pp with
    | None ->
      Cases.singleton Top flow

    | Some (base,offset,mode) ->
      let typ = under_type p.etyp |> void_to_char in
      let elm = sizeof_type typ in

      (* Get the size of the base *)
      eval_base_size base range man flow >>$ fun size flow ->

      (* Convert the size and the offset to numeric *)
      man.eval size flow >>$ fun size flow ->
      man.eval offset flow >>$ fun offset flow ->

      (* Check the bounds: offset ∈ [0, size - |typ|] *)
      let cond = mk_in offset (mk_zero range)
          (sub size (mk_z elm range) range ~typ:T_int)
          range
      in
      assume_num cond
        ~fthen:(fun flow ->
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
            if nb > Z.of_int !opt_deref_expand || not (is_interesting_base base) then
              (* too many cases -> top *)
              let region = Region (base, l, u ,step) in
              man.exec (mk_assume (mk_binop offset O_ge (mk_z l range) range) range) flow >>% fun flow ->
              if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom
              then Cases.empty_singleton flow
              else Cases.singleton region flow
            else
              (* few cases -> iterate fully over [l, u] *)
              let rec aux o =
                if Z.gt o u
                then []
                else
                  let flow = man.exec (mk_assume (mk_binop offset O_eq (mk_z o range) range) range) flow |> post_to_flow man in
                  if Flow.get T_cur man.lattice flow |> man.lattice.is_bottom
                  then aux (Z.add o step)
                  else
                    let c = mk_cell base o typ in
                    Cases.singleton (Cell (c,mode)) flow :: aux (Z.add o step)
              in
              let evals = aux l in
              Cases.join_list ~empty:(fun () -> Cases.empty_singleton flow) evals

          )
        ~felse:(fun eflow ->
            let flow = raise_c_out_bound_alarm base size offset typ range man flow eflow in
            Cases.empty_singleton flow
          )
        man flow


  (** Summarize a set of cells into a single smashed cell *)
  let smash_region base lo hi step t range man flow =
    let top = Eval.singleton (mk_top (void_to_char t) range) flow in
    if not !opt_smash
    || not (is_interesting_base base)
    || not (is_c_pointer_type t) (* For performance reasons, we smash only pointer cells *)
    then
      top
    else
      (* In order to smash a region in something useful, we ensure
         that all covered cells do exist *)
      let a = get_env T_cur man flow in
      let cells = cell_set_filter_range
          (fun c ->
             (* Get only pointer cells that are correctly aligned with the step *)
             c.typ = Pointer
             && Z.(rem (c.offset - lo) step = zero)
          )
          base lo hi a.cells in
      (* Coverage test: |cells| = ((hi - lo) / step) + 1 *)
      let nb_cells = List.length cells in
      if nb_cells = 0 || Z.(of_int nb_cells < (div (hi - lo) step) + one) then
        top
      else
        (* Create a temporary smash and populate it with the values of cells *)
        let smash = mk_range_attr_var range "smash" ~mode:WEAK t in
        let weak_smash = mk_var smash range in
        let strong_smash = mk_var smash ~mode:(Some STRONG) range in
        man.exec (mk_add_var smash range) ~route:scalar flow >>% fun flow ->
        let ecells = List.map (fun c -> mk_pointer_cell_var_expr c t range) cells in
        let hd = List.hd ecells in
        let tl = List.tl ecells in
        man.exec (mk_assign strong_smash hd range) ~route:scalar flow >>% fun flow ->
        List.fold_left
          (fun acc c -> Post.bind (man.exec (mk_assign weak_smash c range) ~route:scalar) acc)
          (Post.return flow) tl
        >>% fun flow ->
        Eval.singleton weak_smash ~cleaners:[mk_remove_var smash range] flow


  let add_base b man flow =
    let a = get_env T_cur man flow in
    let aa = { a with bases = BaseSet.add b a.bases } in
    set_env T_cur aa man flow



  (* Remove a cell and its associated scalar variable *)
  let remove_cell c range man flow =
    let flow = map_env T_cur (fun a ->
        { a with cells = cell_set_remove c a.cells }
      ) man flow
    in
    let v = mk_cell_var c in
    let stmt = mk_remove_var v range in
    man.exec ~route:scalar stmt flow


  (** Remove cells overlapping with cell [c] *)
  let remove_cell_overlappings c range man flow =
    let a = get_env T_cur man flow in
    let overlappings = cell_set_find_overlapping_cell c a.cells in
    List.fold_left (fun acc c' ->
        Post.bind (remove_cell c' range man) acc
      ) (Post.return flow) overlappings


  (** Remove cells overlapping with cell [c] *)
  let remove_region_overlappings base lo hi step range man flow =
    let a = get_env T_cur man flow in
    let overlappings = cell_set_filter_overlapping_range (fun _ -> true) base lo hi a.cells in
    List.fold_left (fun acc c' ->
        Post.bind (remove_cell c' range man) acc
      ) (Post.return flow) overlappings


  let rename_cell c1 c2 range man flow =
    let v1 = mk_cell_var c1 in
    let v2 = mk_cell_var c2 in
    let flow = map_env T_cur (fun a ->
        { a with cells = cell_set_remove c1 a.cells |>
                         cell_set_add c2 }
      ) man flow in
    let stmt = mk_rename_var v1 v2 range in
    man.exec ~route:scalar stmt flow


  let assign_cell c e mode range man flow =
    let a = get_env T_cur man flow in
    let a' = { a with cells = cell_set_add c a.cells } in
    let flow = set_env T_cur a' man flow in
    let v = mk_cell_var c in
    let vv = mk_var v ~mode range in
    begin if cell_set_mem c a.cells then
        Post.return flow
      else
        man.exec (mk_add_var v range) ~route:scalar flow
    end >>% fun flow ->
    man.exec (mk_assign vv e range) ~route:scalar flow >>% fun flow ->
    remove_cell_overlappings c range man flow


  let assign_region base lo hi step range man flow =
    if not (is_interesting_base base) then
      Post.return flow
    else
      remove_region_overlappings base lo hi step range man flow


  let expand_cell c cl range man flow =
    (* Add cells in cl to the state *)
    let flow = map_env T_cur (fun a ->
        { a with cells = List.fold_left (fun s c -> cell_set_add c s) a.cells cl; }
      ) man flow in
    (* Expand cell variables *)
    let v = mk_cell_var c in
    let vl = List.map mk_cell_var cl in
    let stmt = mk_expand_var v vl range in
    man.exec stmt ~route:scalar flow

  let fold_cells c cl range man flow =
    let flow = map_env T_cur (fun a ->
        { a with cells = List.fold_left (fun s c -> cell_set_remove c s) a.cells cl |>
                         cell_set_add c }
      ) man flow in
    let v = mk_cell_var c in
    let vl = List.map mk_cell_var cl in
    let stmt = mk_fold_var v vl range in
    man.exec stmt ~route:scalar flow

  let forget_cell c range man flow =
    let flow = map_env T_cur
        (fun a ->
           { a with cells = cell_set_remove c a.cells }
        ) man flow
    in
    let v = mk_cell_var c in
    let stmt = mk_remove_var v range in
    man.exec stmt ~route:scalar flow


  (** {2 Initial state} *)
  (** ***************** *)

  let init prog man flow =
    set_env T_cur { cells = CellSet.empty; bases = BaseSet.empty } man flow




  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (** 𝔼⟦ *p ⟧ where p is a pointer to a scalar *)
  let eval_deref_scalar_pointer p range man flow =
    (* Expand *p into cells *)
    let t = under_type p.etyp in
    expand p range man flow >>$ fun expansion flow ->
    match expansion with
    | Top ->
      warn_at range "dereferencing ⊤ pointer %a" pp_expr p;
      let flow = raise_c_null_deref_wo_info_alarm ~bottom:false range man flow |>
                 raise_c_invalid_deref_wo_info_alarm ~bottom:false range man |>
                 raise_c_use_after_free_wo_info_alarm ~bottom:false range man |>
                 raise_c_dangling_deref_wo_info_alarm ~bottom:false range man |>
                 raise_c_out_bound_wo_info_alarm ~bottom:false range man in
      man.eval (mk_top (void_to_char t) range) flow

    | Region (base,lo,hi,step) ->
      smash_region base lo hi step t range man flow >>$ fun ret flow ->
      man.eval ret flow ~route:scalar

    | Cell (c,mode) ->
      add_cell c range man flow >>% fun flow ->
      let v =
        if is_pointer_cell c then
          mk_pointer_cell_var_expr c ~mode t range
        else
          mk_numeric_cell_var_expr c ~mode range
      in
      man.eval v flow ~route:scalar

  let eval_scalar_var v mode range man flow =
    let c = mk_cell (mk_var_base v) Z.zero v.vtyp in
    add_cell c range man flow >>% fun flow ->
    let v =
      if is_pointer_cell c then
        mk_pointer_cell_var_expr c ~mode v.vtyp range
      else
        mk_numeric_cell_var_expr c ~mode range
    in
    man.eval v flow ~route:scalar
  

  let eval exp man flow =
    match ekind exp with
    | E_var ({ vkind = V_c_cell _},_) -> None

    | E_var (v,mode) when is_c_scalar_type v.vtyp ->
      eval_scalar_var v mode exp.erange man flow |>
      OptionExt.return

    | E_c_deref p when under_type p.etyp |> void_to_char |> is_c_scalar_type
      ->
      eval_deref_scalar_pointer p exp.erange man flow |>
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
        { a with cells = cell_set_add c a.cells }
      ) man flow |>
      man.exec ~route:scalar (mk_c_declaration vv None scope range)
    else
      Post.return flow


  (** 𝕊⟦ lval = e; ⟧ *)
  let exec_assign lval e range man flow =
    let ptr = mk_c_address_of lval range in
    expand ptr range man flow >>$ fun expansion flow ->
    match expansion with
    | Top ->
      Soundness.warn_at range "ignoring assignment to ⊤ pointer %a = %a;"
        pp_expr lval
        pp_expr e
      ;
      Post.return flow

    | Cell ({ base },_) when is_base_readonly base ->
      let flow = raise_c_modify_read_only_alarm ptr base man flow in
      Post.return flow

    | Cell (c,mode) ->
      assign_cell c e mode range man flow

    | Region (base,lo,hi,step) ->
      man.eval e flow >>$ fun _ flow ->
      assign_region base lo hi step range man flow



  let exec_add b range man flow =
    match b with
    | { base_kind = Var v; base_valid = true; } when is_c_scalar_type v.vtyp ->
      let c = mk_cell b Z.zero v.vtyp in
      add_base b man flow |>
      add_cell c range man

    | _ ->
      add_base b man flow |>
      Post.return



  (* 𝕊⟦ remove v ⟧ *)
  let exec_remove b range man flow =
    let a = get_env T_cur man flow in
    let flow = set_env T_cur { a with bases = BaseSet.remove b a.bases } man flow in
    let cells = cell_set_find_base b a.cells in
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
    let cells1 = cell_set_find_base base1 a.cells in

    (* Cells of base2 *)
    let cells2 = cell_set_find_base base2 a.cells in

    (* Remove cells of base2 *)
    let post = List.fold_left (fun acc c2 -> Post.bind (remove_cell c2 range man) acc) (Post.return flow) cells2 in

    (* Rename cells in base1 by rebasing to base2 *)
    List.fold_left
      (fun acc c1 ->
         let c2 = { c1 with base = base2 } in
         Post.bind (rename_cell c1 c2 range man) acc
      )
      post cells1


  (** Expand a base into a set of bases *)
  let exec_expand b bl range man flow =
    let a = get_env T_cur man flow in
    (* Add the list of bases bl to abstract state a *)
    let aa = { a with bases = BaseSet.union a.bases (BaseSet.of_list bl) } in
    let flow = set_env T_cur aa man flow in
    (* Get the cells of base b *)
    let cells = cell_set_find_base b a.cells in
    (* Expand each cell *)
    List.fold_left (fun acc c ->
        (* Create the corresponding expanded cells *)
        let cl = List.map (fun bb -> { c with base = bb } ) bl in
        Post.bind (expand_cell c cl range man) acc
      ) (Post.return flow) cells


  (** Sets of offsets and types, used by exec_fold *)
  module OffsetTypeSet = SetExt.Make(struct
      type t = Z.t * cell_typ
      let compare = Compare.pair Z.compare compare_cell_typ
    end)


  (** Fold a set of bases into a single base *)
  let exec_fold b bl range man flow =
    let a = get_env T_cur man flow in
    (* Add the base b *)
    let a = { a with bases = BaseSet.add b a.bases } in
    (* Find common offsets and types of cells in bases bl *)
    let common =
      let set_of_base b =
        cell_set_find_base b a.cells |>
        List.map (fun c -> c.offset,c.typ) |>
        OffsetTypeSet.of_list
      in
      match bl with
      | [] -> OffsetTypeSet.empty
      | b::tl ->
        let set0 = set_of_base b in
        let rec iter set = function
          | [] -> set
          | b::tl ->
            let set' = set_of_base b in
            let set'' = OffsetTypeSet.inter set set' in
            if OffsetTypeSet.is_empty set'' then set'' else iter set'' tl
        in
        iter set0 tl
    in
    (* Fold cells *)
    OffsetTypeSet.fold
      (fun (o,t) acc ->
         let c = { base = b; offset = o; typ = t } in
         let cl = List.map (fun bb -> { c with base = bb }) bl in
         Post.bind (fold_cells c cl range man) acc
      ) common (Post.return flow)
    >>% fun flow ->

    (* Remove bases bl *)
    List.fold_left
      (fun acc bb ->
         Post.bind (exec_remove bb range man) acc
      ) (Post.return flow) bl
    
         

  (** Compute the interval of an offset *)
  let offset_interval offset range man flow : Itv.t =
    let evl = man.eval offset flow in
    Cases.apply_some (fun ee flow -> man.ask (Universal.Numeric.Common.mk_int_interval_query ee) flow) Itv.join Itv.meet Itv.bottom evl


  (** Forget the value of an lval *)
  let exec_forget lval range man flow =
    (* Get the pointed base *)
    let ptr = mk_c_address_of lval range in
    resolve_pointer ptr man flow >>$ fun p flow ->
    match p with
    | P_block(base,offset,mode) ->
      (* Compute the interval of the offset *)
      let itv = offset_interval offset range man flow in
      (* Add the size of the pointed cells *)
      let size = sizeof_type (under_type ptr.etyp |> void_to_char) in
      let itv = Bot.bot_lift2 ItvUtils.IntItv.add itv (Itv.of_z Z.zero (Z.pred size)) in
      (* Forget all affected cells *)
      let a = get_env T_cur man flow in
      let cells = cell_set_find_overlapping_itv base itv a.cells in
      List.fold_left (fun acc c -> Post.bind (forget_cell c range man) acc) (Post.return flow) cells

    | _ -> Post.return flow


  let exec_forget_quant quants lval range man flow =
    (* Get the pointed base *)
    let ptr = mk_c_address_of lval range in
    resolve_pointer ptr man flow >>$ fun p flow ->
    match p with
    | P_block(base,offset,mode) ->
      (* Compute the interval of the offset *)
      let itv = offset_interval offset range man flow in
      (* Add the size of the pointed cells *)
      let size = sizeof_type (under_type ptr.etyp |> void_to_char) in
      let itv = Bot.bot_lift2 ItvUtils.IntItv.add itv (Itv.of_z Z.zero (Z.pred size)) in
      (* Forget all affected cells *)
      let a = get_env T_cur man flow in
      let cells = cell_set_find_overlapping_itv base itv a.cells in
      List.fold_left (fun acc c -> Post.bind (forget_cell c range man) acc) (Post.return flow) cells

    | _ -> Post.return flow


  let exec stmt man flow =
    match skind stmt with
    | S_c_declaration (v,init,scope) ->
      exec_declare v scope stmt.srange man flow |>
      OptionExt.return

    | S_assign(x, e) when is_c_scalar_type x.etyp ->
      exec_assign x e stmt.srange man flow |>
      OptionExt.return


    | S_add e when is_base_expr e && is_c_type e.etyp ->
      exec_add (expr_to_base e) stmt.srange man flow |>
      OptionExt.return


    | S_remove e when is_base_expr e && is_c_type e.etyp ->
      exec_remove (expr_to_base e) stmt.srange man flow |>
      OptionExt.return


    | S_rename(e1,e2) when is_base_expr e1 && is_base_expr e2 && is_c_type e1.etyp && is_c_type e2.etyp ->
      exec_rename (expr_to_base e1) (expr_to_base e2) stmt.srange man flow |>
      OptionExt.return

    | S_expand(e,el) when is_base_expr e &&
                          is_c_type e.etyp &&
                          List.for_all is_base_expr el ->
      exec_expand (expr_to_base e) (List.map expr_to_base el) stmt.srange man flow |>
      OptionExt.return

    | S_fold(e,el) when is_base_expr e &&
                        is_c_type e.etyp &&
                        List.for_all is_base_expr el ->
      exec_fold (expr_to_base e) (List.map expr_to_base el) stmt.srange man flow |>
      OptionExt.return

    | S_forget(e) when is_c_type e.etyp ->
      exec_forget e stmt.srange man flow |>
      OptionExt.return

    | S_forget({ ekind = E_stub_quantified_formula(quants, e)}) when is_c_type e.etyp ->
      exec_forget_quant quants e stmt.srange man flow |>
      OptionExt.return

    | _ -> None


  (** {2 Communication handlers} *)
  (** ************************** *)

  let ask query man flow = None

end

let () =
  register_stacked_domain (module Domain)
