(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Cells represent contiguous memory regions defined by a base
   variable, a numeric offset and a type.
*)

open Framework.Essentials
open Base

(* To support different cell-based memory models, an extensible type
   is used and domains can define their own representation of cells.
   *)
type cell = ..


(* Extraction of base and offset *)
(* ============================= *)

let cell_extract_chain : (cell -> base * expr * typ) ref = ref (
    fun _ -> Debug.fail "cell.extract: unknown cell"
  )
let register_cell_extract ex = cell_extract_chain := ex !cell_extract_chain
let extract_cell_info c = !cell_extract_chain c


(* Comparison order *)
(* ================ *)

let cell_compare_chain : (cell -> cell -> int) ref = ref Pervasives.compare
let register_cell_compare cmp = cell_compare_chain := cmp !cell_compare_chain
let compare_cell c1 c2 = !cell_compare_chain c1 c2


(* Pretty printer *)
(* ============== *)

let cell_pp_chain : (Format.formatter -> cell -> unit) ref = ref (
    fun _ _ -> Debug.fail "cell.pp: unknown cell"
  )
let register_cell_pp pp = cell_pp_chain := pp !cell_pp_chain
let pp_cell fmt c = !cell_pp_chain fmt c

(* Registration of a new cell *)
(* ========================== *)

type cell_info = {
  extract : (cell -> base * expr * typ) -> cell -> base * expr * typ;
  compare : (cell -> cell -> int) -> cell -> cell -> int;
  print   : (Format.formatter -> cell -> unit) -> Format.formatter -> cell -> unit;
}

let register_cell info =
  register_cell_extract info.extract;
  register_cell_compare info.compare;
  register_cell_pp info.print;
  ()


(* Cell expression *)
(* =============== *)

type expr_kind +=
  | E_c_cell of cell
  (* Expression representing a cell *)

  | E_c_points_to of base * expr * typ
  (* Reply to a points-to evaluation *)

let () =
  register_expr {
    compare = (fun next e1 e2 ->
        match ekind e1, ekind e2 with
        | E_c_cell c1, E_c_cell c2 -> compare c1 c2

        | E_c_points_to (b1, o1, t1), E_c_points_to (b2, o2, t2) ->
          Compare.compose [
            (fun () -> compare_base b1 b2);
            (fun () -> compare_expr o1 o2);
            (fun () -> compare_typ t1 t2);
          ]

        | _ -> next e1 e2
      );
    print = (fun next fmt e ->
        match ekind e with
        | E_c_cell c -> pp_cell fmt c
        | E_c_points_to(b,o,t) -> Format.fprintf fmt "⇝(%a, %a, %a)" pp_base b pp_expr o pp_typ t
        | _ -> next fmt e
      );
    visit = (fun next e ->
        let open Framework.Visitor in
        match ekind e with
        | E_c_cell c -> leaf e
        | E_c_points_to(b,o,t) ->
          {exprs = [o]; stmts = []},
          (function {exprs = [o]} -> {e with ekind = E_c_points_to(b,o,t)} | _ -> assert false)
        | _ -> next e
      );
  }


(* Cell zoning *)
(* =========== *)

open Framework.Zone

type zone +=
  | Z_c_cell
  | Z_c_cell_deref_free
  | Z_c_cell_points_to

let () =
  register_zone {
      subset = (fun next z1 z2 ->
        match z1, z2 with
        | Z_c_cell_deref_free, Z_c_cell -> true
        | _ -> next z1 z2
        );
      print = (fun next fmt z ->
          match z with
          | Z_c_cell -> Format.fprintf fmt "c/cell"
          | Z_c_cell_deref_free -> Format.fprintf fmt "c/cell/deref-free"
          | Z_c_cell_points_to -> Format.fprintf fmt "c/cell/points-to"
          | _ -> next fmt z
        );
    }
