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
open Framework.Visitor
open Common.Base
open Ast

(* To support different cell-based memory models, an extensible type
   is used and domains can define their own representation of cells.
   *)
type cell = ..

(* Extraction of base and offset *)
(* ============================= *)

let cell_extract_chain : (cell-> base * (range -> expr) * typ) ref = ref (
    fun _ -> panic "cell.extract: unknown cell"
  )
let register_cell_extract ex = cell_extract_chain := ex !cell_extract_chain
let extract_cell_info c = !cell_extract_chain c

let cell_type c =
  let _, _, typ = extract_cell_info c in
  typ

let cell_base c =
  let b, _, _ = extract_cell_info c in
  b

let cell_offset c =
  let _, o, _ = extract_cell_info c in
  o

(* Transformation to variables *)
(* =========================== *)

let cell_var_chain : (cell-> var) ref = ref (fun _ -> panic "cell.to_var: unknown cell")
let register_cell_var f = cell_var_chain := f !cell_var_chain
let cell_to_var c = !cell_var_chain c


(* Comparison order *)
(* ================ *)

let cell_compare_chain : (cell -> cell -> int) ref = ref Pervasives.compare
let register_cell_compare cmp = cell_compare_chain := cmp !cell_compare_chain
let compare_cell c1 c2 = !cell_compare_chain c1 c2


(* Pretty printer *)
(* ============== *)

let cell_pp_chain : (Format.formatter -> cell -> unit) ref = ref (
    fun _ _ -> Exceptions.panic "cell.pp: unknown cell"
  )
let register_cell_pp pp = cell_pp_chain := pp !cell_pp_chain
let pp_cell fmt c = !cell_pp_chain fmt c


(* Registration of a new cell *)
(* ========================== *)

type cell_info = {
  extract : (cell -> base * (range -> expr) * typ) -> cell -> base * (range -> expr) * typ;
  to_var  : (cell -> var) -> cell -> var;
  compare : (cell -> cell -> int) -> cell -> cell -> int;
  print   : (Format.formatter -> cell -> unit) -> Format.formatter -> cell -> unit;
}

let register_cell info =
  register_cell_extract info.extract;
  register_cell_var info.to_var;
  register_cell_compare info.compare;
  register_cell_pp info.print;
  ()





(* Cell expressions and statements *)
(* =============================== *)

type expr_kind +=
  | E_c_cell of cell * mode (* Expression representing a cell *)

type stmt_kind +=
  | S_c_add_cell    of cell (* Add a cell as a new dimension *)
  | S_c_remove_cell of cell (* Ask for the removing of a cell *)
  | S_c_expand_cell of cell * cell list (* Expand a cell into a set of cells *)

let mk_cell c ?(mode = STRONG) range =
  mk_expr (E_c_cell(c, mode)) ~etyp:(cell_type c) range

let mk_remove_cell c range =
  mk_stmt (S_c_remove_cell c) range

let mk_c_add_cell c range =
  mk_stmt (S_c_add_cell c) range

let mk_c_invalid range =
  mk_constant C_c_invalid range ~etyp:(Ast.T_c_pointer(Ast.T_c_void))

let mk_cell_expand c cl range =
  mk_stmt (S_c_expand_cell (c, cl)) range

let cell_of_expr e =
  match ekind e with
  | E_c_cell (c, mode) -> c, mode
  | _ -> assert false

let () =
  register_expr {
    compare = (fun next e1 e2 ->
        match ekind e1, ekind e2 with
        | E_c_cell(c1, s1), E_c_cell(c2, s2) ->
          Compare.compose
            [
              (fun () -> compare c1 c2);
              (fun () -> compare_mode s1 s2 )
            ]

        | _ -> next e1 e2
      );
    print = (fun next fmt e ->
        match ekind e with
        | E_c_cell(c, STRONG) -> pp_cell fmt c
        | E_c_cell(c, WEAK) -> Format.fprintf fmt "_w_%a" pp_cell c
        | _ -> next fmt e
      );
    visit = (fun next e ->
        let open Framework.Visitor in
        match ekind e with
        | E_c_cell(c, s) -> leaf e
        | _ -> next e
      )
  };
  register_stmt {
    compare = (fun next stmt1 stmt2 ->
        match skind stmt1, skind stmt2 with
        | S_c_add_cell c1, S_c_add_cell c2 -> compare_cell c1 c2

        | S_c_remove_cell c1, S_c_remove_cell c2 -> compare_cell c1 c2

        | S_c_expand_cell (c1, cl1), S_c_expand_cell (c2, cl2) ->
          Compare.compose [
            (fun () -> compare_cell c1 c2);
            (fun () -> Compare.list compare_cell cl1 cl2)
          ]

        | _ -> next stmt1 stmt2
      );
    print = (fun next fmt stmt ->
        match skind stmt with
        | S_c_add_cell c ->
          Format.fprintf fmt "S_c_add_cell(%a)" pp_cell c

        | S_c_remove_cell c ->
          Format.fprintf fmt "S_c_remove_cell(%a)" pp_cell c

        | S_c_expand_cell(c, cl) ->
          Format.fprintf fmt "expand(%a,{%a})"
            pp_cell c
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
               pp_cell) cl

        | _ -> next fmt stmt
      );
    visit = (fun next stmt ->
        match skind stmt with
        | S_c_add_cell c -> Visitor.leaf stmt
        | S_c_remove_cell c -> Visitor.leaf stmt
        | S_c_expand_cell _ -> Visitor.leaf stmt
        | _ -> next stmt
      );
  };
  ()


(* Cell zoning *)
(* =========== *)

open Framework.Zone

type zone +=
  | Z_c_cell

let () =
  register_zone {
    zone = Z_c_cell;
    name = "C/Cell";
    subset = None;
    eval = (fun exp ->
        match ekind exp with
        | E_constant _
        | E_c_cell _ -> Keep

        | E_var(v, _) when Universal.Ast.is_math_type v.vtyp -> Keep

        | E_c_deref _ -> Process

        | E_c_cast _
        | E_unop _
        | E_binop _ -> Visit

        | _ -> Process
      );
  }


(* Utility modules *)
(* ============== *)

module Cell =
struct
  type t = cell
  let compare = compare_cell
  let print = pp_cell
end


(** Stub support *)
(** ============ *)

type cell +=
  | C_old of cell (** old version of a cell, used in stubs *)

let mk_old_cell c ?(mode = STRONG) range =
  mk_expr (E_c_cell(C_old c, mode)) ~etyp:(cell_type c) range

let () =
  register_cell {
    extract = (fun next c ->
        match c with
        | C_old cc -> extract_cell_info cc
        | _ -> next c
      );

    to_var = (fun next c ->
        match c with
        | C_old cc ->
          let v = cell_to_var cc in
          { v with vname = "old(" ^ v.vname ^ ")" }
        | _ -> next c
      );

    compare = (fun next c1 c2 ->
        match c1, c2 with
        | C_old cc1, C_old cc2 -> compare_cell cc1 cc2
        | _ -> next c1 c2
      );

    print = (fun next fmt c ->
        match c with
        | C_old cc -> Format.fprintf fmt "old(%a)" pp_cell cc
        | _ -> next fmt c
      );
  }
