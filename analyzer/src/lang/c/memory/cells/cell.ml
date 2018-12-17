(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Cells are contiguous memory blocks identified by a base storage
   region (a variable, a string literal or a dynamically allocated
   memory block), a numeric offset and a type.  *)

open Mopsa
open Framework.Visitor
open Common.Base
open Ast
module Itv = Universal.Numeric.Values.Intervals.Value


(** {2 Cell offset} *)
(** *************** *)

(** Kinds of a cell offset *)
type offset =
  | O_single of Z.t     (** Integer offset of a single cell *)
  | O_region of Itv.t   (** Interval offset covering a contiguous region of cells*)
  | O_out_of_bound      (** Offsets out-of-bound of the allocated size of a base *)

let compare_offset o1 o2 =
  match o1, o2 with
  | O_single n1, O_single n2 -> Z.compare n1 n2
  | O_region itv1, O_region itv2 -> Itv.compare_interval itv1 itv2
  | _ -> compare o1 o2

let pp_offset fmt o =
  match o with
  | O_single n -> Z.pp_print fmt n
  | O_region itv -> Itv.print fmt itv
  | O_out_of_bound -> Format.fprintf fmt "⚠"


(** {2 Cells} *)
(** ********* *)

(** A cell is identified by a base, an offset and a type *)
type cell = {
  b: base ;      (** base of the cell *)
  o: offset;     (** offset of the cell within the base *)
  t: typ;        (** type of the cell *)
}

let cell_base c = c.b

let cell_offset c = c.o

let cell_zoffset c =
  match c.o with
  | O_single n -> n
  | _ -> assert false

let cell_typ c = c.t

(** Compare two cells *)
let compare_cell c1 c2 =
  Compare.compose [
    (fun () -> compare_base c1.b c2.b);
    (fun () -> compare_offset c1.o c2.o);
    (fun () -> compare_typ c1.t c2.t);
  ]

(** Print a cell *)
let pp_cell fmt c =
  Format.fprintf fmt "⟨%a,%a,%a⟩"
    pp_base c.b
    pp_offset c.o
    pp_typ c.t

(** Create a C scalar variable from a cell *)
let cell_to_var c : var =
  let vname =
    let () = Format.fprintf Format.str_formatter
        "{%a:%a:%a}"
        pp_base c.b
        pp_offset c.o
        Pp.pp_c_type_short c.t
    in
    Format.flush_str_formatter ()
  in
  {
    vname;
    vuid = base_uid c.b;
    vtyp = c.t;
  }


(** {2 Cell extensions to the AST} *)

type expr_kind +=
  | E_c_cell of cell * mode (* Expression representing a cell *)

(** Create a cell expression *)
let mk_c_cell c ?(mode = STRONG) range =
  mk_expr (E_c_cell(c, mode)) ~etyp:c.t range


module Cell =
struct
  type t = cell
  let compare = compare_cell
  let print = pp_cell
end


(** {2 Primed cells} *)

module PrimedCell = MakePrimedExt(
  struct
    type t = cell

    type ext = mode

    let compare = compare_cell

    let print = pp_cell

    let match_expr e =
      match ekind e with
      | E_c_cell _ -> true
      | _ -> false

    let from_expr e =
      match ekind e with
      | E_c_cell (c, m) -> c, m
      | _ -> assert false

    let to_expr c mode range = mk_c_cell c ~mode range

  end)


let () =
  register_expr {
    compare = (fun next e1 e2 ->
        match ekind e1, ekind e2 with
        | E_c_cell(c1, m1), E_c_cell(c2, m2) ->
          Compare.compose [
               (fun () -> compare_cell c1 c2);
               (fun () -> compare m1 m2);
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
        match ekind e with
        | E_c_cell _ -> leaf e
        | _ -> next e
      )
   }


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
        | E_c_cell _ -> Keep
        | E_var _ when exp.etyp |> is_c_scalar_type -> Process
        | E_c_address_of _ -> Keep
        | E_primed _ -> Visit
        | Stubs.Ast.E_stub_quantified _ -> Visit
        | _ -> Framework.Zone.eval exp Zone.Z_c_low_level
      );
  }
