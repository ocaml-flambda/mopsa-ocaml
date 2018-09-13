(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Cell to Numerical variable bindings *)

open Framework.Essentials
open Ast

module CellNumEquiv = Equiv.Make(Cell.Cell)(Var)

type ('a, _) Annotation.key +=
  | KCellNumEquiv: ('a, CellNumEquiv.t) Annotation.key

let get_num flow c =
  let annot = Flow.get_annot flow in
  let cne = Annotation.find KCellNumEquiv annot in
  try
    (CellNumEquiv.find_l c cne, flow)
  with
  | Not_found ->
    let v = Cell.cell_to_var c in
    (v, Flow.set_annot (Annotation.add KCellNumEquiv (CellNumEquiv.add (c, v) cne) annot) flow)

let get_cell flow v =
  let annot = Flow.get_annot flow in
  let cne = Annotation.find KCellNumEquiv annot in
  try
    (CellNumEquiv.find_r v cne, flow)
  with
  | Not_found ->
    Debug.fail "Cell2Num get_cell could not find binding of %a in \
                annot: (*FIXME print annot*)"
      pp_var v

(** {2 Domain definition} *)
(** ===================== *)

module Domain : Framework.Domains.Stateless.S =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_cell_2_num : unit domain
  let id = D_c_cell_2_num
  let name = "c.cell_2_num"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_cell_2_num -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let zone = Zone.Z_c
  let import_exec = [Universal.Zone.Z_universal]
  let import_eval = []

  (** Initialization *)
  (** ============== *)

  let init _ _ _ =
    None

  let exec _ _ _ = None

  let eval exp man flow =
    match ekind exp with
    | Cell.E_c_cell c ->
      let range = erange exp in
      let v, flow = get_num flow c in
      let exp = mk_var v (tag_range range "cell2num") in
      Eval.singleton exp flow
      |> Option.return
    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
