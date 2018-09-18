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
open Cell

module CellNumEquiv = Equiv.Make(Cell)(Var)

type ('a, _) Annotation.key +=
  | KCellNumEquiv: ('a, CellNumEquiv.t) Annotation.key


let () =
  Annotation.(register_stateless_annot {
      eq = (let f: type a b. (a, b) key -> (CellNumEquiv.t, b) eq option =
              function
              | KCellNumEquiv -> Some Eq
              | _ -> None
            in
            f);
    }) ();
  ()


let get_num flow c =
  let cne = Flow.get_annot KCellNumEquiv flow in
  try
    (CellNumEquiv.find_l c cne, flow)
  with
  | Not_found ->
    let v = cell_to_var c in
    (v, Flow.set_annot KCellNumEquiv (CellNumEquiv.add (c, v) cne) flow)

let get_cell flow v =
  let cne = Flow.get_annot KCellNumEquiv flow in
  try
    (CellNumEquiv.find_r v cne, flow)
  with
  | Not_found ->
    Debug.fail "Cell2Num get_cell could not find binding of %a in \
                annot: (*FIXME print annot*)"
      pp_var v

let get_num_and_remove flow c =
  let cne = Flow.get_annot KCellNumEquiv flow in
  try
    (CellNumEquiv.find_l c cne, Flow.set_annot KCellNumEquiv (CellNumEquiv.remove_l c cne) flow)
  with
  | Not_found ->
    let v = cell_to_var c in
    (v, flow)

(** {2 Domain definition} *)
(** ===================== *)

module Domain : Framework.Domains.Stateless.S =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_cell_2_num : unit domain
  let id = D_c_cell_2_num
  let name = "c.memory.cells.cell_2_num"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_cell_2_num -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {export = [Z_c_cell]; import = [Zone.Z_c_num]}
  let eval_interface = {export = [Z_c_cell, Zone.Z_c_num]; import = []}

  (** Initialization *)
  (** ============== *)

  let init prog man flow =
    Some (
      Flow.set_annot KCellNumEquiv CellNumEquiv.empty flow
    )

  (** Post-conditions *)
  (** *************** *)

  let exec zone stmt man flow =
    match skind stmt with
    | S_c_remove_cell c when cell_type c |> is_c_int_type ->
      let v, flow = get_num_and_remove flow c in
      man.exec ~zone:Zone.Z_c_num ({stmt with skind = S_remove_var v}) flow
      |> Post.of_flow
      |> Option.return

    | S_assign({ekind = E_c_cell(c, mode)} as lval, rval) when cell_type c |> is_c_int_type ->
      let v, flow = get_num flow c in
      man.exec ~zone:Zone.Z_c_num (mk_assign (mk_var v ~mode:mode lval.erange) rval stmt.srange) flow
      |> Post.of_flow
      |> Option.return

    | _ -> None

  (** Evaluations *)
  (** *********** *)

  let eval zone exp man flow =
    match ekind exp with
    | E_c_cell(c, mode) when cell_type c |> is_c_int_type ->
      let range = erange exp in
      let v, flow = get_num flow c in
      let exp = mk_var v (tag_range range "cell2num") in
      Eval.singleton exp flow
      |> Option.return
    | _ -> None

  (** Queries *)
  (** ******* *)

  let ask _ _ _  = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
