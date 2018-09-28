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
open Zone

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
      print = (fun fmt m -> Format.fprintf fmt "Cells vars: @[%a@]" CellNumEquiv.print m);
    }) ();
  ()

let get_annot flow =
  try Flow.get_annot KCellNumEquiv flow
  with _ -> CellNumEquiv.empty

let get_num flow c =
  let cne = get_annot flow in
  try
    (CellNumEquiv.find_l c cne, flow)
  with
  | Not_found ->
    let v = cell_to_var c in
    (v, Flow.set_annot KCellNumEquiv (CellNumEquiv.add (c, v) cne) flow)

let get_cell flow v =
  let cne = get_annot flow in
  try
    (CellNumEquiv.find_r v cne, flow)
  with
  | Not_found ->
    Debug.fail "Cell2Num get_cell could not find binding of %a in \
                annot: (*FIXME print annot*)"
      pp_var v

let get_num_and_remove flow c =
  let cne = get_annot flow in
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

  let exec_interface = {export = [Z_c_cell]; import = [Z_c_scalar_num]}
  let eval_interface = {export = [Z_c_cell, Z_c_scalar_num]; import = [Z_c_cell, Z_c_scalar_num]}

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
      man.exec ~zone:Z_c_scalar_num ({stmt with skind = S_remove_var v}) flow
      |> Post.of_flow
      |> OptionExt.return

    | S_assign(lval, rval) when etyp lval |> is_c_int_type ->
      man.eval ~zone:(Z_c_cell, Z_c_scalar_num) lval flow |>
      Post.bind_opt man @@ fun lval' flow ->

      man.eval ~zone:(Z_c_cell, Z_c_scalar_num) rval flow |>
      Post.bind_opt man @@ fun rval' flow ->

      man.exec ~zone:Z_c_scalar_num (mk_assign lval' rval' stmt.srange) flow |>
      Post.of_flow |>
      OptionExt.return

    | S_assume(e) ->
      begin
        man.eval ~zone:(Z_c_cell, Z_c_scalar_num) e flow |>
        Post.bind man @@ fun e' flow ->
        let stmt' = {stmt with skind = S_assume e'} in
        man.exec ~zone:Zone.Z_c_scalar_num stmt' flow |>
        Post.of_flow
      end |>
      OptionExt.return

    | _ -> None

  (** Evaluations *)
  (** *********** *)

  let rec eval zone exp man flow =
    match ekind exp with
    | E_c_cell(c, mode) when cell_type c |> is_c_int_type ->
      begin
        match cell_base c with
        (* Case of a static string literal *)
        | Base.S s ->
          begin
            let offset = cell_offset c exp.erange in
            match Universal.Utils.expr_to_z offset with
            (* Case of constant offset => return the num value of the character *)
            | Some z ->
              let ch = String.get s (Z.to_int z) in
              Eval.singleton (Universal.Ast.mk_int (int_of_char ch) exp.erange) flow

            (* Otherwise, return the interval covering all characters of the string *)
            | None ->
              let len = String.length s in
              let at i = String.get s i |> int_of_char in
              let rec aux i a b =
                if i = 0
                then aux (i + 1) (at 0) (at 0)
                else if i = len
                then a, b
                else
                  let a', b' = aux (i + 1) a b in
                  min a a', max b b'
              in
              let min, max = aux 0 0 0 in
              Eval.singleton (Universal.Ast.mk_int_interval min max exp.erange) flow
          end

        | _ ->
          let range = erange exp in
          let v, flow = get_num flow c in
          let exp = mk_var v (tag_range range "cell2num") ~mode in
          Eval.singleton exp flow
      end
      |> Option.return

    | _ -> None

  (** Queries *)
  (** ******* *)

  let ask _ _ _  = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
