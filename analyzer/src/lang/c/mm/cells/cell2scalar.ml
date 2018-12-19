(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Cell to Scalarerical variable bindings *)

open Mopsa
open Ast
open Cell
open Zone
module Itv =  ItvUtils.IntItv

module CellScalarEquiv = Equiv.Make(PrimedCell)(PrimedVar)

type ('a, _) Annotation.key +=
  | A_cell_scalar: ('a, CellScalarEquiv.t) Annotation.key


let () =
  Annotation.(register_stateless_annot {
      eq = (let f: type a b. (a, b) key -> (CellScalarEquiv.t, b) eq option =
              function
              | A_cell_scalar -> Some Eq
              | _ -> None
            in
            f);
      print = (fun fmt m -> Format.fprintf fmt "Cells vars: @[%a@]" CellScalarEquiv.print m);
    }) ();
  ()

let get_annot flow =
  try Flow.get_annot A_cell_scalar flow
  with _ -> CellScalarEquiv.empty

let get_scalar_or_create flow c =
  let annot = get_annot flow in
  try CellScalarEquiv.find_l c annot, flow
  with
  | Not_found ->
    let v = primed_lift cell_to_var c in
    let annot' = CellScalarEquiv.add (c, v) annot in
    let flow' = Flow.set_annot A_cell_scalar annot' flow in
    (v, flow')

let get_cell flow v =
  let annot = get_annot flow in
  try CellScalarEquiv.find_r v annot
  with
  | Not_found ->
    Exceptions.panic
      "Cell2Scalar get_cell could not find binding of %a in annot:@\n @[%a@]"
      PrimedVar.print v
      CellScalarEquiv.print annot

let get_scalar_and_remove flow c =
  let annot = get_annot flow in
  try
    let v = CellScalarEquiv.find_l c annot in
    let annot' = CellScalarEquiv.remove_l c annot in
    let flow' = Flow.set_annot A_cell_scalar annot' flow in
    v, flow'
  with
  | Not_found ->
    let v = primed_lift cell_to_var c in
    (v, flow)


(** {2 Domain definition} *)
(** ===================== *)

module Domain : Framework.Domains.Stateless.S =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_cell_2_scalar : unit domain
  let id = D_c_cell_2_scalar
  let name = "c.memory.cells.cell_2_scalar"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_cell_2_scalar -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {
    export = [Z_c_cell];
    import = [Z_c_scalar]
  }

  let eval_interface = {
    export = [Z_under Z_c_cell, Z_c_scalar];
    import = [Z_under Z_c_cell, Z_c_scalar]
  }

  (** Initialization *)
  (** ============== *)

  let init prog man flow =
    Some (
      Flow.set_annot A_cell_scalar CellScalarEquiv.empty flow
    )

  (** Post-conditions *)
  (** *************** *)

  let exec zone stmt man flow =
    match skind stmt with
    | S_add c when PrimedCell.match_expr c ->
      let pc = PrimedCell.from_expr c in
      let mode = PrimedCell.ext_from_expr c in
      let pv, flow = get_scalar_or_create flow pc in
      let vv = PrimedVar.to_expr pv mode stmt.srange in
      man.exec ~zone:Z_c_scalar (mk_add vv stmt.srange) flow |>
      Post.return

    | S_remove c when PrimedCell.match_expr c ->
      let pc = PrimedCell.from_expr c in
      let mode = PrimedCell.ext_from_expr c in
      let pv, flow = get_scalar_and_remove flow pc in
      let vv = PrimedVar.to_expr pv mode stmt.srange in
      man.exec ~zone:Z_c_scalar (mk_remove vv stmt.srange) flow |>
      Post.return

    | S_expand (c, cl) when PrimedCell.match_expr c &&
                            List.for_all PrimedCell.match_expr cl ->
      let pc = PrimedCell.from_expr c in
      let mode = PrimedCell.ext_from_expr c in
      let pcl = List.map PrimedCell.from_expr cl in
      let model = List.map PrimedCell.ext_from_expr cl in
      let pv, flow = get_scalar_or_create flow pc in
      let vv = PrimedVar.to_expr pv mode stmt.srange in
      let vl, flow =
        let rec doit flow = function
          | [] -> [], flow
          | (pc, mode) :: tl ->
            let pv, flow = get_scalar_or_create flow pc in
            let vv = PrimedVar.to_expr pv mode stmt.srange in
            let vl, flow = doit flow tl in
            vv :: vl, flow
        in
        doit flow (List.combine pcl model)
      in
      man.exec ~zone:Z_c_scalar ({stmt with skind = S_expand (vv, vl)}) flow |>
      Post.return

    | S_rename(c1, c2) when PrimedCell.match_expr c1 &&
                            PrimedCell.match_expr c2
      ->
      let pc1 = PrimedCell.from_expr c1 in
      let mode1 = PrimedCell.ext_from_expr c1 in
      let pv1, flow = get_scalar_and_remove flow pc1 in
      let vv1 = PrimedVar.to_expr pv1 mode1 stmt.srange in

      let pc2 = PrimedCell.from_expr c2 in
      let mode2 = PrimedCell.ext_from_expr c2 in
      let pv2, flow = get_scalar_or_create flow pc2 in
      let vv2 = PrimedVar.to_expr pv2 mode2 stmt.srange in

      man.exec ~zone:Z_c_scalar (mk_rename vv1 vv2 stmt.srange) flow |>
      Post.return


    | S_assign(c, e) when PrimedCell.match_expr c ->
      let pc = PrimedCell.from_expr c in
      let mode = PrimedCell.ext_from_expr c in
      let pv, flow = get_scalar_or_create flow pc in
      let vv = PrimedVar.to_expr pv mode stmt.srange in

      man.eval ~zone:(Z_under Z_c_cell, Z_c_scalar) e flow |>
      Post.bind_opt man @@ fun e flow ->

      man.exec ~zone:Z_c_scalar (mk_assign vv e stmt.srange) flow |>
      Post.return

    | S_assume(e) ->
      man.eval ~zone:(Z_under Z_c_cell, Z_c_scalar) e flow |>
      Post.bind_opt man @@ fun e' flow ->

      let stmt' = {stmt with skind = S_assume e'} in
      man.exec ~zone:Zone.Z_c_scalar stmt' flow |>
      Post.return

    | _ -> None

  (** Evaluations *)
  (** *********** *)

  let rec eval zone exp man flow =
    match ekind exp with
    | E_c_cell({b = S s; o = O_single z}, mode) ->
      (* Case of a static string literal with a constant offset *)
      (* return the scalar value of the character *)
      let ch = String.get s (Z.to_int z) in
      Eval.singleton (Universal.Ast.mk_int (int_of_char ch) ~typ:exp.etyp exp.erange) flow |>
      Eval.return

    | E_c_cell({b = S s; o = O_region itv}, mode) ->
      (* Otherwise, return the interval covering characters of the string within itv *)
      itv |> Bot.bot_dfl1
        (* Case of empty interval *)
        (Eval.empty_singleton flow)

        (* Otherwise, iterate over the values of itv *)
        (fun (imin, imax) ->
           let imin, imax =
             match imin, imax with
             | ItvUtils.IntBound.Finite imin, ItvUtils.IntBound.Finite imax -> imin, imax
             | _ -> assert false
           in
           let at i = String.get s (Z.to_int i) |> int_of_char |> Z.of_int in
           let cst n = (n,n) in
           let add n (a,b) = (Z.min n a), (Z.max n b) in

           let rec aux i =
             if Z.equal i imax
             then cst (at i)
             else add (at i) (aux (Z.succ i))
           in
           let vmin, vmax = aux imin in
           Eval.singleton (Universal.Ast.mk_z_interval vmin vmax ~typ:exp.etyp exp.erange) flow
        )
      |>
      Eval.return

    | c when PrimedCell.match_expr exp ->
      let pc = PrimedCell.from_expr exp in
      let mode = PrimedCell.ext_from_expr exp in
      let pv, flow = get_scalar_or_create flow pc in
      let vv = PrimedVar.to_expr pv mode exp.erange in
      Eval.singleton vv flow |>
      Eval.return

    | _ -> None

  (** Queries *)
  (** ******* *)

  let ask _ _ _  = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
