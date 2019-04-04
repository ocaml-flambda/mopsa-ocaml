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

(** Cell to Scalar variable bindings *)

open Mopsa
open Ast
open Cell
open Zone
module Itv =  ItvUtils.IntItv

module CellScalarEquiv = Equiv.Make(Cell)(Var)

let cell_scalar_ctx =
  let module C = Context.GenUnitKey(
    struct
      type t = CellScalarEquiv.t
      let print fmt m =
        Format.fprintf fmt "Cells vars: @[%a@]" CellScalarEquiv.print m
    end
    )
  in
  C.key

let get_ctx flow =
  try Flow.get_ctx flow |> Context.find_unit cell_scalar_ctx
  with _ -> CellScalarEquiv.empty

let set_ctx ctx flow =
  Flow.set_ctx (Context.add_unit cell_scalar_ctx ctx @@ Flow.get_ctx flow) flow

let init_ctx flow =
  set_ctx CellScalarEquiv.empty flow

let get_scalar_or_create flow c =
  let ctx = get_ctx flow in
  try CellScalarEquiv.find_l c ctx, flow
  with
  | Not_found ->
    let v = cell_to_var c in
    let ctx' = CellScalarEquiv.add (c, v) ctx in
    let flow' = set_ctx ctx' flow in
    (v, flow')

let get_cell flow v =
  let ctx = get_ctx flow in
  try CellScalarEquiv.find_r v ctx
  with
  | Not_found ->
    Exceptions.panic
      "Cell2Scalar get_cell could not find binding of %a in ctx:@\n @[%a@]"
      pp_var v
      CellScalarEquiv.print ctx

let get_scalar_and_remove flow c =
  let ctx = get_ctx flow in
  try
    let v = CellScalarEquiv.find_l c ctx in
    let ctx' = CellScalarEquiv.remove_l c ctx in
    let flow' = set_ctx ctx' flow in
    v, flow'
  with
  | Not_found ->
    let v = cell_to_var c in
    (v, flow)


(** {2 Domain definition} *)
(** ===================== *)

module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  let name = "c.memory.cells.cell_2_scalar"
  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {
      provides = [Z_c_cell];
      uses = [Z_c_scalar]
    };

    ieval = {
      provides = [Z_under Z_c_cell, Z_c_scalar];
      uses = [Z_under Z_c_cell, Z_c_scalar]
    }
  }

  (** Initialization *)
  (** ============== *)

  let init prog man flow =
    Some (init_ctx flow)

  (** Post-conditions *)
  (** *************** *)

  let cell_expr_to_var exp flow =
    match ekind exp with
    | E_c_cell (c, mode) ->
      let v, flow = get_scalar_or_create flow c in
      mk_var v ~mode exp.erange, flow

    | _ -> assert false


  let exec zone stmt man stman flow =
    match skind stmt with
    | S_add ({ ekind = E_c_cell (c, _) } as e)->
      Some (
        match cell_base c with
        | S _ -> Post.return flow
        | _ ->
          let v, flow = cell_expr_to_var e flow in
          man.exec ~zone:Z_c_scalar (mk_add v stmt.srange) flow |>
          Post.return
      )

    | S_remove ({ ekind = E_c_cell _ } as e) ->
      let v, flow = cell_expr_to_var e flow in
      man.exec ~zone:Z_c_scalar (mk_remove v stmt.srange) flow |>
      Post.return |>
      Option.return

    | S_expand (({ekind = E_c_cell _} as e), cl) ->
      let v, flow = cell_expr_to_var e flow in
      let vl, flow =
        let rec doit flow = function
          | [] -> [], flow
          | ee :: tl ->
            let v, flow = cell_expr_to_var ee flow in
            let vl, flow = doit flow tl in
            v :: vl, flow
        in
        doit flow cl
      in
      man.exec ~zone:Z_c_scalar ({stmt with skind = S_expand (v, vl)}) flow |>
      Post.return |>
      Option.return

    | S_rename(({ekind = E_c_cell _} as c1), ({ekind = E_c_cell _} as c2)) ->
      let v1, flow = cell_expr_to_var c1 flow in
      let v2, flow = cell_expr_to_var c2 flow in
      man.exec ~zone:Z_c_scalar (mk_rename v1 v2 stmt.srange) flow |>
      Post.return |>
      Option.return


    | S_assign({ekind = E_c_cell _} as c, e) ->
      Some (
        let v, flow = cell_expr_to_var c flow in
        man.eval ~zone:(Z_under Z_c_cell, Z_c_scalar) e flow |>
        Post.bind_eval man.lattice @@ fun e flow ->

        man.exec ~zone:Z_c_scalar (mk_assign v e stmt.srange) flow |>
        Post.return
      )

    | S_assume(e) ->
      Some (
        man.eval ~zone:(Z_under Z_c_cell, Z_c_scalar) e flow |>
        Post.bind_eval man.lattice @@ fun e' flow ->

        let stmt' = {stmt with skind = S_assume e'} in
        man.exec ~zone:Zone.Z_c_scalar stmt' flow |>
        Post.return
      )

    | _ -> None

  (** Evaluations *)
  (** *********** *)

  let rec eval zone exp man flow =
    match ekind exp with
    | E_c_cell({b = S s; o = O_single z}, mode) ->
      (* Case of a static string literal with a constant offset *)
      (* return the scalar value of the character *)
      let len = String.length s in
      if Z.equal z (Z.of_int len) then
        Eval.singleton (Universal.Ast.mk_zero exp.erange ~typ:exp.etyp) flow |>
        Option.return
      else
        let ch = String.get s (Z.to_int z) in
        Eval.singleton (Universal.Ast.mk_int (int_of_char ch) ~typ:exp.etyp exp.erange) flow |>
        Option.return

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
      Option.return

    | E_c_cell _ ->
      let v, flow = cell_expr_to_var exp flow in
      Eval.singleton v flow |>
      Option.return

    | _ -> None

  (** Queries *)
  (** ******* *)

  let ask _ _ _  = None

end

let () =
  Framework.Core.Sig.Simplified.Stateless.register_stack (module Domain)
