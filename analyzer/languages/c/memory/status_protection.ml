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

(** Domain implementing various checks for protecting memory access *)

open Mopsa
open Sig.Abstraction.Stateless
open Universal.Ast
open Ast
open Stubs.Ast
open Universal.Numeric.Common
open Common
open Common.Base
open Common.Points_to
open Common.Alarms
open Common.Runtime

module Domain =
struct

  (** {2 Domain header *)
  (** ================ *)

  include GenStatelessDomainId(struct
      let name = "c.runtime.status_protection"
    end)

  let dependencies = []

  let checks = [ ]


  (** {2 Utility functions} *)
  (** ===================== *)
  (* let advance_offset (op: operator) (ptr:points_to) (o:expr) range : points_to =
    match ptr with 
    | P_top -> P_top
    | P_fun _ -> failwith "offset on function pointers unsupported"
    | P_invalid -> P_invalid
    | P_null -> P_null
    | P_block(b, off, mode) -> 
      let off' = mk_binop off op o range in 
      P_block(b, off', mode)



  let rec lval_status lval man flow = 
    match ekind lval with 
    | E_c_deref e -> rval_status e man flow 
    | E_var (v, mode) -> 
      let base = mk_base (Var v) in
      Cases.singleton (P_block (base, mk_zero lval.erange, mode)) flow
    | E_c_cast (e, _) -> lval_status e man flow
    | E_constant (C_top t) -> Cases.singleton (P_top) flow
    | E_binop((O_plus | O_minus) as op, e1, e2) when 
      is_c_pointer_type e1.etyp || is_c_pointer_type e2.etyp ->
      let p, i = if is_c_pointer_type e1.etyp || is_c_array_type e1.etyp then e1, e2 else e2, e1
      in
        (* FIXME: this might be broken *)
        lval_status p man flow >>$ fun ptr flow -> 
        Cases.singleton (advance_offset op ptr i lval.erange) flow
    | _ -> 
      let () = Debug.debug ~channel:"eval" "lexpr, failed at %a" pp_expr lval in
      (* Cases.singleton None flow *)
      failwith "unsupported expression"

  and rval_status expr man flow = 
    match ekind expr with 
    | E_c_address_of e -> lval_status e man flow  
    | E_c_cast (e, _) -> rval_status e man flow
    | E_constant (C_top t) -> Cases.singleton (P_top) flow
    | E_var (v, _) -> resolve_pointer expr man flow 
    | E_c_deref a when is_c_array_type (under_type a.etyp) ->
        rval_status a man flow
    | E_c_deref e -> 
      failwith "deref"
      (* let () = Debug.debug ~channel:"eval" "rexpr, failed at deref %a" pp_expr e  in
        Cases.singleton None flow *)
    | E_binop((O_plus | O_minus), e1, e2) when is_c_pointer_type e1.etyp
                                                || is_c_pointer_type e2.etyp->
      let p, i =
        if is_c_pointer_type e1.etyp || is_c_array_type e1.etyp
        then e1, e2
        else e2, e1
      in
      rval_status p man flow 

      (* eval_opt p |>
      OptionExt.lift @@ fun ptr ->
      advance_offset op ptr i p.etyp exp.erange *)
    | _ -> 
      let () = Debug.debug ~channel:"eval" "rexpr, failed at %a" pp_expr expr in
      failwith "unsupported expression"
      (* Cases.singleton None flow *)




  (* let check_lval_access lval man flow =
    let ptr = mk_c_address_of lval lval.erange in
    man.eval ~route:(Below name) ptr flow >>$ fun expr flow ->
    rval_status expr man flow >>$ fun var flow ->
      match var with 
      | None -> 
        failwith (Format.asprintf "could not determine variable from %a" pp_expr expr)
      | Some (PVar v) ->  
        let () = Debug.debug ~channel:"eval" "checking %a" pp_var v in 
        let expr = mk_c_deref expr lval.erange in 
        Cases.singleton expr flow
      | Some _ -> 
        let expr = mk_c_deref expr lval.erange in 
        Cases.singleton expr flow  *)

  let check_ptr_access ptr man flow = 
    let () = Debug.debug ~channel:"eval" "checking %a" pp_points_to ptr in
    match ptr with 
    | P_invalid | P_fun _ | P_null | P_top -> (* okay *) 
      Post.return flow 
    | P_block (base, offset, mode) -> 
      let () = Debug.debug ~channel:"eval" "checking base: %a, offset: %a " pp_base base pp_expr offset in
      Post.return flow 


  (* We rewrite accesses [e] -> * (& e) when e is an l-value 
      and make sure that &e is actually active when we attempt to access it. *)
  let eval_lval_access lval man flow =
    let ptr = mk_c_address_of lval lval.erange in
    let () = Debug.debug ~channel:"eval" "original %a" pp_expr ptr in
    (* we simplify the pointer to something without calls *)
    man.eval ~route:(Below name) ptr flow >>$ fun expr flow ->
      (* we then turn the resulting expression into a pointsto assertion *)
    let () = Debug.debug ~channel:"eval" "simplified %a" pp_expr expr in
    lval_status expr man flow >>$ fun ptr flow ->
    (* we check the pointsto assertion *)
    check_ptr_access ptr man flow >>% fun flow -> 
    (* finally we return *expr for the original expression *)
    let expr = mk_c_deref expr lval.erange in 
    Cases.singleton expr flow 
 *)
(* 

 let check_ptr_access ptr man flow = 
  let () = Debug.debug ~channel:"eval" "checking %a" pp_points_to ptr in
  match ptr with 
  | P_invalid | P_fun _ | P_null | P_top -> (* okay *) 
    Post.return flow 
  | P_block (base, offset, mode) -> 
    let () = Debug.debug ~channel:"eval" "checking base: %a, offset: %a " pp_base base pp_expr offset in
    Post.return flow  *)


  let check_var_access var range man flow =
    resolve_status var man flow >>$ fun status flow ->
    match status with 
    | BOT | Nbt Untracked -> Post.return flow 
    | Nbt Active -> 
      let flow = safe_ffi_value_liveness_check range man flow in 
      Post.return flow
    | TOP | Nbt Stale ->
      let flow = raise_ffi_inactive_value (mk_var var range) man flow in 
      Post.return flow  

 (* evaluate a pointer to its points-to base *without dereferencing it* *)
 let eval_pointer expr man flow =
  match Common.Static_points_to.eval_opt expr with 
  | None -> panic_at expr.erange "evaluation of pointer expression %a not supported" pp_expr expr
  | Some (Null) -> Cases.singleton (P_null) flow
  | Some (Top) -> Cases.singleton (P_top) flow
  | Some (AddrOf (base, expr, mode)) ->
    Cases.singleton (P_block (base, expr, mode)) flow
  | Some (Eval (var, mode, offset)) -> 
    check_var_access var expr.erange man flow >>% fun flow ->
    let bin = mk_binop (mk_var var expr.erange) (O_plus) offset expr.erange in
    resolve_pointer bin man flow
  | Some (Invalid) -> Cases.singleton (P_invalid) flow
  | Some (Fun  f) -> Cases.singleton (P_fun f) flow



  (* We rewrite accesses [e] -> * (& e) when e is an l-value 
     and make sure that &e is actually active when we attempt to access it. *)
  let eval_lval_access lval man flow =
    let ptr = mk_c_address_of lval lval.erange in
    (* we simplify the pointer to something without calls *)
    man.eval ~route:(Below name) ptr flow >>$ fun expr flow ->
    (* we then turn the resulting expression into a pointsto assertion *)
    eval_pointer expr man flow >>$ fun _ flow ->
    (* finally we return *expr for the original expression *)
    let expr = mk_c_deref expr lval.erange in 
    Cases.singleton expr flow  
  
      
      


  (** {2 Transfer functions} *)
  (** ====================== *)

  let init prog man flow = flow

  let exec stmt man flow =
    match skind stmt with
    | S_assign({ekind = E_var _},_) -> None

    | S_assign(lval,rval) when is_c_scalar_type lval.etyp ->
      (eval_lval_access lval man flow >>$ fun lval' flow -> 
      let stmt = mk_assign lval' rval stmt.srange in
      man.exec stmt ~route:(Below name) flow) |>
      OptionExt.return

    | _ -> None

  let eval exp man flow = 
    match ekind exp with
    | E_var _ -> None

    | _ when is_c_lval exp
          && is_c_scalar_type exp.etyp ->
      (eval_lval_access exp man flow >>$ fun lval flow -> 
      man.eval lval ~route:(Below name) flow) |>
      OptionExt.return

    | _ -> None

  let ask query man flow  = None

  let print_expr man flow printer exp = ()

end

let () =
  register_stateless_domain (module Domain)
