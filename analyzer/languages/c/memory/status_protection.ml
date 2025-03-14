(** Domain implementing a status check for pointer dereferences *)

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
  | None ->
    let flow = raise_ffi_internal_error (Format.asprintf "evaluation of pointer expression %a not supported" pp_expr expr) expr.erange man flow in
    Cases.empty flow
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
