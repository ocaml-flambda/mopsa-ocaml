(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Inter-procedural iterator of stubs by inlining. *)

open Framework.Essentials
open Universal.Ast
open Ast
open Zone

module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_stubs_iterators_inlining : unit domain

  let id = D_stubs_iterators_inlining
  let name = "stubs.iterators.inlining"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_stubs_iterators_inlining -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {export = []; import = []}
  let eval_interface = {export = [Z_stubs, Z_any]; import = []}


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow = None


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow = None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval_formula
      (f: formula with_range)
      (man:('a, unit) man)
      (flow:'a flow)
    : ('a, bool) evl =
    match f.content with
    | F_expr e ->
      Eval.assume e
        ~fthen:(fun flow -> Eval.singleton true flow)
        ~felse:(fun flow -> Eval.singleton false flow)
        man flow

    | F_binop (AND, f1, f2) -> assert false

    | F_binop (OR, f1, f2) -> assert false

    | F_binop (IMPLIES, f1, f2) -> assert false

    | F_not ff -> assert false

    | F_forall (v, s, ff) -> assert false

    | F_exists (v, s, ff) -> assert false

    | F_in (v, s) -> assert false

    | F_free(e) -> assert false

  let eval zone exp man flow =
    match ekind exp with
    | E_stub_call (stub, args) ->
      debug "call to stub %s:@\n @[%a@]"
        stub.stub_name
        pp_stub stub
      ;

      (* Initialize parameters *)
      let flow1 =
        List.combine args stub.stub_params |>
        List.fold_left (fun flow (arg, param) ->
            man.exec (mk_assign (mk_var param exp.erange) arg exp.erange) flow
          ) flow
      in

      (* Check requirements *)
      let flow2 =
        stub.stub_requires |>
        List.fold_left (fun flow req ->
            eval_formula req.content man flow |>
            Post.bind_flow man @@ fun b flow ->
            if b then flow
            else assert false
          ) flow1
      in
      panic_at exp.erange "stubs not supported"

    | _ -> None


  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
