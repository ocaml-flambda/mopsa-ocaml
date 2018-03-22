(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Non-relational numeric abstraction. *)

open Framework.Ast
open Framework.Domains
open Framework.Domains.Global
open Framework.Flow
open Framework.Manager
open Framework.Context
open Ast


let name = "universal.numeric.non_relational"
let debug fmt = Debug.debug ~channel:name fmt


module Domain =
struct

  (** {2 Lattice} *)
  module IntMap = Nonrel.Domain.Make(Integers)
  module FloatMap = Nonrel.Domain.Make(Floats)

  type t = {
    i: IntMap.t;
    f: FloatMap.t;
  }

  let bottom = {
    i = IntMap.bottom;
    f = FloatMap.bottom;
  }

  let top = {
    i = IntMap.top;
    f = FloatMap.top;
  }

  let is_bottom a = IntMap.is_bottom a.i || FloatMap.is_bottom a.f

  let is_top a = IntMap.is_top a.i && FloatMap.is_top a.f

  let leq a1 a2 = IntMap.leq a1.i a2.i && FloatMap.leq a1.f a2.f

  let join a1 a2 = {
    i = IntMap.join a1.i a2.i;
    f = FloatMap.join a1.f a2.f;
  }

  let meet a1 a2 = {
    i = IntMap.meet a1.i a2.i;
    f = FloatMap.meet a1.f a2.f;
  }

  let widening ctx a1 a2 = {
    i = IntMap.widening ctx a1.i a2.i;
    f = FloatMap.widening ctx a1.f a2.f;
  }

  let print fmt a =
    Format.fprintf fmt "box:@\n  int: @[%a@]@\n  float: @[%a@]"
      IntMap.print a.i
      FloatMap.print a.f

  let int_man man = {
    man with
    ax = {
      get = (fun env -> (man.ax.get env).i);
      set = (fun i env -> man.ax.set {(man.ax.get env) with i} env);
    }
  }

  let float_man man = {
    man with
    ax = {
      get = (fun env -> (man.ax.get env).f);
      set = (fun f env -> man.ax.set {(man.ax.get env) with f} env);
    }
  }

  (** {2 Transfer functions} *)
  let init prog man flow =
    set_domain_cur top man flow

  let exec stmt man ctx flow =
    match skind stmt with
    | S_assign({ekind = E_var _} as lval, rval) ->
      Eval.compose_exec
        rval
        (fun rval flow ->
           let stmt = {stmt with skind = S_assign(lval, rval)} in
           match etyp rval with
           | T_int -> IntMap.exec stmt (int_man man) ctx flow
           | T_float -> FloatMap.exec stmt (float_man man) ctx flow
           | _ -> None
        )
        (fun flow -> Exec.return flow)
        man ctx flow
    | S_assume(e) ->
      Eval.compose_exec
        e
        (fun e flow ->
           let stmt = {stmt with skind = S_assume(e)} in
           match etyp e with
           | T_int -> IntMap.exec stmt (int_man man) ctx flow
           | T_float -> FloatMap.exec stmt (float_man man) ctx flow
           | _ -> None
        )
        (fun flow -> Exec.return flow)
        man ctx flow
    | _ -> None

  let eval _ _ _ _ = None

  let ask _ _ _ _ = None
    
end

let setup () =
  register_domain name (module Domain)
