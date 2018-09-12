(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of C function calls *)

open Framework.Essentials
open Ast


module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_interproc : unit domain

  let id = D_c_interproc
  let name = "c.iterators.interproc"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_interproc -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let zone = Zone.Z_c
  let import_exec = []
  let import_eval = []


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow = None

  (** Computation of post-conditions *)
  (** ============================== *)

  let exec stmt man flow  = None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval exp man flow =
    match ekind exp with
    | E_c_call(f, args) ->
      panic_at exp.erange "c.iterators.interproc: call %a not supported" pp_expr exp

    | E_c_cast(e, _)
      when (exp |> etyp |> is_c_pointer_type)
        && (exp |> etyp |> under_pointer_type |> is_c_function_type) ->
      panic_at exp.erange "c.iterators.interproc: cast %a not supported" pp_expr exp

    | _ -> None



  (** Handler of queries *)
  (** ================== *)

  let ask query man flow = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
