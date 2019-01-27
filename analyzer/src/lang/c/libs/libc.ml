(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Evaluation of built-in Libc functions *)

open Mopsa
open Universal.Ast
open Ast

let is_builtin_function = function
  | "__builtin_constant_p"
    -> true

  | _ -> false


module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_libs_libc : unit domain
  let id = D_c_libs_libc
  let name = "c.libs.libc"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_libs_libc -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {export = []; import = []}
  let eval_interface = {
    export = [
      Zone.Z_c, Zone.Z_c_low_level
    ];
    import = []
  }

  (** {2 Transfer functions} *)
  (** ====================== *)

  let init prog man flow = None

  let exec zone stmt man flow = None

  let eval zone exp man flow =
    match ekind exp with
    (* 𝔼⟦ __builtin_constant_p(e) ⟧ *)
    | E_c_builtin_call("__builtin_constant_p", [e]) ->
      (* __builtin_constant_ determines if [e] is known to be constant
         at compile time *)
      let ret =
        match ekind e with
        | E_constant _ -> mk_one ~typ:s32 exp.erange
        | _ -> mk_z_interval Z.zero Z.one ~typ:s32 exp.erange
      in
      Eval.singleton ret flow |>
      Eval.return

    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
