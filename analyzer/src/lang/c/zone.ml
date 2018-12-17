(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Zones for the C language. *)

open Mopsa
open Ast

(** Zones of the C language *)
type zone +=
  | Z_c           (* Entire C language *)
  | Z_c_low_level (* C without aggregates (arrays and records) *)
  | Z_c_scalar    (* C with only scalars (no dereferences) *)

let () =
  register_zone {
    zone = Z_c;
    subset = None;
    name = "C";
    eval = (fun exp ->
        match ekind exp with
        (* ------------------------------------------- *)
        | E_constant _
        | E_var _                            -> Keep
        (* ------------------------------------------- *)
        | E_primed _
        | E_unop _
        | E_binop _                          -> Visit
        (* ------------------------------------------- *)
        | E_c_conditional _
        | E_c_array_subscript _
        | E_c_member_access _
        | E_c_function _
        | E_c_builtin_function _
        | E_c_predefined _
        | E_c_call _
        | E_c_builtin_call _
        | E_c_arrow_access _
        | E_c_assign _
        | E_c_compound_assign _
        | E_c_comma _
        | E_c_increment _
        | E_c_address_of _
        | E_c_deref _
        | E_c_cast _
        | E_c_statement _
        | E_c_var_args _
        | E_c_atomic _                      -> Keep
        (* ------------------------------------------- *)
        | _                                 -> Process
      );
    }

let () =
  register_zone {
    zone = Z_c_low_level;
    subset = Some Z_c;
    name = "C/LowLevel";
    eval = (fun exp ->
        match ekind exp with
        (* ------------------------------------------- *)
        | E_constant _
        | E_var _
        | E_c_function _                     -> Keep
        (* ------------------------------------------- *)
        | E_primed _
        | E_unop _
        | E_binop _
        | E_c_cast _                         -> Visit
        (* ------------------------------------------- *)
        | E_c_address_of _                   -> Visit
        | E_c_deref _                        -> Visit
        (* ------------------------------------------- *)
        | _                                  -> Process
      );
    }


let () =
  register_zone {
    zone = Z_c_scalar;
    subset = Some Z_c;
    name = "C/Scalar";
    eval = (fun exp ->
        match ekind exp with
        (* ------------------------------------------- *)
        | E_constant _
        | E_var _
        | E_c_function _                     -> Keep
        (* ------------------------------------------- *)
        | E_primed _
        | E_unop _
        | E_binop _
        | E_c_cast _                         -> Visit
        (* ------------------------------------------- *)
        | E_c_address_of _                   -> Visit
        | E_c_deref p when p.etyp |> under_type |> is_c_array_type -> Visit
        (* ------------------------------------------- *)
        | _                                  -> Process
      );
  }
