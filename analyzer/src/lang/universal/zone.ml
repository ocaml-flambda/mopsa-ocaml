(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Zones for the Universal language. *)

open Framework.Essentials
open Ast

type zone +=
  | Z_u
  | Z_u_num

let () =
  register_zone {
    zone = Z_u;
    subset = None;
    name = "U";
    eval = (fun exp ->
        match ekind exp with
        (* ------------------------------------------- *)
        | E_constant _
        | E_function _
        | E_array _
        | E_subscript _
        | E_addr _
        | E_len _                            -> Keep
        (* ------------------------------------------- *)
        | E_unop _
        | E_binop _                          -> Visit
        (* ------------------------------------------- *)
        | _                                  -> Process
      );
  };

  register_zone {
    zone = Z_u_num;
    subset = Some Z_u;
    name = "U/Num";
    eval = (fun exp ->
        match ekind exp with
        (* ------------------------------------------- *)
        | E_constant _
          when is_numeric_type (etyp exp)
                                             -> Keep
        | E_var _
          when is_numeric_type (etyp exp)
                                             -> Keep
        (* ------------------------------------------- *)
        | E_unop _
        | E_binop _                          -> Visit
        (* ------------------------------------------- *)
        | _                                  -> Process
      );
  };

  ()
