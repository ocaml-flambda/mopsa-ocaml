(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Zones for the C language. *)

open Mopsa
open Framework.Zone
open Universal.Ast
open Ast

type zone +=
   | Z_py
   | Z_py_addr
   | Z_py_obj

let () =
  register_zone {
    zone = Z_py;
    name = "Z_py";
    subset = None;
    eval = (fun exp ->
        match ekind exp with
        | E_py_undefined _
        | E_py_object _
        | E_py_list _
        | E_py_index_subscript _
        | E_py_slice_subscript _
        | E_py_attribute _
        | E_py_dict _
        | E_py_set _
        | E_py_generator_comprehension _
        | E_py_list_comprehension _
        | E_py_set_comprehension _
        | E_py_dict_comprehension _
        | E_py_call _
        | E_py_yield _
        | E_py_if _
        | E_py_tuple _
        | E_py_bytes _
        | E_py_lambda _
        | E_py_multi_compare _ -> Keep

        | _ -> Process);
  };

  register_zone {
    zone = Z_py_addr;
    name = "Z_py_addr";
    subset = None;
    eval = (fun exp ->
        match ekind exp with
        | E_addr _ -> Keep

        | E_py_undefined _
        | E_py_object _
        | E_py_list _
        (* | E_py_index_subscript _
         * | E_py_slice_subscript _ *)
        | E_py_attribute _
        | E_py_dict _
        | E_py_set _
        (* | E_py_generator_comprehension _
         * | E_py_list_comprehension _
         * | E_py_set_comprehension _
         * | E_py_dict_comprehension _ *)
        | E_py_call _
        | E_py_yield _
        | E_py_if _
        | E_py_tuple _
        | E_py_bytes _
        | E_py_lambda _
        | E_py_multi_compare _ -> Visit

        | _ -> Process);
    };

  register_zone {
    zone = Z_py_obj;
    name = "Z_py_object";
    subset = Some Z_py_addr;
    eval = (fun exp ->
        match ekind exp with
        | E_py_object _ -> Keep
        | _ -> Process);
  }
