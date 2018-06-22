(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Language zones of Python. *)

open Framework.Zone

type t +=
  | Z_py
  | Z_py_object
  | Z_py_value

let () =
  register_partial_order (fun next z1 z2 ->
      match z1, z2 with
      | (Z_py_object | Z_py_value), Z_py -> true
      | _ -> next z1 z2
    );
  register_pp (fun next fmt z ->
      match z with
      | Z_py -> Format.fprintf fmt "py"
      | Z_py_object -> Format.fprintf fmt "pyobj"
      | Z_py_value -> Format.fprintf fmt "pyval"
      | _ -> next fmt z
    )
