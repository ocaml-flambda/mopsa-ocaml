(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

let var_uniq_name v =
  let () = Format.fprintf Format.str_formatter "%a" Pp.pp_var v in
  Format.flush_str_formatter ();


module Var = struct
  type t = Ast.var
  let compare = Ast.compare_var
  let print = Pp.pp_var
end
