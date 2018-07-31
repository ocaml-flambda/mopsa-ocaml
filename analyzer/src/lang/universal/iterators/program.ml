(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Main handler of Universal programs. *)

open Framework.Essentials
open Framework.Domains.Stateless
open Ast

let name = "universal.iterators.program"
let debug fmt = Debug.debug ~channel:name fmt


module Domain =
struct

  let zone = Zone.Z_universal

  let init prog man flow = None
  
  let exec stmt man flow =
    match skind stmt with
    | S_program {prog_kind = P_universal{universal_main}} ->
      Some (
        man.exec universal_main flow |>
        Post.singleton
      )

    | _ -> None

  let eval exp man flow = None

  let ask query man flow = None

end

let () =
  register_domain name (module Domain)
