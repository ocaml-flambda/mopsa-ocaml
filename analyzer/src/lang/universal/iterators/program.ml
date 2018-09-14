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


module Domain =
struct

  type _ domain += D_universal_program : unit domain

  let id = D_universal_program
  let name = "universal.iterators.program"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_universal_program -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  let exec_interface = {export = [Zone.Z_universal]; import = []}
  let eval_interface = {export = []; import = []}

  let init prog man flow = None

  let exec stmt man flow =
    match skind stmt with
    | S_program {prog_kind = P_universal{universal_main}} ->
      Some (
        man.exec universal_main flow |>
        Post.of_flow
      )

    | _ -> None

  let eval exp man flow = None

  let ask query man flow = None

end

let () =
  register_domain (module Domain)
