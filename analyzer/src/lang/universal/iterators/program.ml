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

  type _ id += D_universal_program : unit id

  let me = D_universal_program
  let eq : type a. a id -> (unit, a) eq option =
    function
    | D_universal_program -> Some Eq
    | _ -> None

  let zone = Zone.Z_universal
  let import_exec = []
  let import_eval = []

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
