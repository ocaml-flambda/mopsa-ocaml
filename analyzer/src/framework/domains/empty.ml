(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Empty domain. *)

open Domain

include Stateless.Make(
  struct
    let name = "framework.domains.empty"
    type _ domain += D_empty : unit domain
    let id = D_empty
    let identify : type a. a domain -> (unit, a) eq option =
      function
      | D_empty -> Some Eq
      | _ -> None
    let exec_interface = {export = []; import = []}
    let eval_interface = {export = []; import = []}
    let init prog man flow = None
    let exec zone stmt man flow = None
    let eval zone exp man flow = None
    let ask query man flow = None
  end
  )
