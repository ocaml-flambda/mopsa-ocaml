(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Main handler of standalone Python programs. *)


open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Ast
open Universal.Ast
open Ast

let name = "python.programs.standalone"
let debug fmt = Debug.debug ~channel:name fmt


module Domain =
struct

  let init prog manager flow = flow

  let eval exp manager ctx flow = None

  let init_globals filename globals manager ctx flow =
    (* Initialize global variables with C_py_undefined constant *)
    let range = mk_fresh_range () in
    let stmt =
      mk_block
        (List.mapi (fun i v ->
             mk_assign
               (mk_var v (tag_range range "lval %d" i))
               (mk_constant C_py_undefined ~etyp:T_py_undefined (tag_range range "undef %d" i))
               (tag_range range "undef assign %d" i)
           ) globals
        )
        range
    in
    let flow1 = manager.exec stmt ctx flow in

    (** Initialize special variable __name__ *)
    let range = mk_fresh_range () in
    let stmt =
      mk_assign
        (mk_var {orgname = "__name__"; unname = "__name__"; vtyp = T_any} (tag_range range "__name__ lval"))
        (mk_constant (Universal.Ast.C_string "__main__") ~etyp:Universal.Ast.T_string (tag_range range "__name__"))
        range
    in
    let flow2 = manager.exec stmt ctx flow1 in

    (** Initialize special variable __file__ *)
    let range = mk_fresh_range () in
    let stmt =
        mk_assign
          (mk_var {orgname = "__file__"; unname = "__file__"; vtyp = T_any} (tag_range range "__file__ lval"))
          (mk_constant (Universal.Ast.C_string filename) ~etyp:Universal.Ast.T_string (tag_range range "__file__"))
          range
    in
    let flow3 = manager.exec stmt Framework.Context.empty flow2 in

    flow3


  let exec stmt manager ctx flow  =
    match skind stmt with
    | S_program({prog_kind = Py_program(globals, body); prog_file}) ->
      (* Initialize global variables *)
      init_globals prog_file globals manager ctx flow |>
      (* Execute the body *)
      manager.exec body ctx |>
      Exec.return

    | _ -> None

  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
