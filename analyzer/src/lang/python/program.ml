(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                          *)
(****************************************************************************)

(** Main handler of Python programs. *)
(** This domain initializes global variables, creates special
   variables __name__, __main__, __file__, and collects unit-testing
   functions if required *)

open Mopsa
open Addr
open Ast
open Universal.Ast


module Domain =
struct

  let name = "python.program"
  let debug fmt = Debug.debug ~channel:name fmt

  let interface = {
    iexec = {provides = [Zone.Z_py]; uses = []};
    ieval = {provides = []; uses = []}
  }

  let init _ _ flow = flow

  let eval _ _ _ _ = None

  let init_globals man globals range flow =
    (* Initialize global variables with C_py_undefined constant *)
    let stmt =
      mk_block
        (List.mapi (fun i v ->
             let e =
               (* Initialize globals with the same name of a builtin with its address *)
               if is_builtin_name v.org_vname then (mk_py_object (find_builtin v.org_vname) range)
               else mk_expr (E_py_undefined true) range
             in
             mk_assign (mk_var v range) e range
           ) globals
        )
        range
    in
    let flow1 = man.exec stmt flow in

    (** Initialize special variable __name__ *)
    let v = mkfresh (fun uid -> "__name__" ^ (string_of_int uid)) T_any () in
    let stmt =
      let range = tag_range range "__name__ assignment" in
      mk_assign
        (mk_var v range)
        (mk_constant (Universal.Ast.C_string "__main__") ~etyp:Universal.Ast.T_string range)
        range
    in
    let flow2 = man.exec stmt flow1 in

    (** Initialize special variable __file__ *)
    let v = mkfresh (fun uid -> "__file__" ^ (string_of_int uid)) T_any () in
    let stmt =
      let range = tag_range range "__file__ assignment" in
        mk_assign
          (mk_var v range)
          (mk_constant (Universal.Ast.C_string (get_range_file range)) ~etyp:Universal.Ast.T_string range)
          range
    in
    let flow3 = man.exec stmt flow2 in

    flow3


  let get_function_name fundec = fundec.py_func_var.org_vname

  let is_test fundec =
    let name = get_function_name fundec in
    if String.length name < 5 then false
    else String.sub name 0 4 = "test"

  let get_test_functions body =
    Visitor.fold_stmt
        (fun acc exp -> VisitParts acc)
        (fun acc stmt ->
           match skind stmt with
           | S_py_function(fundec)
             when is_test fundec  ->
             Keep (fundec :: acc)
           | _ -> VisitParts (acc)
        ) [] body


  let mk_py_unit_tests tests range =
    let tests =
      tests |> List.map (fun test ->
          (test.py_func_var.org_vname, {skind = S_expression (mk_py_call (mk_var test.py_func_var range) [] range); srange = range})
        )
    in
    mk_stmt (Universal.Ast.S_unit_tests (tests)) range


  let exec zone stmt man flow  =
    match skind stmt with
    | S_program { prog_kind = Py_program(globals, body) }
      when not !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
      init_globals man globals (srange stmt) flow |>
      (* Execute the body *)
      man.exec body |>
      Post.return |>
      Option.return

    | S_program { prog_kind = Py_program(globals, body) }
      when !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
      let flow1 = init_globals man  globals (srange stmt) flow in

      (* Execute the body *)
      let flow2 = man.exec body flow1 in

      (* Collect test functions *)
      let tests = get_test_functions body in
      let stmt = mk_py_unit_tests tests (srange stmt) in
      Post.return (man.exec stmt flow2) |>
      Option.return


    | _ -> None

  let ask _ _ _ = None

end

let () =
  Framework.Core.Sig.Stateless.Domain.register_domain (module Domain)
