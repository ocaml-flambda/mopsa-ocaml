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

(** Main handler of Universal programs. *)

open Mopsa
open Sig.Abstraction.Stateless
open Ast

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "universal.iterators.program"
    end)


  let checks = []

  let init prog man flow = flow

  let eval exp man flow = None

  let ask query man flow = None

  (** Execute tests in a unit test program *)
  let exec_tests main fundecs range man flow =
    (* Execute main body *)
    man.exec main flow >>% fun flow ->

    (* Search for test functions *)
    let tests = List.filter (fun f ->
        let name = f.fun_orig_name in
        if String.length name < 5 then false
        else String.sub name 0 4 = "test"
      ) fundecs
    in

    (* Execute tests *)
    let stmt = mk_stmt (S_unit_tests (
        tests |> List.map (fun f ->
            f.fun_orig_name, mk_expr_stmt (mk_call f [] f.fun_range) range
          )
      )) range
    in
    man.exec stmt flow
    
  

  let exec stmt man flow =
    match skind stmt with
    | S_program ({ prog_kind = P_universal{universal_main} }, _)
      when not !Unittest.unittest_flag ->
      man.exec universal_main flow |>
      OptionExt.return

    | S_program ({ prog_kind = P_universal{universal_main; universal_fundecs} }, _)
      when !Unittest.unittest_flag ->
      exec_tests universal_main universal_fundecs stmt.srange man flow |>
      OptionExt.return

    | _ -> None


  let pretty_print printer exp man flow = ()

end

let () =
  register_stateless_domain (module Domain)
