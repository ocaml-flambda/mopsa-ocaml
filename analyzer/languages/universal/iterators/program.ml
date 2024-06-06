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

  let init prog man flow =
    match prog.prog_kind with
    | P_universal u ->
      set_u_program u flow |>
      Post.return |>
      Option.some

    | _ -> None

  let eval exp man flow = None

  let find_function f functions =
    List.find (function
          {fun_orig_name} -> fun_orig_name = f
      ) functions

  let ask : type r. ('a,r) query -> _ man -> _ flow -> ('a, r) cases option = fun query man flow ->
    let get_locals prog call =
      let f = find_function call prog.universal_fundecs in
      f.fun_parameters @ f.fun_locvars @ [f.fun_return_var] in 
    let open Framework.Engines.Interactive in
    match query with
    | Q_defined_variables (Some call) ->
      let prog = get_u_program flow in
      Cases.singleton (get_locals prog call) flow 
      |> OptionExt.return 

    | Q_defined_variables None ->
      let prog = get_u_program flow in
      let cs = Flow.get_callstack flow in
      let globals = prog.universal_gvars in
      let locals = List.rev @@
        List.fold_left (fun acc call -> (get_locals prog call.Callstack.call_fun_orig_name) @ acc) [] cs in
      Cases.singleton (globals @ locals) flow
      |> OptionExt.return 

    | Q_allocated_addresses ->
      Cases.singleton [] flow
      |> OptionExt.return 

    | _ -> None

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


  let print_expr man flow printer exp = ()

end

let () =
  register_stateless_domain (module Domain)
