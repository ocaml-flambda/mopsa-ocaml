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
open Sig.Domain.Stateless
open Addr
open Ast
open Universal.Ast


module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.program"
    end)

  let interface = {
    iexec = {provides = [Zone.Z_py]; uses = []};
    ieval = {provides = []; uses = []}
  }

  let alarms = []

  let init _ _ flow = flow

  let eval _ _ _ _ = None

  let init_globals man globals range flow =
    (* Initialize global variables with C_py_undefined constant *)
    let stmt =
      mk_block
        (List.mapi (fun i v ->
             let e =
               (* Initialize globals with the same name of a builtin with its address *)
               if is_builtin_var v then (mk_py_object (find_builtin @@ get_orig_vname v) range)
               else mk_expr (E_py_undefined true) range
             in
             mk_assign (mk_var v range) e range
           ) globals
        )
        range
    in
    let flow1 = man.exec stmt flow in

    (** Initialize special variable __name__ *)
    (* TODO: FIXME: __name__/__file__ is in gc, but not globals, so we're a bit stuck here. 140/141 seems to be the right constants **for now** *)
    let v = Frontend.from_var {name="__name__"; uid=140} in
    (* mkfresh (fun uid -> "__name__" ^ (string_of_int uid)) T_any () in *)
    let stmt =
      let range = tag_range range "__name__ assignment" in
      mk_assign
        (mk_var v range)
        (mk_constant (Universal.Ast.C_string "__main__") ~etyp:Universal.Ast.T_string range)
        range
    in
    let flow2 = man.exec stmt flow1 in

    (** Initialize special variable __file__ *)
    (* let v = mkfresh (fun uid -> "__file__" ^ (string_of_int uid)) T_any () in *)
    let v = Frontend.from_var {name="__file__"; uid=141} in
    let stmt =
      let range = tag_range range "__file__ assignment" in
        mk_assign
          (mk_var v range)
          (mk_constant (Universal.Ast.C_string (get_range_file range)) ~etyp:Universal.Ast.T_string range)
          range
    in
    let flow3 = man.exec stmt flow2 in

    flow3


  let get_function_name fundec = get_orig_vname fundec.py_func_var

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
          (get_orig_vname test.py_func_var, {skind = S_expression (mk_py_call (mk_var test.py_func_var range) [] range); srange = range})
        )
    in
    mk_stmt (Universal.Ast.S_unit_tests (tests)) range

  let unprecise_exception_range = mk_fresh_range ()

  let collect_uncaught_exceptions man flow =
    Flow.fold (fun acc tk env ->
        match tk with
        | Alarms.T_py_exception (e, s, k) ->
          let a = Alarms.A_py_uncaught_exception (e,s) in
          let alarm =
            match k with
            | Alarms.Py_exc_unprecise ->
              mk_alarm a unprecise_exception_range ~cs:Callstack.empty

            | Alarms.Py_exc_with_callstack (range,cs) ->
              mk_alarm a range ~cs
          in
          Flow.add_alarm alarm ~force:true man.lattice acc
        | _ -> acc
      ) flow flow


  let exec zone stmt man flow  =
    match skind stmt with
    | S_program ({ prog_kind = Py_program(_, globals, body) }, _)
      when !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
      let flow1 = init_globals man  globals (srange stmt) flow in

      (* Execute the body *)
      let flow2 = man.exec body flow1 in

      (* Collect test functions *)
      let tests = get_test_functions body in
      let stmt = mk_py_unit_tests tests (srange stmt) in
      man.exec stmt flow2 |>
      collect_uncaught_exceptions man |>
      Post.return |>
      OptionExt.return

    | S_program ({ prog_kind = Py_program(_, globals, body) }, _) ->
      (* Initialize global variables *)
      init_globals man globals (srange stmt) flow |>
      (* Execute the body *)
      man.exec body |>
      collect_uncaught_exceptions man |>
      Post.return |>
      OptionExt.return



    | _ -> None

  let ask _ _ _ = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
