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
open Sig.Abstraction.Stateless
open Addr
open Ast
open Universal.Ast


module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "python.program"
    end)

  let checks = [Alarms.CHK_PY_UNCAUGHT_EXCEPTION]

  let init prog man flow =
    match prog.prog_kind with
    | Py_program (name, globals, body) -> set_py_program (name, globals, body) flow
    | _ -> flow

  let eval _ _ _ = None

  let init_globals man globals range (flow: 'a flow) : 'a post =
    (* Initialize global variables with C_py_undefined constant *)
    let stmt =
      mk_block
        (List.mapi (fun i v ->
             let e =
               (* Initialize globals with the same name of a builtin with its address *)
               if is_builtin_var v then (mk_py_object (find_builtin @@ get_orig_vname v) range)
               else mk_expr ~etyp:(T_py None) (E_py_undefined true) range
             in
             mk_assign (mk_var v range) e range
           ) globals
        )
        range
    in
    man.exec stmt flow >>% fun flow1 ->

    (** Initialize special variable __name__ *)
    let v = Frontend.from_var {name="__name__"; uid=140} in
    let stmt =
      let range = tag_range range "__name__ assignment" in
      mk_assign
        (mk_var v range)
        (mk_constant (Universal.Ast.C_string "__main__") ~etyp:(T_py (Some Str)) range)
        range
    in
    man.exec stmt flow1 >>% fun flow2 ->

    (** Initialize special variable __file__ *)
    let v = Frontend.from_var {name="__file__"; uid=141} in
    let stmt =
      let range = tag_range range "__file__ assignment" in
        mk_assign
          (mk_var v range)
          (mk_constant (Universal.Ast.C_string (get_range_file range)) ~etyp:(T_py (Some Str)) range)
          range
    in
    man.exec stmt flow2 >>% fun flow3 ->

    Post.return flow3


  let get_function_name fundec = get_orig_vname fundec.py_func_var

  let is_test fundec =
    let name = get_function_name fundec in
    String.length name >= 5 &&
      String.sub name 0 4 = "test"

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

  let unprecise_exception_range prog_range =
    tag_range prog_range "unprecise exception range"

  let collect_uncaught_exceptions man prog_range flow =
    Post.return @@ Flow.fold (fun acc tk env ->
        match tk with
        | Alarms.T_py_exception (e, s, k) ->
          let a = Alarms.A_py_uncaught_exception (e,s) in
          let alarm =
            match k with
            | Alarms.Py_exc_unprecise ->
              mk_alarm a empty_callstack (unprecise_exception_range prog_range)

            | Alarms.Py_exc_with_callstack (range,cs) ->
              mk_alarm a cs range
          in
          Flow.add_alarm alarm ~force:true man.lattice acc
        | _ -> acc
      ) flow flow


  let exec stmt man (flow: 'a flow) : 'a post option  =
    match skind stmt with
    | S_program ({ prog_kind = Py_program(_, globals, body); prog_range }, _)
      when !Universal.Iterators.Unittest.unittest_flag ->
      (* Initialize global variables *)
       OptionExt.return begin
         init_globals man  globals (srange stmt) flow >>% fun flow1 ->

         (* Execute the body *)
         man.exec body flow1 >>% fun flow2 ->

         (* Collect test functions *)
         let tests = get_test_functions body in
         let stmt = mk_py_unit_tests tests (srange stmt) in
         man.exec stmt flow2 >>%
         collect_uncaught_exceptions man prog_range
         end

    | S_program ({ prog_kind = Py_program(_, globals, body); prog_range }, _) ->
      (* Initialize global variables *)
      init_globals man globals (srange stmt) flow >>%
      (* Execute the body *)
      man.exec body >>%
      collect_uncaught_exceptions man prog_range |>
      OptionExt.return

    | _ -> None

  let find_function orig_name prog =
    List.hd @@ Visitor.fold_stmt
        (fun acc exp -> VisitParts acc)
        (fun acc stmt ->
           match skind stmt with
           | S_py_function(fundec)
             when get_orig_vname fundec.py_func_var = orig_name ->
             Keep (fundec :: acc)
           | _ -> VisitParts (acc)
        ) [] prog

  let ask : type r. ('a, r) query -> _ man -> _ flow -> r option =
    fun query man flow ->
    let open Framework.Engines.Interactive in
    match qkind query with
    | Q_debug_variables ->
       let (_, globals, body) = get_py_program flow in
       let cs = Flow.get_callstack flow in
       let allvars =
         List.fold_left (fun acc call ->
             let fd = find_function call.Callstack.call_fun_orig_name body in
             fd.py_func_parameters
             @ fd.py_func_kwonly_args
             @ (OptionExt.apply (fun x -> [x]) [] fd.py_func_kwarg)
             @ fd.py_func_locals
             @ [fd.py_func_ret_var]
             @ acc

           ) globals cs
       in
       Some allvars
    | _ -> None

  let print_expr _ _ _ _ = ()

end

let () =
  register_stateless_domain (module Domain)
