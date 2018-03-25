(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Visitors of the Universal language extension. *)

open Framework.Ast
open Framework.Visitor
open Ast

let () =
  register_expr_visitor (fun default exp ->
      match ekind exp with
      | E_var _
      | E_constant _
      | E_function _ -> leaf exp

      | E_unop(unop, e) ->
        {exprs = [e]; stmts = []},
        (fun parts -> {exp with ekind = E_unop(unop, List.hd parts.exprs)})

      | E_binop(binop, e1, e2) ->
        {exprs = [e1; e2]; stmts = []},
        (fun parts -> {exp with ekind = E_binop(binop, List.hd parts.exprs, List.nth parts.exprs 1)})

      | E_subscript(v, e) ->
        {exprs = [v; e]; stmts = []},
        (fun parts -> {exp with ekind = (E_subscript(List.hd parts.exprs, List.hd @@ List.tl parts.exprs))})

      | E_addr_attribute _ -> leaf exp

      | E_alloc_addr _ -> leaf exp

      | E_addr _ -> leaf exp

      | E_array(el) ->
        {exprs = el; stmts = []},
        (fun parts -> {exp with ekind = E_array parts.exprs})

      | E_call(f, args) ->
        {exprs = f :: args; stmts = []},
        (fun parts -> {exp with ekind = E_call(List.hd parts.exprs, List.tl parts.exprs)})

      | _ -> default exp
    );

  register_stmt_visitor (fun default stmt ->
      match skind stmt with
      | S_break
      | S_continue
      | S_rename_var _
      | S_remove_var _
      | S_project_vars _
      | S_rebase_addr _ -> leaf stmt

      | S_assign(x, e) ->
        {exprs = [e; x]; stmts = []},
        (function {exprs = [e; x]; stmts = []} -> {stmt with skind = S_assign(x, e)} | _ -> assert false)

      | S_expand(x, e) ->
        {exprs = [e; x]; stmts = []},
        (function {exprs = [e; x]} -> {stmt with skind = S_expand(x, e)} | _ -> assert false)


      | S_assume(e) ->
        {exprs = [e]; stmts = []},
        (fun parts -> {stmt with skind = S_assume(List.hd parts.exprs)})

      | S_expression(e) ->
        {exprs = [e]; stmts = []},
        (fun parts -> {stmt with skind = S_expression(List.hd parts.exprs)})


      | S_if(e, s1, s2) ->
        {exprs = [e]; stmts = [s1; s2]},
        (fun parts -> {stmt with skind = S_if(List.hd parts.exprs, List.hd parts.stmts, List.nth parts.stmts 1)})

      | S_while(e, s)  ->
        {exprs = [e]; stmts = [s]},
        (fun parts -> {stmt with skind = S_while(List.hd parts.exprs, List.hd parts.stmts)})

      | S_block(sl) ->
        {exprs = []; stmts = sl},
        (fun parts -> {stmt with skind = S_block(parts.stmts)})

      | S_return(None) -> leaf stmt

      | S_return(Some e) ->
        {exprs = [e]; stmts = []},
        (function {exprs = [e]} -> {stmt with skind = S_return(Some e)} | _ -> assert false)

      | S_assert(e) ->
        {exprs = [e]; stmts = []},
        (function {exprs = [e]} -> {stmt with skind = S_assert(e)} | _ -> assert false)

      | S_unit_tests(file, tests) ->
        let tests_names, tests_bodies = List.split tests in
        {exprs = []; stmts = tests_bodies},
        (function {stmts = tests_bodies} ->
           let tests = List.combine tests_names tests_bodies in
           {stmt with skind = S_unit_tests(file, tests)}
        )

      | _ -> default stmt
    );
  ()
