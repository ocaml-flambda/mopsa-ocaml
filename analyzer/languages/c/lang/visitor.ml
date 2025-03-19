(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2019 The MOPSA Project.                                    *)
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

open Mopsa
open Ast


let rec exprs_in_init = function
  | C_init_expr e -> [e]
  | C_init_implicit _ -> []
  | C_init_list(l, filler) ->
    let el1 = l |> List.fold_left (fun acc init -> acc @ exprs_in_init init) [] in
    let el2 = exprs_in_init_option filler in
    el1 @ el2

and exprs_in_init_option = function
  | None -> []
  | Some init -> exprs_in_init init

let rec init_from_exprs exprs init =
  match init, exprs with
  | C_init_expr _, e :: tl -> C_init_expr e, tl
  | C_init_implicit t, exprs -> C_init_implicit t, exprs
  | C_init_list(l, filler), exprs ->
    let l, exprs = l |> List.fold_left (fun (init_list, exprs) init ->
        let init, exprs = init_from_exprs exprs init in
        (init :: init_list, exprs)
      ) ([], exprs)
    in
    let filler, exprs = init_option_from_exprs exprs filler in
    C_init_list(List.rev l, filler), exprs
  | _ -> assert false

and init_option_from_exprs exprs init =
  match init, exprs with
  | None, exprs -> None, exprs
  | Some init, exprs ->
    let init, exprs = init_from_exprs exprs init in
    Some init, exprs


let () =
  register_expr_visitor (fun default exp ->
      match ekind exp with
      | E_c_conditional(cond, body, orelse) ->
        {exprs = [cond; body; orelse]; stmts = []},
        (function
          | {exprs = [cond; body; orelse]} -> {exp with ekind = E_c_conditional(cond, body, orelse)}
          | _ -> assert false
        )

      | E_c_array_subscript(arr, idx) ->
        {exprs = [arr; idx]; stmts = []},
        (function
          | {exprs = [arr; idx]} -> {exp with ekind = E_c_array_subscript(arr, idx)}
          | _ -> assert false
        )

      | E_c_member_access(rcd, idx, fld) ->
        {exprs = [rcd]; stmts = []},
        (function
          | {exprs = [rcd]} -> {exp with ekind = E_c_member_access(rcd, idx, fld)}
          | _ -> assert false
        )


      | E_c_function(f) -> leaf exp

      | E_c_builtin_function f -> leaf exp

      | E_c_builtin_call(f, args) ->
        {exprs = args; stmts = []},
        (function
          | {exprs = args} -> {exp with ekind = E_c_builtin_call(f, args)}
        )

      | E_ffi_call(f, args) ->
          {exprs = args; stmts = []},
          (function
            | {exprs = args} -> {exp with ekind = E_ffi_call(f, args)}
          )

      | E_c_arrow_access(p, idx, fld) ->
        {exprs = [p]; stmts = []},
        (function
          | {exprs = [p]} -> {exp with ekind = E_c_arrow_access(p, idx, fld)}
          | _ -> assert false
        )

      | E_c_assign(lval, rval) ->
        {exprs = [lval; rval]; stmts = []},
        (function
          | {exprs = [lval; rval]} -> {exp with ekind = E_c_assign(lval, rval)}
          | _ -> assert false
        )

      | E_c_compound_assign(lval, pretyp, op, rval, posttyp) ->
        {exprs = [lval; rval]; stmts = []},
        (function
          | {exprs = [lval; rval]} -> {exp with ekind = E_c_compound_assign(lval, pretyp, op, rval, posttyp)}
          | _ -> assert false
        )

      | E_c_comma(e1, e2) ->
        {exprs = [e1; e2]; stmts = []},
        (function
          | {exprs = [e1; e2]} -> {exp with ekind = E_c_comma(e1, e2)}
          | _ -> assert false
        )

      | E_c_increment(dir, loc, e) ->
        {exprs = [e]; stmts = []},
        (function
          | {exprs = [e]} -> {exp with ekind = E_c_increment(dir, loc, e)}
          | _ -> assert false
        )

      | E_c_address_of (e) ->
        {exprs = [e]; stmts = []},
        (function
          | {exprs = [e]} -> {exp with ekind = E_c_address_of(e)}
          | _ -> assert false
        )

      | E_c_deref(p) ->
        {exprs = [p]; stmts = []},
        (function
          | {exprs = [p]} -> {exp with ekind = E_c_deref(p)}
          | _ -> assert false
        )

      | E_c_cast(e, is_implicit) ->
        {exprs = [e]; stmts = []},
        (function
          | {exprs = [e]} -> {exp with ekind = E_c_cast(e, is_implicit)}
          | _ -> assert false
        )

      | E_c_predefined _ -> leaf exp

      | E_c_var_args arg ->
        {exprs = [arg]; stmts = []},
        (function
          | {exprs = [arg]} -> {exp with ekind = E_c_var_args arg}
          | _ -> assert false
        )

      | E_c_atomic(op, e1, e2) ->
        {exprs = [e1; e2]; stmts = []},
        (function
          | {exprs = [e1; e2]} -> {exp with ekind = E_c_atomic(op, e1, e2)}
          | _ -> assert false
        )

      | E_c_statement(s) ->
        {exprs = []; stmts = [s]},
        (function
          | {stmts = [s]} -> {exp with ekind = E_c_statement(s)}
          | _ -> assert false
        )

      | E_c_block_object ee ->
        {exprs = [ee]; stmts = []},
        (function
          | {exprs = [ee]} -> {exp with ekind = E_c_block_object(ee)}
          | _ -> assert false
        )

      | _ -> default exp
    );

  register_stmt_visitor (fun default stmt ->
      match skind stmt with
      | S_program ({ prog_kind = C_program _ }, _) ->
        Exceptions.panic "visitor of C_program not yet implemented"

      | S_c_declaration(v, init,scope) ->
        let exprs = exprs_in_init_option init in
        {exprs; stmts = []},
        (function {exprs} ->
           let init, _ = init_option_from_exprs exprs init in
           {stmt with skind = S_c_declaration (v,init,scope)}
        )


      | S_c_do_while(body, cond) ->
        {exprs = [cond]; stmts = [body]},
        (function
          | {exprs = [cond]; stmts = [body]} -> {stmt with skind = S_c_do_while(body, cond)}
          | _ -> assert false
        )
      | S_c_for(init, cond, incr, body) ->
        let exprs = match cond, incr with
          | Some cond, Some incr -> [cond; incr]
          | Some cond, None -> [cond]
          | None, Some incr -> [incr]
          | None, None -> []
        in
        {exprs; stmts = [init; body]},
        (function
          | {exprs; stmts = [init; body]} ->
            let cond, incr = match cond, incr, exprs with
              | _, _, [] -> None, None
              | Some _, Some _, [cond; incr] -> Some cond, Some incr
              | Some _, None, [cond] -> Some cond, None
              | None, Some _, [incr] -> None, Some incr
              | _ -> assert false
            in
            {stmt with skind = S_c_for(init, cond, incr, body)}
          | _ -> assert false
        )
      | S_c_break _ -> leaf stmt
      | S_c_continue _ -> leaf stmt
      | S_c_return (None,_) -> leaf stmt
      | S_c_return (Some e,update) ->
        {exprs=[e]; stmts=[]},
        (function | {exprs=[e]} -> {stmt with skind = S_c_return(Some e,update)}
                  | _ -> assert false)
      | S_c_goto _ -> leaf stmt
      | S_c_goto_stab stmt ->
        {exprs = []; stmts = [stmt]},
        (function
          | {exprs = []; stmts = [stmt]} -> {stmt with skind = S_c_goto_stab stmt}
          | _ -> assert false
        )
      | S_c_switch(cond, body) ->
        {exprs = [cond]; stmts = [body]},
        (function
          | {exprs = [cond]; stmts = [body]} -> {stmt with skind = S_c_switch(cond, body)}
          | _ -> assert false
        )
      | S_c_label _ -> leaf stmt
      | S_c_switch_case(cases,update) ->
        {exprs = cases; stmts = []},
        (function
          | {exprs = cases; stmts = []} -> {stmt with skind = S_c_switch_case(cases,update)}
          | _ -> assert false
        )
      | S_c_switch_default _ -> leaf stmt
      | S_c_asm _ -> leaf stmt
      | S_c_ext_call (f, es) ->
        {exprs = es; stmts = []},
        (function {exprs;stmts = []} -> { stmt with skind = S_c_ext_call (f, exprs) }
          | _ -> assert false )
      | S_unimplemented_ffi_function f ->
        {exprs = []; stmts = []},
        (function {exprs = [];stmts = []} -> { stmt with skind = S_unimplemented_ffi_function f }
          | _ -> assert false )

      | _ -> default stmt
    );
()
