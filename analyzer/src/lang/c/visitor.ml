open Framework.Ast
open Framework.Visitor
open Ast

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

      | E_c_call(f, args) ->
        {exprs = f :: args; stmts = []},
        (function
          | {exprs = f :: args} -> {exp with ekind = E_c_call(f, args)}
          | _ -> assert false
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

      | _ -> default exp
    );

  register_stmt_visitor (fun default stmt ->
      match skind stmt with
      | S_c_local_declaration(_, None) -> leaf stmt
      | S_c_local_declaration(v, Some (C_init_expr e)) -> assert false
      | S_c_local_declaration(v, Some _) -> assert false
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
      | S_c_goto _ -> leaf stmt
      | S_c_switch(cond, body) ->
        {exprs = [cond]; stmts = [body]},
        (function
          | {exprs = [cond]; stmts = [body]} -> {stmt with skind = S_c_switch(cond, body)}
          | _ -> assert false
        )
      | S_c_labeled_stmt(label, body) ->
        {exprs = []; stmts = [body]},
        (function
          | {exprs = []; stmts = [body]} -> {stmt with skind = S_c_labeled_stmt(label, body)}
          | _ -> assert false
        )
      | S_c_switch_case(case, body) ->
        {exprs = [case]; stmts = [body]},
        (function
          | {exprs = [case]; stmts = [body]} -> {stmt with skind = S_c_switch_case(case, body)}
          | _ -> assert false
        )
      | S_c_switch_default(body) ->
        {exprs = []; stmts = [body]},
        (function
          | {exprs = []; stmts = [body]} -> {stmt with skind = S_c_switch_default(body)}
          | _ -> assert false
        )
      | _ -> default stmt
    );
()
