open Framework.Ast
open Framework.Visitor
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
      | S_program(({prog_kind = C_program(globals, funcs)} as prog)) ->
        (* Get initialization expressions from a list of variable declarations *)
        let rec exprs_in_vars = function
          | [] -> []
          | (v, init) :: tl -> (exprs_in_init_option init) @ (exprs_in_vars tl)
        in

        (* Get initialization expressions of local variables *)
        let rec exprs_in_funcs = function
          | [] -> []
          | f :: tl -> (exprs_in_vars f.c_func_local_vars) @ (exprs_in_funcs tl)
        in

        (* Re-construct a list variable declarations from a list of initialization expressions *)
        let rec vars_in_exprs exprs vars =
          match vars with
          | (v, init) :: tl ->
            let init, exprs = init_option_from_exprs exprs init in
            let vars, exprs = vars_in_exprs exprs tl in
            (v, init) :: vars, exprs
          | [] -> [], exprs
        in

        (* Re-construct a list of function declarations from a list of initialization expressions *)
        let rec funcs_in_exprs exprs stmts funcs =
          match stmts, funcs with
          | [], [] -> [], exprs
          | body :: stmts, f :: funcs ->
            let locals, exprs = vars_in_exprs exprs f.c_func_local_vars in
            let f = {f with c_func_local_vars = locals; c_func_body = body} in
            let funcs, exprs = funcs_in_exprs exprs stmts funcs in
            (f :: funcs), exprs
          | _ -> assert false
        in

        (* Construct the parts of the program *)
        let exprs = exprs_in_vars globals @ exprs_in_funcs funcs in
        let stmts = List.map (fun f -> f.c_func_body) funcs in

        {exprs; stmts},
        (function {exprs; stmts} ->
           (* Re-construct the program from its parts *)
           let globals, exprs = vars_in_exprs exprs globals in
           let funcs, _ = funcs_in_exprs exprs stmts funcs in
           {stmt with skind = S_program({prog with prog_kind = C_program(globals, funcs)})}
        )

      | S_c_local_declaration(v, init) ->
        let exprs = exprs_in_init_option init in
        {exprs; stmts = []},
        (function {exprs} ->
           let init, _ = init_option_from_exprs exprs init in
           {stmt with skind = S_c_local_declaration(v, init)}
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
      | S_c_switch_case(case) ->
        {exprs = [case]; stmts = []},
        (function
          | {exprs = [case]; stmts = []} -> {stmt with skind = S_c_switch_case(case)}
          | _ -> assert false
        )
      | S_c_switch_default -> leaf stmt

      | _ -> default stmt
    );
()
