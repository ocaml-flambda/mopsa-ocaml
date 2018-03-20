open Framework.Ast
open Framework.Visitor
open Ast

let () =
  register_expr_visitor ( fun default expr ->
      match ekind expr with
      | EC_address_of e ->
        {exprs = [e] ; stmts = []},
        (fun parts -> {expr with ekind = EC_address_of (List.hd parts.exprs)})
      | EC_deref e ->
        {exprs = [e] ; stmts = []},
        (fun parts -> {expr with ekind = EC_deref (List.hd parts.exprs)})
      | _ -> default expr
    );
  register_stmt_visitor ( fun default stmt ->
      match skind stmt with
      | SC_for (init, cond, it, stmts) ->
        assert false
      | _ -> default stmt
    )
