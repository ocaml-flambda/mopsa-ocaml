(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Visitor of Python AST. *)

open Framework.Ast
open Framework.Visitor
open Ast

let () =
  register_expr_visitor (fun default exp ->
      match ekind exp with
      | E_py_undefined _ -> leaf exp

      | E_py_object _ -> leaf exp

      | E_py_list elts ->
        {exprs = elts; stmts = [];},
        (fun parts -> {exp with ekind = E_py_list(parts.exprs)})

      | E_py_tuple elts ->
        {exprs = elts; stmts = [];},
        (fun parts -> {exp with ekind = E_py_tuple(parts.exprs)})

      | E_py_attribute(obj, attr) ->
        {exprs = [obj]; stmts = [];},
        (fun parts -> {exp with ekind = E_py_attribute(List.hd parts.exprs, attr)})

      | E_py_dict(keys, values) ->
        {exprs = keys @ values; stmts = [];},
        (function
          | {exprs} ->
            let rec nhd n l =
              if n = 0 then []
              else
                match l with
                | hd :: tl -> hd :: (nhd (n - 1) tl)
                | _ -> assert false
            and  ntl n l =
              if n = 0 then l
              else
                match l with
                | _ :: tl -> ntl (n - 1) tl
                | _ -> assert false
            in
            let keys = nhd (List.length keys) exprs
            and values = ntl (List.length keys) exprs
            in
            {exp with ekind = E_py_dict(keys, values)}
        )

      | E_py_index_subscript(obj, index) ->
        {exprs = [obj; index]; stmts = [];},
        (fun parts -> {exp with ekind = E_py_index_subscript(List.hd parts.exprs, List.hd @@ List.tl parts.exprs)})

      | E_py_slice_subscript(obj, a, b, s) ->
        {exprs = [obj; a; s; b]; stmts = [];},
        (function {exprs = [obj; a; s; b]} -> {exp with ekind = E_py_slice_subscript(obj, a, b, s)} | _ -> assert false)

      | E_py_yield(e) ->
        {exprs = [e]; stmts = [];},
        (function {exprs = [e]} -> {exp with ekind = E_py_yield(e)} | _ -> assert false)

      | E_py_if(test, body, orelse) ->
        {exprs = [test; body; orelse]; stmts = [];},
        (function {exprs = [test; body; orelse]} -> {exp with ekind = E_py_if(test, body, orelse)} | _ -> assert false)

      | E_py_list_comprehension(e, comprhs)
      | E_py_set_comprehension(e, comprhs)
      | E_py_generator_comprehension(e, comprhs) ->
        let iters, targets = comprhs |> List.fold_left (fun acc (target, iter, conds) ->
            match conds with
            | [] ->
              iter :: fst acc, target :: snd acc
            | _ -> assert false
          ) ([], [])
        in
        {exprs = e :: iters; stmts = []},
        (function
          | {exprs = e :: iters} ->
            let comprhs =
              List.combine iters targets |>
              List.fold_left (fun acc (iter, target) ->
                  (target, iter, []) :: acc
                ) []
            in
            begin
              match ekind exp with
              | E_py_list_comprehension _ -> {exp with ekind = E_py_list_comprehension(e, comprhs)}
              | E_py_set_comprehension _ -> {exp with ekind = E_py_set_comprehension(e, comprhs)}
              | E_py_generator_comprehension _ -> {exp with ekind = E_py_generator_comprehension(e, comprhs)}
              | _ -> assert false
            end
          | _ -> assert false
        )

      | E_py_dict_comprehension(k, v, comprhs) ->
        let iters, targets = comprhs |> List.fold_left (fun acc (target, iter, conds) ->
            match conds with
            | [] ->
              iter :: fst acc, target :: snd acc
            | _ -> assert false
          ) ([], [])
        in
        {exprs = k :: v :: iters; stmts = []},
        (function
          | {exprs = k :: v :: iters} ->
            let comprhs =
              List.combine iters targets |>
              List.fold_left (fun acc (iter, target) ->
                  (target, iter, []) :: acc
                ) []
            in
            {exp with ekind = E_py_dict_comprehension(k, v, comprhs)}
          | _ -> assert false
        )


      | E_py_call(f, args, keywords) ->
        {exprs = f :: args @ (List.map snd keywords); stmts = [];},
        (fun parts ->
           let f = List.hd parts.exprs in
           let args, kwvals = Utils.partition_list_by_length (List.length args) (List.tl parts.exprs) in
           let keywords = List.combine keywords kwvals |>
                          List.map (fun ((k, _), v) -> (k, v))
           in
           {exp with ekind = E_py_call(f, args, keywords)})

      | E_py_bytes _ -> leaf exp

      | E_py_lambda(l) ->
        let defaults = l.py_lambda_defaults |>
                       List.fold_left (fun acc -> function
                           | None -> acc
                           | Some e -> e :: acc
                         ) [] |>
                       List.rev
        in
        {exprs = l.py_lambda_body :: defaults; stmts = [];},
        (fun parts ->
           let body = List.hd parts.exprs and defaults = List.tl parts.exprs in
           let defaults, _ = l.py_lambda_defaults |>
                          List.fold_left (fun (acc, defaults) -> function
                              | None -> (None :: acc, defaults)
                              | Some e ->
                                let e = List.hd defaults in
                                (Some e :: acc, List.tl defaults)
                            ) ([], defaults)
           in
           let l = { l with py_lambda_body = body; py_lambda_defaults = List.rev defaults} in
           {exp with ekind = E_py_lambda(l)}
        )


      | E_py_multi_compare(left, ops, rights) ->
        {exprs = left :: rights; stmts = [];},
        (function
          | {exprs = left :: rights} -> {exp with ekind = E_py_multi_compare(left, ops, rights)}
          | _ -> assert false
        )

      | _ -> default exp
    );

  register_stmt_visitor (fun default stmt ->
      match skind stmt with
      | S_py_class(cls) ->
        {exprs = cls.py_cls_bases; stmts = [cls.py_cls_body];},
        (function {exprs = bases; stmts = [body]} -> {stmt with skind = S_py_class({cls with py_cls_body = body; py_cls_bases = bases})} | _ -> assert false)
      | S_py_function(func) ->
        {exprs = []; stmts = [func.py_func_body];},
        (fun parts -> {stmt with skind = S_py_function({func with py_func_body = List.hd parts.stmts})})
      | S_py_raise(None) -> leaf stmt
      | S_py_raise(Some e) ->
        {exprs = [e]; stmts = [];},
        (fun parts -> {stmt with skind = S_py_raise(Some (List.hd parts.exprs))})
      | S_py_try(body, excepts, orelse, finally) ->
        {exprs = []; stmts = [body; orelse; finally];},
        (function
          | {stmts = [body'; orelse'; finally']} -> {stmt with skind = S_py_try(body', excepts, orelse', finally')}
          | _ -> assert false)
      | S_py_while(test, body, orelse) ->
        {exprs = [test]; stmts = [body; orelse];},
        (function
          | {exprs = [test']; stmts = [body'; orelse']} -> {stmt with skind = S_py_while(test', body',orelse')}
          | _ -> assert false)

      | S_py_for(({ekind = E_py_tuple _ | E_py_list _} as target), iter, body, orelse) ->
        {exprs = [iter]; stmts = [];},
        (function
          | {exprs = [iter]; stmts = [];} -> {stmt with skind = S_py_for(target, iter, body, orelse)}
          | _ -> assert false)


      | S_py_for(target, iter, body, orelse) ->
        {exprs = [iter]; stmts = []},
        (function
          | {exprs = [iter]; stmts = []} -> {stmt with skind = S_py_for(target, iter, body, orelse)}
          | _ -> assert false)


      | S_py_multi_assign(targets, e) ->
        {exprs = [e]; stmts = []},
        (function {exprs = [e]} -> {stmt with skind = S_py_multi_assign(targets, e)} | _ -> assert false)

      | S_py_aug_assign(x, op, e) ->
        {exprs = [e]; stmts = []},
        (function
          | {exprs = [e]} -> {stmt with skind = S_py_aug_assign(x, op, e)}
          | _ -> assert false
        )


      | S_py_import _ -> leaf stmt
      | S_py_import_from _ -> leaf stmt

      | S_py_delete(e) ->
        {exprs = [e]; stmts = [];},
        (fun parts -> {stmt with skind = S_py_delete(List.hd parts.exprs)})

      | S_py_assert(test, None) ->
        {exprs = [test]; stmts = [];},
        (function
          | {exprs = [test]} -> {stmt with skind = S_py_assert(test, None)}
          | _ -> assert false
        )

      | S_py_assert(test, Some msg) ->
        {exprs = [test; msg]; stmts = [];},
        (function
          | {exprs = [test; msg]} -> {stmt with skind = S_py_assert(test, Some msg)}
          | _ -> assert false
        )



      | S_py_with(ctx, None, body) ->
        {exprs = [ctx]; stmts = [body];},
        (function
          | {exprs = [ctx]; stmts = [body]} -> {stmt with skind = S_py_with(ctx, None, body)}
          | _ -> assert false
        )

      | S_py_with(ctx, Some target, body) ->
        {exprs = [ctx]; stmts = [body]},
        (function
          | {exprs = [ctx]; stmts = [body]} -> {stmt with skind = S_py_with(ctx, Some target, body)}
          | _ -> assert false
        )


      | _ -> default stmt
    );
  ()
