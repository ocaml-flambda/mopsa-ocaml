(**

  Copyright (c) 2017-2019 Aymeric Fromherz and The MOPSA Project

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 *)

(**
   Cst_to_ast - Translates Python CST to a simpler AST.

   The AST contains some additional static information, such as:
   - global variables,
   - functions locals,
   - detection of generator functions,
   - inlining of packages imports.

   But we do not give yet unique IDs to variables.
*)

open Cst
open Ast

let debug fmt = Debug.debug ~channel:"frontend.cst_to_ast" fmt

(** Main entry point that translates a CST to an AST *)
let rec translate_program (sl: Cst.stmt list) : Ast.program =
  {
    prog_body = translate_block sl (block_range sl);
    prog_globals = find_program_globals sl;
  }

(** Collect the set of global variables *)
and find_program_globals (sl: Cst.stmt list) =
  (find_lvals_in_block sl @ find_globals_in_block sl) |>
  List.sort_uniq compare |>
  List.map translate_var

(** Statement visitor to *)
and translate_stmt (stmt: Cst.stmt) : Ast.stmt =
  let range = stmt.srange in
  let skind = match stmt.skind with
    | FunctionDef (id, args, body, decors, return) ->
      let (args, vararg, kwonlyargs, kw_defaults, kwarg, defaults) = args in
      let parameters =
        match vararg, kwonlyargs, kw_defaults, kwarg, defaults with
        | None, [], [], None, _ ->
          List.map (fun v -> fst v) args
        | _ ->
          Exceptions.panic_at range "Unsupported function arguments in %s" id
      in
      let defaults = List.map (fun (e: Cst.expr) ->
          match e.ekind with
          | Null -> None
          | _ -> Some (translate_expr e)
        ) defaults
      in
      (* List.iter (fun (arg, ty) ->
       *     debug "%s: arg %s : %a" id arg (Option.print Pp.print_exp) (translate_expr_option2 ty)
       *   ) args;
       * debug "return %s: %a" id (Option.print Pp.print_exp) (translate_expr_option2 return); *)
      let lvals, globals, nonlocals = find_scopes_in_block body in
      let locals = List.filter (fun v -> not (List.mem v (parameters @ globals @ nonlocals))) lvals in
      S_function {
        func_var = translate_var id;
        func_parameters = List.map translate_var parameters;
        func_defaults = defaults;
        func_locals = List.sort_uniq compare locals |> List.map translate_var;
        func_globals = List.sort_uniq compare globals |> List.map translate_var;
        func_nonlocals = List.sort_uniq compare nonlocals |> List.map translate_var;
        func_body = {skind = S_block (List.map translate_stmt body |> add_implicit_return range); srange = range};
        func_is_generator = detect_yield_in_function body;
        func_decors = List.map translate_expr decors;
        func_types_in = List.map (fun (_, e) -> translate_expr_option2 e) args;
        func_type_out = translate_expr_option2 return;
        func_range = range;
      }

    | ClassDef (name, bases, keywords, body, decors) ->
      let lvals, globals, nonlocals = find_scopes_in_block body in
      let attributes = List.filter (fun v -> not (List.mem v (globals @ nonlocals))) lvals in
      S_class {
        cls_var = translate_var name;
        cls_bases = List.map translate_expr bases;
        cls_body = {skind = S_block (List.map translate_stmt body); srange = range};
        cls_decors = List.map translate_expr decors;
        cls_static_attributes = attributes |> List.sort_uniq compare |> List.map translate_var;
        cls_keywords = List.map (fun (k, v) -> (k, translate_expr v)) keywords;
        cls_range = range;
      }

    | Return value ->
      S_return (translate_expr_option range value)

    | Assign (targets, value) ->
      if List.length targets = 1 then
        (fun t -> S_assign (translate_expr t, translate_expr value)) (List.hd targets)
      else
        S_block (
          List.map (fun t -> {skind = S_assign (translate_expr t, translate_expr value); srange = range}) targets
        )

    | AugAssign (target, op, value) ->
      S_aug_assign (
        translate_expr target,
        O_arithmetic op,
        translate_expr value
      )

    | For (target, iter, body, orelse) ->
      S_for (
        translate_expr target,
        translate_expr iter,
        translate_block body range,
        translate_block_option orelse
      )
    | While (test, body, orelse) ->
      S_while (
        translate_expr test,
        translate_block body range,
        translate_block_option orelse
      )
    | If (test, body, orelse) ->
      S_if (
        translate_expr test,
        translate_block body range,
        translate_block_option orelse
      )

    | Raise (e, c) ->
      S_raise (
        translate_expr_option2 e,
        translate_expr_option2 c
      )

    | Try (body, excepts, orelse, finalbody) ->
      S_try (
        translate_block body range,
        excepts |> List.map (function Cst.ExceptHandler(ex, name, body) ->
            let ex' = (match ex with None -> None | Some e -> Some (translate_expr e))
            and name' = (match name with None -> None | Some n -> Some (translate_var n))
            and body' = translate_block body range (* FIXME: give a correct range for empty bodies *)
            in
            ex', name', body'
          ),
        translate_block_option orelse,
        translate_block_option finalbody
      )

    | Import([(modl, asname)]) ->
      let hd = module_hd modl in
      let asvar = match asname with None -> None | Some name -> Some (translate_var name) in
      S_import (modl, asvar, translate_var hd)

    | Import(aliases) ->
      S_block (
        aliases |> List.map (fun alias ->
          let stmt = {stmt with skind = Import([alias])} in
          translate_stmt stmt
          )
      )

    | ImportFrom (Some modl, [(name, asname)], Some 0) ->
      let hd = module_hd modl in
      let vroot = translate_var hd in
      let asvar = match asname with None -> translate_var name | Some asname -> translate_var asname in
      S_import_from(modl, name, vroot, asvar)

    | ImportFrom (Some modl, aliases, Some 0) ->
      S_block (
        aliases |> List.map (fun alias ->
          let stmt = {stmt with skind = ImportFrom(Some modl, [alias], Some 0)} in
          translate_stmt stmt
          )
      )

    | Global names -> S_pass
    | Nonlocal names -> S_pass

    | Expr e -> S_expression (translate_expr e)
    | Pass -> S_pass
    | Break -> S_break
    | Continue -> S_continue

    | With ([ctx, asname], body) ->
      S_with (
        translate_expr ctx,
        translate_expr_option2 asname,
        translate_block body range
      )

    | With ((ctx, asname) :: tl, body) ->
      S_with (
        translate_expr ctx,
        translate_expr_option2 asname,
        translate_stmt ({stmt with skind = With (tl, body)})
      )

    | Delete [e] ->
      S_delete (translate_expr e)

    | Delete el ->
      S_block (
        el |> List.map (fun e -> {skind = S_delete (translate_expr e); srange = stmt.srange})
      )

    | Assert (test, msg) ->
      S_assert (translate_expr test, translate_expr_option2 msg)

    | AnnAssign (var, typ, expr) ->
      let tya = {skind = S_type_annot (translate_expr var, translate_expr typ); srange = range} in
      Option.apply (fun expr -> S_block (translate_stmt {skind=(Assign ([var], expr)); srange=range} :: tya :: [])) tya.skind expr

    (* Not supported statements *)
    | ImportFrom _ -> failwith "Import from not supported"
    | AsyncFunctionDef (_,_,_,_,_) -> failwith "async not supported"
    | AsyncFor _ -> failwith "Async not supported"
    | AsyncWith (_,_) -> failwith "Async not supported"
    | With ([], _) -> failwith "With not supported"

  in
  {skind = skind; srange = stmt.srange}

and translate_expr_option range (expr: Cst.expr option) : Ast.expr =
  match expr with
  | None -> {ekind = E_none; erange = range}
  | Some e -> translate_expr e

and translate_expr_option2 (expr: Cst.expr option) : Ast.expr option =
  match expr with
  | None -> None
  | Some e -> Some (translate_expr e)

(** Expressions visitor *)
and translate_expr (expr: Cst.expr) : Ast.expr =
  let ekind, range = expr.ekind, expr.erange in
  let ekind' = match ekind with
    | BinOp (left, op, right) ->
      E_binop (
        translate_expr left,
        O_arithmetic op,
        translate_expr right
      )
    | UnaryOp (op, operand) ->
      E_unop (
        op,
        translate_expr operand
      )
    | List (elts, _) ->
      E_list (List.map translate_expr elts)
    | Dict (keys, values) ->
      E_dict (
        List.map translate_expr keys,
        List.map translate_expr values
      )
    | Set elts ->
      E_set (List.map translate_expr elts)
    | ListComp (elt, gens) ->
      E_list_comp (
        translate_expr elt,
        List.map translate_comprehension gens
      )
    | SetComp (elt, gens) ->
      E_set_comp (
        translate_expr elt,
        List.map translate_comprehension gens
      )
    | DictComp (key, value, gens) ->
      E_dict_comp (
        translate_expr key,
        translate_expr value,
        List.map translate_comprehension gens
      )
    | GeneratorExp (elt, gens) ->
      E_generator_comp (
        translate_expr elt,
        List.map translate_comprehension gens
      )
    | Yield eo ->
      E_yield (translate_expr_option range eo)
    | Compare (left, [op], [right]) ->
      E_binop (
        translate_expr left,
        O_comparison op,
        translate_expr right
      )
    | BoolOp (op, values) ->
      let rvalues = List.rev values in
      let hd, tl = List.hd rvalues, List.tl rvalues in
      let hd' = translate_expr hd in
      tl |> List.fold_left (fun acc exp ->
          let left = {ekind = acc; erange = hd'.erange} in
          let right = translate_expr exp in
          E_binop (
            right,
            O_bool op,
            left
          )
        ) hd'.ekind
    | Compare (left, ops, rights) ->
      E_multi_compare (
        translate_expr left,
        List.map (fun op -> O_comparison op) ops,
        List.map translate_expr rights
      )

    | Call (func, args, keywords) ->
      E_call (
        translate_expr func,
        List.map translate_expr args,
        List.map (fun (k, v) -> (k, translate_expr v)) keywords
      )
    | Num n -> E_num n
    | Str s -> E_str s
    | NameConstant (True) -> E_true
    | NameConstant (False) -> E_false
    | NameConstant (SNone) -> E_none
    | NameConstant (SNotImplemented) -> E_notimplemented
    | Attribute (v, attr, _) ->
      E_attr (
        translate_expr v,
        attr
      )
    | Subscript (v, Index i, _) ->
      E_index_subscript (
        translate_expr v,
        translate_expr i
      )
    | Subscript (v, Slice (a, b, s), _) ->
      E_slice_subscript (
        translate_expr v,
        translate_expr_option range a,
        translate_expr_option range b,
        translate_expr_option range s
      )
    | Name (id, _) -> E_id (translate_var id)
    | Tuple (elts, _) -> E_tuple (List.map translate_expr elts)
    | IfExp (test, body, orelse) ->
      E_if (
        translate_expr test,
        translate_expr body,
        translate_expr orelse
      )
    | Lambda(args, body) ->
      let (args, vararg, kwonlyargs, kw_defaults, kwarg, defaults) = args in
      let parameters =
        match vararg, kwonlyargs, kw_defaults, kwarg, defaults with
        | None, [], [], None, _ ->
          List.map (fun v -> fst v) args
        | _ ->
          Exceptions.panic_at range "Unsupported function arguments (lambda function)"
      in
      let defaults = List.map (fun (e: Cst.expr) ->
          match e.ekind with
          | Null -> None
          | _ -> Some (translate_expr e)
        ) defaults
      in
      E_lambda {
        lambda_parameters = List.map translate_var parameters;
        lambda_defaults = defaults;
        lambda_body = translate_expr body;
      }

    | Bytes s -> E_bytes s

    | Ellipsis -> E_ellipsis

    | Subscript (value, ExtSlice s, ctx) ->
      if List.for_all (fun sl -> match sl with | Index _ -> true | _ -> false) s then
        let els = List.map (fun sl -> match sl with | Index i -> i | _ -> assert false) s in
        E_index_subscript (
          translate_expr value,
          translate_expr {ekind = (Tuple (els, ctx)); erange = range}
        )
      else
        failwith "Subscript with ExtSlice not supported"
      (* debug "subscript value = %a, |extslice|=%d" Pp.print_exp (translate_expr value) (List.length s);
       * let rec f fmt = function
       *   | Slice (a, b, c) -> Format.fprintf fmt "slice[%a, %a, %a]" (Option.print Pp.print_exp) (translate_expr_option2 a) (Option.print Pp.print_exp) (translate_expr_option2 b) (Option.print Pp.print_exp) (translate_expr_option2 c)
       *   | ExtSlice l -> Format.fprintf fmt "%a" (Format.pp_print_list ~pp_sep:(fun fmt _ -> Format.pp_print_string fmt ", ") f) l
       *   | Index i -> Format.fprintf fmt "index[%a]" Pp.print_exp (translate_expr i) in
       * debug "extslice = %a" f sl; *)

    (* Not supported expressions *)
    | YieldFrom _ -> failwith "yield from not supported"
    | Await _ -> failwith "await not supported"
    | FormattedValue (v, conv, f_spec) -> failwith "Formatted value not supported"
    | JoinedStr (vals) -> failwith "JoinedStr not supported"
    | Starred (v, ctx) -> failwith "Starred not supported"
    | Null -> failwith "Null not supported"
  in
  {ekind = ekind'; erange = range}

and translate_block (sl : Cst.stmt list) range : Ast.stmt =
  (* FIXME: compute ranges of blocks *)
  match sl with
  | [] -> {skind = S_pass; srange = range}
  (* | [s] -> translate_stmt s *)
  | (hd :: tl) as sl ->
    let range = block_range sl in
    {
      skind = S_block (List.map translate_stmt sl);
      srange = range;
    }

and translate_block_option (sl : Cst.stmt list) : Ast.stmt option =
  match sl with
  | [] -> None
  | _ -> Some (translate_block sl (block_range sl))

and translate_comprehension = function
  | (taregt, iter, conds, _) ->
    (translate_expr taregt, translate_expr iter, List.map translate_expr conds)

and translate_var name = {
  name;
  uid = 0;
}

and find_lvals_in_block sl =
  sl |> List.fold_left (fun acc s -> acc @ find_lvals_in_stmt s) []

and find_lvals_in_stmt stmt =
  let skind, range = stmt.skind, stmt.srange in
  match skind with
  | FunctionDef (name, args, body, decos, returns) -> [name]

  | ClassDef (name, bases, keywords, body, decorators) -> [name]

  | Assign (targets, _) ->
    targets |> List.fold_left (fun acc t -> find_lvals_in_expr t @ acc) []

  | AugAssign (target, _, _) ->
    find_lvals_in_expr target

  | For (target, iter, body, orelse) ->
    find_lvals_in_expr target @ find_lvals_in_block body @ find_lvals_in_block orelse

  | While (target, body, orelse) ->
    find_lvals_in_expr target @ find_lvals_in_block body @ find_lvals_in_block orelse

  | If (test, body, orelse) ->
    find_lvals_in_expr test @ find_lvals_in_block body @ find_lvals_in_block orelse

  | Try (body, handlers, orelse, finalbody) ->
    find_lvals_in_block body @ find_lvals_in_excepts handlers @ find_lvals_in_block orelse @ find_lvals_in_block finalbody

  | Import([(modl, None)]) ->
    let hd = module_hd modl in
    [hd]

  | Import([(modl, Some name)]) ->
    let hd = module_hd modl in
    [hd; name]

  | Import(aliases) ->
    aliases |> List.fold_left (fun acc alias ->
        let stmt = {stmt with skind = Import([alias])} in
        acc @ find_lvals_in_stmt stmt
      ) []

  | ImportFrom (Some modl, names, _) ->
    let hd = module_hd modl in
    hd :: (names |> List.map (function (name, None) -> name | (_, Some asname) -> asname))

  | ImportFrom (None, names, _) ->
    names |> List.map (function (name, None) -> name | (_, Some asname) -> asname)

  | With (ctxl, body) ->
    let targets = ctxl |> List.fold_left (fun acc -> function
        | (_, None) -> acc
        | (_, Some target) -> find_lvals_in_expr target @ acc
      ) []
    in
    targets @ find_lvals_in_block body

  (* Read-only statements *)
  | Assert _ | Raise _ | Return _ | Delete _
  | Global _| Nonlocal _ | Expr _ | Pass | Break | Continue
    -> []

  | AnnAssign (t, _, _) ->
    find_lvals_in_expr t

  (* Not supported statements *)
  | AsyncFunctionDef (_,_,_,_,_) -> failwith "async not supported"
  | AsyncFor _ -> failwith "Async not supported"
  | AsyncWith (_,_) -> failwith "Async not supported"

and find_lvals_in_excepts handlers =
  handlers |> List.fold_left (fun acc -> function
      | ExceptHandler(_, Some id, body) ->
        id :: find_lvals_in_block body @ acc
      | ExceptHandler(_, None, body) ->
        find_lvals_in_block body @ acc
    ) []

and find_lvals_in_expr expr =
  match expr.ekind with
  | Name (id, _) -> [id]
  | Tuple (elts, _)
  | List(elts, _) ->
    elts |> List.fold_left (fun acc e -> acc @ find_lvals_in_expr e) []
  (** Method-based lvals do not make the expression local *)
  | Attribute _ | Subscript _ -> []
  (* Non-lval expressions *)
  | BoolOp _ | BinOp _ | UnaryOp _ | Dict _ | Set _ | ListComp _ | SetComp _
  | DictComp _ | GeneratorExp _ | Yield _ | Compare _ | Call _ | Num _ | Str _ | NameConstant _ | Lambda _ | Bytes _ -> []
  (* Not supported expressions *)
  | IfExp (test, body, orelse) -> failwith "IfExp not supported"
  | YieldFrom _ -> failwith "yield from not supported"
  | Await _ -> failwith "await not supported"
  | FormattedValue (v, conv, f_spec) -> failwith "Formatted value not supported"
  | JoinedStr (vals) -> failwith "JoinedStr not supported"
  | Ellipsis -> failwith "Ellipsis not supported"
  | Starred (v, ctx) -> failwith "Starred not supported"
  | Null -> failwith "Null not supported"


and find_scopes_in_block block =
  block |> List.fold_left (fun acc s ->
      acc +@ find_scopes_in_stmt s
    ) ([], [], [])

and find_scopes_in_stmt stmt =
  let skind, range = stmt.skind, stmt.srange in
  match skind with
  | Global ids -> [], ids, []
  | Nonlocal ids -> [], [], ids

  (* Compound statements *)
  | While (_, body, orelse)
  | If (_, body, orelse) ->
    find_scopes_in_block body +@ find_scopes_in_block orelse

  | For (target, _, body, orelse) ->
    (find_lvals_in_expr target) -@ (find_scopes_in_block body +@ find_scopes_in_block orelse)

  | Try (body, handlers, orelse, finalbody) ->
    find_scopes_in_block body +@ find_scopes_in_block orelse +@ find_scopes_in_block finalbody +@ (
      handlers |> List.fold_left (fun acc -> function
          | ExceptHandler(_, Some v, body) ->
            [v] -@ acc +@ find_scopes_in_block body

          | ExceptHandler(_, None, body) ->
            acc +@ find_scopes_in_block body
        ) ([], [], [])
    )
  | With (ctxl, body) ->
    let targets = ctxl |> List.fold_left (fun acc -> function
        | (_, None) -> acc
        | (_, Some target) -> find_lvals_in_expr target @ acc
      ) []
    in
    targets -@ find_scopes_in_block body

  (* Inner scope statements *)
  | FunctionDef (name, _, _, _, _) | ClassDef (name, _, _, _, _) ->
    [name], [], []

  (* Atomic statements *)
  | Assign (targets, _) ->
    (
      targets |> List.fold_left (fun acc t -> find_lvals_in_expr t @ acc) []
    ), [], []

  | AugAssign (target, _, _) ->
    (find_lvals_in_expr target), [], []

  | Import _ ->
    find_lvals_in_stmt stmt, [], []

  | ImportFrom _ ->
    find_lvals_in_stmt stmt, [], []

  | Assert _ | Raise _ | Return _ | Delete _ | Expr _ | Pass | Break | Continue
    -> [], [], []

  | AnnAssign (t, _, _) ->
    (find_lvals_in_expr t), [], []

  (* Not supported statements *)
  | AsyncFunctionDef (_,_,_,_,_) -> failwith "async not supported"
  | AsyncFor _ -> failwith "Async not supported"
  | AsyncWith (_,_) -> failwith "Async not supported"

and find_globals_in_block block =
  block |> List.fold_left (fun acc stmt ->
      acc @ find_globals_in_stmt stmt
    ) []

and find_globals_in_stmt stmt =
  match stmt.skind with
  | Global ids -> ids

  (* Compound statements *)
  | While (_, body, orelse) | If (_, body, orelse) ->
    find_globals_in_block body @ find_globals_in_block orelse

  | For (_, _, body, orelse) ->
    find_globals_in_block body @ find_globals_in_block orelse

  | Try (body, handlers, orelse, finalbody) ->
    find_globals_in_block body @ find_globals_in_block orelse @ find_globals_in_block finalbody @ (
      handlers |> List.fold_left (fun acc -> function
          | ExceptHandler(_, _, body) ->
            acc @ find_globals_in_block body
        ) []
    )
  | With (items, body) -> find_globals_in_block body

  (* Inner scope statements *)
  | FunctionDef (_, _, body, _, _) | ClassDef (_, _, _, body, _) ->
    find_globals_in_block body

  | Assign _ | AugAssign _ | Nonlocal _ | Import _ | Assert _ | ImportFrom _
  | Raise _ | Return _ | Delete _ | Expr _ | Pass | Break | Continue | AnnAssign _
    -> []
  (* Not supported statements *)
  | AsyncFunctionDef (_,_,_,_,_) -> failwith "async not supported"
  | AsyncFor _ -> failwith "Async not supported"
  | AsyncWith (_,_) -> failwith "Async not supported"


(** Generator function detection *)
and detect_yield_in_function body =
  let rec doit () = detect_yield_in_block body

  and detect_yield_in_block block =
    block |> List.exists detect_yield_in_stmt

  and detect_yield_in_stmt stmt =
    let skind, range = stmt.skind, stmt.srange in
    match skind with
    | Expr e | Assign(_, e) | AugAssign(_, _, e) | AnnAssign (_, _, Some e) -> detect_yield_in_expr e
    (* Compound statements *)
    | For (_, _, body, orelse)
    | While (_, body, orelse)
    | If (_, body, orelse) ->
      detect_yield_in_block body || detect_yield_in_block orelse
    | Try (body, handlers, orelse, finalbody) ->
      detect_yield_in_block body || detect_yield_in_block orelse || detect_yield_in_block finalbody
    | With (_, body)  ->
      detect_yield_in_block body
    (* Inner scope statements *)
    | FunctionDef _ | ClassDef _ -> false
    (* Atomic statements *)
    | Import _ | ImportFrom _ | Assert _ | Raise _ | Return _ | Delete _
    | Pass | Break | Continue | Global _ | Nonlocal _ | AnnAssign (_, _, None) -> false
    (* Not supported statements *)
    | AsyncFunctionDef (_,_,_,_,_) -> failwith "async not supported"
    | AsyncFor _ -> failwith "Async not supported"
    | AsyncWith (_,_) -> failwith "Async not supported"

  and detect_yield_in_expr expr =
    match expr.ekind with
    | Yield _ -> true
    | _ -> false

  in
  doit ()

and add_implicit_return range l =
  match l with
  | [] -> [{skind = S_return {ekind = E_none; erange = range}; srange = range}]
  | l ->
    let last_stmt = List.hd @@ List.rev l in
    match last_stmt.skind with
    | S_return _ -> l
    | _ -> l @ [{skind = S_return {ekind = E_none; erange = range}; srange = range}]

and (+@) (l1, l2, l3) (l1', l2', l3') = (l1 @ l1'), (l2 @ l2'), (l3 @ l3')
and (-@) (l1) (l1', l2', l3') = (l1 @ l1'), l2', l3'

and block_range sl =
  match sl with
  | [] -> Exceptions.panic "block_range: empty block"
  | [s] -> s.srange
  | hd :: tl ->
    let last = List.rev tl |> List.hd in
    let open Location in
    mk_orig_range (get_range_start hd.srange) (get_range_end last.srange)

and module_hd modl =
  Str.split (Str.regexp "\\.") modl |>
  List.hd
