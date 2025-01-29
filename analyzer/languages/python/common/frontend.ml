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

(**
   Python frontend translates the parser's AST into Framework's AST.
*)
open Mopsa
open Lexing
open Ast
open Utils

let opt_gc_after_functioncall = ref false
let opt_gc_percent_calls = ref 100
let gc_call = ref 0
let () =
  register_domain_option "python.frontend" {
      key = "-gc";
      category = "Python";
      doc = " perform abstract garbage collection after function calls";
      spec = Set opt_gc_after_functioncall;
      default = "";
    };
  register_domain_option "python.frontend" {
      key = "-gc-percent";
      category = "Python";
      doc = Format.asprintf " percent of abstract garbage collection calls (default: %d)" !opt_gc_percent_calls;
      spec = Set_int (opt_gc_percent_calls, ArgExt.empty);
      default = "";
    }

let opt_check_type_annot = ref true

let debug fmt = Debug.debug ~channel:"python.frontend" fmt

(** Entry point of the frontend *)
let rec parse_program (files: string list) : program =
  match files with
  | [filename] ->
    debug "parsing %s" filename;
    let () = opt_check_type_annot := (Filename.extension filename <> ".pyi") in
    let ast, counter = Mopsa_py_parser.Main.parse_file ~counter:(Core.Ast.Var.get_vcounter_val ()) filename in
    let body = from_stmt ast.prog_body in
    let globals = List.map from_var (ast.prog_globals @ Mopsa_py_parser.Scoping.gc) in
    let body = snd @@ free_vars globals [] body in
    let body = cell_vars body in
    Visitor.fold_stmt (fun () e -> VisitParts ()) (fun () s -> (match skind s with
                                                               | S_py_function f ->
                                                                  debug "%a: cv=%a, fv=%a, locals=%a" pp_var f.py_func_var (Format.pp_print_list pp_var) f.py_func_cellvars (Format.pp_print_list pp_var) f.py_func_freevars Pp.pp_vars f.py_func_locals
                                                               | _ -> ());
                                                                  VisitParts ()) () body;
    (* debug "detecting closure";
     *    let py_func_cellvars, py_func_body = detect_closures py_func_body in
     *    debug "cellvars of %a: %a; locals: %a, params: %a" pp_var f.py_func_var (Format.pp_print_list pp_var) f.py_func_cellvars (Format.pp_print_list pp_var) f.py_func_locals (Format.pp_print_list pp_var) f.py_func_parameters; *)
    let body = if Filename.extension filename <> ".pyi" && !opt_gc_after_functioncall then
                 Universal.Ast.mk_block ([body;mk_stmt Universal.Heap.Recency.S_perform_gc (Location.mk_fresh_range ())]) body.srange
               else body in
    Hooks.Coverage.Hook.add_file filename body;
    Core.Ast.Var.start_vcounter_at counter;
    {
      prog_kind = Ast.Py_program (filename, globals, body);
      prog_range = mk_program_range [filename];
    }

  | [] -> panic "no input file"

  | _ -> panic "analysis of multiple files not supported"

(** Create a Universal.var variable from Mopsa_py_parser.Ast.var *)
and from_var (v:Mopsa_py_parser.Ast.var) =
  let open Core.Ast in
  (* let () = if Hashtbl.mem tmp v.uid && Hashtbl.find tmp v.uid <> v.name then
   *     Exceptions.panic "%d is already %s, conflict with current %s@\n" v.uid (Hashtbl.find tmp v.uid) v.name
   *   else
   *     Hashtbl.add tmp v.uid v.name in *)
  mk_uniq_var v.name v.uid (T_py None)

(** Translation of a Python statement *)
and from_stmt (stmt: Mopsa_py_parser.Ast.stmt) : stmt =
  let srange' = stmt.srange in
  let skind' =
    match stmt.skind with
    | S_assign (x, ({ekind = E_call _} as e)) when !opt_gc_after_functioncall ->
       incr gc_call;
       if !gc_call mod (100 / !opt_gc_percent_calls) = 0 then
         S_block ([mk_stmt (S_assign (from_exp x, from_exp e)) srange'; mk_stmt Universal.Heap.Recency.S_perform_gc srange'], [])
       else S_assign (from_exp x, from_exp e)

    | S_expression ({ekind = E_call _} as e) when !opt_gc_after_functioncall ->
       incr gc_call;
       if !gc_call mod (100 / !opt_gc_percent_calls) = 0 then
         S_block ([mk_stmt (Universal.Ast.S_expression (from_exp e)) srange'; mk_stmt Universal.Heap.Recency.S_perform_gc srange'], [])
       else Universal.Ast.S_expression (from_exp e)

    | S_assign (x, e) ->
      S_assign (from_exp x, from_exp e)

    | S_type_annot (x, e) ->
       if !opt_check_type_annot then
         S_py_check_annot (from_exp x, from_exp e)
       else
         let expr = from_exp e in
         let expr = {expr with ekind = E_py_annot expr} in
         S_py_annot (from_exp x, expr)

    | S_expression e ->
      Universal.Ast.S_expression (from_exp e)

    | S_while (test, body, orelse) ->
      S_py_while (
        from_exp test,
        from_stmt body,
        from_stmt_option srange' orelse
      )

    | S_break ->
      Universal.Ast.S_break

    | S_continue ->
      Universal.Ast.S_continue

    | S_block sl ->
      S_block (List.map from_stmt sl, [])

    | S_aug_assign (x, op, e) ->
      S_py_aug_assign(from_exp x, from_binop op, from_exp e)

    | S_if (test, body, orelse) ->
      S_py_if (
        from_exp test,
        from_stmt body,
        from_stmt_option (tag_range srange' "empty if else") orelse
      )

    | S_function f ->
       let py_func_body = from_stmt f.func_body in
       let f = {
           py_func_var = from_var f.func_var;
           py_func_parameters = List.map from_var f.func_parameters;
           py_func_defaults = List.map from_exp_option f.func_defaults;
           py_func_vararg = OptionExt.lift from_var f.func_vararg;
           py_func_kwonly_args = List.map from_var f.func_kwonly_args;
           py_func_kwonly_defaults = List.map from_exp_option f.func_kwonly_defaults;
           py_func_kwarg = OptionExt.lift from_var f.func_kwarg;
           py_func_locals = List.map from_var f.func_locals;
           py_func_body;
           py_func_is_generator = f.func_is_generator;
           py_func_decors = List.map from_exp f.func_decors;
           py_func_range = f.func_range;
           py_func_types_in = List.map from_exp_option f.func_types_in;
           py_func_type_out = from_exp_option f.func_type_out;
           py_func_ret_var =
             mk_fresh_uniq_var ("ret_" ^ f.func_var.name) (T_py None) ();
           py_func_cellvars = [];
           py_func_freevars = []
         } in
       Ast.S_py_function f


    | S_class cls ->
      S_py_class {
        py_cls_var = from_var cls.cls_var;
        py_cls_body = from_stmt cls.cls_body;
        py_cls_bases = List.map from_exp cls.cls_bases;
        py_cls_static_attributes = List.map from_var cls.cls_static_attributes;
        py_cls_keywords = List.map (fun (k, v) -> (k, from_exp v)) cls.cls_keywords;
        py_cls_decors = List.map from_exp cls.cls_decors;
        py_cls_range = cls.cls_range;
      }

    | S_for (target,iter,body,orelse) ->
      S_py_for(
        from_exp target,
        from_exp iter,
        from_stmt body,
        from_stmt_option (tag_range srange' "empty for else") orelse
      )

    | S_return e ->
      Universal.Ast.S_return (Some (from_exp e))

    | S_raise(e, c)->
      S_py_raise (match e with None -> None | Some e -> Some (from_exp e))

    | S_try (body, excepts, orelse, finally) ->
      S_py_try (
        from_stmt body,
        excepts |> List.map (fun (typ, name, body) ->
            {
              py_excpt_type = (match typ with None -> None | Some e -> Some (from_exp e));
              py_excpt_name = (match name with None -> None | Some v -> Some (from_var v));
              py_excpt_body = from_stmt body;
            }),
        from_stmt_option (tag_range srange' "empty try else") orelse,
        from_stmt_option (tag_range srange' "empty try finally")finally
      )

    | S_import(modul, None, vroot) -> S_py_import(modul, None, from_var vroot)
    | S_import(modul, Some vasname, vroot) -> S_py_import(modul, Some (from_var vasname), from_var vroot)

    | S_import_from(modul, name, vroot, vname) -> S_py_import_from(modul, name, from_var vroot, from_var vname)

    | S_with(ctx, target, body) ->
      S_py_with(
        from_exp ctx,
        from_exp_option target,
        from_stmt body
      )

    | S_pass -> S_block ([],[])

    | S_delete e -> S_py_delete (from_exp e)

    | S_assert(e, msg) -> S_py_assert(from_exp e, from_exp_option msg)


  in
  {skind = skind'; srange = srange'}

(** Translate an optional statement into en eventual empty one *)
and from_stmt_option : Location.range -> Mopsa_py_parser.Ast.stmt option -> stmt
  = fun none_case_range -> function
    | None -> {skind = S_block ([],[]); srange = none_case_range}
    | Some s -> from_stmt s

and from_exp_option : Mopsa_py_parser.Ast.expr option -> expr option
  = function
    | None -> None
    | Some e -> Some (from_exp e)


(** Translation of expressions *)
and from_exp exp =
  let ekind, etyp = match exp.ekind with
    | E_ellipsis ->
      E_constant (C_py_ellipsis),
      (T_py None)

    | E_true ->
      E_constant (C_bool true),
      (T_py None)

    | E_false ->
      E_constant (C_bool false),
      (T_py None)

    | E_none ->
      E_constant (C_py_none),
      (T_py (Some NoneType))

    | E_notimplemented ->
      E_constant (C_py_not_implemented),
      (T_py (Some NotImplemented))

    | E_num (Mopsa_py_parser.Cst.Int i) ->
      E_constant (Universal.Ast.C_int i),
      (T_py None)

    | E_num (Mopsa_py_parser.Cst.Float f) ->
      E_constant (Universal.Ast.C_float f),
      (T_py (Some (Float F_DOUBLE)))

    | E_num (Mopsa_py_parser.Cst.Imag j) ->
      ignore (Str.string_match (Str.regexp "\\(.*\\)j") j 0);
      let j = Str.matched_group 1 j in
      let j = float_of_string j in
      E_constant (Ast.C_py_imag j),
      (T_py (Some Complex))

    | E_str s ->
       E_constant (Universal.Ast.C_string s),
       (T_py None)

    | E_attr (obj, attr) ->
      E_py_attribute (from_exp obj, attr),
      (T_py None)

    | E_id v ->
      E_var (from_var v, None),
      (T_py None)

    | E_binop (left, op, right) ->
      E_binop (
        from_binop op,
        from_exp left,
        from_exp right
      ),
      (T_py None)

    | E_unop (op, operand) ->
      E_unop (
        from_unop op,
        from_exp operand
      ),
      (T_py None)

    | E_call (f, args, keywords) ->
      E_py_call (
        from_exp f,
        List.map from_exp args,
        List.map (fun (k, v) -> (k, from_exp v)) keywords
      ),
      (T_py None)

    | E_list elts ->
      E_py_list (
        List.map from_exp elts
      ),
      (T_py None)

    | E_index_subscript (obj, index) ->
      E_py_index_subscript (from_exp obj, from_exp index),
      (T_py None)

    | E_slice_subscript (obj,a,b,s) ->
      E_py_slice_subscript(from_exp obj, from_exp a, from_exp b, from_exp s),
      (T_py None)

    | E_yield e ->
      E_py_yield(from_exp e),
      (T_py None)

    | E_yield_from e ->
      E_py_yield_from(from_exp e),
      (T_py None)

    | E_if(test, body, orelse) ->
      E_py_if(
        from_exp test,
        from_exp body,
        from_exp orelse
      ),
      (T_py None)

    | E_tuple el ->
      E_py_tuple(List.map from_exp el),
      (T_py None)

    | E_list_comp (e, comprhs) ->
      Ast.E_py_list_comprehension (
        from_exp e,
        comprhs |> List.map (fun (target, iter, conds) ->
            (from_exp target, from_exp iter, List.map from_exp conds)
          )
      ),
      (T_py None)

    | E_dict (keys, values) ->
      Ast.E_py_dict(
        List.map from_exp keys,
        List.map from_exp values
      ),
      (T_py None)

    | E_lambda l ->
      E_py_lambda {
        py_lambda_body = from_exp l.lambda_body;
        py_lambda_parameters = List.map from_var l.lambda_parameters;
        py_lambda_defaults = List.map from_exp_option l.lambda_defaults;
      },
      (T_py None)

    | E_bytes s ->
      E_py_bytes s, (T_py None)

    | E_set el ->
      E_py_set(List.map from_exp el), (T_py None)

    | E_generator_comp (e,comprhs) ->
      E_py_generator_comprehension (
        from_exp e,
        comprhs |> List.map (fun (target, iter, conds) ->
            (from_exp target, from_exp iter, List.map from_exp conds)
          )
      ),
      (T_py None)

    | E_set_comp (e,comprhs) ->
      E_py_set_comprehension (
        from_exp e,
        comprhs |> List.map (fun (target, iter, conds) ->
            (from_exp target, from_exp iter, List.map from_exp conds)
          )
      ),
      (T_py None)

    | E_dict_comp (k,v,comprhs) ->
      E_py_dict_comprehension (
        from_exp k,
        from_exp v,
        comprhs |> List.map (fun (target, iter, conds) ->
            (from_exp target, from_exp iter, List.map from_exp conds)
          )
      ),
      (T_py None)

    | E_multi_compare(left, ops, rights) ->
      E_py_multi_compare (
        from_exp left,
        List.map from_binop ops,
        List.map from_exp rights
      ),
      (T_py None)


  in
  mk_expr ekind ~etyp exp.erange


and from_binop : Mopsa_py_parser.Ast.binop -> operator = function
  | O_arithmetic op -> from_arithmetic_op op
  | O_comparison op -> from_comparison_op op
  | O_bool op -> from_bool_op op

and from_arithmetic_op = function
  | Add -> Universal.Ast.O_plus
  | Sub -> Universal.Ast.O_minus
  | Mult -> Universal.Ast.O_mult
  | Div -> Universal.Ast.O_div
  | FloorDiv -> O_py_floor_div
  | Mod -> Universal.Ast.O_mod
  | Pow -> Universal.Ast.O_pow
  | BitOr -> Universal.Ast.O_bit_or
  | BitXor -> Universal.Ast.O_bit_xor
  | BitAnd -> Universal.Ast.O_bit_and
  | MatMult -> O_py_mat_mult
  | RShift -> Universal.Ast.O_bit_rshift
  | LShift -> Universal.Ast.O_bit_lshift

and from_bool_op = function
  | And -> O_py_and
  | Or -> O_py_or

and from_comparison_op : Mopsa_py_parser.Cst.cmpop -> operator = function
  | Eq -> O_eq
  | NotEq -> O_ne
  | Lt -> O_lt
  | LtE -> O_le
  | Gt -> O_gt
  | GtE -> O_ge
  | Is -> O_py_is
  | IsNot -> O_py_is_not
  | In -> O_py_in
  | NotIn -> O_py_not_in

and from_unop = function
  | Not -> O_py_not
  | USub -> Universal.Ast.O_minus
  | UAdd -> Universal.Ast.O_plus
  | Invert -> Universal.Ast.O_bit_invert

and free_vars globals bv s =
  let pp_vars = Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_var in
  debug "free_vars %a %a" pp_vars bv pp_stmt s;
  let free_vars_expr globals bv e =
    fst @@ free_vars globals bv {s with skind = Universal.Ast.S_expression e} in
  let (fv, _, _), s = Visitor.fold_map_stmt
                     (fun (fv, globs, bv) expr -> match ekind expr with
                          | E_py_list_comprehension (v, comprs) ->
                             let fv', bv' = List.fold_left (fun (fv, bv) (target, iterator, conds) ->
                                              let target = Visitor.expr_vars target in
                                              let bv = target @ bv in
                                              let fv' = free_vars_expr globals bv iterator in
                                              fv @ fv' @ (List.fold_left (fun fv c -> free_vars_expr globals bv c @ fv) [] conds), bv
                                            ) (fv, bv) comprs in
                             let fv'' = free_vars_expr globals bv' v in
                             debug "list compr, fv = %a, bv' = %a" pp_vars (fv' @ fv'') pp_vars bv';
                             Keep ((fv @ fv' @ fv'', globs, bv), expr)
                          | E_var(v, _) ->
                             if not @@ List.mem v bv && not @@ List.mem v globals && not @@ List.mem v globs then
                               VisitParts ((v::fv, globs, bv), expr)
                             else
                               VisitParts ((fv, globs, bv), expr)
                          | _ -> VisitParts ((fv, globs, bv), expr))
    (fun (fv, globs, bv) stmt -> match skind stmt with
                    | S_py_function i ->
                       let bound_vars = i.py_func_parameters @ [i.py_func_var] in
                       let fv, i_body = free_vars (globals@globs) bound_vars i.py_func_body in
                       let fv = List.filter (fun x -> not @@ List.mem x (i.py_func_locals @ bound_vars)) fv in
                       debug "%a: fv_inner %a, bound_vars %a" pp_var i.py_func_var (Format.pp_print_list pp_var) fv (Format.pp_print_list pp_var) bound_vars;
                       Keep ((fv, globs, bv), {stmt with skind = S_py_function {i with py_func_body = i_body; py_func_freevars = fv}})
                    | S_py_class c ->
                       VisitParts ((fv, c.py_cls_var :: (List.flatten @@ List.map Visitor.expr_vars c.py_cls_bases) @ globs, bv), stmt)
                    | _ -> VisitParts ((fv, globs, bv), stmt)) ([], [], bv) s in
  fv, s

and cell_vars s =
  Visitor.map_stmt
    (fun expr -> VisitParts expr)
    (fun stmt -> match skind stmt with
                 | S_py_function i ->
                    let cv = Visitor.fold_stmt
                                      (fun fv expr -> VisitParts fv)
                                      (fun fv stmt' -> match skind stmt' with
                                                       | S_py_function f -> Keep (f.py_func_freevars @ fv)
                                                       | _ -> VisitParts fv) [] i.py_func_body in
                    let cdefs = Visitor.fold_stmt
                                  (fun fv expr -> VisitParts fv)
                                  (fun fv stmt' -> match skind stmt' with
                                                   | S_py_class c -> Keep (c.py_cls_var :: fv)
                                                   | _ -> VisitParts fv) [] i.py_func_body in
                    let body = cell_vars i.py_func_body in
                    Keep {stmt with skind = S_py_function {i with py_func_cellvars = List.sort_uniq compare_var (List.filter (fun x -> not @@ List.mem x (i.py_func_freevars @ cdefs) && List.mem x (i.py_func_parameters @ i.py_func_locals)) cv) ; py_func_body = body}}
                 | _ -> VisitParts stmt) s

(* Front-end registration *)
let () =
  register_frontend {
    lang = "python";
    parse = parse_program;
  }
