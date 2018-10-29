(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   Universal frontend translates the parser's AST into Framework's AST.
*)

open Framework.Essentials

module NameG =
struct
  let compt = ref 0
  let fresh () =
    let rep = !compt in
    incr compt;
    rep
end

open Lexing
open Ast
open Framework.Ast
open U_ast

module T = Ast
module U = U_ast
module FA = Framework.Ast

module Float = ItvUtils.Float

(* vars to their unique identifier and declared types *)
module MS = MapExt.StringMap
type var_context = (int * Framework.Ast.typ) MS.t
type fun_context = (T.fundec) MS.t

let builtin_functions =
  [
    {name = "subtree"; args = [Some T_tree; Some T_int]; output = T_tree};
    {name = "is_symbol"; args = [Some T_tree]; output = T_bool};
    {name = "read_int"; args = [Some T_tree]; output = T_int};
    {name = "read_symbol"; args = [Some T_tree]; output = T_string};
    {name = "mopsa_assume"; args = [None]; output = T_unit};
  ]

let from_position (pos: U.position) : Framework.Location.loc =
  Framework.Location.{
    loc_file = pos.pos_fname;
    loc_line = pos.pos_lnum;
    loc_column = pos.pos_cnum;
  }

let from_extent ((b, e): extent) : Framework.Location.range =
  Framework.Location.(Range_origin {
      range_begin = from_position b;
      range_end = from_position e;
    })

let from_var (v: string) (ext: U.extent) (var_ctx: var_context): FA.var =
  try
    let (id, typ) = MS.find v var_ctx in
    {
      vname = v;
      vuid = id;
      vtyp = typ;
    }
  with
  | Not_found ->
    Debug.fail "%s at %s was not found in typing/naming context"
      v
      (U_ast_printer.string_of_extent ext)

let rec from_typ (typ: U_ast.typ) : FA.typ =
  match typ with
  | AST_INT     -> T_int
  | AST_REAL    -> T_float F_DOUBLE
  | AST_ARRAY t -> T_array (from_typ t)
  | AST_STRING  -> T_string
  | AST_CHAR    -> T_char
  | AST_TREE    -> T_tree
  | AST_UNIT    -> T_unit

(* find a common type for the arguments of binary operations *)
let unify_typ (x:FA.typ) (y:FA.typ) : FA.typ =
  match x,y with
  | T_int, T_float _ -> y
  | T_float _, T_int -> x
  | _ ->
     if compare_typ x y = 0 then x
     else Debug.fail "cannot unify types %a and %a" pp_typ x pp_typ y

(* cast expression to the given type (if needed) *)
let to_typ (t:FA.typ) (e:FA.expr) : FA.expr =
  let range = erange e in
  let orgt = etyp e in
  if compare_typ orgt t = 0 then e
  else
    match orgt, t with
    | (T_int | T_float _), (T_int | T_float _) ->
       mk_unop O_cast e ~etyp:t range
    | _ ->
       Debug.fail "cannot convert expression %a of type %a to type %a" pp_expr e pp_typ orgt pp_typ t

let from_binop (t: FA.typ) (b: U.binary_op) : FA.operator =
  match t, b with
  | T_int, AST_PLUS          -> O_plus
  | T_int, AST_MINUS         -> O_minus
  | T_int, AST_MULTIPLY      -> O_mult
  | T_int, AST_DIVIDE        -> O_div
  | T_int, AST_EQUAL         -> O_eq
  | T_int, AST_NOT_EQUAL     -> O_ne
  | T_int, AST_LESS          -> O_lt
  | T_int, AST_LESS_EQUAL    -> O_le
  | T_int, AST_GREATER       -> O_gt
  | T_int, AST_GREATER_EQUAL -> O_ge
  | T_int, AST_AND           -> O_log_and
  | T_int, AST_OR            -> O_log_or
  | T_string, AST_CONCAT        -> O_concat
  | T_float _, AST_PLUS          -> O_plus
  | T_float _, AST_MINUS         -> O_minus
  | T_float _, AST_MULTIPLY      -> O_mult
  | T_float _, AST_DIVIDE        -> O_div
  | T_float _, AST_EQUAL         -> O_eq
  | T_float _, AST_NOT_EQUAL     -> O_ne
  | T_float _, AST_LESS          -> O_lt
  | T_float _, AST_LESS_EQUAL    -> O_le
  | T_float _, AST_GREATER       -> O_gt
  | T_float _, AST_GREATER_EQUAL -> O_ge
  | _ -> Debug.fail "operator %a cannot be used with type %a" U_ast_printer.print_binary_op b pp_typ t

let from_unop (t: FA.typ) (b: U.unary_op) : FA.operator =
  match t, b with
  | T_int, AST_UNARY_PLUS    -> O_plus
  | T_int, AST_UNARY_MINUS   -> O_minus
  | T_int, AST_NOT           -> O_log_not
  | T_float f, AST_UNARY_PLUS  -> O_plus
  | T_float f, AST_UNARY_MINUS -> O_minus
  | _ -> Debug.fail "operator %a cannot be used with type %a" U_ast_printer.print_unary_op b pp_typ t

let rec from_expr (e: U.expr) (ext : U.extent) (var_ctx: var_context) (fun_ctx: fun_context option): FA.expr =
  let range = from_extent ext in
  match e with
  | AST_unit_const -> mk_expr ~etyp:T_unit (E_constant (C_unit)) range
  | AST_fun_call((f, f_ext), args) ->
    begin
      let look_in_builtins (fun_ctx) =
        let exception Match of (FA.expr list * fun_builtin) in
        try
          List.iter (fun bi ->
              let () = Debug.debug ~channel:("remove_me") "builtin: %s, fun: %s, b: %b" bi.name f (bi.name = f) in
              if bi.name = f && List.length bi.args = List.length args then
                let exception NoMatch in
                try
                  let el = List.map2 (fun (e, ext) x ->
                      match x with
                      | Some x ->
                        let e' = from_expr e ext var_ctx (fun_ctx) in
                        let typ = etyp e' in
                        let () = Debug.debug ~channel:("remove_me") "x: %a, typ: %a" pp_typ x pp_typ typ in
                        if compare_typ typ x = 0 then
                          e'
                        else
                          raise NoMatch
                      | None -> from_expr e ext var_ctx (fun_ctx)
                    ) args bi.args
                  in
                  raise (Match (el, bi))
                with
                | NoMatch -> ()
            ) builtin_functions;
          Debug.fail "%s at %s was not found in naming context nor in builtin functions"
            f
            (U_ast_printer.string_of_extent ext)
        with
        | Match(el, bi) ->
          (mk_expr ~etyp:(bi.output) (E_call(mk_expr (E_function (Builtin bi)) range, el)) range)
      in
      match fun_ctx with
      | None -> look_in_builtins (None)
      | Some fun_ctx ->
        begin
          try
            let fundec = MS.find f fun_ctx in
            if List.length fundec.fun_parameters = List.length args then
              let el = List.map2 (fun (e, ext) x ->
                  let e' = from_expr e ext var_ctx (Some fun_ctx) in
                  let typ = etyp e' in
                  if compare_typ x.vtyp typ = 0 then
                    e'
                  else
                    Debug.fail "type of %a at %s incompatible with declared function"
                      U_ast_printer.print_expr e
                      (U_ast_printer.string_of_extent ext)
                ) args fundec.fun_parameters in
              (* <<<<<<< HEAD *)
              (* void function return an (unitialized) int *)
              let rettyp = OptionExt.option_dfl T_int fundec.T.fun_return_type in
              (mk_expr ~etyp:rettyp (E_call(mk_expr (E_function (User_defined fundec)) range, el)) range)
              (* ======= *)
              (* (mk_expr ~etyp:(fundec.T.fun_return_type) (E_call(mk_expr (E_function (User_defined fundec)) range, el)) range) *)
            else
              Debug.fail "%s number of arguments incompatible with call at %s"
                f
                (U_ast_printer.string_of_extent ext)
          with
          | Not_found ->
            begin
              look_in_builtins (Some fun_ctx)
            end
        end
    end
  | AST_unary (op, (e, ext)) ->
    begin
      let e = from_expr e ext var_ctx fun_ctx in
      let typ = etyp e in
      let op = from_unop typ op in
      mk_unop op e ~etyp:typ range
    end
  | AST_binary (op, (e1, ext1), (e2, ext2)) ->
    begin
      let e1 = from_expr e1 ext var_ctx fun_ctx in
      let typ1 = etyp e1 in
      let e2 = from_expr e2 ext var_ctx fun_ctx in
      let typ2 = etyp e2 in
      let typ = unify_typ typ1 typ2 in
      let e1,e2 = to_typ typ e1, to_typ typ e2 in
      let op = from_binop typ op in
      mk_binop e1 op e2 ~etyp:typ range
    end

  | AST_identifier (v, ext) ->
    mk_var (from_var v ext var_ctx) range

  | AST_int_const (s, _) ->
    mk_z (Z.of_string s) range

  | AST_bool_const (b, _) ->
    mk_int (if b then 1 else 0) range

  | AST_real_const (s, _) ->
     (* double interval enclosing the real value *)
     let lo = Float.of_string `DOUBLE `DOWN s
     and up = Float.of_string `DOUBLE `UP s
     in
     mk_float_interval ~prec:F_DOUBLE lo up range

  | AST_string_const (s, _) ->
    mk_string s range

  | AST_char_const(c, _) ->
    Debug.fail "char not implemented yet"

  | AST_array_const(a, _) ->
    Debug.fail "array not implemented yet"

  | AST_rand((l, _), (u, _)) ->
    mk_z_interval (Z.of_string l) (Z.of_string u) range

  | AST_array_access((e1, ext1), (e2, ext2)) ->
    begin
      let e1o = e1 in
      let e1 = from_expr e1 ext1 var_ctx fun_ctx in
      let e2 = from_expr e2 ext2 var_ctx fun_ctx in
      let e2 = to_typ T_int e2 in
      match etyp e1 with
      | T_string ->
        {
          ekind = E_subscript(e1, e2);
          etyp  = T_char;
          erange= range
        }
      | T_array t ->
        {
          ekind = E_subscript(e1, e2);
          etyp  = t;
          erange= range
        }
      | _ -> Debug.fail "%a at %s is of type %a and can not be subscripted"
               U_ast_printer.print_expr e1o
               (U_ast_printer.string_of_extent ext)
               (pp_typ) (etyp e1)
    end

  | AST_len (e, ext) ->
    begin
      let e1 = from_expr e ext var_ctx fun_ctx in
      match etyp e1 with
      | T_string
      | T_array _ ->
        {
          ekind = E_len e1;
          etyp  = T_int;
          erange= range
        }
      | _ -> Debug.fail "%a at %s is of type %a and can not be lengthed"
               U_ast_printer.print_expr e
               (U_ast_printer.string_of_extent ext)
               (pp_typ) (etyp e1)
    end
  | AST_tree tc ->
    from_tree_constructor tc ext var_ctx fun_ctx

and from_tree_constructor (tc: U.tree_constructor) (ext: extent) (var_ctx: var_context) (fun_ctx: fun_context option) =
  let range = from_extent ext in
  match tc with
  | Int(e, ext) ->
    {ekind = E_tree (TC_int (from_expr e ext var_ctx fun_ctx));
     etyp  = T_tree;
     erange =range;
    }
  | Symbol((e, ext'), l) ->
    {ekind = E_tree (TC_symbol(from_expr e ext' var_ctx fun_ctx, List.map (fun (e, ext) -> from_expr e ext var_ctx fun_ctx) l));
     etyp  = T_tree;
     erange =range;
    }

let rec from_stmt (s: U.stat) (ext: extent) (var_ctx: var_context) (fun_ctx: fun_context option): FA.stmt =
  let range = from_extent ext in
  match s with
  | AST_block l ->
    mk_block (List.map (fun (x, ext) -> from_stmt x ext var_ctx fun_ctx) l) range

  | AST_assign((e1, ext1), (e2, ext2)) ->
    begin
      let e1o = e1 and e2o = e2 in
      match e1 with
      | AST_array_access(_, _)
      | AST_identifier _ ->
        let e1 = from_expr e1 ext1 var_ctx fun_ctx in
        let e2 = from_expr e2 ext2 var_ctx fun_ctx in
(* <<<<<<< HEAD *)
        let e2 = to_typ (etyp e1) e2 in
        mk_assign e1 e2 range
(* =======
 *         if (compare_typ (etyp e1) (etyp e2) = 0) then
 *           mk_assign e1 e2 range
 *         else
 *           Debug.fail "%a (at %s) has type %a and %a (at %s) has type \
 *                       %a, could not translate assignement"
 *             U_ast_printer.print_expr e1o
 *             (U_ast_printer.string_of_extent ext1)
 *             pp_typ (etyp e1)
 *             U_ast_printer.print_expr e2o
 *             (U_ast_printer.string_of_extent ext2)
 *             pp_typ (etyp e2)
 * >>>>>>> mopsa-v2-universal-w-tree *)
      | _ ->
        Debug.fail "%a at %s not considered a left-value for now "
          U_ast_printer.print_expr e1o
          (U_ast_printer.string_of_extent ext)
    end

  | AST_if((e1, ext_e1), (s1, ext_s1), Some (s2, ext_s2)) ->
    let e1 = from_expr e1 ext_e1 var_ctx fun_ctx in
    let s1 = from_stmt s1 ext_s1 var_ctx fun_ctx in
    let s2 = from_stmt s2 ext_s2 var_ctx fun_ctx in
    mk_if e1 s1 s2 range

  | AST_if((e1, ext_e1), (s1, ext_s1), None) ->
    let e1 = from_expr e1 ext_e1 var_ctx fun_ctx in
    let s1 = from_stmt s1 ext_s1 var_ctx fun_ctx in
    mk_if e1 s1 (mk_nop range) range

  | AST_while((e1, ext_e1), (s1, ext_s1)) ->
    let e1 = from_expr e1 ext_e1 var_ctx fun_ctx in
    let s1 = from_stmt s1 ext_s1 var_ctx fun_ctx in
    mk_while e1 s1 range

  | AST_for((v1, ext_v1), (e1, ext_e1), (e2, ext_e2), (s1, ext_s1)) ->
    let e1 = from_expr e1 ext_e1 var_ctx fun_ctx in
    let e2 = from_expr e2 ext_e2 var_ctx fun_ctx in
    let v  = from_var v1 ext_v1 var_ctx in
    let s1 = from_stmt s1 ext_s1 var_ctx fun_ctx in
    mk_block
      [
        mk_assign
          (mk_var v (tag_range range "var_init_for_variable"))
          e1
          (tag_range range "expr_init_for_variable");
        mk_while
          (mk_binop
             (mk_var v (tag_range range "var_comp_for"))
             O_le
             e2
             ~etyp:(T_int)
             (tag_range range "comp_for")
          )
          (mk_block (
              [
                s1;
                mk_assign
                  (mk_var v (tag_range range "var_incr_for"))
                  (mk_binop
                     (mk_var v (tag_range range "var_incr_for"))
                     O_plus
                     (mk_z Z.one (tag_range range "one_for"))
                     ~etyp:(T_int)
                     (tag_range range "incr_for")
                  )
                  (tag_range range "assign_for")
              ])
              (tag_range range "body_for")
          )
          (tag_range range "total_for")
      ]
      range

  | AST_return (e, ext) ->
    let e = from_expr e ext var_ctx fun_ctx in
    {skind = S_return (Some e);
     srange = range
    }

  | AST_assert (e, ext) ->
    let e = from_expr e ext var_ctx fun_ctx in
    mk_assert e range

  | AST_print ->
    mk_stmt S_print range

  | AST_expr(e, ext) ->
     let e' = from_expr e ext var_ctx fun_ctx in
     mk_expr_stmt e' range


let rec check_declaration_list (dl : U_ast.declaration ext list) =
  match dl with
  | p::q -> aux p q; check_declaration_list q
  | [] -> ()
and aux (((((_,v),e),_),_) as p : U_ast.declaration ext) (dl: U_ast.declaration ext list) =
  match dl with
  | ((((_,v'),e'),_),_)::q when v = v' -> Debug.fail "%s at %s has already been declared at %s"
                                            v'
                                            (U_ast_printer.string_of_extent e)
                                            (U_ast_printer.string_of_extent e')
  | p':: q -> aux p q
  | [] -> ()

let var_ctx_of_declaration (dl : U_ast.declaration ext list) (var_ctx: var_context) =
  let () = check_declaration_list dl in
  let add_var var_ctx v t =
    try
      MS.add v (NameG.fresh (), t) var_ctx
    with
    | Not_found ->
      MS.add v (NameG.fresh (), t) var_ctx
  in
  let var_ctx, gvars = List.fold_left (fun (var_ctx, gvars) ((((t, v), extv ), o), e) ->
      let new_var_ctx = add_var var_ctx v (from_typ t) in
      let vv = from_var v extv new_var_ctx in
      (new_var_ctx, vv :: gvars)
    ) (var_ctx, []) dl in
  var_ctx, gvars

let var_ctx_init_of_declaration (dl : U_ast.declaration ext list) (var_ctx: var_context) (fun_ctx: fun_context option) (nvar_ctx)=
  let add_var var_ctx v t =
    try
      match nvar_ctx with
      | Some nvar_ctx -> MS.add v (MS.find v nvar_ctx) var_ctx
      | None -> MS.add v (NameG.fresh (), t) var_ctx
    with
    | Not_found ->
      assert false
  in
  let var_ctx, init, gvars = List.fold_left (fun (var_ctx, init, gvars) ((((t, v), extv ), o), e) ->
      let new_var_ctx = add_var var_ctx v (from_typ t) in
      let vv = from_var v extv new_var_ctx in
      match o with
      | Some (e, ext) ->
        let e = from_expr e ext var_ctx fun_ctx in
        let range = from_extent ext in
        let stmt_init =
          mk_assign
            (mk_var vv (tag_range range "initializer_var"))
            e
            (tag_range range "initializer")
        in
        (new_var_ctx, stmt_init :: init, vv :: gvars)
      | None   ->
        (new_var_ctx, init, vv :: gvars)
    ) (var_ctx, [], []) dl in
  var_ctx, List.rev init, gvars

let var_ctx_of_function (var_ctx: var_context) (fundec: U.fundec) =
  let add_var var_ctx v t =
    try
      MS.add v (NameG.fresh (), t) var_ctx
    with
    | Not_found ->
      MS.add v (NameG.fresh (), t) var_ctx
  in
  let var_ctx = List.fold_left (fun acc ((t, v), _) ->
      add_var acc v (from_typ t)
    ) var_ctx fundec.parameters in
  let var_ctx, _ =
    var_ctx_of_declaration fundec.locvars var_ctx
  in
  var_ctx

let var_init_of_function (var_ctx: var_context) var_ctx_map (fun_ctx: fun_context) (fundec: U.fundec) =
  let nvar_ctx = MS.find fundec.funname var_ctx_map in
  let add_var var_ctx n_var_ctx v t =
    try
      MS.add v (MS.find v n_var_ctx) var_ctx
    with
    | Not_found ->
      assert false
  in
  let var_ctx = List.fold_left (fun acc ((t, v), _) ->
      add_var acc nvar_ctx v (from_typ t)
    ) var_ctx fundec.parameters in
  let var_ctx, init, _ =
    var_ctx_init_of_declaration fundec.locvars var_ctx (Some (fun_ctx)) (Some nvar_ctx)
  in
  var_ctx, init


let from_fundec (f: U.fundec) (var_ctx: var_context): T.fundec =
  {
    fun_name = f.funname;
    fun_parameters = List.map (fun ((_, v), ext) -> from_var v ext var_ctx) f.parameters;
    fun_locvars = List.map (fun ((((_, v), _), _), ext) -> from_var v ext var_ctx) f.locvars;
    fun_body = mk_nop (from_extent (snd f.body));
    fun_return_type = (OptionExt.option_lift1 from_typ) (f.return_type)
  }

let fun_ctx_of_global (fl: U_ast.fundec ext list) (var_ctx: var_context) =
  List.fold_left (fun (acc, var_ctx_map) (fundec, _) ->
      let var_ctx = var_ctx_of_function var_ctx fundec in
      (MS.add fundec.funname (from_fundec fundec var_ctx) acc, MS.add fundec.funname var_ctx var_ctx_map)
    ) (MS.empty, MS.empty) fl

let add_body (fl: fun_context) (f: string) (b: stmt): unit =
  try
    let fundec = MS.find f fl in
    fundec.fun_body <- b;
  with
  | Not_found -> Debug.fail "[Universal.frontend] should not happen"

let from_prog (p: U_ast.prog) : FA.program_kind =
  let ext = snd (p.main) in
  let var_ctx, init, gvars = var_ctx_init_of_declaration p.gvars MS.empty None None in
  let fun_ctx, var_ctx_map = fun_ctx_of_global p.funs var_ctx in
  List.iter (fun (fundec, ext) ->
      let var_ctx, init = var_init_of_function var_ctx var_ctx_map fun_ctx fundec in
      let body = from_stmt (fst fundec.body) (snd fundec.body) var_ctx (Some fun_ctx) in
      let total = mk_block (init @ [body]) (from_extent ext) in
      add_body fun_ctx fundec.funname total
    ) p.funs;
  let total = from_stmt (fst p.main) (snd p.main) var_ctx (Some fun_ctx) in
  let with_init = mk_block (init @ [total]) (from_extent ext) in
  P_universal
    {
      universal_gvars   = gvars;
      universal_fundecs = (MS.bindings fun_ctx) |> List.map (snd);
      universal_main    = with_init
    }

let rec parse_program (files: string list): Framework.Ast.program =
  match files with
  | [filename] ->
    let ast = U_file_parser.parse_file filename in
    {prog_kind = from_prog ast; prog_file = filename}
  | _ ->
    Debug.fail "only one file supported for universal"
