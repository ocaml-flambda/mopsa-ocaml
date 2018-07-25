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

(* vars to their unique identifier and declared types *)
module MS = MapExt.StringMap
type var_context = (int * Framework.Ast.typ) MS.t
type fun_context = (T.fundec) MS.t

let from_position (pos: U.position) : Framework.Ast.loc =
  {
    loc_file = pos.pos_fname;
    loc_line = pos.pos_lnum;
    loc_column = pos.pos_cnum;
  }

let from_extent ((b, e): extent) =
  Range_origin {
    range_begin = from_position b;
    range_end = from_position e;
  }

let from_var (v: string) (ext: U.extent) (var_ctx: var_context): FA.var=
  try
    let (id, typ) = MS.find v var_ctx in
    {
      vname = v;
      vuid = id;
      vtyp = typ;
      vkind = FA.V_orig;
    }
  with
  | Not_found ->
    Debug.fail "%s at %s was not found in typing/naming context"
      v
      (U_ast_printer.string_of_extent ext)

let rec from_typ (typ: U_ast.typ) : FA.typ = match typ with
    | AST_INT -> T_int
    | AST_REAL -> T_float
    | AST_ARRAY t -> T_array (from_typ t)
    | AST_STRING -> T_string
    | AST_BOOL -> T_bool
    | AST_CHAR -> T_char

let translate_and_type_binop (b: U.binary_op) (t1: FA.typ) (t2: FA.typ) (ext: extent): FA.operator * FA.typ =
  match b, t1, t2 with
  | AST_PLUS, T_int, T_int -> O_plus T_int, T_int
  | AST_MINUS, T_int, T_int -> O_minus T_int, T_int
  | AST_MULTIPLY, T_int, T_int -> O_mult T_int, T_int
  | AST_DIVIDE, T_int, T_int -> O_div T_int, T_int
  | AST_PLUS, T_float, T_float -> O_plus T_float, T_float
  | AST_MINUS, T_float, T_float -> O_minus T_float, T_float
  | AST_MULTIPLY, T_float, T_float -> O_mult T_float, T_float
  | AST_DIVIDE, T_float, T_float -> O_div T_float, T_float
  | AST_EQUAL, _, _ when compare_typ t1 t2 = 0 -> O_eq, T_bool
  | AST_NOT_EQUAL, _, _ when compare_typ t1 t2 = 0 -> O_ne, T_bool
  | AST_LESS, T_int, T_int -> O_lt, T_bool
  | AST_LESS_EQUAL, T_int, T_int -> O_le, T_bool
  | AST_GREATER, T_int, T_int -> O_gt, T_bool
  | AST_GREATER_EQUAL, T_int, T_int -> O_ge, T_bool
  | AST_LESS, T_float, T_float -> O_lt, T_bool
  | AST_LESS_EQUAL, T_float, T_float -> O_le, T_bool
  | AST_GREATER, T_float, T_float -> O_gt, T_bool
  | AST_GREATER_EQUAL, T_float, T_float -> O_ge, T_bool
  | AST_AND, T_bool, T_bool -> O_log_and, T_bool
  | AST_OR, T_bool, T_bool -> O_log_or, T_bool
  | AST_CONCAT, T_string, T_string -> O_concat, T_string
  | AST_CONCAT, T_array x, T_array y when compare_typ x y = 0 -> O_concat, T_array x
  | _ -> Debug.fail "typing failed on %a(%a, %a) at %s"
           U_ast_printer.print_binary_op b
           Framework.Pp.pp_typ t1
           Framework.Pp.pp_typ t2
           (U_ast_printer.string_of_extent ext)

let translate_and_type_unop (b: U.unary_op) (t1: FA.typ) (ext: extent): FA.operator * FA.typ =
  match b, t1 with
  | AST_UNARY_PLUS, T_int -> O_plus T_int, T_int
  | AST_UNARY_MINUS, T_int -> O_plus T_int, T_int
  | AST_UNARY_PLUS, T_float -> O_plus T_float, T_float
  | AST_UNARY_MINUS, T_float -> O_plus T_float, T_float
  | AST_NOT, T_bool -> O_log_not, T_bool
  | _ -> Debug.fail "typing failed on %a(%a) at %s"
           U_ast_printer.print_unary_op b
           Framework.Pp.pp_typ t1
           (U_ast_printer.string_of_extent ext)

let rec from_expr (e: U.expr) (ext : U.extent) (var_ctx: var_context) (fun_ctx: fun_context option): FA.expr =
  let range = from_extent ext in
  match e with
  | AST_fun_call((f, f_ext), args) ->
    begin
      match fun_ctx with
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
              (mk_expr ~etyp:(fundec.T.fun_return_type) (E_call(mk_expr (E_function fundec) range, el)) range)
            else
              Debug.fail "%s number of arguments incompatible with call at %s"
                f
                (U_ast_printer.string_of_extent ext)
          with
          | Not_found ->
            Debug.fail "%s at %s was not found in typing/naming context"
              f
              (U_ast_printer.string_of_extent ext)
        end
      | None ->
        Debug.fail "%a at %s function call in global variable declaration"
          U_ast_printer.print_expr e
          (U_ast_printer.string_of_extent ext)
    end
  | AST_unary (op, (e, ext)) ->
    begin
      let e = from_expr e ext var_ctx fun_ctx in
      let typ = etyp e in
      let op, typ' = translate_and_type_unop op typ ext in
      mk_unop op e ~etyp:typ' range
    end

  | AST_binary (op, (e1, ext1), (e2, ext2)) ->
    begin
      let e1 = from_expr e1 ext var_ctx fun_ctx in
      let typ1 = etyp e1 in
      let e2 = from_expr e2 ext var_ctx fun_ctx in
      let typ2 = etyp e2 in
      let op, typ' = translate_and_type_binop op typ1 typ2 ext in
      mk_binop e1 op e2 ~etyp:typ' range
    end

  | AST_identifier (v, ext) ->
    mk_var (from_var v ext var_ctx) range

  | AST_int_const (s, _) ->
    mk_constant T_int (C_int (Z.of_string s)) range

  | AST_bool_const (b, _) ->
    mk_bool b range

  | AST_real_const (s, _) ->
    (* TODO: this looks like a very bad idea: *)
    mk_float (float_of_string s) range

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
               (Framework.Pp.pp_typ) (etyp e1)
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
               (Framework.Pp.pp_typ) (etyp e1)
    end

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
        if (compare_typ (etyp e1) (etyp e2) = 0) then
          mk_assign e1 e2 range
        else
          Debug.fail "%a (at %s) has type %a and %a (at %s) has type \
                       %a, could not translate assignement"
            U_ast_printer.print_expr e1o
            (U_ast_printer.string_of_extent ext1)
            Framework.Pp.pp_typ (etyp e1)
            U_ast_printer.print_expr e2o
            (U_ast_printer.string_of_extent ext2)
            Framework.Pp.pp_typ (etyp e2)
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
             ~etyp:(T_bool)
             (tag_range range "comp_for")
          )
          (mk_block (
              [
                s1;
                mk_assign
                  (mk_var v (tag_range range "var_incr_for"))
                  (mk_binop
                     (mk_var v (tag_range range "var_incr_for"))
                     (O_plus T_int)
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
    fun_return_type = from_typ (f.return_type)
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
  U_program
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
