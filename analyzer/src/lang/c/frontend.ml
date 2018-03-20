open C_AST
open Framework.Ast
open Universal.Ast
open Ast

let debug fmt =
  Debug.debug ~channel:"c.frontend" fmt

(* let translate_var v =
 *   let open Framework.Ast in
 *   let open Universal.Ast in
 *   E_var
 *     {unname = (v.var_unique_name) ^ (string_of_int v.var_uid) ;
 *      orgname = v.var_org_name}
 *
 * let from_var v =
 *   let open Framework.Ast in
 *   let open Universal.Ast in
 *   {unname = (v.var_unique_name) ^ (string_of_int v.var_uid) ;
 *    orgname = v.var_org_name} *)

let rec parse_program (file: string) : Framework.Ast.program =
  let target_options = Clang_parser.get_default_target_options () in
  let target_info = Clang_parser.get_target_info target_options in
  let x, diag = Clang_parser.parse (target_options) file [||] in
  debug "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
       (Format.pp_print_string)
    ) (List.map (Clang_dump.string_of_diagnostic) diag)
  ;
  let ctx = Clang_to_C.create_context file target_info in
  Clang_to_C.add_translation_unit ctx file x;
  let prj = Clang_to_C.link_project ctx in
  from_project prj

and from_project (prj: C_AST.project) : Framework.Ast.program =
  let globals = StringMap.bindings prj.proj_vars |>
                List.map snd |>
                List.map from_var_with_init
  in
  let funcs = StringMap.bindings prj.proj_funcs |>
              List.map snd |>
              List.map from_function
  in
  Ast.C_program {
    c_program_global_variables = globals;
    c_program_functions = funcs;
  }

and from_var_with_init (v: C_AST.variable) : Universal.Ast.var * Ast.c_init option =
  from_var v, from_init_option v.var_init

and from_var (v: C_AST.variable) : Universal.Ast.var =
  from_var_name v.var_org_name v.var_unique_name v.var_uid (from_typ v.var_type)

and from_var_name (org_name: string) (unique_name: string) (uid: int) (typ: Framework.Ast.typ) : Universal.Ast.var =
  {
    unname = unique_name ^ (string_of_int uid) ;
    orgname = org_name;
    vtyp = typ;
  }


and from_typ : C_AST.type_qual -> Framework.Ast.typ = function
  | _ -> assert false

and from_init_option : C_AST.init option -> Ast.c_init option = function
  | None -> None
  | Some i -> Some (from_init i)

and from_init : C_AST.init -> Ast.c_init = function
  | I_init_expr e -> C_init_expr (from_expr e)
  | I_init_list(il, i) -> C_init_list (List.map from_init il, from_init_option i)
  | I_init_implicit t -> C_init_implicit (from_typ t)

and from_function : C_AST.func -> Ast.c_fundec =
  fun func ->
    let typ = TC_function (Some {
          c_ftype_return = from_typ func.func_return;
          c_ftype_params = Array.to_list func.func_parameters |>
                           List.map (fun p -> from_typ p.var_type);
          c_ftype_variadic = func.func_variadic;
        })
    in
    {
      c_func_var = from_var_name func.func_org_name func.func_unique_name func.func_uid typ;
      c_func_is_static = func.func_is_static;
      c_func_return = from_typ func.func_return;
      c_func_parameters = Array.to_list func.func_parameters |> List.map from_var ;
      c_func_body = from_block_option (from_range func.func_range) func.func_body;
      c_func_static_vars = List.map from_var_with_init func.func_static_vars;
      c_func_local_vars = List.map from_var_with_init func.func_local_vars;
      c_func_variadic = func.func_variadic;
}


and from_range : Clang_AST.range -> Framework.Ast.range =
  fun range ->
    let open Clang_AST in
    Range_origin {
      range_begin = {
        loc_file = range.range_begin.loc_file;
        loc_line = range.range_begin.loc_line;
        loc_column = range.range_begin.loc_column;
      };
      range_end = {
        loc_file = range.range_begin.loc_file;
        loc_line = range.range_begin.loc_line;
        loc_column = range.range_begin.loc_column;
      }
    }

and from_expr ((ekind, tc , range) : C_AST.expr) : Framework.Ast.expr =
  let erange = from_range range in
  let etyp = from_typ tc in
  let ekind =
    match ekind with
    | _ -> assert false
  in
  {ekind; erange; etyp}

and from_block_option (range: Framework.Ast.range) (block: C_AST.block option) : Framework.Ast.stmt =
  match block with
  | None -> mk_nop range
  | Some stmtl -> mk_block (List.map from_stmt stmtl) range

and from_stmt ((skind, range): C_AST.statement) : Framework.Ast.stmt =
  let srange = from_range range in
  let skind = match skind with
    | _ -> assert false
  in
  {skind; srange}
