open C_AST
open Framework.Ast
open Universal.Ast
open Ast

let debug fmt =
  Debug.debug ~channel:"c.frontend" fmt

(** {2 Entry points} *)

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

  let globals = StringMap.bindings prj.proj_vars |>
                List.map snd |>
                List.map from_var_with_init
  in

  let funcs = StringMap.bindings prj.proj_funcs |>
              List.map snd |>
              List.map from_function
  in
  {
    prog_kind = Ast.C_program {
        c_program_global_variables = globals;
        c_program_functions = funcs;
      };
    prog_file = file;
  }

(** {2 Variables} *)

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

and from_init_option : C_AST.init option -> Ast.c_init option = function
  | None -> None
  | Some i -> Some (from_init i)

and from_init : C_AST.init -> Ast.c_init = function
  | I_init_expr e -> C_init_expr (from_expr e)
  | I_init_list(il, i) -> C_init_list (List.map from_init il, from_init_option i)
  | I_init_implicit t -> C_init_implicit (from_typ t)

(** {2 functions} *)

and from_function : C_AST.func -> Ast.c_fundec =
  fun func ->
    let typ = T_c_function (Some {
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

(** {2 Types} *)

and from_typ (tc: C_AST.type_qual) : Framework.Ast.typ =
  let typ, qual = tc in
  let typ' = match typ with
    | C_AST.T_void -> failwith "C_AST.T_void not supported"
    | C_AST.T_bool -> failwith "C_AST.T_bool not supported"
    | C_AST.T_integer _ -> failwith "C_AST.T_integer not supported"
    | C_AST.T_float _ -> failwith "C_AST.T_float not supported"
    | C_AST.T_pointer _ -> failwith "C_AST.T_pointer not supported"
    | C_AST.T_array (_,_) -> failwith "C_AST.T_array not supported"
    | C_AST.T_bitfield (_,_) -> failwith "C_AST.T_bitfield not supported"
    | C_AST.T_function _ -> failwith "C_AST.T_function not supported"
    | C_AST.T_builtin_fn
    | C_AST.T_typedef _ -> failwith "C_AST.T_typedef not supported"
    | C_AST.T_record _ -> failwith "C_AST.T_record not supported"
    | C_AST.T_enum _ -> failwith "C_AST.T_enum not supported"
  in
  if qual.C_AST.qual_is_const then
    T_c_qualified({c_qual_is_const = true; c_qual_is_restrict = false; c_qual_is_volatile = false}, typ')
  else
    typ'
(** {2 Expressions} *)

and from_expr ((ekind, tc , range) : C_AST.expr) : Framework.Ast.expr =
  let erange = from_range range in
  let etyp = from_typ tc in
  let ekind =
    match ekind with
    | C_AST.E_conditional (_,_,_) -> failwith "E_conditional not supported"
    | C_AST.E_array_subscript (_,_) -> failwith "E_array_subscript not supported"
    | C_AST.E_member_access (_,_,_) -> failwith "E_member_access not supported"
    | C_AST.E_arrow_access (_,_,_) -> failwith "E_arrow_access not supported"
    | C_AST.E_compound_assign (_,_,_,_,_) -> failwith "E_compound_assign not supported"
    | C_AST.E_binary (_,_,_) -> failwith "E_binary not supported"
    | C_AST.E_assign (_,_) -> failwith "E_assign not supported"
    | C_AST.E_comma (_,_) -> failwith "E_comma not supported"
    | C_AST.E_unary (_,_) -> failwith "E_unary not supported"
    | C_AST.E_increment (_,_,_) -> failwith "E_increment not supported"
    | C_AST.E_address_of _ -> failwith "E_address_of not supported"
    | C_AST.E_deref _ -> failwith "E_deref not supported"
    | C_AST.E_cast (_,_) -> failwith "E_cast not supported"
    | C_AST.E_call (_,_) -> failwith "E_call not supported"
    | C_AST.E_character_literal (_,_) -> failwith "E_character_literal not supported"
    | C_AST.E_integer_literal _ -> failwith "E_integer_literal not supported"
    | C_AST.E_float_literal _ -> failwith "E_float_literal not supported"
    | C_AST.E_string_literal (_,_) -> failwith "E_string_literal not supported"
    | C_AST.E_compound_literal _ -> failwith "E_compound_literal not supported"
    | C_AST.E_variable _ -> failwith "E_variable not supported"
    | C_AST.E_function _ -> failwith "E_function not supported"
    | C_AST.E_predefined _ -> failwith "E_predefined not supported"
    | C_AST.E_statement _ -> failwith "E_statement not supported"
    | C_AST.E_var_args _ -> failwith "E_var_args not supported"
    | C_AST.E_atomic (_,_,_) -> failwith "E_atomic not supported"
  in
  {ekind; erange; etyp}


(** {2 Ranges and locations} *)

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


(** {2 Statements} *)

and from_stmt ((skind, range): C_AST.statement) : Framework.Ast.stmt =
  let srange = from_range range in
  let skind = match skind with
    | C_AST.S_local_declaration _ -> failwith "C_AST.S_local_declaration not supprted"
    | C_AST.S_expression _ -> failwith "C_AST.S_expression not supprted"
    | C_AST.S_block _ -> failwith "C_AST.S_block not supprted"
    | C_AST.S_if (_,_,_) -> failwith "C_AST.S_if not supprted"
    | C_AST.S_while (_,_) -> failwith "C_AST.S_while not supprted"
    | C_AST.S_do_while (_,_) -> failwith "C_AST.S_do_while not supprted"
    | C_AST.S_for (_,_,_,_) -> failwith "C_AST.S_for not supprted"
    | C_AST.S_jump _ -> failwith "C_AST.S_jump not supprted"
    | C_AST.S_target _ -> failwith "C_AST.S_target not supprted"
  in
  {skind; srange}

and from_block_option (range: Framework.Ast.range) (block: C_AST.block option) : Framework.Ast.stmt =
  match block with
  | None -> mk_nop range
  | Some stmtl -> mk_block (List.map from_stmt stmtl) range
