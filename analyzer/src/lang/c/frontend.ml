open C_AST
open Framework.Ast
open Universal.Ast
open Ast

let debug fmt =
  Debug.debug ~channel:"c.frontend" fmt


(** {2 Entry points} *)

let rec parse_program (files: string list) =
  match files with
  | [filename] when Filename.extension filename = ".db" -> parse_db filename
  | _ -> parse_files files

and parse_db (dbfile: string) : Framework.Ast.program =
  let open Clang_parser in
  let open Clang_to_C in
  let open Build_DB in
  let target = get_target_info (get_default_target_options ()) in
  let db = load_db dbfile in
  let execs = get_executables db in
  match execs with
  | [exec] ->
    let ctx = create_context exec target in
    let srcs = get_executable_sources db exec in
    let nb = List.length srcs
    and i = ref 0 in
    List.iter
      (fun src ->
         incr i;
         debug "%i/%i\n" !i nb;
         match src.source_kind with
         | SOURCE_C | SOURCE_CXX ->
           let cwd = Sys.getcwd() in
           Sys.chdir src.source_cwd;
           (try
              (* parse file in the original compilation directory *)
              let p = parse_file src.source_opts src.source_path in
              Sys.chdir cwd;
              (* translate to SAST *)
              add_translation_unit ctx (Filename.basename src.source_path) p
            with x ->
              (* make sure we get back to cwd in all cases *)
              Sys.chdir cwd;
              raise x
           )
         | _ -> Debug.warn "ignoring file %s\n%!" src.source_path
      ) srcs;
    from_project (link_project ctx)

  | l ->
    assert false

and parse_files files =
  let open Clang_parser in
  let open Clang_to_C in
  let one_file = List.hd files in
  let target = get_target_info (get_default_target_options ()) in
  let ctx = Clang_to_C.create_context one_file target in
  List.iter
    (fun file ->
       let p = parse_file [] file in
       add_translation_unit ctx (Filename.basename file) p
    ) files;
  let prj = Clang_to_C.link_project ctx in
  from_project prj

and parse_file (opts: string list) (file: string) =
  let target_options = Clang_parser.get_default_target_options () in
  (* remove some options that are in the way *)
  let filter_out_opts opts =
    List.filter (fun o -> not (List.mem o ["-MF"])) opts
  in
  let opts =
    List.map (fun stub -> "-I" ^ stub) Framework.Options.(common_options.stubs) |>
    (@) opts |>
    filter_out_opts
  in
  let x, diag = Clang_parser.parse (target_options) file (Array.of_list opts) in
  let () =
    match diag with
    | [] -> ()
    | _ ->
      let error_diag = List.exists (function Clang_AST.({diag_level = Level_Error | Level_Fatal}) -> true | _ -> false) diag in
      if error_diag then
        Framework.Exceptions.panic "Fatal parsing errors:@\n @[%a@]"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
             (Format.pp_print_string)
          ) (List.map (Clang_dump.string_of_diagnostic) diag)

  in
  x

and from_project prj =
  debug "%a" (fun fmt prj -> C_print.print_project stdout prj) prj;

  let globals = StringMap.bindings prj.proj_vars |>
                List.map snd |>
                List.map from_var_with_init
  in

  let funcs = StringMap.bindings prj.proj_funcs |>
              List.map snd |>
              List.map from_function
  in
  {
    prog_kind = Ast.C_program (globals, funcs);
    prog_file = prj.C_AST.proj_name;
  }

(** {2 Variables} *)

and from_var_with_init (v: C_AST.variable) : var * Ast.c_init option =
  from_var v, from_init_option v.var_init

and from_var (v: C_AST.variable) : var =
  from_var_name v.var_org_name v.var_uid (from_typ v.var_type)

and from_var_name (org_name: string) (uid: int) (typ: Framework.Ast.typ) : var =
  {
    vname = org_name;
    vuid = uid;
    vtyp = typ;
    vkind = V_orig;
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
      c_func_var = from_var_name func.func_org_name func.func_uid typ;
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
    | C_AST.T_void -> Ast.T_c_void
    | C_AST.T_bool -> Universal.Ast.T_bool
    | C_AST.T_integer t -> Ast.T_c_integer (from_integer_type t)
    | C_AST.T_float t -> Ast.T_c_float (from_float_type t)
    | C_AST.T_pointer t -> Ast.T_c_pointer (from_typ t)
    | C_AST.T_array (t,l) -> Ast.T_c_array (from_typ t, from_array_length l)
    | C_AST.T_function None -> Ast.T_c_function None
    | C_AST.T_function (Some t) -> Ast.T_c_function (Some (from_function_type t))
    | C_AST.T_builtin_fn -> Ast.T_c_builtin_fn
    | C_AST.T_typedef t -> Ast.T_c_typedef {
        c_typedef_org_name = t.typedef_org_name;
        c_typedef_unique_name = t.typedef_unique_name;
        c_typedef_def =  from_typ t.typedef_def;
        c_typedef_range = from_range t.typedef_range;
      }
    | C_AST.T_record r -> Ast.T_c_record {
        c_record_kind = (match r.record_kind with C_AST.STRUCT -> C_struct | C_AST.UNION -> C_union);
        c_record_org_name = r.record_org_name;
        c_record_unique_name = r.record_unique_name;
        c_record_defined = r.record_defined;
        c_record_sizeof = r.record_sizeof;
        c_record_alignof = r.record_alignof;
        c_record_fields =
          List.map (fun f -> {
                c_field_org_name = f.field_org_name;
                c_field_name = f.field_name;
                c_field_offset = f.field_offset;
                c_field_bit_offset = f.field_bit_offset;
                c_field_type = from_typ f.field_type;
                c_field_range = from_range f.field_range;
                c_field_index = f.field_index;
              })
            (Array.to_list r.record_fields);
        c_record_range = from_range r.record_range;
      }

    | C_AST.T_bitfield (_,_) -> failwith "C_AST.T_bitfield not supported"
    | C_AST.T_enum _ -> failwith "C_AST.T_enum not supported"
  in
  if qual.C_AST.qual_is_const then
    T_c_qualified({c_qual_is_const = true; c_qual_is_restrict = false; c_qual_is_volatile = false}, typ')
  else
    typ'

and from_integer_type : C_AST.integer_type -> Ast.c_integer_type = function
  | C_AST.Char SIGNED -> Ast.C_signed_char
  | C_AST.Char UNSIGNED -> Ast.C_unsigned_char
  | C_AST.SIGNED_CHAR -> Ast.C_signed_char
  | C_AST.UNSIGNED_CHAR -> Ast.C_unsigned_char
  | C_AST.SIGNED_SHORT -> Ast.C_signed_short
  | C_AST.UNSIGNED_SHORT -> Ast.C_unsigned_short
  | C_AST.SIGNED_INT -> Ast.C_signed_int
  | C_AST.UNSIGNED_INT -> Ast.C_unsigned_int
  | C_AST.SIGNED_LONG -> Ast.C_signed_long
  | C_AST.UNSIGNED_LONG -> Ast.C_unsigned_long
  | C_AST.SIGNED_LONG_LONG -> Ast.C_signed_long_long
  | C_AST.UNSIGNED_LONG_LONG -> Ast.C_unsigned_long_long
  | C_AST.SIGNED_INT128 -> Ast.C_signed_int128
  | C_AST.UNSIGNED_INT128 -> Ast.C_unsigned_int128

and from_float_type : C_AST.float_type -> Ast.c_float_type = function
  | C_AST.FLOAT -> Ast.C_float
  | C_AST.DOUBLE -> Ast.C_double
  | C_AST.LONG_DOUBLE -> Ast.C_long_double

and from_array_length : C_AST.array_length -> Ast.c_array_length = function
  | C_AST.No_length -> Ast.C_array_no_length
  | C_AST.Length_cst n -> Ast.C_array_length_cst n
  | C_AST.Length_expr e -> Ast.C_array_length_expr (from_expr e)

and from_function_type : C_AST.function_type -> Ast.c_function_type = fun f ->
  {
    c_ftype_return = from_typ f.ftype_return;
    c_ftype_params = List.map from_typ f.ftype_params;
    c_ftype_variadic = f.ftype_variadic;
  }

(** {2 Expressions} *)

and from_expr ((ekind, tc , range) : C_AST.expr) : Framework.Ast.expr =
  let erange = from_range range in
  let etyp = from_typ tc in
  let ekind =
    match ekind with
    | C_AST.E_integer_literal n -> Universal.Ast.(E_constant (C_int n))
    | C_AST.E_float_literal f -> Universal.Ast.(E_constant (C_float (float_of_string f)))
    | C_AST.E_character_literal (c, k)  -> E_constant(Ast.C_c_character (c, from_character_kind k))
    | C_AST.E_string_literal (s, k) -> Universal.Ast.(E_constant (C_c_string (s, from_character_kind k)))
    | C_AST.E_variable v -> E_var (from_var v)
    | C_AST.E_function f -> Ast.E_c_function (from_function f)
    | C_AST.E_call (f, args) -> Ast.E_c_call(from_expr f, Array.map from_expr args |> Array.to_list)
    | C_AST.E_unary (op, e) -> E_unop (from_unary_operator op, from_expr e)
    | C_AST.E_binary (op, e1, e2) -> E_binop (from_binary_operator op, from_expr e1, from_expr e2)
    | C_AST.E_cast (e,C_AST.EXPLICIT) -> Ast.E_c_cast(from_expr e, true)
    | C_AST.E_cast (e,C_AST.IMPLICIT) -> Ast.E_c_cast(from_expr e, false)
    | C_AST.E_assign (lval, rval) -> Ast.E_c_assign(from_expr lval, from_expr rval)
    | C_AST.E_address_of(e) -> Ast.E_c_address_of(from_expr e)
    | C_AST.E_deref(p) -> Ast.E_c_deref(from_expr p)
    | C_AST.E_array_subscript (a, i) -> Ast.E_c_array_subscript(from_expr a, from_expr i)
    | C_AST.E_member_access (r, i, f) -> Ast.E_c_member_access(from_expr r, i, f)
    | C_AST.E_arrow_access (r, i, f) -> Ast.E_c_arrow_access(from_expr r, i, f)

    | C_AST.E_conditional (_,_,_) -> Framework.Exceptions.panic "E_conditional not supported"
    | C_AST.E_compound_assign (_,_,_,_,_) -> Framework.Exceptions.panic "E_compound_assign not supported"
    | C_AST.E_comma (_,_) -> Framework.Exceptions.panic "E_comma not supported"
    | C_AST.E_increment (_,_,_) -> Framework.Exceptions.panic "E_increment not supported"
    | C_AST.E_compound_literal _ -> Framework.Exceptions.panic "E_compound_literal not supported"
    | C_AST.E_predefined _ -> Framework.Exceptions.panic "E_predefined not supported"
    | C_AST.E_statement _ -> Framework.Exceptions.panic "E_statement not supported"
    | C_AST.E_var_args _ -> Framework.Exceptions.panic "E_var_args not supported"
    | C_AST.E_atomic (_,_,_) -> Framework.Exceptions.panic "E_atomic not supported"
  in
  {ekind; erange; etyp}

and from_expr_option : C_AST.expr option -> Framework.Ast.expr option = function
  | None -> None
  | Some e -> Some (from_expr e)

and from_unary_operator : C_AST.unary_operator -> Framework.Ast.operator = function
  | C_AST.NEG -> Universal.Ast.O_minus
  | C_AST.BIT_NOT -> Universal.Ast.O_bit_invert
  | C_AST.LOGICAL_NOT -> Universal.Ast.O_log_not

and from_binary_operator : C_AST.binary_operator -> Framework.Ast.operator = function
  | C_AST.O_arithmetic (C_AST.ADD) -> Universal.Ast.O_plus
  | C_AST.O_arithmetic (C_AST.SUB) -> Universal.Ast.O_minus
  | C_AST.O_arithmetic (C_AST.MUL) -> Universal.Ast.O_mult
  | C_AST.O_arithmetic (C_AST.DIV) -> Universal.Ast.O_div
  | C_AST.O_arithmetic (C_AST.MOD) -> Universal.Ast.O_mod
  | C_AST.O_arithmetic (C_AST.LEFT_SHIFT) -> Universal.Ast.O_bit_lshift
  | C_AST.O_arithmetic (C_AST.RIGHT_SHIFT) -> Universal.Ast.O_bit_rshift
  | C_AST.O_arithmetic (C_AST.BIT_AND) -> Universal.Ast.O_bit_and
  | C_AST.O_arithmetic (C_AST.BIT_OR) -> Universal.Ast.O_bit_or
  | C_AST.O_arithmetic (C_AST.BIT_XOR) -> Universal.Ast.O_bit_xor
  | C_AST.O_logical (C_AST.LESS) -> Universal.Ast.O_lt
  | C_AST.O_logical (C_AST.LESS_EQUAL) -> Universal.Ast.O_le
  | C_AST.O_logical (C_AST.GREATER) -> Universal.Ast.O_gt
  | C_AST.O_logical (C_AST.GREATER_EQUAL) -> Universal.Ast.O_ge
  | C_AST.O_logical (C_AST.EQUAL) -> Universal.Ast.O_eq
  | C_AST.O_logical (C_AST.NOT_EQUAL) -> Universal.Ast.O_ne
  | C_AST.O_logical (C_AST.LOGICAL_AND) -> Universal.Ast.O_log_and
  | C_AST.O_logical (C_AST.LOGICAL_OR) -> Universal.Ast.O_log_or

and from_character_kind : C_AST.character_kind -> Ast.c_character_kind = function
  | Clang_AST.Char_Ascii -> Ast.C_char_ascii
  | Clang_AST.Char_Wide -> Ast.C_char_wide
  | Clang_AST.Char_UTF8 -> Ast.C_char_utf8
  | Clang_AST.Char_UTF16 -> Ast.C_char_utf16
  | Clang_AST.Char_UTF32 -> Ast.C_char_utf8

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
    | C_AST.S_local_declaration v ->
      let v, init = from_var_with_init v in
      Ast.S_c_local_declaration (v, init)
    | C_AST.S_expression e -> Universal.Ast.S_expression (from_expr e)
    | C_AST.S_block block -> from_block srange block |> Framework.Ast.skind
    | C_AST.S_if (cond, body, orelse) -> Universal.Ast.S_if (from_expr cond, from_block srange body, from_block srange orelse)
    | C_AST.S_while (cond, body) -> Universal.Ast.S_while (from_expr cond, from_block srange body)
    | C_AST.S_for (init, test, increm, body) -> Ast.S_c_for(from_block srange init, from_expr_option test, from_expr_option increm, from_block srange body)
    | C_AST.S_jump (C_AST.S_goto label) -> S_c_goto label
    | C_AST.S_jump (C_AST.S_break) -> Universal.Ast.S_break
    | C_AST.S_jump (C_AST.S_continue) -> Universal.Ast.S_continue
    | C_AST.S_jump (C_AST.S_return None) -> Universal.Ast.S_return None
    | C_AST.S_jump (C_AST.S_return (Some e)) -> Universal.Ast.S_return (Some (from_expr e))
    | C_AST.S_jump (C_AST.S_switch (cond, body)) -> Ast.S_c_switch (from_expr cond, from_block srange body)
    | C_AST.S_target(C_AST.S_case(e)) -> Ast.S_c_switch_case(from_expr e)
    | C_AST.S_target(C_AST.S_default) -> Ast.S_c_switch_default
    | C_AST.S_target(C_AST.S_label l) -> Ast.S_c_label l
    | C_AST.S_do_while (_,_) -> Framework.Exceptions.panic "C_AST.S_do_while not supprted"
  in
  {skind; srange}

and from_block range (block: C_AST.block) : Framework.Ast.stmt =
  mk_block (List.map from_stmt block) range

and from_block_option (range: Framework.Ast.range) (block: C_AST.block option) : Framework.Ast.stmt =
  match block with
  | None -> mk_nop range
  | Some stmtl -> from_block range stmtl


and construct_string_table globals funcs =
  (* Collect all string litterals and replace them by unique variables *)
  let module StringTable = Map.Make(struct type t = string let compare = compare end) in
  let counter = ref 0 in
  let type_of_string s = T_c_array(T_c_integer(C_signed_char), C_array_length_cst (Z.of_int (1 + String.length s))) in

  let rec visit_expr table e =
    match ekind e with
    | E_constant(C_string s) ->
      let v, table =
        try StringTable.find s table, table
        with Not_found ->
          let v = {
            vname = "_string_" ^ (string_of_int !counter);
            vuid = 0;
            vtyp = type_of_string s;
            vkind = V_orig;
          }
          in
          incr counter;
          v, StringTable.add s v table
      in
      table, {ekind = E_var v; etyp = type_of_string s; erange = e.erange}
    | _ -> table, e


  and visit_init table init =
    match init with
    | C_init_expr e ->
      let table, e = visit_expr table e in
      table, C_init_expr e
    | C_init_list (l, filler) ->
      let table, l = List.fold_left (fun (table, l) init ->
          let table, init = visit_init table init in
          table, init :: l
        ) (table, []) l
      in
      let table, filler = visit_init_option table filler in
      table, C_init_list(List.rev l, filler)
    | C_init_implicit _ -> table, init


  and visit_init_option table init =
    match init with
    | None -> table, None
    | Some init ->
      let table, init = visit_init table init in
      table, Some init

  in
  let table, funcs =
    List.fold_left (fun (table, funcs) f ->
        (* Visit and change the body *)
        let table, body' =
          Framework.Visitor.fold_map_stmt
            (fun acc e -> visit_expr acc e)
            (fun acc s -> acc, s)
            table f.c_func_body
        in
        (* Visit and change the locals *)
        let table, locals' = List.fold_left (fun (table, locals) (v, init) ->
            let table, init' = visit_init_option table init in
            table, (v, init') :: locals
          ) (table, []) f.c_func_local_vars
        in
        debug "fun: %a@\nbody = @[%a@]@\nbody' = @[%a@]" Framework.Pp.pp_var f.c_func_var Framework.Pp.pp_stmt f.c_func_body Framework.Pp.pp_stmt body';
        table, {f with c_func_body = body'; c_func_local_vars = List.rev locals'} :: funcs
      ) (StringTable.empty, []) funcs
  in
  let funcs = List.fold_left (fun acc f ->
      let body' =
        Framework.Visitor.map_stmt
          (fun e ->
             match ekind e with
             | E_c_function f ->
               debug "call to %a" Framework.Pp.pp_var f.c_func_var;
               let f' = List.find (function {c_func_var} -> c_func_var.vname = f.c_func_var.vname) funcs in
               {e with ekind = E_c_function f'}

             | _ -> e
          )
          (fun s -> s)
          f.c_func_body
      in
      debug "fun2: %a@\nbody = @[%a@]@\nbody' = @[%a@]" Framework.Pp.pp_var f.c_func_var Framework.Pp.pp_stmt f.c_func_body Framework.Pp.pp_stmt body';
      {f with c_func_body = body'} :: funcs
    ) ([]) funcs
  in
  (* Add table entries to the global variables *)
  let range = mk_fresh_range () in
  let globals = StringTable.fold (fun s v acc ->
      (v, Some (C_init_expr (mk_constant (C_string s) ~etyp:(type_of_string s) range))) :: acc
    ) table globals
  in

  globals, funcs
