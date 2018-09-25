(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

open C_AST
open Framework.Essentials
open Universal.Ast
open Ast

let debug fmt =
  Debug.debug ~channel:"c.frontend" fmt



(** {2 Command-line options} *)

let c_opts = ref []
(** Extra options to pass to clang when parsing  *)

let () =
  register_option (
      "-I",
      Arg.String (fun l -> c_opts := !c_opts @ [ "-I"; l ]),
      " add the directory to the search path for include files in C analysis"
    );
  register_option (
      "-ccopt",
      Arg.String (fun l -> c_opts := !c_opts @ [l]),
      " pass the option to the Clang frontend"
    )




(** {2 Entry points} *)

type type_space = TS_TYPEDEF | TS_RECORD | TS_ENUM

type ctx = {
    ctx_fun: Ast.c_fundec list;

    ctx_type: (type_space*string,Framework.Ast.typ) Hashtbl.t;
    (* cache the translation of all named types;
       this is required for records defining recursive data-types
     *)
  }


let rec parse_program (files: string list) =
  let open Clang_parser in
  let open Clang_to_C in
  let target = get_target_info (get_default_target_options ()) in
  let ctx = Clang_to_C.create_context "_project" target in
  List.iter
    (fun file ->
       match Filename.extension file with
       | ".c" | ".h" -> parse_file !c_opts file ctx
       | ".db" -> parse_db file ctx
       | x -> Debug.fail "Unknown C extension %s" x
    ) files;
  let prj = Clang_to_C.link_project ctx in
  from_project prj

and parse_db (dbfile: string) ctx : unit =
  let open Clang_parser in
  let open Clang_to_C in
  let open Build_DB in
  let db = load_db dbfile in
  let execs = get_executables db in
  match execs with
  | [exec] ->
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
              parse_file src.source_opts src.source_path ctx;
              Sys.chdir cwd;
            with x ->
              (* make sure we get back to cwd in all cases *)
              Sys.chdir cwd;
              raise x
           )
         | _ -> Debug.warn "ignoring file %s\n%!" src.source_path
      ) srcs

  | l ->
    assert false

and parse_file (opts: string list) (file: string) ctx =
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
  let opts = "-fparse-all-comments"::opts in (* needed to get all comments *)
  debug
    "clang %s %s %a"
    target_options.Clang_AST.target_triple file
    (ListExt.fprint ListExt.printer_plain Format.pp_print_string) opts;
  let x, diag, coms = Clang_parser.parse target_options file (Array.of_list opts) in
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
  Clang_to_C.add_translation_unit ctx (Filename.basename file) x coms


and from_project prj =
  debug "%a" (fun fmt prj -> C_print.print_project stdout prj) prj;

  let funcs_and_origins =
    StringMap.bindings prj.proj_funcs |>
    List.map snd |>
    List.map from_function
  in
  let funcs = List.map fst funcs_and_origins in
  let ctx = {
      ctx_fun = funcs;
      ctx_type = Hashtbl.create 16;
    }
  in
  List.iter (fun (f, o) ->
      let typ = T_c_function (Some {
          c_ftype_return = from_typ ctx o.func_return;
          c_ftype_params = Array.to_list o.func_parameters |>
                           List.map (fun p -> from_typ ctx p.var_type);
          c_ftype_variadic = o.func_variadic;
        })
      in
      f.c_func_var <- from_var_name o.func_org_name o.func_uid typ;
      f.c_func_return <- from_typ ctx o.func_return;
      f.c_func_parameters <- Array.to_list o.func_parameters |> List.map (from_var ctx);
      f.c_func_static_vars <- List.map (from_var_with_init ctx) o.func_static_vars;
      f.c_func_local_vars <- List.map (from_var_with_init ctx) o.func_local_vars;
      f.c_func_body <- from_body_option ctx (from_range o.func_range) o.func_body;
    ) funcs_and_origins;

  let globals = StringMap.bindings prj.proj_vars |>
                List.map snd |>
                List.map (from_var_with_init ctx)
  in


  {
    prog_kind = Ast.C_program (globals, funcs);
    prog_file = prj.C_AST.proj_name;
  }

(** {2 Variables} *)

and from_var_with_init (fun_ctx) (v: C_AST.variable) : var * Ast.c_init option * range =
  from_var fun_ctx v, from_init_option fun_ctx v.var_init, from_range v.var_range

and from_var fun_ctx (v: C_AST.variable) : var =
  from_var_name v.var_org_name v.var_uid (from_typ fun_ctx v.var_type)

and from_var_name (org_name: string) (uid: int) (typ: Framework.Ast.typ) : var =
  {
    vname = org_name;
    vuid = uid;
    vtyp = typ;
  }

and from_init_option fun_ctx init = match init with
  | None -> None
  | Some i -> Some (from_init fun_ctx i)

and from_init fun_ctx init = match init with
  | I_init_expr e -> C_init_expr (from_expr fun_ctx e)
  | I_init_list(il, i) -> C_init_list (List.map (from_init fun_ctx) il, from_init_option fun_ctx i)
  | I_init_implicit t -> C_init_implicit (from_typ fun_ctx t)

(** {2 functions} *)

and from_function =
  fun func ->
    {
      c_func_var = {vname = ""; vuid = 0; vtyp = T_any};
      c_func_is_static = func.func_is_static;
      c_func_return = T_any;
      c_func_parameters = [];
      c_func_body = None ;
      c_func_static_vars = [];
      c_func_local_vars = [];
      c_func_variadic = func.func_variadic;
    }, func

(** {2 Types} *)

and from_typ fun_ctx (tc: C_AST.type_qual) : Framework.Ast.typ =
  let typ, qual = tc in
  let typ' = match typ with
    | C_AST.T_void -> Ast.T_c_void
    | C_AST.T_bool -> Universal.Ast.T_bool
    | C_AST.T_integer t -> Ast.T_c_integer (from_integer_type t)
    | C_AST.T_float t -> Ast.T_c_float (from_float_type t)
    | C_AST.T_pointer t -> Ast.T_c_pointer (from_typ fun_ctx t)
    | C_AST.T_array (t,l) -> Ast.T_c_array (from_typ fun_ctx t, from_array_length fun_ctx l)
    | C_AST.T_function None -> Ast.T_c_function None
    | C_AST.T_function (Some t) -> Ast.T_c_function (Some (from_function_type fun_ctx t))
    | C_AST.T_builtin_fn -> Ast.T_c_builtin_fn
    | C_AST.T_typedef t ->
       if Hashtbl.mem fun_ctx.ctx_type (TS_TYPEDEF,t.typedef_unique_name)
       then Hashtbl.find fun_ctx.ctx_type (TS_TYPEDEF,t.typedef_unique_name)
       else
         let x = {
             c_typedef_org_name = t.typedef_org_name;
             c_typedef_unique_name = t.typedef_unique_name;
             c_typedef_def =  Ast.T_c_void;
             c_typedef_range = from_range t.typedef_range;
           }
         in
         let y = Ast.T_c_typedef x in
         Hashtbl.add fun_ctx.ctx_type (TS_TYPEDEF,t.typedef_unique_name) y;
         x.c_typedef_def <-  from_typ fun_ctx t.typedef_def;
         y
    | C_AST.T_record r ->
       if Hashtbl.mem fun_ctx.ctx_type (TS_RECORD,r.record_unique_name)
       then Hashtbl.find fun_ctx.ctx_type (TS_RECORD,r.record_unique_name)
       else
         let x = {
             c_record_kind =
               (match r.record_kind with C_AST.STRUCT -> C_struct | C_AST.UNION -> C_union);
             c_record_org_name = r.record_org_name;
             c_record_unique_name = r.record_unique_name;
             c_record_defined = r.record_defined;
             c_record_sizeof = r.record_sizeof;
             c_record_alignof = r.record_alignof;
             c_record_fields = [];
             c_record_range = from_range r.record_range;
           }
         in
         let y = Ast.T_c_record x in
         Hashtbl.add fun_ctx.ctx_type (TS_RECORD,r.record_unique_name) y;
         x.c_record_fields <-
           List.map
             (fun f -> {
                  c_field_org_name = f.field_org_name;
                  c_field_name = f.field_name;
                  c_field_offset = f.field_offset;
                  c_field_bit_offset = f.field_bit_offset;
                  c_field_type = from_typ fun_ctx f.field_type;
                  c_field_range = from_range f.field_range;
                  c_field_index = f.field_index;
             })
             (Array.to_list r.record_fields);
         y
    | C_AST.T_enum e ->
       if Hashtbl.mem fun_ctx.ctx_type (TS_ENUM,e.enum_unique_name)
       then Hashtbl.find fun_ctx.ctx_type (TS_ENUM,e.enum_unique_name)
       else
         let x =
           Ast.T_c_enum {
               c_enum_org_name = e.enum_org_name;
               c_enum_unique_name = e.enum_unique_name;
               c_enum_defined = e.enum_defined;
               c_enum_values =
                 List.map
                   (fun v -> {
                        c_enum_val_org_name = v.enum_val_org_name;
                        c_enum_val_unique_name = v.enum_val_unique_name;
                        c_enum_val_value = v.enum_val_value;
                        c_enum_val_range = from_range v.enum_val_range;
                   }) e.enum_values;
               c_enum_integer_type = from_integer_type e.enum_integer_type;
               c_enum_range = from_range e.enum_range;
             }
         in
         Hashtbl.add fun_ctx.ctx_type (TS_ENUM,e.enum_unique_name) x;
         x
    | C_AST.T_bitfield (_,_) -> failwith "C_AST.T_bitfield not supported"
    | C_AST.T_complex _ -> failwith "C_AST.T_complex not supported"
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

and from_array_length fun_ctx al = match al with
  | C_AST.No_length -> Ast.C_array_no_length
  | C_AST.Length_cst n -> Ast.C_array_length_cst n
  | C_AST.Length_expr e -> Ast.C_array_length_expr (from_expr fun_ctx e)

and from_function_type fun_ctx f =
  {
    c_ftype_return = from_typ fun_ctx f.ftype_return;
    c_ftype_params = List.map (from_typ fun_ctx) f.ftype_params;
    c_ftype_variadic = f.ftype_variadic;
  }

and from_function_in_context fun_ctx (f: C_AST.func) =
  try
    List.find (fun c_fun ->
        c_fun.c_func_var.vuid = f.func_uid
      ) fun_ctx.ctx_fun
  with
  | Not_found -> Debug.fail "Could not find function in function context"

(** {2 Expressions} *)

and from_expr fun_ctx ((ekind, tc , range) : C_AST.expr) : Framework.Ast.expr =
  let erange = from_range range in
  let etyp = from_typ fun_ctx tc in
  let ekind =
    match ekind with
    | C_AST.E_integer_literal n -> Universal.Ast.(E_constant (C_int n))
    | C_AST.E_float_literal f -> Universal.Ast.(E_constant (C_float (float_of_string f)))
    | C_AST.E_character_literal (c, k)  -> E_constant(Ast.C_c_character (c, from_character_kind k))
    | C_AST.E_string_literal (s, k) ->
      Universal.Ast.(E_constant (C_c_string (s, from_character_kind k)))
    | C_AST.E_variable v -> E_var (from_var fun_ctx v, STRONG)
    | C_AST.E_function f -> Ast.E_c_function (from_function_in_context fun_ctx f)
    | C_AST.E_call (f, args) -> Ast.E_c_call(from_expr fun_ctx f, Array.map (from_expr fun_ctx) args |> Array.to_list)
    | C_AST.E_unary (op, e) -> E_unop (from_unary_operator op etyp, from_expr fun_ctx e)
    | C_AST.E_binary (op, e1, e2) -> E_binop (from_binary_operator op etyp, from_expr fun_ctx e1, from_expr fun_ctx e2)
    | C_AST.E_cast (e,C_AST.EXPLICIT) -> Ast.E_c_cast(from_expr fun_ctx e, true)
    | C_AST.E_cast (e,C_AST.IMPLICIT) -> Ast.E_c_cast(from_expr fun_ctx e, false)
    | C_AST.E_assign (lval, rval) -> Ast.E_c_assign(from_expr fun_ctx lval, from_expr fun_ctx rval)
    | C_AST.E_address_of(e) -> Ast.E_c_address_of(from_expr fun_ctx e)
    | C_AST.E_deref(p) -> Ast.E_c_deref(from_expr fun_ctx p)
    | C_AST.E_array_subscript (a, i) -> Ast.E_c_array_subscript(from_expr fun_ctx a, from_expr fun_ctx i)
    | C_AST.E_member_access (r, i, f) -> Ast.E_c_member_access(from_expr fun_ctx r, i, f)
    | C_AST.E_arrow_access (r, i, f) -> Ast.E_c_arrow_access(from_expr fun_ctx r, i, f)
    | C_AST.E_statement s -> Ast.E_c_statement (from_block fun_ctx erange s)

    | C_AST.E_conditional (_,_,_) -> Framework.Exceptions.panic "E_conditional not supported"
    | C_AST.E_compound_assign (_,_,_,_,_) -> Framework.Exceptions.panic "E_compound_assign not supported"
    | C_AST.E_comma (_,_) -> Framework.Exceptions.panic "E_comma not supported"
    | C_AST.E_increment (_,_,_) -> Framework.Exceptions.panic "E_increment not supported"
    | C_AST.E_compound_literal _ -> Framework.Exceptions.panic "E_compound_literal not supported"
    | C_AST.E_predefined _ -> Framework.Exceptions.panic "E_predefined not supported"
    | C_AST.E_var_args _ -> Framework.Exceptions.panic "E_var_args not supported"
    | C_AST.E_atomic (_,_,_) -> Framework.Exceptions.panic "E_atomic not supported"
  in
  {ekind; erange; etyp}

and from_expr_option fun_ctx : C_AST.expr option -> Framework.Ast.expr option = function
  | None -> None
  | Some e -> Some (from_expr fun_ctx e)

and from_unary_operator op t = match op with
  | C_AST.NEG -> O_minus
  | C_AST.BIT_NOT -> O_bit_invert
  | C_AST.LOGICAL_NOT -> O_log_not

and from_binary_operator op t = match op with
  | C_AST.O_arithmetic (C_AST.ADD) -> O_plus
  | C_AST.O_arithmetic (C_AST.SUB) -> O_minus
  | C_AST.O_arithmetic (C_AST.MUL) -> O_mult
  | C_AST.O_arithmetic (C_AST.DIV) -> O_div
  | C_AST.O_arithmetic (C_AST.MOD) -> O_mod
  | C_AST.O_arithmetic (C_AST.LEFT_SHIFT) -> O_bit_lshift
  | C_AST.O_arithmetic (C_AST.RIGHT_SHIFT) -> O_bit_rshift
  | C_AST.O_arithmetic (C_AST.BIT_AND) -> O_bit_and
  | C_AST.O_arithmetic (C_AST.BIT_OR) -> O_bit_or
  | C_AST.O_arithmetic (C_AST.BIT_XOR) -> O_bit_xor
  | C_AST.O_logical (C_AST.LESS) -> O_lt
  | C_AST.O_logical (C_AST.LESS_EQUAL) -> O_le
  | C_AST.O_logical (C_AST.GREATER) -> O_gt
  | C_AST.O_logical (C_AST.GREATER_EQUAL) -> O_ge
  | C_AST.O_logical (C_AST.EQUAL) -> O_eq
  | C_AST.O_logical (C_AST.NOT_EQUAL) -> O_ne
  | C_AST.O_logical (C_AST.LOGICAL_AND) -> Ast.O_c_and
  | C_AST.O_logical (C_AST.LOGICAL_OR) -> Ast.O_c_or

and from_character_kind : C_AST.character_kind -> Ast.c_character_kind = function
  | Clang_AST.Char_Ascii -> Ast.C_char_ascii
  | Clang_AST.Char_Wide -> Ast.C_char_wide
  | Clang_AST.Char_UTF8 -> Ast.C_char_utf8
  | Clang_AST.Char_UTF16 -> Ast.C_char_utf16
  | Clang_AST.Char_UTF32 -> Ast.C_char_utf8

(** {2 Ranges and locations} *)

and from_range : Clang_AST.range -> Framework.Location.range =
  fun range ->
    let open Clang_AST in
    let open Framework.Location in
    Range_origin {
      range_begin = {
        loc_file = range.range_begin.loc_file;
        loc_line = range.range_begin.loc_line;
        loc_column = range.range_begin.loc_column;
      };
      range_end = {
        loc_file = range.range_end.loc_file;
        loc_line = range.range_end.loc_line;
        loc_column = range.range_end.loc_column;
      }
    }


(** {2 Statements} *)

and from_stmt fun_ctx ((skind, range): C_AST.statement) : Framework.Ast.stmt =
  let srange = from_range range in
  let skind = match skind with
    | C_AST.S_local_declaration v ->
      let v, init, range = from_var_with_init fun_ctx v in
      Ast.S_c_local_declaration (v, init)
    | C_AST.S_expression e -> Universal.Ast.S_expression (from_expr fun_ctx e)
    | C_AST.S_block block -> from_block fun_ctx srange block |> Framework.Ast.skind
    | C_AST.S_if (cond, body, orelse) -> Universal.Ast.S_if (from_expr fun_ctx cond, from_block fun_ctx srange body, from_block fun_ctx srange orelse)
    | C_AST.S_while (cond, body) -> Universal.Ast.S_while (from_expr fun_ctx cond, from_block fun_ctx srange body)
    | C_AST.S_do_while (body, cond) -> Ast.S_c_do_while (from_block fun_ctx srange body, from_expr fun_ctx cond)
    | C_AST.S_for (init, test, increm, body) -> Ast.S_c_for(from_block fun_ctx srange init, from_expr_option fun_ctx test, from_expr_option fun_ctx increm, from_block fun_ctx srange body)
    | C_AST.S_jump (C_AST.S_goto label) -> S_c_goto label
    | C_AST.S_jump (C_AST.S_break) -> Universal.Ast.S_break
    | C_AST.S_jump (C_AST.S_continue) -> Universal.Ast.S_continue
    | C_AST.S_jump (C_AST.S_return None) -> Universal.Ast.S_return None
    | C_AST.S_jump (C_AST.S_return (Some e)) -> Universal.Ast.S_return (Some (from_expr fun_ctx e))
    | C_AST.S_jump (C_AST.S_switch (cond, body)) -> Ast.S_c_switch (from_expr fun_ctx cond, from_block fun_ctx srange body)
    | C_AST.S_target(C_AST.S_case(e)) -> Ast.S_c_switch_case(from_expr fun_ctx e)
    | C_AST.S_target(C_AST.S_default) -> Ast.S_c_switch_default
    | C_AST.S_target(C_AST.S_label l) -> Ast.S_c_label l
  in
  {skind; srange}

and from_block fun_ctx range (block: C_AST.block) : Framework.Ast.stmt =
  mk_block (List.map (from_stmt fun_ctx) block) range

and from_block_option fun_ctx (range: Framework.Location.range) (block: C_AST.block option) : Framework.Ast.stmt =
  match block with
  | None -> mk_nop range
  | Some stmtl -> from_block fun_ctx range stmtl

and from_body_option (fun_ctx) (range: Framework.Location.range) (block: C_AST.block option) : Framework.Ast.stmt option =
  match block with
  | None -> None
  | Some stmtl -> Some (from_block fun_ctx range stmtl)

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
          }
          in
          incr counter;
          v, StringTable.add s v table
      in
      table, {ekind = E_var(v, STRONG); etyp = type_of_string s; erange = e.erange}
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
        let body_init = get_c_fun_body f in
        let table, body' =
          Framework.Visitor.fold_map_stmt
            (fun acc e -> visit_expr acc e)
            (fun acc s -> acc, s)
            table body_init
        in
        (* Visit and change the locals *)
        let table, locals' = List.fold_left (fun (table, locals) (v, init, range) ->
            let table, init' = visit_init_option table init in
            table, (v, init', range) :: locals
          ) (table, []) f.c_func_local_vars
        in
        debug "fun: %a@\nbody = @[%a@]@\nbody' = @[%a@]" pp_var f.c_func_var pp_stmt body_init pp_stmt body';
        table, {f with c_func_body = Some body'; c_func_local_vars = List.rev locals'} :: funcs
      ) (StringTable.empty, []) funcs
  in
  let funcs = List.fold_left (fun acc f ->
      let body_init = get_c_fun_body f in
      let body' =
        Framework.Visitor.map_stmt
          (fun e ->
             match ekind e with
             | E_c_function f ->
               debug "call to %a" pp_var f.c_func_var;
               let f' = List.find (function {c_func_var} -> c_func_var.vname = f.c_func_var.vname) funcs in
               {e with ekind = E_c_function f'}

             | _ -> e
          )
          (fun s -> s)
          body_init
      in
      debug "fun2: %a@\nbody = @[%a@]@\nbody' = @[%a@]" pp_var f.c_func_var pp_stmt body_init pp_stmt body';
      {f with c_func_body = Some body'} :: funcs
    ) ([]) funcs
  in
  (* Add table entries to the global variables *)
  let range = Framework.Location.mk_fresh_range () in
  let globals = StringTable.fold (fun s v acc ->
      (v, Some (C_init_expr (mk_constant (C_string s) ~etyp:(type_of_string s) range))) :: acc
    ) table globals
  in

  globals, funcs
