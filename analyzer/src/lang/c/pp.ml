open Format
open Framework.Essentials
open Framework.Ast
open Ast

let rec pp_c_init fmt = function
  | C_init_expr(e) -> pp_expr fmt e
  | C_init_list(l, None) ->
    fprintf fmt "{%a}"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_c_init) l
  | C_init_list([], Some filler) ->
    fprintf fmt "{%a ...}" pp_c_init filler
  | _ -> assert false

let () =
  register_pp_typ (fun default fmt typ ->
      match typ with
      | T_c_void -> pp_print_string fmt "void"

      | T_c_integer(C_signed_char) -> pp_print_string fmt "signed char"
      | T_c_integer(C_unsigned_char) -> pp_print_string fmt "unsigned char"
      | T_c_integer(C_signed_short) -> pp_print_string fmt "signed short"
      | T_c_integer(C_unsigned_short) -> pp_print_string fmt "unsigned short"
      | T_c_integer(C_signed_int) -> pp_print_string fmt "signed int"
      | T_c_integer(C_unsigned_int) -> pp_print_string fmt "unsigned int"
      | T_c_integer(C_signed_long) -> pp_print_string fmt "signed long"
      | T_c_integer(C_unsigned_long) -> pp_print_string fmt "unsigned long"
      | T_c_integer(C_signed_long_long) -> pp_print_string fmt "signed long long"
      | T_c_integer(C_unsigned_long_long) -> pp_print_string fmt "unsigned long long"
      | T_c_integer(C_signed_int128) -> pp_print_string fmt "signed int128"
      | T_c_integer(C_unsigned_int128) -> pp_print_string fmt "unsigned int128"

      | T_c_float(C_float) -> pp_print_string fmt "float"
      | T_c_float(C_double) -> pp_print_string fmt "double"
      | T_c_float(C_long_double) -> pp_print_string fmt "long double"

      | T_c_pointer(t) -> fprintf fmt "(ptr %a *)" pp_typ t

      | T_c_array(t, C_array_no_length) -> fprintf fmt "%a[]" pp_typ t
      | T_c_array(t, C_array_length_cst n) -> fprintf fmt "%a[%s]" pp_typ t (Z.to_string n)
      | T_c_array(t, C_array_length_expr e) -> fprintf fmt "%a[%a]" pp_typ t pp_expr e

      | T_c_function None -> ()
      | T_c_function (Some f) -> fprintf fmt "(fun %a)" pp_typ f.c_ftype_return

      | T_c_typedef(typedef) -> pp_typ fmt typedef.c_typedef_def

      | T_c_record({c_record_kind = C_struct} as record) -> fprintf fmt "struct %s" record.c_record_org_name

      | T_c_record({c_record_kind = C_union} as record) -> fprintf fmt "union %s" record.c_record_org_name

      | T_c_qualified(qual, t) ->
        let l =
          (if qual.c_qual_is_const then ["const"] else []) @
          (if qual.c_qual_is_volatile then ["volatile"] else []) @
          (if qual.c_qual_is_restrict then ["restrict"] else [])
        in
        let qual = String.concat " " l in
        fprintf fmt "%s %a" qual pp_typ t

      | T_c_enum(enum) -> fprintf fmt "enum %s" enum.c_enum_org_name

      | T_c_bitfield(t, size) -> assert false
      | T_c_builtin_fn -> fprintf fmt "builtin_fn"
      | _ -> default fmt typ
    );
  register_pp_constant (fun next fmt c ->
      match c with
      | C_c_character(c, C_char_ascii) -> fprintf fmt "'%c'" (char_of_int @@ Z.to_int c)
      | C_c_string(s, _) -> fprintf fmt "C_c_string(\"%s\")" s
      | _ -> next fmt c
    );
  register_pp_operator (fun next fmt op ->
      match op with
      | O_c_and -> pp_print_string fmt "&&"
      | O_c_or -> pp_print_string fmt "||"
      | _ -> next fmt op
    );
  register_pp_expr (fun default fmt expr ->
      match ekind expr with
      | E_c_conditional(cond, body, orelse) -> assert false
      | E_c_array_subscript(arr, idx) -> fprintf fmt "%a[%a]" pp_expr arr pp_expr idx
      | E_c_member_access(rcd, idx, fld) -> fprintf fmt "%a.%s" pp_expr rcd fld
      | E_c_function(f) -> pp_var fmt f.c_func_var
      | E_c_builtin_function(f) -> fprintf fmt "builtin %s" f
      | E_c_call(f, args) -> fprintf fmt "%a(%a)" pp_expr f (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp_expr) args
      | E_c_builtin_call(f, args) -> fprintf fmt "builtin %s(%a)" f (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp_expr) args
      | E_c_arrow_access(p, idx, fld) -> fprintf fmt "%a->%s" pp_expr p fld
      | E_c_assign(lval, rval) -> fprintf fmt "%a = %a" pp_expr lval pp_expr rval
      | E_c_compound_assign _ -> assert false
      | E_c_comma _ -> assert false
      | E_c_increment _ -> assert false
      | E_c_address_of (e) -> fprintf fmt "&(%a)" pp_expr e
      | E_c_deref(p) -> fprintf fmt "*(%a)" pp_expr p
      | E_c_cast(e, _) -> fprintf fmt "((%a) %a)" pp_typ (etyp expr) pp_expr e
      | E_c_statement s -> fprintf fmt "{@\n  @[%a@]@\n}" pp_stmt s
      | E_c_predefined _ -> assert false
      | E_c_var_args _ -> assert false
      | E_c_atomic _ -> assert false
      | _ -> default fmt expr
    );
  register_pp_stmt (fun default fmt stmt ->
      match skind stmt with
      | S_c_global_declaration (v, None)
      | S_c_local_declaration (v, None) -> fprintf fmt "%a %a;" pp_typ v.vtyp pp_var v
      | S_c_global_declaration (v, Some init) -> fprintf fmt "%a %a = %a;" pp_typ v.vtyp pp_var v pp_c_init init
      | S_c_local_declaration (v, Some init) -> fprintf fmt "%a %a = %a;" pp_typ v.vtyp pp_var v pp_c_init init
      | S_c_for (init,cond,it,stmts) ->
        fprintf fmt "@[<v 2>for (%a;%a;%a) {@,%a@]@,}"
          pp_stmt init
          (Printers.print_option pp_expr) cond
          (Printers.print_option pp_expr) it
          pp_stmt stmts
      | S_c_do_while (body,cond) ->
        fprintf fmt "do {@\n  @[%a]@\n} while (%a);"
          pp_stmt body
          pp_expr cond
      | S_c_switch(cond, body) ->
        fprintf fmt "switch (%a) {@\n  @[%a@]@\n}"
          pp_expr cond
          pp_stmt body
      | S_c_switch_case(e) -> fprintf fmt "case %a:" pp_expr e
      | S_c_switch_default -> fprintf fmt "default:"
      | S_c_label l -> fprintf fmt "%s:" l
      | S_c_goto l -> fprintf fmt "goto %s;" l
      | S_c_goto_stab s -> fprintf fmt "goto_stab {%a};" pp_stmt s
      | _ -> default fmt stmt
    );
  register_pp_program (fun default fmt prg ->
      match prg.prog_kind with
      | Ast.C_program (globals, funcs) ->
        pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
          (fun fmt f ->
             fprintf fmt "%a %a(%a) {@\n@[<v 2>  %a@]@\n}"
               pp_typ f.c_func_return
               pp_var f.c_func_var
               (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_var) f.c_func_parameters
               pp_stmt (Ast.get_c_fun_body f)
          )
          fmt funcs

      | _ -> default fmt prg
    );
  ()
