open Format
open Framework.Pp
open Framework.Ast
open Ast

let () =
  register_pp_typ (fun default fmt typ ->
      match typ with
        | T_c_void -> pp_print_string fmt "void"

        | T_c_integer(C_char C_signed)
        | T_c_integer(C_signed_char) -> pp_print_string fmt "signed char" 
        | T_c_integer(C_char C_unsigned)
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

        | T_c_pointer(t) -> fprintf fmt "* %a" pp_typ t

        | T_c_array(t, C_array_no_length) -> fprintf fmt "%a[]" pp_typ t
        | T_c_array(t, C_array_length_cst n) -> fprintf fmt "%a[%s]" pp_typ t (Z.to_string n)
        | T_c_array(t, C_array_length_expr e) -> fprintf fmt "%a[%a]" pp_typ t pp_expr e

        | T_c_function None -> ()
        | T_c_function (Some f) -> pp_typ fmt f.c_ftype_return

        | T_c_bitfield(t, size) -> assert false
        | T_c_builtin_fn -> assert false
        | T_c_typedef(typedef) -> assert false
        | T_c_record(record) -> assert false
        | T_c_enum(enum) -> assert false
        | T_c_qualified(qual, t) -> assert false
        | _ -> default fmt typ
    );
  register_pp_expr (fun default fmt expr ->
      match ekind expr with
      | E_c_conditional(cond, body, orelse) -> assert false
      | E_c_array_subscript(arr, idx) -> fprintf fmt "%a[%a]" pp_expr arr pp_expr idx
      | E_c_member_access(rcd, idx, fld) -> fprintf fmt "%a.%s" pp_expr rcd fld
      | E_c_function(f) -> Universal.Pp.pp_var fmt f.c_func_var
      | E_c_call(f, args) -> fprintf fmt "%a(%a)" pp_expr f (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp_expr) args
      | E_c_arrow_access(p, idx, fld) -> fprintf fmt "%a->%s" pp_expr p fld
      | E_c_assign(lval, rval) -> fprintf fmt "%a = %a" pp_expr lval pp_expr rval
      | E_c_compound_assign _ -> assert false
      | E_c_comma _ -> assert false
      | E_c_increment _ -> assert false
      | E_c_address_of (e) -> fprintf fmt "&%a" pp_expr e
      | E_c_deref(p) -> fprintf fmt "*%a" pp_expr p
      | E_c_cast(e, _) -> fprintf fmt "cast(%a) %a" pp_typ (etyp expr) pp_expr e
      | E_c_predefined _ -> assert false
      | E_c_var_args _ -> assert false
      | E_c_atomic _ -> assert false
      | _ -> default fmt expr
    );
  register_pp_stmt (fun default fmt stmt ->
      match skind stmt with
      | S_c_local_declaration v -> fprintf fmt "%a %a;" pp_typ v.Universal.Ast.vtyp Universal.Pp.pp_var v
      | S_c_for (init,cond,it,stmts) ->
        fprintf fmt "@[<v 2>for (%a;%a;%a) {@,%a@]@,}"
          Framework.Pp.pp_stmt init
          (Printers.print_option Framework.Pp.pp_expr) cond
          (Printers.print_option Framework.Pp.pp_expr) it
          Framework.Pp.pp_stmt stmts
      | _ -> default fmt stmt
    );
  register_pp_program (fun default fmt prg ->
      match prg.prog_kind with
      | Ast.C_program (globals, funcs) ->
        pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
          (fun fmt f ->
             fprintf fmt "%a %a(%a) {@\n@[<v 2>  %a@]@\n}"
               pp_typ f.c_func_return
               Universal.Pp.pp_var f.c_func_var
               (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") Universal.Pp.pp_var) f.c_func_parameters
               pp_stmt f.c_func_body
          )
          fmt funcs
          
      | _ -> default fmt prg
    );
  ()
