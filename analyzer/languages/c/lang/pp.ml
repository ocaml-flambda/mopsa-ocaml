(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2019 The MOPSA Project.                                    *)
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

open Format
open Mopsa
open Ast

let print_implicit_cast = false
 
let rec pp_c_type_short fmt =
  function
  | T_c_void -> pp_print_string fmt "void"
  | T_c_bool -> pp_print_string fmt "b"
  | T_c_integer(C_signed_char) -> pp_print_string fmt "s8"
  | T_c_integer(C_unsigned_char) -> pp_print_string fmt "u8"
  | T_c_integer(C_signed_short) -> pp_print_string fmt "s16"
  | T_c_integer(C_unsigned_short) -> pp_print_string fmt "u16"
  | T_c_integer(C_signed_int) -> pp_print_string fmt "s32"
  | T_c_integer(C_unsigned_int) -> pp_print_string fmt "u32"
  | T_c_integer(C_signed_long) -> pp_print_string fmt "sl"
  | T_c_integer(C_unsigned_long) -> pp_print_string fmt "ul"
  | T_c_integer(C_signed_long_long) -> pp_print_string fmt "sll"
  | T_c_integer(C_unsigned_long_long) -> pp_print_string fmt "ull"
  | T_c_integer(C_signed_int128) -> pp_print_string fmt "s128"
  | T_c_integer(C_unsigned_int128) -> pp_print_string fmt "u128"
  | T_c_float(C_float) -> pp_print_string fmt "f"
  | T_c_float(C_double) -> pp_print_string fmt "d"
  | T_c_float(C_long_double) -> pp_print_string fmt "ld"
  | T_c_float(C_float128) -> pp_print_string fmt "q"
  | T_c_pointer(t) -> fprintf fmt "%a*" pp_c_type_short t
  | T_c_array(t, C_array_no_length) -> fprintf fmt "%a[]" pp_c_type_short t
  | T_c_array(t, C_array_length_cst n) -> fprintf fmt "%a[%s]" pp_c_type_short t (Z.to_string n)
  | T_c_array(t, C_array_length_expr e) -> fprintf fmt "%a[%a]" pp_c_type_short t pp_expr e
  | T_c_function None -> ()
  | T_c_function (Some f) -> fprintf fmt "(%a)" pp_c_type_short f.c_ftype_return
  | T_c_typedef(typedef) -> pp_c_type_short fmt typedef.c_typedef_def
  | T_c_record({c_record_kind = C_struct} as record) -> fprintf fmt "s %s" record.c_record_org_name
  | T_c_record({c_record_kind = C_union} as record) -> fprintf fmt "u %s" record.c_record_org_name
  | T_c_bitfield(t, s) -> fprintf fmt "bf(%a:%d)" pp_c_type_short t s
  | T_c_qualified(qual, t) ->
    let l =
      (if qual.c_qual_is_const then ["c"] else []) @
      (if qual.c_qual_is_volatile then ["v"] else []) @
      (if qual.c_qual_is_restrict then ["r"] else [])
    in
    let qual = String.concat " " l in
    fprintf fmt "%s %a" qual pp_c_type_short t
  | T_c_enum(enum) -> fprintf fmt "e %s" enum.c_enum_org_name
  | t -> panic "pp_c_type_short: unsupported type %a" pp_typ t

let rec pp_c_init fmt = function
  | C_init_expr(e) -> pp_expr fmt e
  | C_init_list([], Some filler) -> fprintf fmt "{%a ...}" pp_c_init filler
  | C_init_list(l, filler) -> fprintf fmt "{%a, filler=%a}"
                                (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_c_init) l
                                (OptionExt.print pp_c_init) filler
  | C_init_implicit t -> assert false

let pp_character_kind fmt = function
  | C_char_ascii -> ()
  | C_char_wide -> pp_print_string fmt "L"
  | C_char_utf8 -> pp_print_string fmt "u8"
  | C_char_utf16 -> pp_print_string fmt "u"
  | C_char_utf32 -> pp_print_string fmt "U"


let () =
  register_typ_pp (fun default fmt typ ->
      match typ with
      | T_c_void -> pp_print_string fmt "void"

      | T_c_bool -> pp_print_string fmt "bool"

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
      | T_c_float(C_float128) -> pp_print_string fmt "__float128"

      | T_c_pointer(t) -> fprintf fmt "%a *" pp_typ t

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

      | T_c_bitfield(t, size) -> fprintf fmt "bf %a:%d" pp_typ t size

      | T_c_builtin_fn -> fprintf fmt "builtin_fn"
      | T_c_block_object tt -> Format.fprintf fmt "block-object(%a)" pp_typ tt
      | _ -> default fmt typ
    );
  register_constant_pp (fun next fmt c ->
      match c with
      | C_c_character(c, k) -> fprintf fmt "%a'\\x%s'" pp_character_kind k (Z.format "%X" c)
      | C_c_string(s, k) -> fprintf fmt "%a\"%s\"" pp_character_kind k (String.escaped s)
      | C_c_invalid -> fprintf fmt "INVALID"
      | C_ffi_alive_value -> fprintf fmt "<alive>"
      | _ -> next fmt c
    );
  register_operator_pp (fun next fmt op ->
      match op with
      | O_c_and -> pp_print_string fmt "&&"
      | O_c_or -> pp_print_string fmt "||"
      | _ -> next fmt op
    );
  register_expr_pp (fun default fmt expr ->
      match ekind expr with
      | E_c_conditional(cond, body, orelse) -> fprintf fmt "(%a ? %a : %a)" pp_expr cond pp_expr body pp_expr orelse
      | E_c_array_subscript(arr, idx) -> fprintf fmt "%a[%a]" pp_expr arr pp_expr idx
      | E_c_member_access(rcd, idx, fld) -> fprintf fmt "%a.%s" pp_expr rcd fld
      | E_c_function(f) -> pp_print_string fmt f.c_func_org_name
      | E_c_builtin_function(f) -> fprintf fmt "builtin %s" f
      | E_c_builtin_call(f, args) -> fprintf fmt "builtin %s(%a)" f (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp_expr) args
      | E_c_arrow_access(p, idx, fld) -> fprintf fmt "%a->%s" pp_expr p fld
      | E_c_assign(lval, rval) -> fprintf fmt "%a = %a" pp_expr lval pp_expr rval
      | E_c_compound_assign _ -> assert false
      | E_c_comma _ -> assert false
      | E_c_increment _ -> assert false
      | E_c_address_of (e) -> fprintf fmt "&%a" pp_expr e
      | E_c_deref(p) -> fprintf fmt "*%a" pp_expr p
      | E_c_cast(e, x) ->
         if x || print_implicit_cast then
           fprintf fmt "(%a) %a" pp_typ (etyp expr) pp_expr e
         else
           pp_expr fmt e
      | E_c_statement s -> fprintf fmt "@[<v 4>{@,%a@]@,}" pp_stmt s
      | E_c_var_args e -> fprintf fmt "__builtin_va_arg(%a)" pp_expr e
      | E_c_block_object e -> fprintf fmt "block_object(%a)" pp_expr e
      | E_c_predefined _ -> assert false
      | E_c_atomic (op,e1,e2) -> fprintf fmt "__atomic(%i,%a,%a)" op pp_expr e1 pp_expr e2
      | _ -> default fmt expr
    );
  register_stmt_pp (fun default fmt stmt ->
      match skind stmt with
      | S_c_declaration (v,None,_) -> fprintf fmt "%a %a;" pp_typ v.vtyp pp_var v
      | S_c_declaration (v,Some init,_) -> fprintf fmt "%a %a = %a;" pp_typ v.vtyp pp_var v pp_c_init init
      | S_c_for (init,cond,it,stmts) ->
        fprintf fmt "@[<v 4>for (%a;%a;%a) {@,%a@]@,}"
          pp_stmt init
          (OptionExt.print pp_expr) cond
          (OptionExt.print pp_expr) it
          pp_stmt stmts
      | S_c_do_while (body,cond) ->
        fprintf fmt "@[<v 4>do {@,%a@]@, while (%a);"
          pp_stmt body
          pp_expr cond
      | S_c_switch(cond, body) ->
        fprintf fmt "@[<v 4>switch (%a) {@,%a@]@,}"
          pp_expr cond
          pp_stmt body
      | S_c_return(None,_) -> fprintf fmt "return;"
      | S_c_return(Some e,_) -> fprintf fmt "return %a;" pp_expr e
      | S_c_break _ -> fprintf fmt "break;"
      | S_c_continue _ -> fprintf fmt "continue;"
      | S_c_switch_case(e,_) -> fprintf fmt "case %a:" pp_expr e
      | S_c_switch_default _ -> fprintf fmt "default:"
      | S_c_label l -> fprintf fmt "%s:" l
      | S_c_goto (l,_) -> fprintf fmt "goto %s;" l
      | S_c_goto_stab s -> fprintf fmt "@[<v 4>goto_stab {@,%a@]@,};" pp_stmt s
      | _ -> default fmt stmt
    );
  register_program_pp (fun default fmt prg ->
      match prg.prog_kind with
      | Ast.C_program prog ->
        (* Remove empty functions *)
        let funs = List.filter (fun f ->
            match f.c_func_body with
            | None -> false
            | Some _ -> true
          ) prog.c_functions
        in
        fprintf fmt "@[<v>";
        pp_print_list
          ~pp_sep:(fun fmt () -> fprintf fmt "@,@,")
          (fun fmt f ->
             fprintf fmt "@[<v 4>%a %s(%a) {@,%a@]@,}"
               pp_typ f.c_func_return
               f.c_func_org_name
               (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_var) f.c_func_parameters
               (OptionExt.print pp_stmt) f.c_func_body
          )
          fmt funs
        ;
        fprintf fmt "@]"

      | _ -> default fmt prg
    );
  ()
