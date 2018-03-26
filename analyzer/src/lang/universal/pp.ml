(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Pretty printer of the Universal extension to the AST. *)

open Framework.Ast
open Framework.Pp
open Ast
open Format


let pp_var fmt v =
  fprintf fmt "var %s" v.unname

let rec pp_addr_chain : (formatter -> addr -> unit) ref = ref (fun fmt addr ->
    failwith "Pp: Unknown address"
  )

and register_pp_addr pp = pp_addr_chain := pp !pp_addr_chain

and pp_addr fmt addr = !pp_addr_chain fmt addr

let () =
  register_pp_operator (fun default fmt -> function
      | O_plus -> pp_print_string fmt "+"
      | O_minus -> pp_print_string fmt "-"
      | O_mult -> pp_print_string fmt "*"
      | O_div -> pp_print_string fmt "/"
      | O_pow -> pp_print_string fmt "**"
      | O_lt -> pp_print_string fmt "<"
      | O_le -> pp_print_string fmt "<="
      | O_gt -> pp_print_string fmt ">"
      | O_ge -> pp_print_string fmt ">="
      | O_eq -> pp_print_string fmt "=="
      | O_ne -> pp_print_string fmt "!="
      | O_bit_and -> pp_print_string fmt "&"
      | O_bit_or -> pp_print_string fmt "|"
      | O_bit_xor -> pp_print_string fmt "^"
      | O_mod -> pp_print_string fmt "%"
      | O_log_or -> pp_print_string fmt "||"
      | O_log_and -> pp_print_string fmt "&&"
      | O_log_not -> pp_print_string fmt "!"
      | O_sqrt -> pp_print_string fmt "sqrt"
      | O_invert -> pp_print_string fmt "~"
      | O_bit_rshift -> pp_print_string fmt ">>"
      | O_bit_lshift -> pp_print_string fmt "<<"
      | O_abs -> pp_print_string fmt "abs"
      | op -> default fmt op
    );
  register_pp_constant (fun default fmt -> function
      | C_string(s) -> fprintf fmt "\"%s\"" s
      | C_int(n) -> Z.pp_print fmt n
      | C_float(f) -> pp_print_float fmt f
      | C_int_range(a,b) -> fprintf fmt "[%a,%a]" Z.pp_print a Z.pp_print b
      | C_float_range(a,b) -> fprintf fmt "[%a,%a]" pp_print_float a pp_print_float b
      | C_true -> pp_print_string fmt "True"
      | C_false -> pp_print_string fmt "False"
      | C_empty -> pp_print_string fmt "empty"
      | c -> default fmt c
    );

  register_pp_typ (fun default fmt typ ->
    match typ with
      | T_int -> pp_print_string fmt "int"
      | T_float -> pp_print_string fmt "float"
      | T_string -> pp_print_string fmt "string"
      | T_bool -> pp_print_string fmt "bool"
      | T_addr -> pp_print_string fmt "addr"
      | _ -> default fmt typ
  );
  register_pp_expr (fun default fmt exp ->
      match ekind exp with
      | E_constant c -> pp_constant fmt c
      | E_var(v) -> pp_var fmt v
      | E_unop(op, e) -> fprintf fmt "%a (%a)" pp_operator op pp_expr e
      | E_binop(op, e1, e2) -> fprintf fmt "(%a %a %a)" pp_expr e1 pp_operator op pp_expr e2
      | E_array(el) ->
        fprintf fmt "[@[<h>%a@]]"
          (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp_expr) el
      | E_subscript(v, e) -> fprintf fmt "%a[%a]" pp_expr v pp_expr e
      | E_function(f) -> fprintf fmt "fun %s" f.fun_name
      | E_call(f, args) ->
        fprintf fmt "%a(%a);"
          pp_expr f
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_expr) args
      | E_addr_attribute(addr, attr) ->
        fprintf fmt "%a.%s"
          pp_addr addr
          attr
      | E_alloc_addr(akind, range) ->
        fprintf fmt "alloc(%a)" pp_range range
      | E_addr addr -> pp_addr fmt addr
      | _ -> default fmt exp
    );

  register_pp_stmt (fun default fmt stmt ->
      match skind stmt with
      | S_remove_var(v) ->
        fprintf fmt "remove(@[<h>%a@])" pp_var v
      | S_project_vars(vl) ->
        fprintf fmt "project(@[<h>%a@])"
          (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp_var) vl

      | S_rename_var(v, v') -> fprintf fmt "rename(%a, %a)" pp_var v pp_var v'
      | S_rebase_addr(a, a') -> fprintf fmt "rebase(%a, %a)" pp_addr a pp_addr a'
      | S_assign(v, e, STRONG) -> fprintf fmt "%a = %a;" pp_expr v pp_expr e
      | S_assign(v, e, WEAK) -> fprintf fmt "%a ≈ %a;" pp_expr v pp_expr e
      | S_assign(v, e, EXPAND) -> fprintf fmt "%a ≋ %a;" pp_expr v pp_expr e
      | S_assume(e) -> fprintf fmt "assume(%a)" pp_expr e
      | S_expression(e) -> fprintf fmt "%a;" pp_expr e
      | S_if(e, s1, s2) ->
        fprintf fmt "if (%a) {@\n@[<v 2>  %a@]@\n} else {@\n@[<v 2>  %a@]@\n}" pp_expr e pp_stmt s1 pp_stmt s2
      | S_block(l) ->
         begin
           fprintf fmt "@[<v>%a@]"
             (pp_print_list
                ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
                pp_stmt
             ) l
         end
      | S_return(None) -> pp_print_string fmt "return;"
      | S_return(Some e) -> fprintf fmt "return %a;" pp_expr e
      | S_while(e, s) ->
        fprintf fmt "while %a {@\n@[<v 2>  %a@]@\n}" pp_expr e pp_stmt s
      | S_break -> pp_print_string fmt "break;"
      | S_continue -> pp_print_string fmt "continue;"
      | S_unit_tests (_, tests) -> pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n") (fun fmt (name, test) -> fprintf fmt "test %s:@\n  @[%a@]" name pp_stmt test) fmt tests
      | S_assert e -> fprintf fmt "assert(%a);" pp_expr e
      | _ -> default fmt stmt
    );
  ()
