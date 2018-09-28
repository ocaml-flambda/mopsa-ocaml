(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Pretty printer of the Universal extension to the AST. *)

open Framework.Essentials
open Ast
open Format

let () =
  register_pp_operator (fun default fmt -> function
      | O_plus -> pp_print_string fmt "+"
      | O_minus -> pp_print_string fmt "-"
      | O_mult -> pp_print_string fmt "*"
      | O_div -> pp_print_string fmt "/"
      | O_mod -> pp_print_string fmt "%"
      | O_pow -> pp_print_string fmt "**"
      | O_sqrt -> pp_print_string fmt "sqrt"
      | O_bit_invert -> pp_print_string fmt "~"
      | O_wrap(l,u)  -> Format.fprintf fmt "wrap(%a, %a)" Z.pp_print l Z.pp_print u
      | O_concat -> pp_print_string fmt "@"
      | O_bit_and -> pp_print_string fmt "&"
      | O_bit_or -> pp_print_string fmt "|"
      | O_bit_xor -> pp_print_string fmt "^"
      | O_bit_rshift -> pp_print_string fmt ">>"
      | O_bit_lshift -> pp_print_string fmt "<<"
      | op -> default fmt op
    );
  register_pp_constant (fun default fmt -> function
      | C_bool(b) -> fprintf fmt "%a" Format.pp_print_bool b
      | C_string(s) -> fprintf fmt "\"%s\"" s
      | C_int(n) -> Z.pp_print fmt n
      | C_float(f) -> pp_print_float fmt f
      | C_int_interval(a,b) -> fprintf fmt "[%a,%a]" Z.pp_print a Z.pp_print b
      | C_float_interval(a,b) -> fprintf fmt "[%a,%a]" pp_print_float a pp_print_float b
      | c -> default fmt c
    );

  register_pp_typ (fun default fmt typ ->
      match typ with
      | T_bool -> pp_print_string fmt "bool"
      | T_int -> pp_print_string fmt "int"
      | T_float -> pp_print_string fmt "float"
      | T_string -> pp_print_string fmt "string"
      | T_addr -> pp_print_string fmt "addr"
      | T_char -> pp_print_string fmt "char"
      | T_array t -> Format.fprintf fmt "[%a]" pp_typ t
      | _ -> default fmt typ
  );
  register_pp_expr (fun default fmt exp ->
      match ekind exp with
      | E_array(el) ->
        fprintf fmt "[@[<h>%a@]]"
          (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp_expr) el
      | E_subscript(v, e) -> fprintf fmt "%a[%a]" pp_expr v pp_expr e
      | E_function(f) -> fprintf fmt "fun %s" f.fun_name
      | E_call(f, args) ->
        fprintf fmt "%a(%a)"
          pp_expr f
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_expr) args
      | E_alloc_addr(akind) -> fprintf fmt "alloc()"
      | E_addr addr -> pp_addr fmt addr
      | E_len exp -> Format.fprintf fmt "|%a|" pp_expr exp
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
      | S_rebase_addr(a, a', STRONG) -> fprintf fmt "rebase %a = %a" pp_addr a pp_addr a'
      | S_rebase_addr(a, a', WEAK) -> fprintf fmt "rebase %a ≈ %a" pp_addr a pp_addr a'
      | S_assign(v, e) -> fprintf fmt "%a = %a;" pp_expr v pp_expr e
      (* FIXME: improve pretty printer by checking whether this is a
         Strong or a Weak assign*)
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
      | S_simple_assert(e,b,b') ->
        begin
          match b,b' with
          | true, true -> fprintf fmt "is_bottom(assume(%a))" pp_expr e
          | true, false -> fprintf fmt "is_bottom(assume(!%a))" pp_expr e
          | false, false -> fprintf fmt "!is_bottom(assume(!%a))" pp_expr e
          | false, true -> fprintf fmt "!is_bottom(assume(%a))" pp_expr e
        end
      | S_print -> fprintf fmt "print();"
      | _ -> default fmt stmt
    );
  register_pp_program (fun default fmt prg ->
      match prg.prog_kind with
      | Ast.P_universal (u_prog) ->
        Format.fprintf fmt "@[<v>%a@,%a@]"
          (
            pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
              (fun fmt f ->
                 fprintf fmt "%a %a(%a) {@\n@[<v 2>  %a@]@\n}"
                   pp_typ f.fun_return_type
                   Format.pp_print_string f.fun_name
                   (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")
                      (fun fmt v -> Format.fprintf fmt "%a %a"
                          pp_typ v.vtyp
                          pp_var v
                      )
                   ) f.fun_parameters
                   pp_stmt f.fun_body
              )
          ) u_prog.universal_fundecs
          pp_stmt u_prog.universal_main
      | _ -> default fmt prg
    );
  ()
