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

(**
  Pretty-printer for abstract syntax trees.
 *)

open U_ast
open Lexing


(* operators *)
(* ********* *)


let string_of_unary_op = function
  | AST_UNARY_PLUS -> "+"
  | AST_UNARY_MINUS -> "-"
  | AST_NOT -> "!"
  | AST_ROUND -> "round"

let string_of_binary_op = function
  | AST_MULTIPLY -> "*"
  | AST_DIVIDE -> "/"
  | AST_PLUS -> "+"
  | AST_MINUS -> "-"
  | AST_EQUAL -> "=="
  | AST_NOT_EQUAL -> "!="
  | AST_LESS -> "<"
  | AST_LESS_EQUAL -> "<="
  | AST_GREATER -> ">"
  | AST_GREATER_EQUAL -> ">="
  | AST_AND -> "&&"
  | AST_OR -> "||"
  | AST_CONCAT -> "^"

let print_unary_op fmt op =
  Format.pp_print_string fmt (string_of_unary_op op)

let print_binary_op fmt op =
  Format.pp_print_string fmt (string_of_binary_op op)

(* precedence of the operator at the root of the expression;
   this is used to avoid printing unnecessary parentheses
*)

let expr_precedence = function
  | AST_unary (op,_) -> 99
  | AST_binary ((AST_MULTIPLY | AST_DIVIDE),_,_) -> 6
  | AST_binary ((AST_PLUS | AST_MINUS),_,_) -> 5
  | _ -> 100

(* utility to print lists *)
let print_list f sep fmt l =
  let rec aux = function
    | [] -> ()
    | [a] -> f fmt a
    | a::b -> f fmt a; Format.pp_print_string fmt sep; aux b
  in
  aux l


let print_ext pp fmt (a, _) =
  pp fmt a

(* types *)
(* ***** *)

let rec print_typ fmt t =
  match t with
  | AST_INT -> Format.pp_print_string fmt "int"
  | AST_REAL -> Format.pp_print_string fmt "real"
  | AST_ARRAY t -> Format.fprintf fmt "[%a]" print_typ t
  | AST_STRING -> Format.pp_print_string fmt "string"
  | AST_CHAR -> Format.pp_print_string fmt "char"

and print_typ_opt fmt = function
  | None -> Format.pp_print_string fmt "void"
  | Some x -> print_typ fmt x

and print_typed_var fmt ((t, v), _) =
  Format.fprintf fmt "%a %a"
    print_typ t
    print_var v

(* expressions *)
(* *********** *)

and print_var fmt v =
  Format.pp_print_string fmt v

and print_var_ext fmt ve =
  print_ext print_var fmt ve

and print_expr fmt e =
  match e with
  | AST_fun_call(f, args) ->
    Format.fprintf fmt "%a(%a)"
      print_var_ext f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         print_expr_ext
      ) args

  | AST_unit_const ->
    Format.fprintf fmt "()"
  | AST_unary (op,(e1,_)) ->
    Format.pp_print_string fmt (string_of_unary_op op);
    if expr_precedence e1 <= expr_precedence e
    then Format.fprintf fmt " (%a)" print_expr e1
    else Format.fprintf fmt " %a" print_expr e1

  | AST_binary (op,(e1,_),(e2,_)) ->
    if expr_precedence e1 < expr_precedence e
    then Format.fprintf fmt "(%a) " print_expr e1
    else Format.fprintf fmt "%a " print_expr e1;
    Format.pp_print_string fmt (string_of_binary_op op);
    if expr_precedence e2 <= expr_precedence e
    then Format.fprintf fmt " (%a)" print_expr e2
    else Format.fprintf fmt " %a" print_expr e2

  | AST_int_const (s, _)
  | AST_real_const(s, _) -> Format.pp_print_string fmt s
  | AST_string_const(s, _) -> Format.fprintf fmt "\"%s\"" s
  | AST_bool_const(b, _) -> Format.pp_print_bool fmt b
  | AST_char_const(c, _) -> Format.fprintf fmt "\'%c\'" c
  | AST_array_const(ea, _) ->
    Format.fprintf fmt "[%a]"
      (
        Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
          (print_expr_ext)
      )
      (Array.to_list ea)
  | AST_rand ((i1,_),(i2,_)) ->
    Format.fprintf fmt "rand(%s,%s)" i1 i2

  | AST_randf ((i1,_),(i2,_)) ->
    Format.fprintf fmt "randf(%s,%s)" i1 i2

  | AST_rand_string -> 
    Format.fprintf fmt "rand_string()"

  | AST_identifier (v,_) -> print_var fmt v

  | AST_array_access ((e1, _), (e2, _)) ->
    Format.fprintf fmt "%a[%a]"
      print_expr e1
      print_expr e2

  | AST_len (e, _) ->
    Format.fprintf fmt "|%a|"
      print_expr e

and print_expr_ext fmt (e, _) =
  print_expr fmt e

(* statements *)
(* ********** *)

and print_stat fmt = function

  | AST_block l ->
    Format.fprintf fmt "%a;"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
         print_stat_ext
      )
      l

  | AST_assign ((e',_),(e,_)) ->
    Format.fprintf fmt "%a = %a"
      print_expr e' print_expr e

  | AST_if (e, b1, None) ->
    Format.fprintf fmt "@[<v 2>if (%a) {@,%a@,}@]"
      print_expr_ext e print_stat_ext b1

  | AST_if (e, b, Some b2) ->
    Format.fprintf fmt "@[<v 2>if (%a) {@,%a@,} else {%a@,}@]"
      print_expr_ext e print_stat_ext b
      print_stat_ext b2

  | AST_while (e, s) ->
    Format.fprintf fmt "@[<v 2>while (%a) {@,%a@,}@]"
      print_expr_ext e
      print_stat_ext s

  | AST_for (v, e, e', s) ->
    Format.fprintf fmt "@[<v 2>for %a = %a to %a {@,%a@,}@]"
      print_var_ext v
      print_expr_ext e
      print_expr_ext e'
      print_stat_ext s

  | AST_assert e ->
    Format.fprintf fmt "assert (%a)"
      print_expr_ext e

  | AST_assume e ->
    Format.fprintf fmt "assume (%a)"
      print_expr_ext e

  | AST_print ->
    Format.fprintf fmt "print ()"

  | AST_return (Some e) ->
    Format.fprintf fmt "return (%a)"
      print_expr_ext e

  | AST_return None ->
    Format.fprintf fmt "return"

  | AST_expr (e,_) ->
    Format.fprintf fmt "%a"
      print_expr e

  | AST_continue ->
    Format.pp_print_string fmt "continue"

  | AST_break ->
    Format.pp_print_string fmt "break"


and print_stat_ext fmt (s, _) =
  print_stat fmt s

and print_declaration fmt ((s, o) : declaration) =
  match o with
  | None -> print_typed_var fmt s
  | Some e -> Format.fprintf fmt "%a = %a"
                print_typed_var s
                print_expr_ext e

and print_declaration_ext fmt x =
  print_ext print_declaration fmt x

and print_fundec fmt (f: fundec) =
  Format.fprintf fmt "@[<v>@[<v 2>%a %a(%a) {@,%a;@,%a;@]@,}@]"
    print_typ_opt f.return_type
    print_var f.funname
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       print_typed_var
    ) f.parameters
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
       print_declaration_ext
    ) f.locvars
    print_stat_ext f.body

and print_fundec_ext fmt f =
  print_ext print_fundec fmt f
(* programs *)
(* ******** *)

let print_prog fmt p =
  Format.fprintf fmt "@[<v>%a;@,%a@,%a@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
       print_declaration_ext
    ) p.gvars
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
       print_fundec_ext
    ) p.funs
    print_stat_ext p.main
