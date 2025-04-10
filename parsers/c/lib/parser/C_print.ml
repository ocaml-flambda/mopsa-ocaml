(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(* Copyright (c) 2025 Jane Street Group LLC                                 *)
(* opensource-contacts@janestreet.com                                       *)
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
  C_print - Printing, converts C AST to valid C code
 *)

open C_AST
open Clang_utils


(** {2 Config} *)

let ignore_implicit_casts = true
(** Only print explicit casts, not implici ones. *)

let print_loc = ref true
(** Prints location information in comment for global declarations. *)

let print_comments = ref true
(** Prints comments attached to declarations. *)

let print_scope = false
(** Prints scope update information (end of blocks & jumps). *)


(** {2 Internal printing utilities} *)


let bp = Printf.bprintf

let bp_str = Buffer.add_string

let bp_char = Buffer.add_char

let bp_option none f buf a =
  match a with
  | None -> bp_str buf none
  | Some x -> f buf x

let bp_array f sep buf ee =
  for i=0 to Array.length ee-1 do
    bp buf "%s%a" (if i=0 then "" else sep) f ee.(i)
  done

let bp_list f sep buf ee =
  let first = ref true in
  List.iter
    (fun e ->
      bp buf "%s%a" (if !first then "" else sep) f e;
      first := false
    ) ee

let bp_paren doit f buf a =
  if doit then bp buf "(%a)" f a
  else f buf a

let string_from_buffer f a =
  let buf = Buffer.create 128 in
  f buf a;
  Buffer.contents buf


let inc_indent indent =
  indent^"    "
(* add one level of interation (i.e., some spaces) *)


let bp_loc indent buf loc =
  if !print_loc
  then bp buf "%s/* %s */\n" indent (Clang_dump.string_of_loc loc.Clang_AST.range_begin)



(** {2 C pretty-printing} *)


let string_of_signedness = function
  | SIGNED -> "signed"
  | UNSIGNED -> "unsigned"

let string_of_integer_type = function
  | Char _ -> "char"
  | SIGNED_CHAR -> "signed char"
  | UNSIGNED_CHAR -> "unsigned char"
  | SIGNED_SHORT -> "short"
  | UNSIGNED_SHORT -> "unsigned short"
  | SIGNED_INT -> "int"
  | UNSIGNED_INT -> "unsigned int"
  | SIGNED_LONG -> "long"
  | UNSIGNED_LONG -> "unsigned long"
  | SIGNED_LONG_LONG -> "long long"
  | UNSIGNED_LONG_LONG -> "unsigned long long"
  | SIGNED_INT128 -> "__int128"
  | UNSIGNED_INT128 -> "unsigned __uint128"

let integer_suffix = function
  | Char SIGNED | SIGNED_CHAR | SIGNED_SHORT | SIGNED_INT -> ""
  | Char UNSIGNED | UNSIGNED_CHAR | UNSIGNED_SHORT | UNSIGNED_INT -> "U"
  | SIGNED_LONG -> "L"
  | UNSIGNED_LONG -> "UL"
  | SIGNED_LONG_LONG | SIGNED_INT128 -> "LL"
  | UNSIGNED_LONG_LONG | UNSIGNED_INT128 -> "ULL"

let string_of_float_type = function
  | FLOAT -> "float"
  | DOUBLE -> "double"
  | LONG_DOUBLE -> "long double"
  | FLOAT128 -> "__float128"

let float_suffix = function
  | FLOAT -> "F"
  | DOUBLE -> ""
  | LONG_DOUBLE -> "L"
  | FLOAT128 -> "Q"

let string_of_record_kind = function
  | STRUCT -> "struct"
  | UNION -> "union"

let string_of_qualifier q =
  if q.qual_is_const then "const " else ""

let string_of_binary_arithmetic = function
  | ADD -> "+"
  | SUB -> "-"
  | MUL -> "*"
  | DIV -> "/"
  | MOD -> "%"
  | LEFT_SHIFT -> "<<"
  | RIGHT_SHIFT -> ">>"
  | BIT_AND -> "&"
  | BIT_OR -> "|"
  | BIT_XOR -> "^"

let string_of_binary_logical = function
  | LESS -> "<"
  | LESS_EQUAL -> "<="
  | GREATER -> ">"
  | GREATER_EQUAL -> ">="
  | EQUAL -> "=="
  | NOT_EQUAL -> "!="
  | LOGICAL_AND -> "&&"
  | LOGICAL_OR -> "||"

let string_of_binary_operator = function
  | O_arithmetic op -> string_of_binary_arithmetic op
  | O_logical op -> string_of_binary_logical op

let string_of_unary_operator = function
  | NEG -> "-"
  | BIT_NOT -> "~"
  | LOGICAL_NOT -> "!"

let string_of_inc_direction = function
  | INC -> "++"
  | DEC -> "--"


let rec prio_expr ((e,_,_):expr) =
  match e with
  | E_cast (ee,IMPLICIT) when ignore_implicit_casts -> prio_expr ee
  | E_comma _ -> 0
  | E_compound_assign _ | E_assign _ -> 1
  | E_conditional _ | E_binary_conditional _ -> 2
  | E_binary (O_logical LOGICAL_OR,_,_) -> 3
  | E_binary (O_logical LOGICAL_AND,_,_) -> 4
  | E_binary (O_arithmetic BIT_OR,_,_) -> 5
  | E_binary (O_arithmetic BIT_XOR,_,_) -> 6
  | E_binary (O_arithmetic BIT_AND,_,_) -> 7
  | E_binary (O_logical (EQUAL | NOT_EQUAL),_,_) -> 8
  | E_binary (O_logical (LESS | LESS_EQUAL | GREATER | GREATER_EQUAL),_,_) -> 9
  | E_binary (O_arithmetic (LEFT_SHIFT | RIGHT_SHIFT),_,_) -> 10
  | E_binary (O_arithmetic (ADD | SUB),_,_) -> 11
  | E_binary (O_arithmetic (MUL | DIV | MOD),_,_) -> 12
  | E_unary _ | E_increment (_,PRE,_) | E_cast _ | E_address_of _ | E_deref _ -> 13
  | E_increment (_,POST,_) | E_array_subscript _ | E_member_access _ | E_arrow_access _ | E_call _ -> 14
  | E_character_literal _ | E_integer_literal _ | E_float_literal _
  | E_string_literal _ | E_compound_literal _
  | E_variable _ | E_function _ | E_predefined _ | E_statement _
  | E_var_args _ | E_atomic _
  | E_convert_vector _ | E_vector_element _ | E_shuffle_vector _
    -> 15
(* get the natural priority of an experssion, to avoid spurious parentheses *)

let force_paren prio =
  prio == 10 || (prio >= 3 && prio <= 7)
(* force the arguments of <<, >>, etc., to have parentheses, even when useless,
   but this avoids compiler warnings *)

let is_comma ((e,_,_):expr) =
  match e with
  | E_comma _ -> true
  | _ -> false
(* comma expressions are special and require extra parentheses in many contexts *)


let rec raw_buf_type_qual buf (t,q) =
  bp buf "%s%a" (string_of_qualifier q) raw_buf_type t
and raw_buf_type buf = function
    | T_void -> bp_str buf "void"
    | T_bool -> bp_str buf "_Bool"
    | T_integer i -> bp_str buf (string_of_integer_type i)
    | T_float f -> bp_str buf (string_of_float_type f)
    | T_builtin_fn -> bp_str buf "__builtin_fn"
    | T_pointer tq -> bp buf "ptr(%a)" raw_buf_type_qual tq
    | T_array (tq,l) -> bp buf "array(%a)" raw_buf_type_qual tq
    | T_bitfield (t,l) -> bp buf "bitfield%i(%a)" l raw_buf_type t
    | T_function None -> bp_str buf "func()"
    | T_function (Some f) -> bp buf "func(%a, { %a },%B)" raw_buf_type_qual f.ftype_return (bp_list raw_buf_type_qual ", ") f.ftype_params f.ftype_variadic
    | T_typedef t -> bp buf "typedef %s" t.typedef_unique_name
    | T_enum e -> bp buf "enum %s" e.enum_unique_name
    | T_record r -> bp buf "%s %s" (string_of_record_kind r.record_kind) r.record_unique_name
    | T_complex f -> bp buf "%s _Complex" (string_of_float_type f)
    | T_vector v -> bp buf "vector(%a, %i, %i)" raw_buf_type_qual v.vector_type v.vector_size v.vector_kind
    | T_unknown_builtin s -> bp_str buf s
(* raw (non-C) representation of a type, somewhat more clear than C sytnax *)



(* these internal functions print to a buffer and use a current indentation string *)

let rec c_buf_type_qual indent (var:string) (buf:Buffer.t) (tq:type_qual) =
  c_buf_type_base buf tq;
  if var <>"" then bp_str buf " ";
  c_buf_type_suffix buf var indent false bp_str tq

(* prints the base type without pointers and arrays *)
and c_buf_type_base buf (t,q) = match t with
  | T_void -> bp buf "%svoid" (string_of_qualifier q)
  | T_bool -> bp buf "%s_Bool" (string_of_qualifier q)
  | T_integer i -> bp buf "%s%s" (string_of_qualifier q) (string_of_integer_type i)
  | T_float f -> bp buf "%s%s" (string_of_qualifier q) (string_of_float_type f)
  | T_builtin_fn -> bp buf "%s__builtin_fn" (string_of_qualifier q)
  | T_pointer tq -> c_buf_type_base buf tq
  | T_array (tq,l) -> c_buf_type_base buf tq
  | T_bitfield (t,l) -> c_buf_type_base buf (t,q)
  | T_function None -> bp buf "%svoid" (string_of_qualifier q)
  | T_function (Some f) -> c_buf_type_base buf f.ftype_return
  | T_typedef t -> bp buf "%s%s" (string_of_qualifier q) t.typedef_unique_name
  | T_enum e -> bp buf "%senum %s" (string_of_qualifier q) e.enum_unique_name
  | T_record r ->
     bp buf "%s%s %s"
        (string_of_qualifier q) (string_of_record_kind r.record_kind) r.record_unique_name
  | T_complex f -> bp buf "%s%s _Complex" (string_of_qualifier q) (string_of_float_type f)
  | T_vector v -> bp buf "%a" c_buf_type_base v.vector_type
  | T_unknown_builtin s -> bp_str buf s

(* prints the rest of the type; inner prints the innermost part of the type *)
and c_buf_type_suffix buf var indent inptr inner (t,q) = match t with
    | T_void | T_bool  | T_integer _ | T_float _ | T_builtin_fn | T_complex _ ->
       inner buf var

    | T_pointer tq ->
       let inner' buf var = bp buf "*%s%a" (string_of_qualifier q) inner var in
       c_buf_type_suffix buf var indent true inner' tq

    | T_array (tq,l) ->
       let inner' buf var =  bp buf "%a[%a]" (bp_paren inptr inner) var (len indent) l in
       c_buf_type_suffix buf var indent false inner' tq

    | T_bitfield (t,l) ->
       bp buf "%a : %i" inner var l

    | T_function None ->
       bp buf "(%a)()" (bp_paren inptr inner) var

    | T_function (Some f) ->
       let variadic = if f.ftype_variadic then if f.ftype_params = [] then "..." else ", ..." else "" in
       let inner' buf var = bp buf "(%a)(%a%s)" inner var (bp_list (c_buf_type_qual indent "") ", ") f.ftype_params variadic
       in
       c_buf_type_suffix buf var indent true inner' f.ftype_return

    | T_typedef _ | T_enum _ | T_record _ ->
       inner buf var

    | T_vector v ->
       bp buf "__attribute__((__vector_size__(%i)))" v.vector_size

    | T_unknown_builtin _ ->
       inner buf var

 and c_atomic_op = function
  | AO__c11_atomic_init -> "__c11_atomic_init"
  | AO__c11_atomic_load -> "__c11_atomic_load"
  | AO__c11_atomic_store -> "__c11_atomic_store"
  | AO__c11_atomic_exchange -> "__c11_atomic_exchange"
  | AO__c11_atomic_compare_exchange_strong -> "__c11_atomic_compare_exchange_strong"
  | AO__c11_atomic_compare_exchange_weak -> "__c11_atomic_compare_exchange_weak"
  | AO__c11_atomic_fetch_add -> "__c11_atomic_fetch_add"
  | AO__c11_atomic_fetch_sub -> "__c11_atomic_fetch_sub"
  | AO__c11_atomic_fetch_and -> "__c11_atomic_fetch_and"
  | AO__c11_atomic_fetch_or -> "__c11_atomic_fetch_or"
  | AO__c11_atomic_fetch_xor -> "__c11_atomic_fetch_xor"
  | AO__c11_atomic_fetch_nand -> "__c11_atomic_fetch_nand"
  | AO__c11_atomic_fetch_max -> "__c11_atomic_fetch_max"
  | AO__c11_atomic_fetch_min -> "__c11_atomic_fetch_min"
  | AO__atomic_load -> "__atomic_load"
  | AO__atomic_load_n -> "__atomic_load_n"
  | AO__atomic_store -> "__atomic_store"
  | AO__atomic_store_n -> "__atomic_store_n"
  | AO__atomic_exchange -> "__atomic_exchange"
  | AO__atomic_exchange_n -> "__atomic_exchange_n"
  | AO__atomic_compare_exchange -> "__atomic_compare_exchange"
  | AO__atomic_compare_exchange_n -> "__atomic_compare_exchange_n"
  | AO__atomic_fetch_add -> "__atomic_fetch_add"
  | AO__atomic_fetch_sub -> "__atomic_fetch_sub"
  | AO__atomic_fetch_and -> "__atomic_fetch_and"
  | AO__atomic_fetch_or -> "__atomic_fetch_or"
  | AO__atomic_fetch_xor -> "__atomic_fetch_xor"
  | AO__atomic_fetch_nand -> "__atomic_fetch_nand"
  | AO__atomic_add_fetch -> "__atomic_add_fetch"
  | AO__atomic_sub_fetch -> "__atomic_sub_fetch"
  | AO__atomic_and_fetch -> "__atomic_and_fetch"
  | AO__atomic_or_fetch -> "__atomic_or_fetch"
  | AO__atomic_xor_fetch -> "__atomic_xor_fetch"
  | AO__atomic_max_fetch -> "__atomic_max_fetch"
  | AO__atomic_min_fetch -> "__atomic_min_fetch"
  | AO__atomic_nand_fetch -> "__atomic_nand_fetch"
  | AO__atomic_test_and_set -> "__atomic_test_and_set"
  | AO__atomic_clear -> "__atomic_clear"
  | AO__scoped_atomic_load -> "__scoped_atomic_load"
  | AO__scoped_atomic_load_n -> "__scoped_atomic_load_n"
  | AO__scoped_atomic_store -> "__scoped_atomic_store"
  | AO__scoped_atomic_store_n -> "__scoped_atomic_store_n"
  | AO__scoped_atomic_exchange -> "__scoped_atomic_exchange"
  | AO__scoped_atomic_exchange_n -> "__scoped_atomic_exchange_n"
  | AO__scoped_atomic_compare_exchange -> "__scoped_atomic_compare_exchange"
  | AO__scoped_atomic_compare_exchange_n -> "__scoped_atomic_compare_exchange_n"
  | AO__scoped_atomic_fetch_add -> "__scoped_atomic_fetch_add"
  | AO__scoped_atomic_fetch_sub -> "__scoped_atomic_fetch_sub"
  | AO__scoped_atomic_fetch_and -> "__scoped_atomic_fetch_and"
  | AO__scoped_atomic_fetch_or -> "__scoped_atomic_fetch_or"
  | AO__scoped_atomic_fetch_xor -> "__scoped_atomic_fetch_xor"
  | AO__scoped_atomic_fetch_nand -> "__scoped_atomic_fetch_nand"
  | AO__scoped_atomic_fetch_min -> "__scoped_atomic_fetch_min"
  | AO__scoped_atomic_fetch_max -> "__scoped_atomic_fetch_max"
  | AO__scoped_atomic_add_fetch -> "__scoped_atomic_add_fetch"
  | AO__scoped_atomic_sub_fetch -> "__scoped_atomic_sub_fetch"
  | AO__scoped_atomic_and_fetch -> "__scoped_atomic_and_fetch"
  | AO__scoped_atomic_or_fetch -> "__scoped_atomic_or_fetch"
  | AO__scoped_atomic_xor_fetch -> "__scoped_atomic_xor_fetch"
  | AO__scoped_atomic_nand_fetch -> "__scoped_atomic_nand_fetch"
  | AO__scoped_atomic_min_fetch -> "__scoped_atomic_min_fetch"
  | AO__scoped_atomic_max_fetch -> "__scoped_atomic_max_fetch"
  | AO__opencl_atomic_init -> "__opencl_atomic_init"
  | AO__opencl_atomic_load -> "__opencl_atomic_load"
  | AO__opencl_atomic_store -> "__opencl_atomic_store"
  | AO__opencl_atomic_compare_exchange_weak -> "__opencl_atomic_compare_exchange_weak"
  | AO__opencl_atomic_compare_exchange_strong -> "__opencl_atomic_compare_exchange_strong"
  | AO__opencl_atomic_exchange -> "__opencl_atomic_exchange"
  | AO__opencl_atomic_fetch_add -> "__opencl_atomic_fetch_add"
  | AO__opencl_atomic_fetch_sub -> "__opencl_atomic_fetch_sub"
  | AO__opencl_atomic_fetch_and -> "__opencl_atomic_fetch_and"
  | AO__opencl_atomic_fetch_or -> "__opencl_atomic_fetch_or"
  | AO__opencl_atomic_fetch_xor -> "__opencl_atomic_fetch_xor"
  | AO__opencl_atomic_fetch_min -> "__opencl_atomic_fetch_min"
  | AO__opencl_atomic_fetch_max -> "__opencl_atomic_fetch_max"
  | AO__atomic_fetch_max -> "__atomic_fetch_max"
  | AO__atomic_fetch_min -> "__atomic_fetch_min"
  | AO__hip_atomic_load -> "__hip_atomic_load"
  | AO__hip_atomic_store -> "__hip_atomic_store"
  | AO__hip_atomic_compare_exchange_weak -> "__hip_atomic_compare_exchange_weak"
  | AO__hip_atomic_compare_exchange_strong -> "__hip_atomic_compare_exchange_strong"
  | AO__hip_atomic_exchange -> "__hip_atomic_exchange"
  | AO__hip_atomic_fetch_add -> "__hip_atomic_fetch_add"
  | AO__hip_atomic_fetch_sub -> "__hip_atomic_fetch_sub"
  | AO__hip_atomic_fetch_and -> "__hip_atomic_fetch_and"
  | AO__hip_atomic_fetch_or -> "__hip_atomic_fetch_or"
  | AO__hip_atomic_fetch_xor -> "__hip_atomic_fetch_xor"
  | AO__hip_atomic_fetch_min -> "__hip_atomic_fetch_min"
  | AO__hip_atomic_fetch_max -> "__hip_atomic_fetch_max"


(* array length *)
and len indent buf = function
  | No_length -> ()
  | Length_cst c -> Z.bprint buf c
  | Length_expr e -> c_buf_expr indent buf e

and c_buf_type indent (var:string) (buf:Buffer.t) (t:typ) =
  c_buf_type_qual indent var buf (t,no_qual)

(* helper for unary operations, cast, increment, dereference, etc *)
and c_buf_expr_unary prio indent buf pre e post =
  bp buf "%s%a%s"
     pre (bp_paren (prio_expr e < prio) (c_buf_expr indent)) e post

(* helper for binary operations, assign, array access,etc. *)
(* one version for left-associative and another one for right associative *)
and c_buf_expr_binary_left prio indent buf pre e1 mid e2 post =
  bp buf "%s%a%s%a%s"
     pre (bp_paren (force_paren prio || prio_expr e1 <  prio) (c_buf_expr indent)) e1
     mid (bp_paren (force_paren prio || prio_expr e2 <= prio) (c_buf_expr indent)) e2 post
and c_buf_expr_binary_right prio indent buf pre e1 mid e2 post =
  bp buf "%s%a%s%a%s"
     pre (bp_paren (force_paren prio || prio_expr e1 <= prio) (c_buf_expr indent)) e1
     mid (bp_paren (force_paren prio || prio_expr e2 <  prio) (c_buf_expr indent)) e2 post

and c_buf_expr indent buf ((e,t,_) as ee:expr) =
  let prio = prio_expr ee in
  match e with
  | E_conditional (e1,e2,e3) ->
     bp buf "%a ? %a : %a"
        (bp_paren (prio_expr e1 <= prio) (c_buf_expr indent)) e1
        (bp_paren (prio_expr e2 <= prio) (c_buf_expr indent)) e2
        (bp_paren (prio_expr e3 <= prio) (c_buf_expr indent)) e3

  | E_binary_conditional (e1,e2) ->
     bp buf "%a ? : %a"
        (bp_paren (prio_expr e1 <= prio) (c_buf_expr indent)) e1
        (bp_paren (prio_expr e2 <= prio) (c_buf_expr indent)) e2

  | E_array_subscript (e1,e2) ->
     bp buf "%a[%a]"
        (bp_paren (prio_expr e1 < prio) (c_buf_expr indent)) e1
        (c_buf_expr indent) e2

  | E_member_access (e1,_,n) ->
     c_buf_expr_unary prio indent buf "" e1 ("."^n)

  | E_arrow_access (e1,_,n) ->
     c_buf_expr_unary prio indent buf "" e1 ("->"^n)

  | E_compound_assign (e1,_,op,e2,_) ->
     c_buf_expr_binary_right prio indent buf "" e1 (" "^(string_of_binary_arithmetic op)^"= ") e2 ""

  | E_binary (op,e1,e2) ->
     c_buf_expr_binary_left prio indent buf "" e1 (" "^(string_of_binary_operator op)^" ") e2 ""

  | E_assign (e1,e2) ->
     c_buf_expr_binary_right prio indent buf "" e1 " = " e2 ""

  | E_comma (e1,e2) ->
     c_buf_expr_binary_left prio indent buf "" e1 ", " e2 ""

  | E_unary (op,e1) ->
     c_buf_expr_unary prio indent buf ((string_of_unary_operator op)^" ") e1 ""

  | E_increment (op,dir,e1) ->
     let inc = string_of_inc_direction op in
     let pre,post = if dir = PRE then inc,"" else "",inc in
     c_buf_expr_unary prio indent buf pre e1 post

   | E_address_of e1 ->
     c_buf_expr_unary prio indent buf "&" e1 ""

   | E_deref e1 ->
     c_buf_expr_unary prio indent buf "*" e1 ""

   | E_cast (e1,IMPLICIT) when ignore_implicit_casts ->
      c_buf_expr indent buf e1

   | E_cast (e1,ex) ->
      bp buf "(%a)%a"
         (c_buf_type_qual indent "") t
         (bp_paren (prio_expr e1 < prio) (c_buf_expr indent)) e1

   | E_call (e1,el) ->
      bp buf "%a(%a)"
         (bp_paren (prio_expr e1 < prio) (c_buf_expr indent)) e1
         (bp_array (fun buf ee -> bp_paren (is_comma ee) (c_buf_expr indent) buf ee) ", ") el

   | E_character_literal (c,_) ->
      if c >= Z.of_int 32 && c < Z.of_int 127
      then bp buf "'%a'" c_buf_char_literal (Char.chr (Z.to_int c))
      else Z.bprint buf c

   | E_integer_literal c ->
      Z.bprint buf c;
      bp_str buf (integer_suffix (as_int_type t))

   | E_float_literal s ->
      bp_str buf s;
      bp_str buf (float_suffix (as_float_type t))

   | E_string_literal (s,_) ->
      bp buf "\"%a\""c_buf_string_literal s

   | E_compound_literal i ->
      bp buf "(%a){ %a }"
         (c_buf_type_qual indent "") t
         (c_buf_init indent) i

   | E_variable v -> bp_str buf v.var_unique_name
   | E_function f -> bp_str buf f.func_unique_name
   | E_predefined s -> bp buf "/* predefined */ \"%a\""c_buf_string_literal s

   | E_statement b ->
      bp buf "({ %a })"
         (c_buf_statement_list indent) b.blk_stmts

   | E_var_args e1 ->
      bp buf "__builtin_va_arg(%a,%a)"
         (bp_paren (is_comma e1) (c_buf_expr indent)) e1 (c_buf_type indent "") (fst t)

   | E_atomic (op,el) ->
      bp buf "__atomic_%s(%a)"
         (c_atomic_op op)
         (bp_array (fun buf ee -> bp_paren (is_comma ee) (c_buf_expr indent) buf ee) ", ") el

   | E_convert_vector e ->
      bp buf "__builtin_convertvector(%a,%a)"
        (bp_paren (is_comma e) (c_buf_expr indent)) e
        (c_buf_type indent "") (fst t)

   | E_vector_element (e,s) ->
      bp buf "%a.%s"
        (bp_paren (is_comma e) (c_buf_expr indent)) e
        s

   | E_shuffle_vector ea ->
      bp buf "__builtin_suffle(%a)"
        (bp_array (fun buf ee -> bp_paren (is_comma ee) (c_buf_expr indent) buf ee) ", ") ea


and c_buf_expr_bool indent buf e =
  bp buf "%a" (bp_paren (prio_expr e == 1) (c_buf_expr indent)) e
(* add parentheses around assignments in boolean context to avoid warnings *)

and c_buf_char_literal buf c =
  let o = Char.code c in
  if c = '\n' then bp_str buf "\\n"
  else if c = '\\' then bp_str buf "\\\\"
  else if c = '"' then bp_str buf "\\\""
  else if c = '\'' then bp_str buf "\\\'"
  else if o = 0 then bp_str buf "\\0"
  else if o < 32 || o >= 127 then bp buf "\\x%02x" o
  else bp_char buf c

and c_buf_string_literal buf s =
  String.iter (c_buf_char_literal buf) s

and c_buf_init indent buf i = match i with
  | I_init_expr e -> c_buf_expr indent buf e
  | I_init_list (l,o) ->
     bp buf "{ %a }" (bp_list (c_buf_init indent) ", ") l
  | I_init_implicit tq ->
     bp_str buf "/*implicit*/"

and c_buf_statement_list indent buf l =
  List.iter (c_buf_statement indent buf) l

and c_buf_block indent buf blk = match blk.blk_stmts with
  | [] -> bp_str buf ";\n"
  (*  | [s] -> bp buf "\n%a" (c_buf_statement (inc_indent indent)) s*)
  | l -> bp buf "{\n%a%a%s}\n" (c_buf_statement_list (inc_indent indent)) l (c_buf_scope (inc_indent indent)) blk indent

and c_buf_scope indent buf blk =
  let p buf v = bp_str buf v.var_unique_name in
  if not print_scope || blk.blk_local_vars = [] then ()
  else bp buf "%s/* to remove: %a */\n" indent (bp_list p ",") blk.blk_local_vars

and c_buf_update buf upd =
  let p buf v = bp_str buf v.var_unique_name in
  if print_scope && upd.scope_var_removed <> []
  then bp buf " /* to remove: %a */" (bp_list p ",") upd.scope_var_removed;
  if print_scope && upd.scope_var_added <> []
  then bp buf " /* to add: %a */" (bp_list p ",") upd.scope_var_added

and c_buf_for_init indent buf b =
  match b.blk_stmts with
  | [] -> ()
  | [S_local_declaration v,_] -> c_buf_var_decl_inner indent buf v
  | [S_expression e,_] -> c_buf_expr indent buf e
  | _ -> c_buf_expr indent buf (E_statement b, (T_void, no_qual), empty_range)

and c_buf_statement indent buf ((s,r):statement) =
  let indent2 = inc_indent indent in
  match s with
  | S_local_declaration v ->
     c_buf_var_decl indent buf v

  | S_expression e ->
     bp buf "%s%a;\n" indent (c_buf_expr indent2) e

  | S_block l ->
     bp buf "%s{\n%a%a%s}\n" indent (c_buf_statement_list indent2) l.blk_stmts (c_buf_scope indent2) l indent

  | S_if (e1,b1, { blk_stmts=[] } ) ->
     bp buf "%sif (%a) %a"
        indent (c_buf_expr_bool indent2) e1
        (c_buf_block indent) b1

  | S_if (e1,b1,b2) ->
     bp buf "%sif (%a) %a%selse %a"
        indent (c_buf_expr_bool indent2) e1
        (c_buf_block indent) b1
        indent
        (c_buf_block indent) b2

  | S_while (e1,b1) ->
     bp buf "%swhile (%a) %a"
        indent (c_buf_expr_bool indent2) e1
        (c_buf_block indent) b1

  | S_do_while (b1,e1) ->
     bp buf "%sdo %a%swhile (%a);\n"
        indent (c_buf_block indent) b1
        indent (c_buf_expr_bool indent2) e1

  | S_for (b1,e1,e2,b2) ->
     bp buf "%sfor (%a; %a; %a) %a" indent
        (c_buf_for_init indent2) b1
        (bp_option "" (c_buf_expr_bool indent2)) e1
        (bp_option "" (c_buf_expr indent2)) e2
        (c_buf_block indent) b2

  | S_jump (S_goto (s,u)) -> bp buf "%sgoto %s;%a\n" indent s c_buf_update u
  | S_jump (S_break u) -> bp buf "%sbreak;%a\n" indent c_buf_update u
  | S_jump (S_continue u) -> bp buf "%scontinue;%a\n" indent c_buf_update u

  | S_jump (S_return (e1, u)) ->
     bp buf "%sreturn %a;%a\n"
        indent (bp_option "" (c_buf_expr indent2)) e1 c_buf_update u

  | S_jump (S_switch (e1,b1)) ->
     bp buf "%sswitch (%a) {\n%a%a%s}\n" indent
        (c_buf_expr indent2) e1 (c_buf_statement_list indent) b1.blk_stmts (c_buf_scope indent) b1 indent

  | S_target (S_label s) -> bp buf "%s%s:;\n" indent s

  | S_target (S_case ([e1], u)) ->
     bp buf "%scase %a:;%a\n"
        indent (c_buf_expr indent2) e1 c_buf_update u

  | S_target (S_case ([], u)) -> assert false

  | S_target (S_case (e1::tl, u)) ->
    bp buf "%scase %a:;%a\n"
      indent (c_buf_expr indent2) e1
      (c_buf_statement indent) (S_target (S_case (tl, u)), r)

  | S_target (S_default u) -> bp buf "%sdefault:;%a\n" indent c_buf_update u

  | S_asm a ->
     bp buf "%s__asm__%s%s(\"%a\" : %a %s %a %s %a %s %a);"
       indent
       (if a.asm_is_volatile then " __volatile__ " else "")
       (if Array.length a.asm_labels > 0 then " goto " else "")
       c_buf_string_literal a.asm_body
       (bp_array (fun buf o ->
            bp buf "\"%a\" (%a)"
              (* already declared in output_string? *)
              (* (match o.asm_output_constraint with *)
              (*  | ASM_OUTPUT_INOUT -> "+" *)
              (*  | ASM_OUTPUT_OUT   -> "=") *)
              c_buf_string_literal o.asm_output_string
              (c_buf_expr indent2) o.asm_output_expr
          ) ", "
       ) a.asm_outputs
       (if Array.length a.asm_inputs = 0 then "" else ":")
       (bp_array (fun buf o ->
            bp buf "\"%a\" (%a)"
              c_buf_string_literal o.asm_input_string
              (c_buf_expr indent2) o.asm_input_expr
          ) ", "
       ) a.asm_inputs
       (if Array.length a.asm_clobbers = 0 then "" else ":")
       (bp_array (fun buf c -> bp buf "\"%a\"" c_buf_string_literal c) ", ") a.asm_clobbers
       (if Array.length a.asm_labels = 0 then "" else ":")
       (bp_array (fun buf c -> bp buf "%s" c) ", ") a.asm_labels

and c_buf_com indent buf v =
  if !print_comments
  then List.iter (fun c -> bp buf "%s%s\n" indent c.Clang_AST.com_text) v

and c_buf_var_decl_inner indent buf v =
  let indent2 = inc_indent indent in
  bp buf "%s%a"
     (if variable_is_static v.var_kind then "static " else "")
     (c_buf_type_qual indent2 v.var_unique_name) v.var_type;
  (match v.var_init with
   | None -> ()
   | Some i -> bp buf " = %a" (c_buf_init indent2) i
  )

and c_buf_var_decl indent buf v =
  if variable_is_global v.var_kind
  then bp_loc indent buf v.var_range;
  c_buf_com indent buf v.var_com;
  bp buf "%s%a;\n" indent (c_buf_var_decl_inner indent) v

and c_buf_var_advance_decl indent buf v =
  if variable_is_global v.var_kind
  then bp_loc indent buf v.var_range;
  c_buf_com indent buf v.var_com;
  bp buf "%s%s%a;\n" indent
     (if v.var_kind = Variable_extern then "extern " else "")
     (c_buf_var_decl_inner indent) { v with var_init = None; }

and c_buf_func_decl indent buf f =
  let indent2 = inc_indent indent in
  bp_loc indent buf f.func_range;
  c_buf_com indent buf (List.map fst f.func_com);
  let variadic = if f.func_variadic then if f.func_parameters= [||] then "..." else ", ..." else ""
  and param buf v = c_buf_type_qual indent v.var_unique_name buf v.var_type in
  let inner buf var = bp buf "%a(%a%s)" bp_str var (bp_array param ", ") f.func_parameters variadic
  in
  if f.func_is_static then bp_str buf "static ";
  c_buf_type_base buf f.func_return;
  bp_str buf " ";
  c_buf_type_suffix buf f.func_unique_name indent false inner f.func_return;
  (match f.func_body with
   | None -> bp_str buf ";\n"
   | Some l -> bp buf "\n%s{\n%a%a%s}\n" indent (c_buf_statement_list indent2) l.blk_stmts (c_buf_scope indent2) l indent
  )

and c_buf_func_proto indent buf f =
  c_buf_func_decl indent buf { f with func_body = None; }

let c_buf_enum_decl indent buf e =
  let indent2 = inc_indent indent in
  let f buf v =
    c_buf_com indent buf v.enum_val_com;
    bp buf "%s%s = %a,\n" indent2 v.enum_val_unique_name Z.bprint v.enum_val_value
  in
  bp_loc indent buf e.enum_range;
  c_buf_com indent buf e.enum_com;
  if e.enum_defined then
    bp buf "%senum %s { /* type: %s */\n%a%s};\n" indent
       e.enum_unique_name
       (match e.enum_integer_type with
        | None -> "None"
        | Some s -> string_of_integer_type s)
       (bp_list f "") e.enum_values
       indent
  else
    bp buf "%senum %s;\n" indent e.enum_unique_name

let c_buf_record_decl indent buf r =
  let indent2 = inc_indent indent in
  let f buf v =
    c_buf_com indent buf v.field_com;
    bp buf "%s%a;\n" indent2
       (c_buf_type_qual indent2 v.field_name) v.field_type
  in
  bp_loc indent buf r.record_range;
  c_buf_com indent buf r.record_com;
  if r.record_defined then
    bp buf "%s%s %s { /* sizeof: %a, alignof: %a */\n%a%s};\n" indent
       (string_of_record_kind r.record_kind)
       r.record_unique_name
       Z.bprint r.record_sizeof
       Z.bprint r.record_alignof
       (bp_array f "") r.record_fields
       indent
  else
    bp buf "%s%s %s;\n" indent
       (string_of_record_kind r.record_kind)
       r.record_unique_name

let c_buf_typedef indent buf t =
  let indent2 = inc_indent indent in
  bp_loc indent buf t.typedef_range;
  c_buf_com indent buf t.typedef_com;
  bp buf "%stypedef %a;\n" indent
     (c_buf_type_qual indent2 t.typedef_unique_name) t.typedef_def


let string_of_var_decl = string_from_buffer (c_buf_var_decl "")
let string_of_var_advance_decl = string_from_buffer (c_buf_var_advance_decl "")
let string_of_func_decl = string_from_buffer (c_buf_func_decl "")
let string_of_func_proto = string_from_buffer (c_buf_func_proto "")
let string_of_expr = string_from_buffer (c_buf_expr "")
let string_of_type = string_from_buffer (c_buf_type "" "")
let string_of_type_qual = string_from_buffer (c_buf_type_qual "" "")
let string_of_string_literal = string_from_buffer c_buf_string_literal
let string_of_enum_decl = string_from_buffer (c_buf_enum_decl "")
let string_of_record_decl = string_from_buffer (c_buf_record_decl "")
let string_of_typedef = string_from_buffer (c_buf_typedef "")
let string_of_statement = string_from_buffer (c_buf_statement "")




(** {2 Full source printing} *)

let builtin_typedef =
  ["__NSConstantString"; "__builtin_va_list"; "__uint128_t"; "__u128"]
(* some built-in typedef we should not print *)

let builtin_funcs =
  ["__builtin_va_start"; "__builtin_va_end"; "__builtin_va_copy"; "__sigsetjmp";
   "_gl_verify_function2"; "_gl_verify_function3"; "_gl_verify_function4";
   "_gl_verify_function5"; "_gl_verify_function6"; "_gl_verify_function7";
   "_gl_verify_function8"; "_gl_verify_function9"; "_gl_verify_function10";
   "_gl_verify_function11"; "_gl_verify_function12"; "_gl_verify_function13";
   "_gl_verify_function14";
   "__builtin_mul_overflow";
   "__atomic_is_lock_free";
  ]
(* some built-in functions, the declaration of which we should omit when printing *)


let print_types_ordered
      (ch:out_channel)
      (td_omit:string list) (td:typedef StringMap.t)
      (re_omit:string list) (re:record_type StringMap.t) =
  let black, gray = Hashtbl.create 16, Hashtbl.create 16  in
  let rec typedef t =
    let rec explore t = match t with
      | T_typedef t -> typedef t
      | T_void | T_bool | T_integer _ | T_float _ | T_complex _ -> ()
      | T_pointer (t,_) -> explore t
      | T_array ((t,_),_) ->
         (* array elements must have a complete type even in typedefs *)
         explore t;
         let rec def = function
           | T_typedef tt -> def (fst tt.typedef_def)
           | T_record r -> record true r
           | T_array ((tt,_),_) -> def tt
           | _ -> ()
         in
         def t
      | T_bitfield (t,_) -> explore t
      | T_function None -> ()
      | T_function (Some f) ->
         explore (fst f.ftype_return);
         List.iter (fun (t,_) -> explore t) f.ftype_params
      | T_builtin_fn -> ()
      | T_record r -> ()
      | T_enum _ -> ()
      | T_vector v -> explore (fst v.vector_type)
      | T_unknown_builtin _ -> ()
    in
    if not (Hashtbl.mem black t.typedef_uid) then (
      if Hashtbl.mem gray t.typedef_uid then invalid_arg "cyclic type dependencies";
      Hashtbl.add gray t.typedef_uid ();
      if not (List.mem t.typedef_org_name td_omit) then (
        explore (fst t.typedef_def);
        output_string ch (string_of_typedef t);
      );
      Hashtbl.add black t.typedef_uid ()
    )
  and record mustdef r =
    let rec explore mustdef t = match t with
      | T_typedef t -> explore mustdef (fst t.typedef_def)
      | T_void | T_bool | T_integer _ | T_float _ | T_complex _ -> ()
      | T_pointer (t,_) -> explore false t
      | T_array ((t,_),_) -> explore mustdef t
      | T_bitfield (t,_) -> explore mustdef t
      | T_function None -> ()
      | T_function (Some f) ->
         explore true (fst f.ftype_return);
         List.iter (fun (t,_) -> explore true t) f.ftype_params
      | T_builtin_fn -> ()
      | T_record r -> if mustdef then record true r
      | T_enum _ -> ()
      | T_vector v -> explore mustdef (fst v.vector_type)
      | T_unknown_builtin _ -> ()
    in
    if not (Hashtbl.mem black r.record_uid) then (
      if Hashtbl.mem gray r.record_uid then invalid_arg "cyclic type dependencies";
      Hashtbl.add gray r.record_uid ();
      if not (List.mem r.record_org_name re_omit) then (
        Array.iter (fun f ->  explore mustdef (fst f.field_type)) r.record_fields;
        output_string ch (string_of_record_decl r)
      );
      Hashtbl.add black r.record_uid ();
    )
  in
  StringMap.iter (fun _ -> typedef) td;
  StringMap.iter (fun _ -> record true) re
(* internal function to print records and typedefs in correct order of dependency *)


let print_project ?(verbose = true) (ch:out_channel) (p:project) =
  let old_pl, old_pc = !print_loc, !print_comments in
  if not verbose then (
    print_loc := false;
    print_comments := false;
  );

  let pr f _ x = output_string ch (f x) in
  let pf = Printf.fprintf in

  (* types *)
  output_string ch "\n/* enum definitions  */\n\n";
  StringMap.iter (pr string_of_enum_decl) p.proj_enums;

  output_string ch "\n/* struct and union declarations */\n\n";
  StringMap.iter
    (fun _ r ->
      pf ch "%s %s;\n" (string_of_record_kind r.record_kind) r.record_unique_name
    ) p.proj_records;

  output_string ch "\n/* struct, union and typedefs */\n\n";
  print_types_ordered ch builtin_typedef p.proj_typedefs [] p.proj_records;

  (* variable declarations and prototype *)
  let vars = StringMap.filter (fun _ v -> variable_is_global v.var_kind) p.proj_vars in
  output_string ch "\n/* global variable declarations */\n\n";
  StringMap.iter (pr string_of_var_advance_decl) vars;

  output_string ch "\n/* function declarations */\n\n";
  let funcs = StringMap.filter (fun _ f -> not (List.mem f.func_org_name builtin_funcs)) p.proj_funcs in
  StringMap.iter (pr string_of_func_proto) funcs;

  (* variable with initializer and functions with bodies *)
  output_string ch "\n/* global variable definitions */\n\n";
  let vars = StringMap.filter (fun _ v -> v.var_init <> None) vars in
  StringMap.iter (pr string_of_var_decl) vars;

  output_string ch "\n/* functions definitions */\n\n";
  let funcs = StringMap.filter (fun _ v -> v.func_body <> None) funcs in
  StringMap.iter (pr string_of_func_decl) funcs;

  if not verbose then (
    print_loc := old_pl;
    print_comments := old_pc;
  )
