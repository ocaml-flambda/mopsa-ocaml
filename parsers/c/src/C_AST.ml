(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
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
  C_AST - Simpler C AST

  Provides a simpler version of the AST converted from Clang.

  The goal of this AST is to:
  - be independent from Clang's version
  - support linking of several translation units into a single AST
  - only support C for now
 *)

type uid = Clang_AST.uid
(** Unique identifiers (for variables and functions). *)

module UidMap = Map.Make(struct type t=uid let compare=compare end)

module StringMap = Map.Make(String)

type range = Clang_AST.range
module RangeMap = MapExt.Make(struct type t = range let compare = Stdlib.compare end)
(** Source locations. *)

type comment = Clang_AST.comment
(** Comments in file. *)

type macro = Clang_AST.macro
(** preprocessor macros *)
                
type character_kind = Clang_AST.character_kind

type target_info = Clang_AST.target_info


(** {2 Operators} *)

type binary_arithmetic =
  | ADD (** + *)
  | SUB (** - *)
  | MUL (** * *)
  | DIV (** / *)
  | MOD (** % *)
  | LEFT_SHIFT (** << *)
  | RIGHT_SHIFT (** >> *)
  | BIT_AND (** & *)
  | BIT_OR (** | *)
  | BIT_XOR (** ^ *)


type binary_logical =
  | LESS (** < *)
  | LESS_EQUAL (** <= *)
  | GREATER (** > *)
  | GREATER_EQUAL (** >= *)
  | EQUAL (** == *)
  | NOT_EQUAL (** != *)
  | LOGICAL_AND (** && *)
  | LOGICAL_OR (** || *)

type binary_operator =
  | O_arithmetic of binary_arithmetic
  | O_logical of binary_logical

type unary_operator =
  | NEG (** - *)
  | BIT_NOT (** ~ *)
  | LOGICAL_NOT (** ! *)

type explicitness =
  | EXPLICIT
  | IMPLICIT
(** Whether a cast is explicit (in the source) or implicit (added for implicit conversions *)

type inc_location =
  | PRE
  | POST
(** Whether an incrementation is performed before (PRE) or after (POST) the expression value is used *)

type inc_direction =
   | INC
   | DEC
(** Whether an incrementation is ++ or -- *)



(** {2 Types} *)

type signedness = SIGNED | UNSIGNED
(** Whether an integer type is signed or not. *)

type integer_type =
  | Char of signedness (** plain 'char', where the signeness is defined by the platform *)
  | SIGNED_CHAR | UNSIGNED_CHAR
  | SIGNED_SHORT | UNSIGNED_SHORT
  | SIGNED_INT | UNSIGNED_INT
  | SIGNED_LONG | UNSIGNED_LONG
  | SIGNED_LONG_LONG | UNSIGNED_LONG_LONG
  | SIGNED_INT128 | UNSIGNED_INT128
(** Integer types. *)

type float_type = FLOAT | DOUBLE | LONG_DOUBLE
(** Floating-point types. *)

type record_kind = STRUCT | UNION
(** Whether a record type is a struct or a union. *)

type qualifier = {
    qual_is_const: bool;
  }
(** Type qualifiers. For now, only 'const' is useful. *)

 (** Types. *)
type typ =
  | T_void
  (** Void type. *)

  | T_bool
  | T_integer of integer_type
  | T_float of float_type
  | T_complex of float_type
  | T_pointer of type_qual
  (** Scalar types. *)

  | T_array of type_qual * array_length
  (** Arrays. *)

  | T_bitfield of typ (* integer or enum type *) * int (* bit-size *)
  (** Bitfields, with bit-width, only used in struct. *)

  | T_function of function_type option
  (** Function, with or without a prototype *)

  | T_builtin_fn
  (** Bult-in functions *)

  | T_typedef of typedef
  (** Typedefs *)

  | T_record of record_type
  (** struct and union *)

  | T_enum of enum_type
 (** enums *)


 and type_qual = typ * qualifier
 (** Type with qualifier. *)

 and typedef = {
     typedef_org_name: string; (** name as in source *)
     mutable typedef_uid: uid;
     mutable typedef_unique_name: string; (** unique name *)
     mutable typedef_def: type_qual; (** declaration *)
     mutable typedef_range: range; (** declaration location *)
     mutable typedef_com: comment list; (** comments associated to the declaration *)
   }

 and record_type = {
     record_kind: record_kind;
     record_org_name: string; (** name as in source, may be empty *)
     mutable record_uid: uid; (** unique identifier *)
     mutable record_unique_name: string; (** unique, non-empty name *)
     mutable record_defined: bool; (** false if only declared *)
     mutable record_sizeof: Z.t;  (** size of record, in bytes *)
     mutable record_alignof: Z.t; (** alignment, in bytes *)
     mutable record_fields: record_field array;
     mutable record_range: range; (** declaration location *)
     mutable record_com: comment list; (** comments associated to the declaration *)
   }
 (** Struct or union type. *)

 and record_field = {
     field_uid: uid; (** unique identifier *)
     field_org_name: string; (** may be empty for anonymous or padding fields *)
     field_name: string; (** non-empty name *)
     field_offset: int;
     field_bit_offset: int;
     mutable field_type: type_qual;
     field_range: range; (** declaration location *)
     field_record: record_type;
     field_index: int;
     mutable field_com: comment list; (** comments associated to the declaration *)
   }
 (** Struct or union field. *)

 and enum_type = {
     enum_org_name: string; (** name as in source, may be empty *)
     mutable enum_uid: uid; (** unoque identifier *)
     mutable enum_unique_name: string; (** unique, non-empty name *)
     mutable enum_defined: bool; (** false if only declared *)
     mutable enum_values: enum_value list;
     mutable enum_integer_type: integer_type;
     mutable enum_range: range; (** declaration location *)
     mutable enum_com: comment list; (** comments associated to the declaration *)
   }
 (** Enumerated type. *)

 and enum_value = {
     enum_val_uid: uid; (** unique identifier *)
     enum_val_org_name: string; (** name as in source *)
     enum_val_unique_name: string; (** unique name *)
     enum_val_value: Z.t;
     enum_val_enum: enum_type;
     enum_val_range: range;
     mutable enum_val_com: comment list; (** comments associated to the declaration *)
   }
 (** A possible value in an enumerated type. *)

 and array_length =
   | No_length
   | Length_cst of Z.t
   | Length_expr of expr
 (** Length of an array. *)

 and function_type = {
     mutable ftype_return: type_qual;
     mutable ftype_params: type_qual list;
     ftype_variadic: bool;
   }
 (** Type of functions and prototypes. *)


 (** {2 Variables and functions} *)

 and variable = {
     var_uid: uid; (** unique identifier *)
     var_org_name: string; (** original name *)
     var_unique_name: string; (** unique name for globals and statics *)
     mutable var_kind: variable_kind; (** variable kind and life-time *)
     mutable var_type: type_qual;
     mutable var_init: init option;
     mutable var_range: range;
     mutable var_com: comment list; (** comments associated to the declaration *)
   }

 and variable_kind =
   | Variable_global (** global shared among translation units *)
   | Variable_extern (** declared but not defined *)
   | Variable_local of func (** local to a function *)
   | Variable_parameter of func (** formal argument *)
   | Variable_file_static of translation_unit (** restricted to a translation unit *)
   | Variable_func_static of func (** restricted to a function *)

 and func = {
     func_uid: uid; (** unique identifier *)
     func_org_name: string; (** original name *)
     func_unique_name: string; (** unique name for globals and statics *)
     func_is_static: bool;
     mutable func_return: type_qual; (** type of returned value *)
     mutable func_parameters: variable array; (** function parameters *)
     mutable func_body: block option; (** function body *)
     mutable func_static_vars: variable list; (** static variables declared in the function *)
     mutable func_local_vars: variable list; (** local variables declared in the function (excluding parameters) *)
     mutable func_variadic: bool; (** whether the has a variable number of arguments *)
     mutable func_range: range; (** range of the full declaration *)
     mutable func_name_range: range; (** range of the name part of the declaration *)
     mutable func_com: comment list; (** comments associated to the declaration *)
   }


 (** {2 Statements and expressions} *)

 and statement = statement_kind * range

 and block = {
     blk_stmts: statement list; (** statements in the block *)
     blk_local_vars: variable list; (** local variables declared within the block (excluding sub-blocks); these should be deallocated when exiting the block *)
   }

 and statement_kind =
   | S_local_declaration of variable (** local variable declaration *)

   | S_expression of expr (** expression statement *)

   | S_block of block (** statement block *)

   | S_if of expr (** condition *) * block (** then branch *) * block (** else branch *)
   (** if conditional *)

   | S_while of expr (** condition *) * block (** body *)
   (** while loop *)

   | S_do_while of block (** body *) * expr (** condition *)
   (** do-while loop *)

   | S_for of block (** init *) * expr option (** condition *) * expr option (** increment *) * block (** body *)
   (** for loop; the scope of the locals declared in the init block is the while for loop *)

   | S_jump of jump_kind
   (** jump instruction *)

   | S_target of target_kind
   (** target of a jump *)

 and jump_kind =
   | S_goto of string * scope_update
   | S_break of scope_update
   | S_continue of scope_update
   | S_return of expr option * scope_update
   | S_switch of expr * block
 (** various ways to jump *)

 and target_kind =
   | S_label of string
   | S_case of expr * scope_update
   | S_default of scope_update
 (** various targets of jumps *)

 and scope_update = {
     mutable scope_var_added:   variable list;
     mutable scope_var_removed: variable list;
   }
 (** variables to remove and to add (uninitialized) when jumping into
     another scope;
     may be attached to a jump source (break, continue, return, goto)
     or a jump target (switch case and default), depending on
     what's more convinient
  *)


 and expr = expr_kind * type_qual * range

 and expr_kind =
   | E_conditional of expr (** condition *) * expr (** then *) * expr (** else *)
   (** ?: ternary operator *)

   | E_binary_conditional of expr (** condition and then *) * expr (** else *)
   (** binary version of the ?: operator *)

   | E_array_subscript of expr (** array *) * expr (** index *)
   (** Array access. *)

   | E_member_access of expr (** record *) * int (** field index *) * string (** field *)
   (** record.field access *)

   | E_arrow_access of expr (** pointer *) * int (** field index *) * string (** field *)
   (** pointer->field access *)

   | E_compound_assign of
       expr (** lvalue *) * type_qual (** promoted type of lvalue before operation *) *
       binary_arithmetic (** operator *) *
       expr (** rvalue *) *
         type_qual (** type of the result, before converting back to lvalue type *)
   (** Assignment wth an operation: e1 += e2, etc. *)

   | E_binary of binary_operator * expr (** left argument *) * expr (** right argument *)
   (** Plain operation. *)

   | E_assign of expr (** lvalue *) * expr (** rvalue *)
   (** Plain assignment *)

   | E_comma of expr * expr (** , operator *)

   | E_unary of unary_operator * expr

   | E_increment of inc_direction * inc_location * expr

   | E_address_of of expr
   (** & operator (address of lvalue) *)

   | E_deref of expr
   (** * operator (pointer dereference) *)

   | E_cast of expr * explicitness

   | E_call of expr (** target function *) * expr array (** actual arguments *)


   | E_character_literal of Z.t * character_kind (** literal character *)
   | E_integer_literal of Z.t (** literal integer *)
   | E_float_literal of string (** literal float, in binary string notation *)
   | E_string_literal of string * character_kind

   | E_compound_literal of init

   | E_variable of variable
   | E_function of func
   | E_predefined of string (** predefined identifier *)

   | E_statement of block (** a block of statements viewed as an expression (GCC extension) *)

   | E_var_args of expr (** __builtin_va_arg *)

   | E_atomic of int (** operation *) * expr * expr

 and init =
  | I_init_expr of expr
  | I_init_list of init list (** specified elements *) * init option (** filler *)
  | I_init_implicit of type_qual


 and translation_unit = {
     tu_uid: int;
     tu_name: string;
     tu_range: range;
   }

 and project = {
     proj_name: string; (** project name *)
     proj_tu: translation_unit list; (** translation units composing the project *)
     proj_target: target_info;

     proj_typedefs: typedef StringMap.t; (** typedefs, by unique name *)
     proj_enums: enum_type StringMap.t; (** enums, by unique name *)
     proj_records: record_type StringMap.t; (** records, by unique name *)
     proj_vars: variable StringMap.t; (** variables with global lifetime, by unique name *)
     proj_funcs: func StringMap.t; (** functions, by unique name *)

     proj_comments: comment list RangeMap.t; (** all comments *)
     proj_macros : macro StringMap.t; (** macros, by name *)
   }
(** A project is a set of translation units linked together. *)



 (** {2 Useful definitions} *)

let no_qual = { qual_is_const = false; }
(** No qualifier. *)

let int_type = T_integer SIGNED_INT, no_qual
let bool_type = T_bool, no_qual
let void_ptr_type = T_pointer (T_void,no_qual), no_qual
let uchar_ptr_type = T_pointer (T_integer UNSIGNED_CHAR,no_qual), no_qual


let merge_qualifiers (q1:qualifier) (q2:qualifier) : qualifier =
  { qual_is_const = q1.qual_is_const || q2.qual_is_const; }


let rec resolve_typedef ((t,q):type_qual) : type_qual =
  match t with
  | T_typedef t ->
     let tt,qq = resolve_typedef t.typedef_def in
     tt, merge_qualifiers q qq
  | _ -> t,q
(** Replace toplevel occurences of typedefs with their definition. *)

let rec as_int_type (tq:type_qual) : integer_type =
  match fst (resolve_typedef tq) with
  | T_integer i -> i
  | T_enum e -> e.enum_integer_type
  | T_bool -> SIGNED_INT
  | _ -> invalid_arg "as_int_type: not an integer type"
(** Interprets the type as an integer type, if possible *)

let rec as_float_type (tq:type_qual) : float_type =
  match fst (resolve_typedef tq) with
  | T_float f -> f
  | _ -> invalid_arg "as_float_type: not a float type"
(** Interprets the type as an integer type, if possible *)


let rec type_is_scalar (t:typ) =
  match t with
  | T_void -> false
  | T_bool | T_integer _ | T_float _ | T_pointer _ -> true
  | T_array _ -> false
  | T_bitfield _ -> true
  | T_function _ -> false
  | T_builtin_fn -> false
  | T_typedef t -> type_is_scalar (fst t.typedef_def)
  | T_enum _ -> true
  | T_record _ -> false
  | T_complex _ -> false
(** Whether a type yields a scalar value. *)



(** {2 Variable utilities} *)

let variable_is_global kind =
  match kind with
  | Variable_extern | Variable_global | Variable_file_static _ | Variable_func_static _ -> true
  | Variable_local _ | Variable_parameter _ -> false
(** Whether variables of this kind have global lifetime or not. *)

let variable_is_static kind =
  match kind with
  | Variable_file_static _ | Variable_func_static _ -> true
  | Variable_extern | Variable_global | Variable_local _ | Variable_parameter _ -> false
(** Whether variables of this kind are static or not. *)

let variable_is_file_static kind =
  match kind with
  | Variable_file_static _ -> true
  | _ -> false
(** Whether variables of this kind are file-level static. *)
