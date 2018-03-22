(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** AST of the C language. *)

open Framework.Ast
open Framework.Visitor
open Universal.Ast


(*==========================================================================*)
                           (** {2 Types} *)
(*==========================================================================*)

type c_typedef = {
  c_typedef_org_name: string; (** name as in source *)
  c_typedef_unique_name: string; (** unique name *)
  c_typedef_def: typ; (** declaration *)
  c_typedef_range: range; (** declaration location *)
}
(** Type definition. *)

and c_record_kind = C_struct | C_union
(** Whether a record is struct or union. *)

and c_record_type = {
  c_record_kind: c_record_kind;
  c_record_org_name: string; (** name as in source, may be empty *)
  c_record_unique_name: string; (** unique, non-empty name *)
  c_record_defined: bool; (** false if only declared *)
  c_record_sizeof: Z.t;  (** size of record, in bytes *)
  c_record_alignof: Z.t; (** alignment, in bytes *)
  c_record_fields: c_record_field list;
  c_record_range: range; (** declaration location *)
}
(** Struct or union type. *)

and c_record_field = {
  c_field_org_name: string; (** may be empty for anonymous or padding fields *)
  c_field_name: string; (** non-empty name *)
  c_field_offset: int;
  c_field_bit_offset: int;
  c_field_type: typ;
  c_field_range: range; (** declaration location *)
  c_field_record: c_record_type;
  c_field_index: int;
}
(** Struct or union field. *)

and c_enum_type = {
  c_enum_org_name: string; (** name as in source, may be empty *)
  c_enum_unique_name: string; (** unique, non-empty name *)
  c_enum_defined: bool; (** false if only declared *)
  c_enum_values: c_enum_value list;
  c_enum_integer_type: c_integer_type;
  c_enum_range: range; (** declaration location *)
}
(** Enumerated type. *)

and c_enum_value = {
  c_enum_val_org_name: string; (** name as in source *)
  c_enum_val_unique_name: string; (** unique name *)
  c_enum_val_value: Z.t;
  c_enum_val_enum: c_enum_type;
}
(** A possible value in an enumerated type. *)


and c_signedness = C_signed | C_unsigned
(** Whether an integer type is signed or not. *)

and c_integer_type =
  | C_char of c_signedness (** plain 'char', where the signeness is defined by the platform *)
  | C_signed_char
  | C_unsigned_char
  | C_signed_short
  | C_unsigned_short
  | C_signed_int
  | C_unsigned_int
  | C_signed_long
  | C_unsigned_long
  | C_signed_long_long
  | C_unsigned_long_long
  | C_signed_int128
  | C_unsigned_int128
  (** Integer types. *)

and c_float_type = C_float | C_double | C_long_double
(** Floating-point types. *)


and c_array_length =
  | C_array_no_length
  | C_array_length_cst of Z.t
  | C_array_length_expr of expr
(** Cases of arrays length. *)  

and c_qual = {
  c_qual_is_const: bool;
  c_qual_is_volatile: bool;
  c_qual_is_restrict: bool;
}
(** Type qualifiers. *)

and c_function_type = {
  c_ftype_return: typ;
  c_ftype_params: typ list;
  c_ftype_variadic: bool;
}
(** Function type. *)

type typ +=
  | T_c_void
  (** Void type. *)

  | T_c_integer of c_integer_type
  | T_c_float of c_float_type
  | T_c_pointer of typ
  (** Scalar types. *)

  | T_c_array of typ * c_array_length
  (** Arrays. *)

  | T_c_bitfield of
      typ (* integer or enum type *) *
      int (* bit-size *)
  (** Bitfields, with bit-width, only used in struct. *)

  | T_c_function of c_function_type option
  (** Function, with or without a prototype *)

  | T_c_builtin_fn
  (** Built-in functions *)

  | T_c_typedef of c_typedef
  (** Typedefs *)

  | T_c_record of c_record_type
  (** struct and union *)

  | T_c_enum of c_enum_type
  (** enums *)

  | T_c_qualified of c_qual * typ
  (** Qualified type. *)

(*==========================================================================*)
                           (** {2 Expressions} *)
(*==========================================================================*)

type c_inc_location =
  | PRE
  | POST
  (** Whether an incrementation is performed before (PRE) or after (POST) the expression value is used *)

type c_inc_direction =
  | INC
  | DEC
  (** Whether an incrementation is ++ or -- *)

type constant +=
  | C_c_character of char
  (** Constant character *)

type c_init =
  | C_init_expr of expr
  | C_init_list of c_init list (** specified elements *) * c_init option (** filler *)
  | C_init_implicit of typ
(** Variable initialization. *)

type c_fundec = {
  c_func_var: var; (** function name variable with unique identifier *)
  c_func_is_static: bool;
  c_func_return: typ; (** type of returned value *)
  c_func_parameters: var list; (** function parameters *)
  c_func_body: stmt; (** function body *)
  c_func_static_vars: (var * c_init option) list; (** static variables declared in the function and their initialization *)
  c_func_local_vars: (var * c_init option) list; (** local variables declared in the function (exclusing parameters) and their initialization *)
  c_func_variadic: bool; (** whether the function has a variable number of arguments *)
}
(** Function descriptor. *)

type expr_kind +=
  | E_c_conditional of expr (** condition *) * expr (** then *) * expr (** else *)
  (** ?: ternary operator *)

  | E_c_array_subscript of expr (** array *) * expr (** index *)
  (** Array access. *)

  | E_c_member_access of expr (** record *) * int (** field index *) * string (** field *)
  (** record.field access *)

  | E_c_function of c_fundec

  | E_c_arrow_access of expr (** pointer *) * int (** field index *) * string (** field *)
  (** pointer->field access *)

  | E_c_assign of expr (** lvalue *) * expr (** rvalue*)
  (** Assignment as an expression *)

  | E_c_compound_assign of
      expr (** lvalue *) * typ (** promoted type of lvalue before operation *) *
      operator (** operator *) *
      expr (** rvalue *) *
      typ (** type of the result, before converting back to lvalue type *)
  (** Assignment with an operation: e1 += e2, etc. *)

  | E_c_comma of expr * expr (** , operator *)

  | E_c_increment of c_inc_direction * c_inc_location * expr

  | E_c_address_of of expr
  (** & operator (address of lvalue) *)

  | E_c_deref of expr
  (** * operator (pointer dereference) *)

  | E_c_cast of expr * bool (** explicitness *)
  (** casted expression *)

  | E_c_predefined of string (** predefined identifier *)

  | E_c_var_args of expr (** __builtin_va_arg *)

  | E_c_atomic of int (** operation *) * expr * expr


(*==========================================================================*)
                           (** {2 Statements} *)
(*==========================================================================*)

type c_program = {
  c_program_global_variables : (var * c_init option) list;
  c_program_functions : c_fundec list;
}
(** Program descriptor. *)

type stmt_kind +=
  | S_c_local_declaration of var
  (** declaration of a local variable *)
      
  | S_c_do_while of
      stmt (** body *) *
      expr (** condition *)
  (** do-while loop *)

  | S_c_for of
      stmt (** init *) *
      expr option (** condition *) *
      expr option (** increment *) *
      stmt (** body *)
  (** for loop; the scope of the locals declared in the init block
      is the while for loop *)

  | S_c_goto of string
  (** goto statements. *)

  | S_c_switch of expr * stmt
  (** switch statement. *)

  | S_c_labeled_stmt of string * stmt
  (** labeled statements. *)

  | S_c_switch_case of expr * stmt
  (** case of a switch statement. *)

  | S_c_switch_default of stmt
  (** default case of switch statements. *)


type program_kind +=
  | C_program of c_program
  (** A complete C program. *)
      


(*==========================================================================*)
                  (** {2 Conversion to Clang parser types} *)
(*==========================================================================*)

let rec to_clang_type : typ -> C_AST.type_qual = function
  | T_c_void -> C_AST.T_void, C_AST.no_qual
  | Universal.Ast.T_bool -> C_AST.T_bool, C_AST.no_qual
  | T_c_integer(i) -> C_AST.T_integer (to_clang_int_type i), C_AST.no_qual
  | T_c_float(f) -> C_AST.T_float (to_clang_float_type f), C_AST.no_qual
  | T_c_pointer(t) -> C_AST.T_pointer (to_clang_type t), C_AST.no_qual
  | T_c_array(t, array_length) -> C_AST.T_array (to_clang_type t, to_clang_array_length array_length), C_AST.no_qual
  | T_c_bitfield(t, size) -> C_AST.T_bitfield (fst @@ to_clang_type t, size), C_AST.no_qual
  | T_c_function(None) -> C_AST.T_function None, C_AST.no_qual
  | T_c_builtin_fn -> C_AST.T_builtin_fn, C_AST.no_qual
  | T_c_typedef(typedef) -> C_AST.T_typedef (to_clang_typedef typedef), C_AST.no_qual
  | T_c_record(record) -> C_AST.T_record (to_clang_record_type record), C_AST.no_qual
  | T_c_enum(enum) -> C_AST.T_enum (to_clang_enum_type enum), C_AST.no_qual
  | T_c_qualified(qual, t) ->
    let (t, other_qual) = to_clang_type t in
    let qual = to_clang_type_qualifier qual in
    let q = C_AST.merge_qualifiers qual other_qual in
    t,  q
  | _ -> assert false

and to_clang_type_qualifier : c_qual -> C_AST.qualifier = fun qual ->
  {
    C_AST.qual_is_const = qual.c_qual_is_const;
  }
  
  

and to_clang_int_type : c_integer_type -> C_AST.integer_type = function
  | C_char(C_signed) -> C_AST.Char (C_AST.SIGNED)
  | C_char(C_unsigned) -> C_AST.Char (C_AST.UNSIGNED)
  | C_signed_char -> C_AST.SIGNED_CHAR
  | C_unsigned_char -> C_AST.UNSIGNED_CHAR
  | C_signed_short -> C_AST.SIGNED_SHORT
  | C_unsigned_short -> C_AST.UNSIGNED_SHORT
  | C_signed_int -> C_AST.SIGNED_INT
  | C_unsigned_int -> C_AST.UNSIGNED_INT
  | C_signed_long -> C_AST.SIGNED_LONG
  | C_unsigned_long -> C_AST.UNSIGNED_LONG
  | C_signed_long_long -> C_AST.SIGNED_LONG_LONG
  | C_unsigned_long_long -> C_AST.UNSIGNED_LONG_LONG
  | C_signed_int128 -> C_AST.SIGNED_INT128
  | C_unsigned_int128 -> C_AST.UNSIGNED_INT128

and to_clang_float_type : c_float_type -> C_AST.float_type = function
  | C_float -> C_AST.FLOAT
  | C_double -> C_AST.DOUBLE
  | C_long_double -> C_AST.LONG_DOUBLE

and to_clang_array_length : c_array_length -> C_AST.array_length = function
  | C_array_no_length -> C_AST.No_length
  | C_array_length_cst(n) -> C_AST.Length_cst n
  | C_array_length_expr(e) -> assert false

and to_clang_typedef : c_typedef -> C_AST.typedef = fun typedef ->
  {
    C_AST.typedef_org_name = typedef.c_typedef_org_name;
    typedef_uid = -1;
    typedef_unique_name = typedef.c_typedef_unique_name;
    typedef_def = to_clang_type typedef.c_typedef_def;
    typedef_range = to_clang_range typedef.c_typedef_range;
  }

and to_clang_record_type : c_record_type -> C_AST.record_type = fun record ->
  {
    C_AST.record_kind = to_clang_record_kind record.c_record_kind;
    record_org_name = record.c_record_org_name;
    record_uid = -1;
    record_unique_name = record.c_record_unique_name;
    record_defined = record.c_record_defined;
    record_sizeof = record.c_record_sizeof;
    record_alignof = record.c_record_alignof;
    record_fields = List.map to_clang_record_field record.c_record_fields |>
                    Array.of_list;
    record_range = to_clang_range record.c_record_range;
  }

and to_clang_record_kind : c_record_kind -> C_AST.record_kind = function
  | C_struct -> C_AST.STRUCT
  | C_union -> C_AST.UNION
                 
and to_clang_record_field : c_record_field -> C_AST.record_field = fun field ->
  {
    C_AST.field_uid = -1;
    field_org_name = field.c_field_org_name;
    field_name = field.c_field_name;
    field_offset = field.c_field_offset;
    field_bit_offset = field.c_field_bit_offset;
    field_type = to_clang_type field.c_field_type;
    field_range = to_clang_range field.c_field_range;
    field_record = to_clang_record_type field.c_field_record;
    field_index = field.c_field_index;
  }

and to_clang_enum_type : c_enum_type -> C_AST.enum_type = fun enum ->
  {
    C_AST.enum_org_name = enum.c_enum_org_name;
    enum_uid = -1;
    enum_unique_name = enum.c_enum_unique_name;
    enum_defined = enum.c_enum_defined;
    enum_values = List.map to_clang_enum_value enum.c_enum_values;
    enum_integer_type = to_clang_int_type enum.c_enum_integer_type;
    enum_range = to_clang_range enum.c_enum_range;
  }

and to_clang_enum_value : c_enum_value -> C_AST.enum_value = fun enum_val ->
  {
    C_AST.enum_val_uid = -1;
    enum_val_org_name = enum_val.c_enum_val_org_name;
    enum_val_unique_name = enum_val.c_enum_val_unique_name;
    enum_val_value = enum_val.c_enum_val_value;
    enum_val_enum = to_clang_enum_type enum_val.c_enum_val_enum;
  }

and to_clang_range (range: Framework.Ast.range) : Clang_AST.range =
  let origin_range = Framework.Ast.get_origin_range range in
  {
    Clang_AST.range_begin = {
      Clang_AST.loc_file = origin_range.range_begin.loc_file;
      loc_line = origin_range.range_begin.loc_line;
      loc_column = origin_range.range_begin.loc_column;
    };
    Clang_AST.range_end = {
      Clang_AST.loc_file = origin_range.range_end.loc_file;
      loc_line = origin_range.range_end.loc_line;
      loc_column = origin_range.range_end.loc_column;
    };
  }
  



(*==========================================================================*)
                  (** {2 Sizes and alignments} *)
(*==========================================================================*)

(** [sizeof t] computes the size (in bytes) of a C type [t] *)
let sizeof_type (t : typ) : Z.t =
  let target_options = Clang_parser.get_default_target_options () in
  let target_info = Clang_parser.get_target_info target_options in
  let t, _ = to_clang_type t in
  C_utils.sizeof_type target_info t

let sizeof_expr (t:typ) range : expr =
  let rec doit t =
    match t with
    | T_c_void -> invalid_arg "sizeof_expr: size of void"
    | Universal.Ast.T_bool | T_c_integer _ | T_c_float _ | T_c_pointer _ | T_c_record _ | T_c_enum _ ->
       mk_z (sizeof_type t) range
    | T_c_array (t,l) ->
       let len = match l with
         | C_array_length_cst len -> mk_z len range
         | C_array_length_expr e -> e
         | C_array_no_length ->
            (* TODO: fix *)
            (* error range "sizeof" "array with no size"*)
            mk_zero range
       in
       mk_binop (doit t) O_mult len range
    | T_c_bitfield (t,_) -> invalid_arg "sizeof_expr: size of bitfield"
    | T_c_function _ | T_c_builtin_fn -> invalid_arg "sizeof_expr: size of function"
    | T_c_typedef t -> doit (t.c_typedef_def)
    | _ -> assert false
  in
  doit t
(** Size (in bytes) of a type, as an expression. Handles variable-length ararys. *)


(** [is_signed t] whether [t] is signed *)    
let rec is_signed (t : typ) : bool=
  match t with
  | T_c_integer it ->
     begin
       match it with
       | C_char C_signed | C_signed_char | C_signed_short | C_signed_int
       | C_signed_long | C_signed_long_long | C_signed_int128 -> true
       | _ -> false
     end
  | T_c_qualified(_, t) -> is_signed t
  | _ -> failwith "[issigned] not an integer type"

(** [range t] computes the interval range of type [t] *)
let range (t : typ) =
  let part = 8*Z.to_int (sizeof_type t) in
  if is_signed t then
    let part' = Z.pow (Z.of_int (2)) (part -1) in
    ( Z.neg part', Z.sub part' (Z.of_int 1))
  else
    let part' = Z.pow (Z.of_int 2) part in
    ( Z.of_int 0 , Z.sub part' (Z.of_int 1))

(** [range t] computes the interval range of type [t] *)
let range_int t =
  let a,b = range t in
  (Z.to_int a, Z.to_int b)

(** [warp v (l,h)] expression needed to bring back [v] in range ([l],[h]) *)
let warp (v : Universal.Ast.var) ((l,h) : int * int) range : Framework.Ast.expr =
  let open Universal.Ast in
  mk_binop
    (mk_int 1 (tag_range range "1"))
    O_plus
    (mk_binop
       (mk_binop
          (mk_var v (tag_range range "v"))
          O_minus
          (mk_int l (tag_range range "l"))
          (tag_range range "?")
       )
       O_mod
       (mk_int h (tag_range range "h"))
       (tag_range range "??")
    )
    range

(** [is_inttype t] wheter [t] is an integer type *)
let rec is_inttype ( t : typ) =
  match t with
  | T_c_integer _ -> true
  | T_c_qualified(_, t) -> is_inttype t
  | _ -> false

(** [is_scalartype t] wheter [t] is a scalar type *)
let is_scalartype ( t : typ) =
  to_clang_type t |>
  fst |>
  C_AST.type_is_scalar

(** [is_pointer t] wheter [t] is a pointer *)
let rec is_pointer ( t : typ) =
  match t with
  | T_c_pointer _ -> true
  | T_c_qualified(_, t) -> is_pointer t
  | _ -> false

(** [is_scalartype t] lifts [t] to a pointer to [t] *)
let pointer_type (t : typ) =
  (T_c_pointer t)

(** [is_scalartype t] types pointed to by [t] fails if [t] is not a
   pointer type *)
let under_type (t : typ) : typ =
  match t with
  | T_c_pointer t' -> t'
  | _ -> failwith "[under_type] called with a non pointer argument"
