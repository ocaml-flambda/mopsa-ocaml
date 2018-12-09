(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** AST of the C language. *)

open Framework
open Framework.Ast
open Framework.Visitor
open Universal.Ast
open Framework.Essentials

(*==========================================================================*)
                           (** {2 Types} *)
(*==========================================================================*)

type c_typedef = {
  c_typedef_org_name: string; (** name as in source *)
  c_typedef_unique_name: string; (** unique name *)
  mutable c_typedef_def: typ; (** declaration *)
  c_typedef_range: Location.range; (** declaration location *)
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
  mutable c_record_fields: c_record_field list;
  c_record_range: Location.range; (** declaration location *)
}
(** Struct or union type. *)

and c_record_field = {
  c_field_org_name: string; (** may be empty for anonymous or padding fields *)
  c_field_name: string; (** non-empty name *)
  c_field_offset: int;
  c_field_bit_offset: int;
  c_field_type: typ;
  c_field_range: Location.range; (** declaration location *)
  c_field_index: int;
}
(** Struct or union field. *)

and c_enum_type = {
  c_enum_org_name: string; (** name as in source, may be empty *)
  c_enum_unique_name: string; (** unique, non-empty name *)
  c_enum_defined: bool; (** false if only declared *)
  c_enum_values: c_enum_value list;
  c_enum_integer_type: c_integer_type;
  c_enum_range: Location.range; (** declaration location *)
}
(** Enumerated type. *)

and c_enum_value = {
  c_enum_val_org_name: string; (** name as in source *)
  c_enum_val_unique_name: string; (** unique name *)
  c_enum_val_value: Z.t;
  c_enum_val_range: range;
}
(** A possible value in an enumerated type. *)


and c_integer_type =
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

type operator +=
  | O_c_and
  | O_c_or

type c_inc_location =
  | PRE
  | POST
  (** Whether an incrementation is performed before (PRE) or after (POST) the expression value is used *)

type c_inc_direction =
  | INC
  | DEC
  (** Whether an incrementation is ++ or -- *)

type c_character_kind =
  | C_char_ascii
  | C_char_wide
  | C_char_utf8
  | C_char_utf16
  | C_char_utf32

type constant +=
  | C_c_character of Z.t * c_character_kind
  (** Constant character *)

  | C_c_string of string * c_character_kind
  (** Constant string literal *)

  | C_c_invalid
  (** Invalid pointer value *)


type c_init =
  | C_init_expr of expr
  | C_init_list of c_init list (** specified elements *) * c_init option (** filler *)
  | C_init_implicit of typ
(** Variable initialization. *)

type c_fundec = {
  mutable c_func_var: var; (** function name variable with unique identifier *)
  c_func_is_static: bool;
  c_func_range: range; (** range of the function *)
  mutable c_func_return: typ; (** type of returned value *)
  mutable c_func_parameters: var list; (** function parameters *)
  mutable c_func_body: stmt option; (** function body *)
  mutable c_func_static_vars: (var * c_init option * range) list; (** static variables declared in the function and their initialization *)
  mutable c_func_local_vars: (var * c_init option * range) list; (** local variables declared in the function (exclusing parameters) and their initialization *)
  mutable c_func_stub : Stubs.Ast.stub with_range option; (** stub specification of the function *)
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

  | E_c_builtin_function of string

  | E_c_call of expr (** target *) * expr list (** arguments *)

  | E_c_builtin_call of string * expr list

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

  | E_c_statement of stmt

  | E_c_predefined of string (** predefined identifier *)

  | E_c_var_args of expr (** __builtin_va_arg *)

  | E_c_atomic of int (** operation *) * expr * expr

(*==========================================================================*)
                           (** {2 Statements} *)
(*==========================================================================*)

type stmt_kind +=
  | S_c_goto_stab of stmt
  (** stabilization point for goto statement *)

  | S_c_global_declaration of var * c_init option
  (** declaration of a global variable with optional initialization *)

  | S_c_local_declaration of var * c_init option
  (** declaration of a local variable with optional initialization *)

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

  | S_c_label of string
  (** statement label. *)

  | S_c_switch_case of expr
  (** case of a switch statement. *)

  | S_c_switch_default
  (** default case of switch statements. *)


type program +=
  | C_program of
      (var * c_init option * range) list (** global variables *) *
      c_fundec list (** functions *)
(** A complete C program. *)


(*==========================================================================*)
                  (** {2 Conversion to Clang parser types} *)
(*==========================================================================*)

let to_clang_int_type : c_integer_type -> C_AST.integer_type = function
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

let to_clang_float_type : c_float_type -> C_AST.float_type = function
  | C_float -> C_AST.FLOAT
  | C_double -> C_AST.DOUBLE
  | C_long_double -> C_AST.LONG_DOUBLE

                   
    (* NOTE: this may cause issue with recutsive types

let rec to_clang_type : typ -> C_AST.type_qual = function
  | T_c_void -> C_AST.T_void, C_AST.no_qual
  | T_c_integer(i) -> C_AST.T_integer (to_clang_int_type i), C_AST.no_qual
  | T_c_float(f) -> C_AST.T_float (to_clang_float_type f), C_AST.no_qual
  | T_c_pointer(t) -> C_AST.T_pointer (to_clang_type t), C_AST.no_qual
  | T_c_array(t, array_length) -> C_AST.T_array (to_clang_type t, to_clang_array_length array_length), C_AST.no_qual
  | T_c_bitfield(t, size) -> C_AST.T_bitfield (fst @@ to_clang_type t, size), C_AST.no_qual
  | T_c_function(f) -> C_AST.T_function(to_clang_function_type_option f), C_AST.no_qual
  | T_c_builtin_fn -> C_AST.T_builtin_fn, C_AST.no_qual
  | T_c_typedef(typedef) -> C_AST.T_typedef (to_clang_typedef typedef), C_AST.no_qual
  | T_c_record(record) -> C_AST.T_record (to_clang_record_type record), C_AST.no_qual
  | T_c_enum(enum) -> C_AST.T_enum (to_clang_enum_type enum), C_AST.no_qual
  | T_c_qualified(qual, t) ->
    let (t, other_qual) = to_clang_type t in
    let qual = to_clang_type_qualifier qual in
    let q = C_AST.merge_qualifiers qual other_qual in
    t,  q
  | t ->
    Exceptions.panic "to_clang_type: %a not a C type" pp_typ t

and to_clang_type_qualifier : c_qual -> C_AST.qualifier = fun qual ->
  {
    C_AST.qual_is_const = qual.c_qual_is_const;
  }


and to_clang_function_type : c_function_type -> C_AST.function_type =
  fun f -> {
      C_AST.ftype_return = to_clang_type f.c_ftype_return;
      ftype_params = List.map to_clang_type f.c_ftype_params;
      ftype_variadic = f.c_ftype_variadic;
    }

and to_clang_function_type_option = function
  | None -> None
  | Some t -> Some (to_clang_function_type t)

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
    typedef_com = [];
  }

and to_clang_record_type : c_record_type -> C_AST.record_type = fun record ->
  let c_record = {
    C_AST.record_kind = to_clang_record_kind record.c_record_kind;
    record_org_name = record.c_record_org_name;
    record_uid = -1;
    record_unique_name = record.c_record_unique_name;
    record_defined = record.c_record_defined;
    record_sizeof = record.c_record_sizeof;
    record_alignof = record.c_record_alignof;
    record_fields = [||];
    record_range = to_clang_range record.c_record_range;
    record_com = [];
  } in
  c_record.C_AST.record_fields <- Array.map (fun field ->
      to_clang_record_field c_record field
    ) (Array.of_list record.c_record_fields);
  c_record

and to_clang_record_kind : c_record_kind -> C_AST.record_kind = function
  | C_struct -> C_AST.STRUCT
  | C_union -> C_AST.UNION

and to_clang_record_field : C_AST.record_type -> c_record_field -> C_AST.record_field = fun record field ->
  {
    C_AST.field_uid = -1;
    field_org_name = field.c_field_org_name;
    field_name = field.c_field_name;
    field_offset = field.c_field_offset;
    field_bit_offset = field.c_field_bit_offset;
    field_type = to_clang_type field.c_field_type;
    field_range = to_clang_range field.c_field_range;
    field_record = record;
    field_index = field.c_field_index;
    field_com = [];
  }

and to_clang_enum_type : c_enum_type -> C_AST.enum_type = fun enum ->
  let c_enum = {
    C_AST.enum_org_name = enum.c_enum_org_name;
    enum_uid = -1;
    enum_unique_name = enum.c_enum_unique_name;
    enum_defined = enum.c_enum_defined;
    enum_values = [];
    enum_integer_type = to_clang_int_type enum.c_enum_integer_type;
    enum_range = to_clang_range enum.c_enum_range;
    enum_com = [];
  }
  in
  c_enum.C_AST.enum_values <- List.map (fun v ->
      to_clang_enum_value v c_enum
    ) enum.c_enum_values;
  c_enum

and to_clang_enum_value : c_enum_value -> C_AST.enum_type -> C_AST.enum_value = fun enum_val enum ->
  {
    C_AST.enum_val_uid = -1;
    enum_val_org_name = enum_val.c_enum_val_org_name;
    enum_val_unique_name = enum_val.c_enum_val_unique_name;
    enum_val_value = enum_val.c_enum_val_value;
    enum_val_enum = enum;
    enum_val_range = to_clang_range enum_val.c_enum_val_range;
    enum_val_com = [];
  }


and to_clang_range (range: Framework.Location.range) : Clang_AST.range =
  let origin_range = Framework.Location.get_origin_range range in
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

  *)


(*==========================================================================*)
                  (** {2 Sizes and alignments} *)
(*==========================================================================*)

(** [sizeof t] computes the size (in bytes) of a C type [t] *)
let rec sizeof_type (t : typ) : Z.t =
  let get_target () = 
    Clang_parser.get_target_info (Clang_parser.get_default_target_options ())
  in
  match t with
  | T_c_void -> Z.zero
  | T_c_integer i -> to_clang_int_type i |> C_utils.sizeof_int (get_target()) |> Z.of_int
  | T_c_float f -> to_clang_float_type f |> C_utils.sizeof_float (get_target()) |> Z.of_int
  | T_c_pointer _ -> fst C_AST.void_ptr_type |> C_utils.sizeof_type (get_target())
  | T_c_array (t, C_array_length_cst x) -> Z.mul x (sizeof_type t)
  | T_c_array (_, (C_array_no_length | C_array_length_expr _)) ->
     Exceptions.panic "sizeof_type: %a has no length information" pp_typ t
  | T_c_bitfield(t, size) ->
     Exceptions.panic "sizeof_type: %a is a bitfield" pp_typ t
  | T_c_function _ | T_c_builtin_fn ->
     Exceptions.panic "sizeof_type: %a is a function" pp_typ t
  | T_c_typedef td -> sizeof_type td.c_typedef_def
  | T_c_record r ->
     if not r.c_record_defined then
       Exceptions.panic "sizeof_type: %a is undefined" pp_typ t;
     r.c_record_sizeof
  | T_c_enum e ->
     if not e.c_enum_defined then
       Exceptions.panic "sizeof_type: %a is undefined" pp_typ t;
     sizeof_type (T_c_integer e.c_enum_integer_type)
  | T_c_qualified (_,t) -> sizeof_type t
  | t -> Exceptions.panic "to_clang_type: %a not a C type" pp_typ t

let sizeof_expr (t:typ) range : expr =
  let rec doit t =
    match t with
    | T_c_void -> invalid_arg "sizeof_expr: size of void"
    | T_c_integer _ | T_c_float _ | T_c_pointer _ | T_c_record _ | T_c_enum _ ->
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
       mk_binop (doit t) (O_mult) len range
    | T_c_bitfield (t,_) -> invalid_arg "sizeof_expr: size of bitfield"
    | T_c_function _ | T_c_builtin_fn -> invalid_arg "sizeof_expr: size of function"
    | T_c_typedef t -> doit (t.c_typedef_def)
    | _ -> assert false
  in
  doit t
(** Size (in bytes) of a type, as an expression. Handles variable-length ararys. *)



let rec remove_typedef = function
  | T_c_typedef(td) -> remove_typedef (td.c_typedef_def)
  | t -> t

let rec remove_qual = function
  | T_c_qualified(_, t) -> remove_qual t
  | t -> t

(** [is_signed t] whether [t] is signed *)
let rec is_signed (t : typ) : bool=
  match remove_typedef t |> remove_qual with
  | T_c_integer it ->
     begin
       match it with
       | C_signed_char | C_signed_short | C_signed_int
       | C_signed_long | C_signed_long_long | C_signed_int128 -> true
       | _ -> false
     end

  | T_c_enum e -> is_signed (T_c_integer e.c_enum_integer_type)
  | _ -> Exceptions.panic "[is_signed] not an integer type %a" pp_typ t

(** [range t] computes the interval range of type [t] *)
let rangeof (t : typ) =
  let part = 8*Z.to_int (sizeof_type t) in
  if is_signed t then
    let part' = Z.pow (Z.of_int (2)) (part -1) in
    ( Z.neg part', Z.sub part' (Z.of_int 1))
  else
    let part' = Z.pow (Z.of_int 2) part in
    ( Z.of_int 0 , Z.sub part' (Z.of_int 1))

(** [range t] computes the interval range of type [t] as integers *)
let int_rangeof t =
  let a,b = rangeof t in
  (Z.to_int a, Z.to_int b)

(** [wrap_expr e (l,h)] expression needed to bring back [e] in range ([l],[h]) *)
let wrap_expr (e: expr) ((l,h) : int * int) range : Framework.Ast.expr =
    let open Universal.Ast in
  mk_binop
    (mk_int l (tag_range range "l"))
    O_plus
    (mk_binop
       (mk_binop
          e
          O_minus
          (mk_int l (tag_range range "l"))
          (tag_range range "?")
       )
       O_mod
       (mk_binop
          (mk_binop
             (mk_int h (tag_range range "v"))
             (O_minus)
             (mk_int l (tag_range range "l"))
             (tag_range range "?")
          )
          O_plus
          (mk_one (tag_range range "1"))
          (tag_range range "+1")
       )
       (tag_range range "h-l+1")
    )
    range

(** [wrap v (l,h)] expression needed to bring back [v] in range ([l],[h]) *)
let wrap (v : var) ((l,h) : int * int) range : Framework.Ast.expr =
  wrap_expr (mk_var v (tag_range range "v")) (l,h) range



(** [is_c_int_type t] wheter [t] is an integer type *)
let is_c_int_type ( t : typ) =
  match remove_typedef t |> remove_qual with
  | T_c_integer _ -> true
  | _ -> false

let is_c_float_type ( t : typ) =
  match remove_typedef t |> remove_qual with
  | T_c_float _ -> true
  | _ -> false

let is_c_record_type ( t : typ) =
  match remove_typedef t |> remove_qual with
  | T_c_record _ -> true
  | _ -> false

let is_c_struct_type (t : typ) =
  match remove_typedef t |> remove_qual with
  | T_c_record({c_record_kind = C_struct}) -> true
  | _ -> false

let is_c_union_type (t : typ) =
  match remove_typedef t |> remove_qual with
  | T_c_record({c_record_kind = C_union}) -> true
  | _ -> false


(** [is_c_scalar_type t] wheter [t] is a scalar type *)
let is_c_scalar_type ( t : typ) =
  match remove_typedef t |> remove_qual with
  | T_c_integer _ | T_c_float _ | T_c_pointer _ -> true
  | T_c_bitfield _ -> true
  | T_c_enum _ -> true
  | _ -> false

(** [is_c_pointer t] wheter [t] is a pointer *)
let rec is_c_pointer_type ( t : typ) =
  match remove_typedef t |> remove_qual with
  | T_c_pointer _ -> true
  | _ -> false

let rec is_c_array_type (t: typ) =
  match remove_typedef t |> remove_qual with
  | T_c_array _ -> true
  | _ -> false

let rec is_c_function_type (t: typ) =
  match remove_typedef t |> remove_qual with
  | T_c_function _ -> true
  | _ -> false

(** [is_scalartype t] lifts [t] to a pointer to [t] *)
let pointer_type (t : typ) =
  (T_c_pointer t)

let rec under_pointer_type (t : typ) : typ =
  match remove_typedef t |> remove_qual with
  | T_c_pointer t' -> t'
  | _ -> failwith "[under_pointer_type] called with a non pointer argument"

let rec under_array_type (t : typ) : typ =
  match remove_typedef t |> remove_qual with
  | T_c_array (t', _) -> t'
  | _ -> failwith "[under_array_type] called with a non array argument"

let under_type (t: typ) : typ =
  match remove_typedef t |> remove_qual with
  | T_c_array _ -> under_array_type t
  | T_c_pointer _ -> under_pointer_type t
  | _ -> failwith "[under_type] called with a non array/pointer argument"

let get_c_float_type ( t : typ) =
  match remove_typedef t |> remove_qual with
  | T_c_float t -> t
  | _ -> failwith "[get_c_float_type] called with a non-float type"
       
let get_array_constant_length t =
  match remove_typedef t |> remove_qual with
  | T_c_array(_, C_array_length_cst n) -> Z.to_int n
  | _ -> assert false

let align_byte t i =
  match remove_typedef t |> remove_qual with
  | T_c_record crt -> (List.nth crt.c_record_fields i).c_field_offset
  | _ -> assert false

let is_c_type = function
  | T_c_void
  | T_c_integer _
  | T_c_float _
  | T_c_pointer _
  | T_c_array _
  | T_c_bitfield _
  | T_c_function  _
  | T_c_builtin_fn
  | T_c_typedef _
  | T_c_record  _
  | T_c_enum _
  | T_c_qualified _ -> true
  | _ -> false


let mk_c_address_of e range =
  mk_expr (E_c_address_of e) ~etyp:(T_c_pointer e.etyp) range

let mk_c_deref e range =
  mk_expr (E_c_deref e) ~etyp:(under_pointer_type e.etyp) range

let mk_c_member_access r f range =
  mk_expr (E_c_member_access (r, f.c_field_index, f.c_field_org_name)) ~etyp:f.c_field_type range

let mk_c_subscript_access a i range =
  mk_expr (E_c_array_subscript (a, i)) ~etyp:(under_array_type a.etyp) range

let mk_c_character c range =
  mk_constant (C_c_character ((Z.of_int @@ int_of_char c), C_char_ascii)) range ~etyp:(T_c_integer(C_unsigned_char))

let u8 = T_c_integer(C_unsigned_char)
let s8 = T_c_integer(C_signed_char)

let type_of_string s = T_c_array(s8, C_array_length_cst (Z.of_int (1 + String.length s)))

let mk_c_string s range =
  mk_constant (C_c_string (s, C_char_ascii)) range ~etyp:(type_of_string s)

let mk_c_call f args range =
  let ftype = {
    c_ftype_return = f.c_func_return;
    c_ftype_params = List.map (fun p -> p.vtyp) f.c_func_parameters;
    c_ftype_variadic = f.c_func_variadic;
  }
  in
  mk_expr (E_c_call (mk_expr (E_c_function f) range ~etyp:(T_c_function (Some ftype)), args)) range ~etyp:(f.c_func_return)

let mk_c_call_stmt f args range =
  let exp = mk_c_call f args (tag_range range "call") in
  mk_stmt (S_expression exp) range

let mk_c_cast e t range =
  mk_expr (E_c_cast(e, true)) ~etyp:t range

let () =
  register_typ_compare (fun next t1 t2 ->
      match remove_typedef t1, remove_typedef t2 with
      | T_c_void, T_c_void -> 0
      | T_c_integer i1, T_c_integer i2 -> compare i1 i2
      | T_c_float f1, T_c_float f2 -> compare f1 f2
      | T_c_pointer t1, T_c_pointer t2 -> compare_typ t1 t2
      | T_c_array(t1, l1), T_c_array(t2, l2) ->
        Compare.compose [
          (fun () -> compare_typ t1 t2);
          (fun () -> match l1, l2 with
             | C_array_length_cst n1, C_array_length_cst n2 -> Z.compare n1 n2
             | C_array_length_expr e1, C_array_length_expr e2 -> Exceptions.panic "type compare on arrays with expr length not supported"
             | C_array_no_length, C_array_no_length -> 0
             | _ -> compare l1 l2
          )
        ]
      | T_c_bitfield(t1, n1), T_c_bitfield(t2, n2) ->
        Compare.compose [
          (fun () -> compare_typ t1 t2);
          (fun () -> compare n1 n2)
        ]
      | T_c_function f1, T_c_function f2 ->
        begin
          match f1, f2 with
          | Some ff1, Some ff2 ->
            if List.length ff1.c_ftype_params = List.length ff2.c_ftype_params then
              let l = [
                (fun () -> compare_typ ff1.c_ftype_return ff2.c_ftype_return);
                (fun () -> compare ff1.c_ftype_variadic ff2.c_ftype_variadic)
              ] @ (List.map2 (fun t t' -> fun () -> compare_typ t t') ff1.c_ftype_params ff2.c_ftype_params)
              in
              Compare.compose l
            else 1
          | None, None -> 0
          | _ -> 1
        end
      | T_c_builtin_fn, T_c_builtin_fn -> 0
      | T_c_typedef td1, T_c_typedef td2 -> compare_typ td1.c_typedef_def td2.c_typedef_def
      | T_c_record r1, T_c_record r2 ->
        Compare.compose ([
          (fun () -> compare r1.c_record_kind r2.c_record_kind);
          (fun () -> Z.compare r1.c_record_sizeof r2.c_record_sizeof);
          (fun () -> compare (List.length r1.c_record_fields) (List.length r2.c_record_fields);
          )
        ] @ (
          List.map2 (fun f1 f2 ->
              (fun () -> compare_typ f1.c_field_type f2.c_field_type)
            ) r1.c_record_fields r2.c_record_fields
        ))
      | T_c_enum e1, T_c_enum e2 -> compare e1.c_enum_unique_name e2.c_enum_unique_name
      | T_c_qualified (q1, t1), T_c_qualified (q2, t2) ->
        Compare.compose [
          (fun () -> compare q1.c_qual_is_const q2.c_qual_is_const);
          (fun () -> compare q1.c_qual_is_volatile q2.c_qual_is_volatile);
          (fun () -> compare q1.c_qual_is_restrict q2.c_qual_is_restrict);
          (fun () -> compare_typ t1 t2)
        ]
      | _ -> next t1 t2
    )

let range_cond e_mint rmin rmax range =
  let condle = {ekind = E_binop(O_le, e_mint, mk_z rmax (tag_range range "wrap_le_z"));
                etyp  = T_bool;
                erange = tag_range range "wrap_le"
               } in
  let condge = {ekind = E_binop(O_ge, e_mint, mk_z rmin (tag_range range "wrap_ge_z"));
                etyp  = T_bool;
                erange = tag_range range "wrap_ge"
               } in
  {ekind = E_binop(O_log_and, condle, condge);
   etyp = T_bool;
   erange = tag_range range "wrap_full"
  }

let rec remove_casts e =
  match ekind e with
  | E_c_cast (e', _) -> remove_casts e'
  | _ -> e
