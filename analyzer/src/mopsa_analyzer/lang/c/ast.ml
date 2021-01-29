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

(** AST of the C language. *)

open Mopsa
open Mopsa_c_parser
open Universal.Ast


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

  | T_c_bool
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

  | T_c_block_object of typ
  (** Type of block objects.  *)  


(** {2 Function descriptor} *)
(** *********************** *)

type c_fundec = {
  mutable c_func_uid: int; (** unique identifier *)
  mutable c_func_org_name: string; (** original name *)
  mutable c_func_unique_name: string; (** unique name for globals and statics *)
  c_func_is_static: bool;
  mutable c_func_return: typ; (** type of returned value *)
  mutable c_func_parameters: var list; (** function parameters *)
  mutable c_func_body: stmt option; (** function body *)
  mutable c_func_static_vars: var list; (** static variables declared in the function *)
  mutable c_func_local_vars: var list; (** local variables declared in the function (excluding parameters) *)
  mutable c_func_variadic: bool; (** whether the has a variable number of arguments *)
  mutable c_func_range: range; (** location range of the declaration *)
  mutable c_func_name_range: range; (** location range of the name in the declaration *)
  mutable c_func_stub: Stubs.Ast.stub_func option; (** stub comment *)
}
(** Function descriptor. *)



(** {2 C variables} *)
(*  *************** *)

type c_var_scope =
  | Variable_global (** global shared among translation units *)
  | Variable_extern (** declared but not defined *)
  | Variable_local of c_fundec (** local to a function *)
  | Variable_parameter of c_fundec (** formal argument *)
  | Variable_file_static of string (** restricted to a translation unit *)
  | Variable_func_static of c_fundec (** restricted to a function *)


let pp_scope fmt s =
  Format.fprintf fmt "%s" (match s with
  | Variable_global -> "variable_global"
  | Variable_extern -> "extern" (** declared but not defined *)
  | Variable_local _ -> "local"
  | Variable_parameter _ -> "parameter"
  | Variable_file_static _ -> "file static"
  | Variable_func_static _ -> "func static")


(** Variable initialization. *)
type c_var_init =
  | C_init_expr of expr
  | C_init_list of c_var_init list (** specified elements *) * c_var_init option (** filler *)
  | C_init_implicit of typ


type cvar = {
  cvar_scope: c_var_scope; (** life-time scope of the variable *)
  cvar_range: range; (** declaration range *)
  cvar_uid: int;
  cvar_orig_name : string;
  cvar_uniq_name : string;
}

type var_kind +=
  | V_cvar of cvar
  (** C variable *)

let () =
  register_var {
    print = (fun next fmt v ->
        match vkind v with
        | V_cvar cvar -> Format.fprintf fmt "%s" cvar.cvar_orig_name
        | _ -> next fmt v
      );

    compare = (fun next v1 v2 ->
        match vkind v1, vkind v2 with
        | V_cvar cvar1, V_cvar cvar2 ->
          Compare.compose [
            (fun () -> Stdlib.compare cvar1.cvar_uid cvar2.cvar_uid);
            (fun () -> Stdlib.compare cvar1.cvar_uniq_name cvar2.cvar_uniq_name)
          ]

        | _ -> next v1 v2
      );
  }



(** {2 C expressions} *)
(*  ***************** *)

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


type expr_kind +=
  | E_c_conditional of expr (** condition *) * expr (** then *) * expr (** else *)
  (** ?: ternary operator *)

  | E_c_array_subscript of expr (** array *) * expr (** index *)
  (** Array access. *)

  | E_c_member_access of expr (** record *) * int (** field index *) * string (** field *)
  (** record.field access *)

  | E_c_function of c_fundec

  | E_c_builtin_function of string

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

  | E_c_block_object of expr
  (** Block objects are useful to distinguish between operations on
      the block itself and its content.  For, expanding the contents of
      a block will duplicate every cell in the block, while expanding
      the block object will update the pointers that point to the
      block.  *)  



(*==========================================================================*)
                           (** {2 Scope update} *)
(*==========================================================================*)


type c_scope_update = {
  c_scope_var_added:   var list;
  c_scope_var_removed: var list;
}
(** Scope update information for jump statements *)


(*==========================================================================*)
                           (** {2 Statements} *)
(*==========================================================================*)

type stmt_kind +=
  | S_c_goto_stab of stmt
  (** stabilization point for goto statement *)

  | S_c_declaration of var * c_var_init option * c_var_scope
  (** declaration of a variable *)

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

  | S_c_return of expr option * c_scope_update
  (** return statement *)

  | S_c_break of c_scope_update
  (** break statement *)

  | S_c_continue of c_scope_update
  (** continue statement *)

  | S_c_goto of string * c_scope_update
  (** goto statements. *)

  | S_c_switch of expr * stmt
  (** switch statement. *)

  | S_c_label of string
  (** statement label. *)

  | S_c_switch_case of expr * c_scope_update
  (** case of a switch statement. *)

  | S_c_switch_default of c_scope_update
  (** default case of switch statements. *)


type c_program = {
  c_globals : (var * c_var_init option) list; (** global variables of the program *)
  c_functions : c_fundec list; (** functions of the program *)
  c_stub_directives : Stubs.Ast.stub_directive list; (** list of stub directives *)
}

type prog_kind +=
  | C_program of c_program


module CProgramKey = GenContextKey(struct
    type 'a t = c_program
    let print pp fmt prog = Format.fprintf fmt "C program"
  end)


(** Flow-insensitive context to keep the analyzed C program *)
let c_program_ctx = CProgramKey.key

(** Set the C program in the flow *)
let set_c_program prog flow =
  Flow.set_ctx (Flow.get_ctx flow |> add_ctx c_program_ctx prog) flow

(** Get the C program from the flow *)
let get_c_program flow =
  Flow.get_ctx flow |> find_ctx c_program_ctx


(*==========================================================================*)
                  (** {2 Conversion to/from Clang parser types} *)
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

let from_clang_int_type : C_AST.integer_type -> c_integer_type = function
  | C_AST.(Char SIGNED) -> C_signed_char
  | C_AST.(Char UNSIGNED) -> C_unsigned_char
  | C_AST.SIGNED_CHAR -> C_signed_char
  | C_AST.UNSIGNED_CHAR -> C_unsigned_char
  | C_AST.SIGNED_SHORT -> C_signed_short
  | C_AST.UNSIGNED_SHORT -> C_unsigned_short
  | C_AST.SIGNED_INT -> C_signed_int
  | C_AST.UNSIGNED_INT -> C_unsigned_int
  | C_AST.SIGNED_LONG -> C_signed_long
  | C_AST.UNSIGNED_LONG -> C_unsigned_long
  | C_AST.SIGNED_LONG_LONG -> C_signed_long_long
  | C_AST.UNSIGNED_LONG_LONG -> C_unsigned_long_long
  | C_AST.SIGNED_INT128 -> C_signed_int128
  | C_AST.UNSIGNED_INT128 -> C_unsigned_int128

let from_clang_float_type : C_AST.float_type -> c_float_type = function
  | C_AST.FLOAT -> C_float
  | C_AST.DOUBLE -> C_double
  | C_AST.LONG_DOUBLE -> C_long_double

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

let target_info = Clang_parser.get_target_info (Clang_parser.get_default_target_options ())

(** [sizeof t] computes the size (in bytes) of a C type [t] *)
let rec sizeof_type (t : typ) : Z.t =
  match t with
  | T_c_void -> C_utils.sizeof_type target_info C_AST.T_void

  | T_c_bool -> C_utils.sizeof_type target_info C_AST.T_bool

  | T_c_integer i -> to_clang_int_type i |> C_utils.sizeof_int target_info |> Z.of_int

  | T_c_float f -> to_clang_float_type f |> C_utils.sizeof_float target_info |> Z.of_int

  | T_c_pointer _ -> fst C_AST.void_ptr_type |> C_utils.sizeof_type target_info

  | T_c_array (t, C_array_length_cst x) -> Z.mul x (sizeof_type t)

  | T_c_array (_, (C_array_no_length | C_array_length_expr _)) -> panic ~loc:__LOC__ "%a has no length information" pp_typ t

  | T_c_bitfield(t, size) -> panic ~loc:__LOC__ "%a is a bitfield" pp_typ t

  | T_c_function _ | T_c_builtin_fn -> panic ~loc:__LOC__ "%a is a function" pp_typ t

  | T_c_typedef td -> sizeof_type td.c_typedef_def

  | T_c_record r ->
     if not r.c_record_defined then panic ~loc:__LOC__ " %a is undefined" pp_typ t;
     r.c_record_sizeof

  | T_c_enum e ->
     if not e.c_enum_defined then panic ~loc:__LOC__ "%a is undefined" pp_typ t;
     sizeof_type (T_c_integer e.c_enum_integer_type)

  | T_c_qualified (_,t) -> sizeof_type t

  | t -> panic ~loc:__LOC__ "%a not a C type" pp_typ t


let sizeof_expr (t:typ) range : expr =
  let rec doit t =
    match t with
    | T_c_void | T_c_bool | T_c_integer _ | T_c_float _ | T_c_pointer _ | T_c_record _ | T_c_enum _ ->
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
  | T_c_pointer t -> T_c_pointer (remove_qual t)
  | t -> t

let rec remove_typedef_qual = function
  | T_c_qualified(_, t) -> remove_typedef_qual t
  | T_c_typedef(td) -> remove_typedef_qual (td.c_typedef_def)
  | t -> t

(** [is_signed t] whether [t] is signed *)
let rec is_signed (t : typ) : bool=
  match remove_typedef_qual t with
  | T_c_bool -> true

  | T_c_integer it ->
     begin
       match it with
       | C_signed_char | C_signed_short | C_signed_int
       | C_signed_long | C_signed_long_long | C_signed_int128 -> true
       | _ -> false
     end

  | T_c_enum e -> is_signed (T_c_integer e.c_enum_integer_type)

  | _ -> panic ~loc:__LOC__ "%a is not an integer type" pp_typ t

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
let wrap_expr (e: expr) ((l,h) : Z.t * Z.t) range : expr =
  mk_unop (O_wrap(l,h)) e ~etyp:e.etyp range

let is_c_char_type (t:typ) =
  match remove_typedef_qual t with
  | T_c_integer (C_signed_char | C_unsigned_char) -> true
  | _ -> false

(** [is_c_int_type t] tests whether [t] is an integer type *)
let is_c_int_type ( t : typ) =
  match remove_typedef_qual t with
  | T_c_bool -> true
  | T_c_enum _ -> true
  | T_c_integer _ -> true
  | _ -> false

let is_c_signed_int_type (t:typ) =
  match remove_typedef_qual t with
  | T_c_bool -> false
  | T_c_enum _ -> false
  | T_c_integer (C_signed_char | C_signed_short | C_signed_int | C_signed_int128 | C_signed_long | C_signed_long_long) -> true
  | T_c_integer (C_unsigned_char | C_unsigned_short | C_unsigned_int | C_unsigned_int128 | C_unsigned_long | C_unsigned_long_long) -> false
  | _ -> false

let is_c_bool_type (t:typ) =
  match remove_typedef_qual t with
  | T_c_bool -> true
  | _ -> false

(** [is_c_int_type t] tests whether [t] is a floating point type *)
let is_c_float_type ( t : typ) =
  match remove_typedef_qual t with
  | T_c_float _ -> true
  | _ -> false

let get_c_float_type ( t : typ) =
  match remove_typedef_qual t with
  | T_c_float t -> t
  | _ -> panic ~loc:__LOC__ "get_c_float_type called on a non-float type %a" pp_typ t

(** Get the float precision from a C type *)
let get_c_float_precision t =
  match get_c_float_type t with
  | C_float -> F_SINGLE
  | C_double -> F_DOUBLE
  | C_long_double -> F_LONG_DOUBLE


(** [is_c_int_type t] tests whether [t] is a numeric type *)
let is_c_num_type (t:typ) =
  is_c_int_type t || is_c_float_type t

(** [is_c_scalar_type t] tests whether [t] is a scalar type *)
let is_c_scalar_type ( t : typ) =
  match remove_typedef_qual t with
  | T_c_bool | T_c_integer _ | T_c_float _ | T_c_pointer _ -> true
  | T_c_bitfield _ -> true
  | T_c_enum _ -> true
  | _ -> false

(** [is_c_pointer t] tests whether [t] is a pointer *)
let is_c_pointer_type ( t : typ) =
  match remove_typedef_qual t with
  | T_c_pointer _ -> true
  | T_c_array _ -> true
  | _ -> false

let is_c_void_type (t:typ) =
  match remove_typedef_qual t with
  | T_c_void -> true
  | _ -> false

let is_c_record_type ( t : typ) =
  match remove_typedef_qual t with
  | T_c_record _ -> true
  | _ -> false

let is_c_struct_type (t : typ) =
  match remove_typedef_qual t with
  | T_c_record({c_record_kind = C_struct}) -> true
  | _ -> false

let is_c_union_type (t : typ) =
  match remove_typedef_qual t with
  | T_c_record({c_record_kind = C_union}) -> true
  | _ -> false

let rec is_c_array_type (t: typ) =
  match remove_typedef_qual t with
  | T_c_array _ -> true
  | _ -> false

let rec is_c_function_type (t: typ) =
  match remove_typedef_qual t with
  | T_c_function _ -> true
  | _ -> false

(** [is_scalartype t] lifts [t] to a pointer to [t] *)
let pointer_type (t : typ) =
  (T_c_pointer t)

let rec under_pointer_type (t : typ) : typ =
  match remove_typedef_qual t with
  | T_c_pointer t' -> t'
  | _ -> failwith "[under_pointer_type] called with a non pointer argument"

let rec under_array_type (t : typ) : typ =
  match remove_typedef_qual t with
  | T_c_array (t', _) -> t'
  | _ -> failwith "[under_array_type] called with a non array argument"

let under_type (t: typ) : typ =
  match remove_typedef_qual t with
  | T_c_array _ -> under_array_type t
  | T_c_pointer _ -> under_pointer_type t
  | _ -> failwith "[under_type] called with a non array/pointer argument"

let void_to_char t =
  match remove_typedef_qual t with
  | T_c_void -> T_c_integer C_unsigned_char
  | _ -> t

let get_array_constant_length t =
  match remove_typedef_qual t with
  | T_c_array(_, C_array_length_cst n) -> n
  | _ -> assert false

let align_byte t i =
  match remove_typedef_qual t with
  | T_c_record crt -> (List.nth crt.c_record_fields i).c_field_offset
  | _ -> assert false

let is_c_type = function
  | T_c_void
  | T_c_bool
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
  | T_addr -> true (* XXX is it safe to consider heap addresses as C objects? *)
  | _ -> false

let is_c_function_parameter v =
  match v.vkind with
  | V_cvar { cvar_scope = Variable_parameter _ } -> true
  | _ -> false

let mk_c_address_of e range =
  mk_expr (E_c_address_of e) ~etyp:(T_c_pointer e.etyp) range

let mk_c_deref e range =
  mk_expr (E_c_deref e) ~etyp:(under_pointer_type e.etyp) range

let mk_c_member_access r f range =
  mk_expr (E_c_member_access (r, f.c_field_index, f.c_field_org_name)) ~etyp:f.c_field_type range

let mk_c_subscript_access a i range =
  mk_expr (E_c_array_subscript (a, i)) ~etyp:(under_type a.etyp) range

let mk_c_character c range =
  mk_constant (C_c_character ((Z.of_int @@ int_of_char c), C_char_ascii)) range ~etyp:(T_c_integer(C_unsigned_char))

(* extract a multi-byte integer of type t starting at offset off of s *)
let extract_multibyte_integer (s:string) (off:int) t =
  let n = Z.to_int (sizeof_type t) in
  (* get bytes in right order according to endianess *)
  let rec doit acc i =
    if i >= n then acc else
      let off' = if target_info.target_big_endian then off+i else off+n-i-1 in
      doit (Z.add (Z.mul (Z.of_int 256) acc) (Z.of_int (int_of_char s.[off']))) (i+1)
  in
  let v = doit Z.zero 0 in
  (* sign correction *)
  if is_signed t && v >= Z.shift_left Z.one (n*8-1)
  then Z.sub v (Z.shift_left Z.one (n*8))
  else v

let mk_c_multibyte_integer (s:string) (off:int) t range =
  mk_z (extract_multibyte_integer s off t) ~typ:t range


let mk_c_invalid_pointer range =
  mk_constant C_c_invalid ~etyp:(T_c_pointer T_c_void) range

let void = T_c_void
let u8 = T_c_integer(C_unsigned_char)
let s8 = T_c_integer(C_signed_char)
let s16 = T_c_integer(C_signed_short)
let u16 = T_c_integer(C_unsigned_short)
let s32 = T_c_integer(C_signed_int)
let u32 = T_c_integer(C_unsigned_int)
let s64 = T_c_integer(C_signed_long)
let u64 = T_c_integer(C_unsigned_long)
let ul = T_c_integer(C_unsigned_long)
let sl = T_c_integer(C_signed_long)
let ull = T_c_integer(C_unsigned_long_long)
let sll = T_c_integer(C_signed_long_long)
let array_type typ size = T_c_array(typ,C_array_length_cst size)

let size_type =
  let t = C_utils.size_type target_info |>
          from_clang_int_type in
  T_c_integer t

let max_pointer_value = rangeof size_type |> snd

let type_of_string s = T_c_array(s8, C_array_length_cst (Z.of_int (1 + String.length s)))

let is_c_block_object_type = function T_c_block_object _ -> true | _ -> false

let to_c_block_object e = mk_expr (E_c_block_object e) e.erange ~etyp:(T_c_block_object e.etyp)

let of_c_block_object e =
  match ekind e with
  | E_c_block_object ee -> ee
  | _ -> assert false


let mk_c_string ?(kind=C_char_ascii) s range =
  mk_constant (C_c_string (s, kind)) range ~etyp:(type_of_string s)

let mk_c_fun_typ f =
  let ftype = {
    c_ftype_return = f.c_func_return;
    c_ftype_params = List.map (fun p -> p.vtyp) f.c_func_parameters;
    c_ftype_variadic = f.c_func_variadic;
  }
  in
  T_c_function (Some ftype)

let mk_c_call f args range =
  mk_expr (E_call (mk_expr (E_c_function f) range ~etyp:(mk_c_fun_typ f), args)) range ~etyp:(f.c_func_return)

let mk_c_builtin_call builtin args typ range =
  mk_expr (E_c_builtin_call (builtin,args)) range ~etyp:typ

let mk_c_call_stmt f args range =
  let exp = mk_c_call f args range in
  mk_stmt (S_expression exp) range

let mk_c_cast e t range =
  mk_expr (E_c_cast(e, true)) ~etyp:t range

let mk_c_null range =
  mk_c_cast (mk_zero ~typ:u8 range) (pointer_type void) range

let mk_c_declaration v init scope range =
  mk_stmt (S_c_declaration (v, init, scope)) range

let var_scope v =
  match v.vkind with
  | V_cvar { cvar_scope } -> cvar_scope
  | _ -> assert false

let is_c_global_scope = function
  | Variable_global | Variable_extern | Variable_file_static _ -> true
  | Variable_func_static _ | Variable_local _ | Variable_parameter _ -> false

let () =
  register_typ_compare (fun next t1 t2 ->
      match remove_typedef t1, remove_typedef t2 with
      | T_c_void, T_c_void -> 0
      | T_c_bool, T_c_bool -> 0
      | T_c_integer i1, T_c_integer i2 -> compare i1 i2
      | T_c_float f1, T_c_float f2 -> compare f1 f2
      | T_c_pointer t1, T_c_pointer t2 -> compare_typ t1 t2
      | T_c_array(t1, l1), T_c_array(t2, l2) ->
        Compare.compose [
          (fun () -> compare_typ t1 t2);
          (fun () -> match l1, l2 with
             | C_array_length_cst n1, C_array_length_cst n2 -> Z.compare n1 n2
             | C_array_length_expr e1, C_array_length_expr e2 -> panic ~loc:__LOC__ "type compare on arrays with expr length not supported"
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
      | T_c_record r1, T_c_record r2 -> compare r1.c_record_unique_name r2.c_record_unique_name
      | T_c_enum e1, T_c_enum e2 -> compare e1.c_enum_unique_name e2.c_enum_unique_name
      | T_c_qualified (q1, t1), T_c_qualified (q2, t2) ->
        Compare.compose [
          (fun () -> compare q1.c_qual_is_const q2.c_qual_is_const);
          (fun () -> compare q1.c_qual_is_volatile q2.c_qual_is_volatile);
          (fun () -> compare q1.c_qual_is_restrict q2.c_qual_is_restrict);
          (fun () -> compare_typ t1 t2)
        ]
      | T_c_block_object tt1, T_c_block_object tt2 -> compare_typ tt1 tt2
      | _ -> next t1 t2
    )

let () =
  register_expr_compare
    (fun next e1 e2 ->
       match ekind e1, ekind e2 with
       | E_c_conditional(cond1,then1,else1), E_c_conditional(cond2,then2,else2) ->
         Compare.triple compare_expr compare_expr compare_expr
           (cond1,then1,else1)
           (cond2,then2,else2)

       | E_c_array_subscript(a1,i1), E_c_array_subscript(a2,i2) ->
         Compare.pair compare_expr compare_expr
           (a1,i1)
           (a2,i2)

       | E_c_member_access(s1,i1,f1), E_c_member_access(s2,i2,f2) ->
         Compare.triple compare_expr compare compare
           (s1,i1,f1)
           (s2,i2,f2)

       | E_c_function(f1), E_c_function(f2) ->
         compare f1.c_func_unique_name f2.c_func_unique_name

       | E_c_builtin_function(f1), E_c_builtin_function(f2) ->
         compare f1 f2

       | E_c_builtin_call(f1,args1), E_c_builtin_call(f2,args2) ->
         Compare.pair compare (Compare.list compare_expr)
           (f1,args1)
           (f2,args2)

       | E_c_arrow_access(p1,i1,f1), E_c_arrow_access(p2,i2,f2) ->
         Compare.triple compare_expr compare compare
           (p1,i1,f1)
           (p2,i2,f2)

       | E_c_assign(x1,e1), E_c_assign(x2,e2) ->
         Compare.pair compare_expr compare_expr
           (x1,e1)
           (x2,e2)

       | E_c_compound_assign(lval1,t1,op1,rval1,tt1), E_c_compound_assign(lval2,t2,op2,rval2,tt2) ->
         Compare.compose [
           (fun () -> compare_expr lval1 lval2);
           (fun () -> compare_typ t1 t2);
           (fun () -> compare_operator op1 op2);
           (fun () -> compare_expr rval1 rval2);
           (fun () -> compare_typ tt1 tt2);
         ]

       | E_c_comma(e1,ee1), E_c_comma(e2,ee2) ->
         Compare.pair compare_expr compare_expr
           (e1,ee1)
           (e2,ee2)

       | E_c_increment(dir1,loc1,e1), E_c_increment(dir2,loc2,e2) ->
         Compare.triple compare compare compare_expr
           (dir1,loc1,e1)
           (dir2,loc2,e2)

       | E_c_address_of(e1), E_c_address_of(e2) ->
         compare_expr e1 e2

       | E_c_deref(e1), E_c_deref(e2) ->
         compare_expr e1 e2

       | E_c_cast(e1,b1), E_c_cast(e2,b2) ->
         Compare.pair compare_expr compare
           (e1,b1)
           (e2,b2)

       | E_c_statement(s1), E_c_statement(s2) ->
         compare_stmt s1 s2

       | E_c_predefined(s1), E_c_predefined(s2) ->
         compare s1 s2

       | E_c_var_args(e1), E_c_var_args(e2) ->
         compare_expr e1 e2

       | E_c_atomic(op1,e1,ee1), E_c_atomic(op2,e2,ee2) ->
         Compare.triple compare compare_expr compare_expr
           (op1,e1,ee1)
           (op2,e2,ee2)

       | E_c_block_object(e1), E_c_block_object(e2) ->
         compare_expr e1 e2

       | _ -> next e1 e2
    )



let range_cond e_mint rmin rmax range =
  let condle = mk_binop e_mint O_le (mk_z rmax range) ~etyp:T_bool range in
  let condge = mk_binop e_mint O_ge (mk_z rmin range) ~etyp:T_bool range in
  mk_binop condle O_log_and condge ~etyp:T_bool range

let rec remove_casts e =
  match ekind e with
  | E_c_cast (e', _) -> remove_casts e'
  | _ -> e


(** Simplify C constant expressions to constants *)
let rec c_expr_to_z (e:expr) : Z.t option =
  match ekind e with
  | E_constant (C_int n) -> Some n

  | E_constant (C_c_character (ch,_)) -> Some ch

  | E_unop (O_minus, e') ->
    c_expr_to_z e' |> OptionExt.bind @@ fun n ->
    Some (Z.neg n)

  | E_unop (O_bit_invert, e') ->
    c_expr_to_z e' |> OptionExt.bind @@ fun n ->
    Some (Z.lognot n)

  | E_unop (O_log_not, e') ->
    c_expr_to_z e' |> OptionExt.bind @@ fun n ->
    if Z.equal n Z.zero then Some Z.one else Some Z.zero

  | E_binop(O_c_and, e1, e2) ->
    c_expr_to_z e1 |> OptionExt.bind @@ fun n1 ->
    if Z.equal n1 Z.zero then Some Z.zero else c_expr_to_z e2

  | E_binop(O_c_or, e1, e2) ->
    c_expr_to_z e1 |> OptionExt.bind @@ fun n1 ->
    if Z.equal n1 Z.zero then c_expr_to_z e2 else Some Z.one

  | E_binop(op, e1, e2) ->
    c_expr_to_z e1 |> OptionExt.bind @@ fun n1 ->
    c_expr_to_z e2 |> OptionExt.bind @@ fun n2 ->
    begin
      match op with
      | O_plus -> Some (Z.add n1 n2)
      | O_minus -> Some (Z.sub n1 n2)
      | O_mult -> Some (Z.mul n1 n2)
      | O_div -> if Z.equal n2 Z.zero then None else Some (Z.div n1 n2)
      | O_bit_lshift -> begin try Some (Z.shift_left n1 (Z.to_int n2)) with _ -> None end
      | O_bit_rshift -> begin try Some (Z.shift_right n1 (Z.to_int n2)) with _ -> None end
      | O_bit_and -> Some (Z.logand n1 n2)
      | O_bit_or -> Some (Z.logor n1 n2)
      | O_eq -> Some (if Z.equal n1 n2 then Z.one else Z.zero)
      | O_ne -> Some (if Z.equal n1 n2 then Z.zero else Z.one)
      | O_gt -> Some (if Z.gt n1 n2 then Z.one else Z.zero)
      | O_ge -> Some (if Z.geq n1 n2 then Z.one else Z.zero)
      | O_lt -> Some (if Z.lt n1 n2 then Z.one else Z.zero)
      | O_le -> Some (if Z.leq n1 n2 then Z.one else Z.zero)
      | _ -> None
    end

  | E_c_conditional(cond,e1,e2) ->
    c_expr_to_z cond |> OptionExt.bind @@ fun c ->
    if not (Z.equal c Z.zero)
    then c_expr_to_z e1
    else c_expr_to_z e2

  | E_c_cast(ee,_) when is_c_int_type e.etyp ->
    c_expr_to_z ee |> OptionExt.bind @@ fun n ->
    let a,b = rangeof e.etyp in
    if Z.leq a n && Z.leq n b then Some n else None

  | _ -> None


let is_c_expr_equals_z e z =
  match c_expr_to_z e with
  | None -> false
  | Some n -> Z.equal n z


let is_c_constant e =
  match c_expr_to_z e with
  | None -> false
  | Some _ -> true

let rec is_c_lval e =
  match ekind e with
  | E_var _ | E_c_deref _ | E_c_array_subscript _ | E_c_member_access _ | E_c_arrow_access _ -> true
  | Stubs.Ast.E_stub_primed ee -> is_c_lval ee
  | _ -> false

let is_c_deref e =
  match remove_casts e |> ekind with
  | E_c_deref _ -> true
  | _ -> false

let get_c_deref_type e =
  match remove_casts e |> ekind with
  | E_c_deref p -> under_type p.etyp
  | _ -> assert false

(** Check if v is declared as a variable length array *)
let is_c_variable_length_array_type t =
  match remove_typedef_qual t with
  | T_c_array(_, C_array_length_expr _) -> true
  | _ -> false

(** Check if v is declared as an array without length (as for many auxiliary variables) *)
let is_c_no_length_array_type t =
  match remove_typedef_qual t with
  | T_c_array(_, C_array_no_length) -> true
  | _ -> false

(** Find the definition of a C function *)
let find_c_fundec_by_name name flow =
  let prog = get_c_program flow in
  List.find (fun f -> f.c_func_org_name = name) prog.c_functions

(** Check if a pointer points to a nul-terminated array *)
let assert_valid_string (p:expr) range man flow =
  let f = find_c_fundec_by_name "_mopsa_assert_valid_string" flow in
  let stmt = mk_c_call_stmt f [p] range in
  man.exec stmt flow

(** Check if a pointer points to a nul-terminated wide char array *)
let assert_valid_wide_string (p:expr) range man flow =
  let f = find_c_fundec_by_name "_mopsa_assert_valid_wide_string" flow in
  let stmt = mk_c_call_stmt f [p] range in
  man.exec stmt flow

(** Check if a pointer points to a valid stream *)
let assert_valid_stream (p:expr) range man flow =
  let f = find_c_fundec_by_name "_mopsa_assert_valid_stream" flow in
  let stmt = mk_c_call_stmt f [p] range in
  man.exec stmt flow

(** Check if a pointer points to a valid file descriptor *)
let assert_valid_file_descriptor (p:expr) range man flow =
  let f = find_c_fundec_by_name "_mopsa_assert_valid_file_descriptor" flow in
  let stmt = mk_c_call_stmt f [p] range in
  man.exec stmt flow


(** Check if a pointer is valid *)
let assert_valid_ptr (p:expr) range man flow =
  let f = find_c_fundec_by_name "_mopsa_assert_valid_ptr" flow in
  let stmt = mk_c_call_stmt f [p] range in
  man.exec stmt flow


(** Randomize an entire array *)
let memrand (p:expr) (i:expr) (j:expr) range man flow =
  let f = find_c_fundec_by_name "_mopsa_memrand" flow in
  let stmt = mk_c_call_stmt f [p; i; j] range in
  man.exec stmt flow

(** Randomize a string *)
let strrand (p:expr) range man flow =
  let f = find_c_fundec_by_name "_mopsa_strrand" flow in
  let stmt = mk_c_call_stmt f [p] range in
  man.exec stmt flow

(** Randomize a substring *)
let strnrand (p:expr) (n:expr) range man flow =
  let f = find_c_fundec_by_name "_mopsa_strnrand" flow in
  let stmt = mk_c_call_stmt f [p; n] range in
  man.exec stmt flow


(** Randomize a wide substring *)
let wcsnrand (p:expr) (n:expr) range man flow =
  let f = find_c_fundec_by_name "_mopsa_wcsnrand" flow in
  let stmt = mk_c_call_stmt f [p; n] range in
  man.exec stmt flow


(** Set elements of an array with the same value [c] *)
let memset (p:expr) (c:expr) (i:expr) (j:expr) range man flow =
  let f = find_c_fundec_by_name "_mopsa_memset" flow in
  let stmt = mk_c_call_stmt f [p; c; i; j] range in
  man.exec stmt flow


(** Copy elements of an array *)
let memcpy (dst:expr) (src:expr) (i:expr) (j:expr) range man flow =
  let f = find_c_fundec_by_name "_mopsa_memcpy" flow in
  let stmt = mk_c_call_stmt f [dst; src; i; j] range in
  man.exec stmt flow

(** Exit if status is non-zero *)
let error_error (p:expr) range man flow =
  let f = find_c_fundec_by_name "_mopsa_error" flow in
  let stmt = mk_c_call_stmt f [p] range in
  man.exec stmt flow

(** Exit if status is non-zero *)
let error_error_at_line (p:expr) (n:expr) range man flow =
  let f = find_c_fundec_by_name "_mopsa_error_at_line" flow in
  let stmt = mk_c_call_stmt f [p; n] range in
  man.exec stmt flow

let asprintf_stub (dst:expr) range man flow =
  let f = find_c_fundec_by_name "_mopsa_asprintf" flow in
  let exp = mk_c_call f [dst] range in
  man.eval exp flow
