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
  C_utils - Utilities to access the simple C AST
 *)

open C_AST
open C_print
open Mopsa_utils
module C =
  struct
    include Clang_AST
    include Clang_dump
  end


(** {2 Debug} *)

let log_type_unify = ref false
(** verbose logging during type unification, for debugging *)

let log_type_compare = false
(** verbose logging during type comparison, for debugging *)



(** {2 Location utilities} *)


let range_of_block b =
  if b = [] then invalid_arg "range_of_block: empty block";
  { C.range_begin = (ListExt.hd b).C.range_begin;
    C.range_end = (ListExt.last b).C.range_end;
  }
(** From the begining of the first block statement to the end of the last block statement.
    Raises Invalid_argument for empty blocks.
 *)



(** {2 Size and alignment} *)


let sizeof_int target t : int =
  match t with
  | Char _ | UNSIGNED_CHAR | SIGNED_CHAR -> 1
  | SIGNED_SHORT | UNSIGNED_SHORT -> target.C.target_short_width / 8
  | SIGNED_INT | UNSIGNED_INT -> target.C.target_int_width / 8
  | SIGNED_LONG | UNSIGNED_LONG -> target.C.target_long_width  / 8
  | SIGNED_LONG_LONG | UNSIGNED_LONG_LONG -> target.C.target_long_long_width / 8
  | SIGNED_INT128 | UNSIGNED_INT128 -> 128 / 8
(** Size (in bytes) of an integer type. *)

let alignof_int target t : int =
  match t with
  | Char _ | UNSIGNED_CHAR | SIGNED_CHAR -> 1
  | SIGNED_SHORT | UNSIGNED_SHORT -> target.C.target_short_align / 8
  | SIGNED_INT | UNSIGNED_INT -> target.C.target_int_align / 8
  | SIGNED_LONG | UNSIGNED_LONG -> target.C.target_long_align  / 8
  | SIGNED_LONG_LONG | UNSIGNED_LONG_LONG -> target.C.target_long_long_align / 8
  | SIGNED_INT128 | UNSIGNED_INT128 -> 128 / 8
(** Alignment (in bytes) of an integer type. *)

let signedness_int t =
  match t with
  | Char s -> s
  | SIGNED_CHAR | SIGNED_SHORT | SIGNED_INT | SIGNED_LONG | SIGNED_LONG_LONG | SIGNED_INT128 -> SIGNED
  | UNSIGNED_CHAR | UNSIGNED_SHORT | UNSIGNED_INT | UNSIGNED_LONG | UNSIGNED_LONG_LONG | UNSIGNED_INT128 -> UNSIGNED
(** Signedness of an integer type. *)


let sizeof_float target t : int =
  match t with
  | FLOAT -> target.C.target_float_width / 8
  | DOUBLE -> target.C.target_double_width / 8
  | LONG_DOUBLE -> target.C.target_long_double_width / 8
(** Size (in bytes) of a float type. *)

let alignof_float target t : int =
  match t with
  | FLOAT -> target.C.target_float_align / 8
  | DOUBLE -> target.C.target_double_align / 8
  | LONG_DOUBLE -> target.C.target_long_double_align / 8
(** Alignment (in bytes) of a float type. *)


let rec sizeof_type target t : Z.t =
  match t with
  | T_void -> invalid_arg "sizeof_type: size of void"
  | T_bool -> Z.of_int (target.C.target_bool_width / 8)
  | T_integer i -> Z.of_int (sizeof_int target i)
  | T_float f -> Z.of_int (sizeof_float target f)
  | T_complex f -> Z.of_int (2 * (sizeof_float target f))
  | T_pointer _ -> Z.of_int (target.C.target_pointer_width / 8)
  | T_array ((t,_),Length_cst len) -> Z.mul len (sizeof_type target t)
  | T_array _ -> invalid_arg "sizeof_type: size of array with unknown size"
  | T_bitfield (t,_) -> invalid_arg "sizeof_type: size of bitfield"
  | T_function _ | T_builtin_fn -> invalid_arg "sizeof_type: size of function"
  | T_typedef t -> sizeof_type target (fst t.typedef_def)
  | T_record r ->
     if not r.record_defined then invalid_arg "sizeof_type: size of incomplete record";
     r.record_sizeof
  | T_enum e ->
     if not e.enum_defined then invalid_arg "sizeof_type: size of incomplete enum";
     Z.of_int (sizeof_int target e.enum_integer_type)
(** Size (in bytes) of a type. Raises an Invalid_argument if the size is not a constant. *)


let sizeof_expr target (range:C.range) (result_type:type_qual) (t:typ) : expr =
  let rec doit t =
    match t with
    | T_void -> invalid_arg "sizeof_expr: size of void"
    | T_bool | T_integer _ | T_float _ | T_pointer _ | T_record _ | T_enum _ | T_complex _ ->
       E_integer_literal (sizeof_type target t), result_type, range
    | T_array ((t,_),l) ->
       let len = match l with
         | Length_cst len -> E_integer_literal len, result_type, range
         | Length_expr e -> e
         | No_length ->
            (* TODO: fix *)
            (* error range "sizeof" "array with no size"*)
            E_integer_literal Z.zero, result_type, range
       in
       E_binary (O_arithmetic MUL, doit t, len), result_type, range
    | T_bitfield (t,_) -> invalid_arg "sizeof_expr: size of bitfield"
    | T_function _ | T_builtin_fn -> invalid_arg "sizeof_expr: size of function"
    | T_typedef t -> doit (fst t.typedef_def)
  in
  doit t
(** Size (in bytes) of a type, as an expression. Handles variable-length ararys. *)


let rec alignof_type target t : Z.t =
  match t with
  | T_void -> invalid_arg "alignof_type: align of void"
  | T_bool -> Z.of_int (target.C.target_bool_align / 8)
  | T_integer i -> Z.of_int (alignof_int target i)
  | T_float f | T_complex f -> Z.of_int (alignof_float target f)
  | T_pointer _ -> Z.of_int (target.C.target_pointer_align / 8)
  | T_array ((t,_),_) -> alignof_type target t
  | T_bitfield (t,_) -> invalid_arg "alignof_type: align of bitfield"
  | T_function _ | T_builtin_fn -> invalid_arg "alignof_type: align of function"
  | T_typedef t -> alignof_type target (fst t.typedef_def)
  | T_record r ->
     if not r.record_defined then invalid_arg "alignof_type: size of incomplete record";
     r.record_alignof
  | T_enum e ->
     if not e.enum_defined then invalid_arg "alignof_type: size of incomplete enum";
     Z.of_int (alignof_int target e.enum_integer_type)
(** Alignment (in bytes) of a type. *)


let rec type_declarable = function
  | T_void -> false
  | T_bool | T_integer _ | T_float _  | T_complex _ -> true
  | T_pointer _ -> true
  | T_array (t,len) -> len <> No_length && type_qual_declarable t
  | T_bitfield (t,_) -> type_declarable t
  | T_function f -> false
  | T_builtin_fn -> false
  | T_typedef t -> type_qual_declarable t.typedef_def
  | T_record r -> r.record_defined
  | T_enum e -> e.enum_defined

and type_qual_declarable (t,q) =
  type_declarable t
(** Whether we can declare a variable of this type (sizeof defined). *)



(** {2 Useful target-specific types} *)


let target_int = function
  | C.Target_NoInt -> invalid_arg "target_int: NoInt"
  | C.Target_SignedChar -> SIGNED_CHAR
  | C.Target_UnsignedChar -> UNSIGNED_CHAR
  | C.Target_SignedShort -> SIGNED_SHORT
  | C.Target_UnsignedShort -> UNSIGNED_SHORT
  | C.Target_SignedInt -> SIGNED_INT
  | C.Target_UnsignedInt -> UNSIGNED_INT
  | C.Target_SignedLong -> SIGNED_LONG
  | C.Target_UnsignedLong -> UNSIGNED_LONG
  | C.Target_SignedLongLong -> SIGNED_LONG_LONG
  | C.Target_UnsignedLongLong -> UNSIGNED_LONG_LONG
(** Converts target int to SAST int types. *)

let size_type target = target_int target.C.target_size_type
let intmax_type target  = target_int target.C.target_intmax_type
let ptrdiff_type target  = target_int target.C.target_ptrdiff_type
let intptr_type target  = target_int target.C.target_intptr_type
let int64_type target  = target_int target.C.target_int64_type
let wchar_type target = target_int target.C.target_wchar_type
let wint_type target = target_int target.C.target_wint_type
let char16_type target = target_int target.C.target_char16_type
let char32_type target = target_int target.C.target_char32_type
let sigatomic_type target = target_int target.C.target_sigatomic_type
let processid_type target = target_int target.C.target_processid_type
(** Base integer type of a derived integer type. *)


                       
(** {2 Comments} *)

(** Ensure that comments are not duplicated. *)
let comment_unify (c1:comment list) (c2:comment list) : comment list =  
  match c1,c2 with
  | [], x | x, [] -> x
  | [a], [b] -> if a=b then [a] else [a;b]
  | _ ->
     (* could be improved, but we expect the lists to have length 1 at most *)
     List.sort_uniq compare (c1@c2)
  

(** {2 Type compatibility} *)


type type_cmp = {
    cmp_ignore_qual:bool;
    (** if true, type_compatible does not take  qualifiers into account in comparison *)
    cmp_int_size: bool;
    (** if true, type_compatible use integer size and signess instead of name *)
    cmp_enum_as_int: bool;
    (** if true, type_compatible handles an enum as an its integer type *)
    cmp_ignore_name: bool;
    (** if true, type_compatible disregards type names in comparison *)
    cmp_ignore_undefined: bool;
    (** if true, an undefined enum, struct or union compares equal to a defined one *)
    cmp_ignore_typedef: bool;
    (** if true, a typedef is replaced with its defining type during comparison *)
    cmp_ignore_array_size: bool;
    (** if true, arrays with undefined size compare equal to that of defined size *)
  }
(** Comfigures the test equality functions, to allow various relaxation. *)

let cmp_compatible = {
    cmp_ignore_qual = true;
    cmp_int_size = false;
    cmp_enum_as_int = false;
    cmp_ignore_name = false;
    cmp_ignore_undefined = true;
    cmp_ignore_typedef = true;
    cmp_ignore_array_size = true;
  }
(** Type compatibility. *)

let cmp_unifiable = {
    cmp_ignore_qual = true;
    cmp_int_size = false;
    cmp_enum_as_int = false;
    cmp_ignore_name = false;
    cmp_ignore_undefined = true;
    cmp_ignore_typedef = false;
    cmp_ignore_array_size = true;
  }
(** Unifiable compatibility. *)

let cmp_equal = {
    cmp_ignore_qual = false;
    cmp_int_size = false;
    cmp_enum_as_int = false;
    cmp_ignore_name = false;
    cmp_ignore_undefined = false;
    cmp_ignore_typedef = false;
    cmp_ignore_array_size = false;
  }
(** Strict type equality. *)


let rec type_compare cmp gray (target:C.target_info) (t1:typ) (t2:typ) =
  if log_type_compare then Printf.printf "type_compare: %s and %s\n" (string_of_type t1) (string_of_type t2);
  (t1 == t2) ||
    match t1,t2 with
    | T_void, T_void -> true

    | T_bool, T_bool -> true

    | T_integer i1, T_integer i2 ->
       if cmp.cmp_int_size then
         sizeof_int target i1 = sizeof_int target i2 &&
         signedness_int i1 = signedness_int i2
       else i1 = i2

    | T_float f1, T_float f2 ->
       f1 = f2

    | T_pointer p1, T_pointer p2 ->
       type_qual_compare cmp gray target p1 p2

    | T_array (a1,l1), T_array (a2,l2) ->
       type_qual_compare cmp gray target a1 a2 &&
         (match l1,l2 with
          | Length_cst c1, Length_cst c2 -> c1 = c2
          | Length_expr e1, Length_expr e2 -> true (* TODO *)
          | No_length, No_length -> true
          | No_length,_ -> cmp.cmp_ignore_array_size
          | _, No_length -> cmp.cmp_ignore_array_size
          | _ -> false
         )

    | T_bitfield (t1,l1), T_bitfield (t2,l2) ->
       type_compare cmp gray target t1 t2 && l1 = l2

    | T_function (Some f1), T_function (Some f2) ->
       type_qual_compare cmp gray target f1.ftype_return f2.ftype_return  &&
       List.length f1.ftype_params = List.length f2.ftype_params &&
       List.for_all2 (type_qual_compare cmp gray target) f1.ftype_params f2.ftype_params &&
       f1.ftype_variadic = f2.ftype_variadic
    | T_function None, T_function _ -> true
    | T_function _, T_function None -> true
    | T_builtin_fn, T_builtin_fn -> true

    | T_typedef t1, _ when cmp.cmp_ignore_typedef ->
       type_compare cmp gray target (fst t1.typedef_def) t2
    | _, T_typedef t2 when cmp.cmp_ignore_typedef ->
       type_compare cmp gray target t1 (fst t2.typedef_def)
    | T_typedef t1, T_typedef t2 ->
       (cmp.cmp_ignore_name || t1.typedef_org_name = t2.typedef_org_name) &&
       type_qual_compare cmp gray target t1.typedef_def t2.typedef_def

    | T_enum e1, _ when cmp.cmp_enum_as_int ->
       (not e1.enum_defined) || type_compare cmp gray target (T_integer e1.enum_integer_type) t2
    | _, T_enum e2 when cmp.cmp_enum_as_int ->
       (not e2.enum_defined) || type_compare cmp gray target t1 (T_integer e2.enum_integer_type)
    | T_enum e1, T_enum e2 ->
       (cmp.cmp_ignore_name || e1.enum_org_name = e2.enum_org_name) &&
       ((cmp.cmp_ignore_undefined && (not e1.enum_defined || not e2.enum_defined)) ||
          (List.length e1.enum_values = List.length e2.enum_values &&
           List.for_all2
             (fun v1 v2 ->
               v1.enum_val_org_name = v2.enum_val_org_name &&
               v1.enum_val_value = v2.enum_val_value
             ) e1.enum_values e2.enum_values
          )
       )

    | T_record r1, T_record r2 ->
       if Hashtbl.mem gray (r1.record_uid, r2.record_uid) then true
       else (
         Hashtbl.add gray (r1.record_uid, r2.record_uid) ();
         (cmp.cmp_ignore_name || r1.record_org_name = r2.record_org_name) &&
         (r1.record_kind = r2.record_kind) &&
         ((cmp.cmp_ignore_undefined && (not r1.record_defined || not r2.record_defined)) ||
           (Array.length r1.record_fields = Array.length r2.record_fields &&
            r1.record_sizeof = r2.record_sizeof &&
            let l1,l2 = Array.to_list r1.record_fields, Array.to_list r2.record_fields in
            List.for_all2
              (fun f1 f2 ->
                (cmp.cmp_ignore_name || (f1.field_org_name = f2.field_org_name)) &&
                 f1.field_offset = f2.field_offset &&
                 f1.field_bit_offset = f2.field_bit_offset &&
                 type_qual_compare cmp gray target f1.field_type f2.field_type
              )
              l1 l2
           )
         )
       )

    | _ -> false

and qual_compare cmp (q1:qualifier) (q2:qualifier) =
  cmp.cmp_ignore_qual || q1 = q2

and type_qual_compare cmp gray target ((t1,q1):type_qual) ((t2,q2):type_qual) =
  type_compare cmp gray target t1 t2 && qual_compare cmp q1 q2
(* internal functions passing along a set of gray nodes to avoid infinite loops on cyclic types *)

let type_compatible ctx a b = type_compare cmp_compatible (Hashtbl.create 16) ctx a b
let type_qual_compatible ctx a b = type_qual_compare cmp_compatible (Hashtbl.create 16) ctx a b
(** Type compatibility. Two declarations for the same object must have compatible types. *)

let type_equal ctx a b = type_compare cmp_equal (Hashtbl.create 16) ctx a b
let type_qual_equal ctx a b = type_qual_compare cmp_equal (Hashtbl.create 16) ctx a b
(** Strict type equality, to allow type merging. *)

let type_unifiable ctx a b = type_compare cmp_unifiable (Hashtbl.create 16) ctx a b
let type_qual_unifiable ctx a b = type_qual_compare cmp_unifiable (Hashtbl.create 16) ctx a b
(** Arguments are eligible to call type_unify. *)


let rec type_unify gray target (t1:typ) (t2:typ) =
  if !log_type_unify then Printf.printf "type_unify: %s and %s\n" (string_of_type t1) (string_of_type t2);
  if t1==t2 then t1 else
    match t1,t2 with

    | T_void, T_void -> t1
    | T_bool, T_bool -> t1

    | T_integer i1, T_integer i2 ->
       if sizeof_int target i1 = sizeof_int target i2 &&
          signedness_int i1 = signedness_int i2
       then t1
       else invalid_arg (Printf.sprintf "type_unify: incompatible integer types %s and %s" (string_of_integer_type i1) (string_of_integer_type i2))

    | T_float f1, T_float f2 ->
       if f1 <> f2 then invalid_arg (Printf.sprintf "type_unify: incompatible float types %s and %s" (string_of_float_type f1) (string_of_float_type f2))
       else t1

    | T_pointer p1, T_pointer p2 ->
       let p = type_qual_unify gray target p1 p2 in
       if p == p1 then t1 else T_pointer p

    | T_array (a1,l1), T_array (a2,l2) ->
       let a = type_qual_unify gray target a1 a2 in
       let l = match l1, l2 with
         | _, No_length -> l1
         | No_length, _ -> l2
         | Length_cst c1, Length_cst c2 when c1 = c2 -> l1
         | Length_expr _, Length_expr _ -> l1 (* TODO: check expressions? *)
         | _ -> invalid_arg "type_unify: incompatible array length"
       in
       if a==a1 && l==l1 then t1 else T_array (a,l)

    | T_bitfield (b1,l1), T_bitfield (b2,l2) ->
       let b = type_unify gray target b1 b2 in
       if l1 <> l2 then invalid_arg (Printf.sprintf "type_unify: incompatible bitfield length %i and %i" l1 l2);
       if b == b1 then t1 else T_bitfield (b,l1)

    | T_function _, T_function None -> t1
    | T_function None, T_function _ -> t2
    | T_function (Some f1), T_function (Some f2) ->
       let r = type_qual_unify gray target f1.ftype_return f2.ftype_return in
       if List.length f1.ftype_params <> List.length f2.ftype_params
       then invalid_arg (Printf.sprintf "type_unify: incompatible function parameter number %i and %i" (List.length f1.ftype_params) (List.length f2.ftype_params));
       let a = List.map2 (type_qual_unify gray target) f1.ftype_params f2.ftype_params in
       if f1.ftype_variadic <> f2.ftype_variadic
       then invalid_arg "type_unify: incompatible variadic function type";
       f1.ftype_return <- r;
       f2.ftype_return <- r;
       f1.ftype_params <- a;
       f2.ftype_params <- a;
       t1

    | T_builtin_fn, T_builtin_fn -> t1

    | T_typedef d1, T_typedef d2 ->
       typedef_unify gray target d1 d2;
       t1
    | T_typedef d1,_ ->
       let t = type_qual_unify gray target d1.typedef_def (t2,no_qual) in
       d1.typedef_def <- t;
       t1
    | _, T_typedef d2 ->
       let t = type_qual_unify gray target (t1,no_qual) d2.typedef_def in
       d2.typedef_def <- t;
       t1

    | T_enum e1, T_enum e2 ->
       enum_unify gray target e1 e2;
       t1

    | T_record r1, T_record r2 ->
       record_unify gray target r1 r2;
       t1

    | _ -> invalid_arg "type_unify: incompatible types"

and type_qual_unify gray target (t1,q1) (t2,q2) =
  type_unify gray target t1 t2, merge_qualifiers q1 q2

and typedef_unify gray target d1 d2 =
  if d1.typedef_org_name <> d2.typedef_org_name
  then invalid_arg ("typedef_unify: incompatible typedef names: "^ d1.typedef_org_name^" and "^d2.typedef_org_name);
  let t = type_qual_unify gray target d1.typedef_def d2.typedef_def in
  d1.typedef_def <- t;
  d2.typedef_def <- t;
  d2.typedef_unique_name <- d1.typedef_unique_name;
  let c = comment_unify d1.typedef_com d2.typedef_com in 
  d1.typedef_com <- c;
  d2.typedef_com <- c

and record_unify gray target r1 r2 =
  if !log_type_unify then Printf.printf "record_unify: %s and %s\n" (string_of_type (T_record r1)) (string_of_type (T_record r2));
  if Hashtbl.mem gray (r1.record_uid, r2.record_uid) then ()
  else (
    Hashtbl.add gray (r1.record_uid, r2.record_uid) ();
    if r1.record_org_name <> r2.record_org_name
    then invalid_arg (Printf.sprintf "record_unify: incompatible record names %s and %s" r1.record_org_name r2.record_org_name);
    if r1.record_kind <> r2.record_kind;
    then invalid_arg "record_unify: incompatible record kinds";
    (match r1.record_defined, r2.record_defined with
     | true, false ->
        r2.record_uid <- r1.record_uid;
        r2.record_unique_name <- r1.record_unique_name;
        r2.record_defined <- true;
        r2.record_sizeof <- r1.record_sizeof;
        r2.record_alignof <- r1.record_alignof;
        r2.record_fields <- r1.record_fields;
        r2.record_range <- r1.record_range
     | false, true ->
        r1.record_uid <- r2.record_uid;
        r1.record_unique_name <- r2.record_unique_name;
        r1.record_defined <- true;
        r1.record_sizeof  <- r2.record_sizeof;
        r1.record_alignof <- r2.record_alignof;
        r1.record_fields <- r2.record_fields;
        r1.record_range <- r2.record_range
     | true, true ->
        if r1.record_sizeof <> r2.record_sizeof
        then (
          invalid_arg ("record_unify: incompatible record sizeof "^(Z.to_string  r1.record_sizeof)^" and "^(Z.to_string r2.record_sizeof))
        );
        if r1.record_alignof <> r2.record_alignof
        then invalid_arg "record_unify: incompatible record alignof";
        if not (Array.length r1.record_fields = Array.length r2.record_fields)
        then invalid_arg (Printf.sprintf "record_unify: incompatible record field numbers %i and %i" (Array.length r1.record_fields) (Array.length r2.record_fields));
        for i=0 to Array.length r1.record_fields-1 do
          let f1, f2 = r1.record_fields.(i), r2.record_fields.(i) in
          if f1.field_org_name <> f2.field_org_name ||
             f1.field_offset <> f2.field_offset ||
             f1.field_bit_offset <> f2.field_bit_offset
          then invalid_arg "record_unify: incompatible record layout";
          let t = type_qual_unify gray target f1.field_type f2.field_type in
          f1.field_type <- t;
          f2.field_type <- t;
          let c = comment_unify f1.field_com f2.field_com in 
          f1.field_com <- c;
          f2.field_com <- c
        done
     | false, false -> ()
    );
    let c = comment_unify r1.record_com r2.record_com in 
    r1.record_com <- c;
    r2.record_com <- c
  )

and enum_unify gray target e1 e2 =
  if e1.enum_org_name <> e2.enum_org_name
  then invalid_arg "enum_unify: incompatible enum names";
  (match e1.enum_defined, e2.enum_defined with
   | true, false ->
      e2.enum_uid <- e1.enum_uid;
      e2.enum_unique_name <- e1.enum_unique_name;
      e2.enum_defined <- true;
      e2.enum_values <- e1.enum_values;
      e2.enum_integer_type <- e1.enum_integer_type;
      e2.enum_range <- e1.enum_range
   | false, true ->
      e1.enum_uid <- e2.enum_uid;
      e1.enum_unique_name <- e2.enum_unique_name;
      e1.enum_defined <- true;
      e1.enum_values <- e2.enum_values;
      e1.enum_integer_type <- e2.enum_integer_type;
      e1.enum_range <- e2.enum_range
   | true, true ->
      ignore (type_unify gray target (T_integer e1.enum_integer_type) (T_integer e2.enum_integer_type));
      if not
           (List.length e1.enum_values = List.length e2.enum_values &&
              List.for_all2
                (fun v1 v2 ->
                  v1.enum_val_org_name = v2.enum_val_org_name &&
                  v1.enum_val_value = v2.enum_val_value
                ) e1.enum_values e2.enum_values
           )
      then invalid_arg "enum_unify: incompatible enum values";
      List.iter2
        (fun v1 v2 ->
          let c = comment_unify v1.enum_val_com v2.enum_val_com in 
          v1.enum_val_com <- c;
          v2.enum_val_com <- c
        ) e1.enum_values e2.enum_values
   | false, false -> ()
  );
  let c = comment_unify e1.enum_com e2.enum_com in 
  e1.enum_com <- c;
  e2.enum_com <- c


let type_unify = type_unify (Hashtbl.create 16)
let type_unify_qual = type_qual_unify (Hashtbl.create 16)
let enum_unify target e1 e2 = enum_unify (Hashtbl.create 16) target e1 e2; e1
let record_unify target r1 r2 = record_unify (Hashtbl.create 16) target r1 r2; r1
let typedef_unify target d1 d2 = typedef_unify (Hashtbl.create 16) target d1 d2; d1
(** Type unification. *)


let rec is_void ((t,_):type_qual) = match t with
  | T_void -> true
  | T_typedef t -> is_void t.typedef_def
  | _ -> false



(** {2 Expressions utilities} *)


let expr_type ((_,t,_):expr) = t
(** Type of an expression. *)

let expr_integer_cst range (t:integer_type) (cst:Z.t) : expr =
  E_integer_literal cst, (T_integer t, no_qual), range

let expr_float_cst range (t:float_type) (cst:float) : expr =
  E_float_literal (string_of_float cst), (T_float t, no_qual), range

let expr_complex_cst range (t:float_type) (cst:float) : expr =
  E_float_literal (string_of_float cst), (T_complex t, no_qual), range

let expr_int_zero range : expr = expr_integer_cst range SIGNED_INT Z.zero
let expr_int_one range : expr = expr_integer_cst range SIGNED_INT Z.one

let expr_double_zero range : expr = expr_float_cst range DOUBLE 0.

let expr_bool_true range : expr =
  E_cast (expr_int_one range, IMPLICIT), (T_bool, no_qual), range

let expr_bool_false range : expr =
  E_cast (expr_int_zero range, IMPLICIT), (T_bool, no_qual), range

let expr_null range : expr =
  E_cast (expr_int_zero range, IMPLICIT), (T_bool, no_qual), range
(** (void* )0 *)

let expr_void range : expr =
  E_cast (expr_int_zero range, IMPLICIT), (T_void, no_qual), range
(** (void)0 *)


let rec zero_init range (t:typ) : init =
  match t with
  | T_void -> invalid_arg "zero_init: void type"
  | T_bool -> I_init_expr (expr_bool_false range)
  | T_integer i -> I_init_expr (expr_integer_cst range i Z.zero)
  | T_float f -> I_init_expr (expr_float_cst range f 0.)
  | T_complex f -> I_init_expr (expr_complex_cst range f 0.)
  | T_pointer tq -> I_init_expr (expr_null range)
  | T_array ((t,_),_) -> I_init_list ([], Some (zero_init range t))
  | T_bitfield (t,_) -> zero_init range t
  | T_function _ | T_builtin_fn -> invalid_arg "zero_init: function type"
  | T_typedef t -> zero_init range (fst t.typedef_def)
  | T_record r ->
     let l =
       if r.record_kind = UNION && r.record_fields <> [||] then [r.record_fields.(0)]
       else Array.to_list r.record_fields
     in
     I_init_list (List.map (fun f -> zero_init range (fst f.field_type)) l, None)
  | T_enum e -> I_init_expr (expr_integer_cst range e.enum_integer_type Z.zero)



(** {2 Statement utilities} *)


let make_block (s:statement list) : block =
  let v =
    (* local variables declared in s, but not in sub-blocks *)
    ListExt.map_filter (function (S_local_declaration v,_) -> Some v | _ -> None) s
  in
  { blk_stmts = s; blk_local_vars = v; }
(** Creates a block from a list of statements.
    Computes the list of local variables declared in the block and not 
    in sub-blocks.
 *)


module VarSet =
  SetExt.Make(struct
      type t = variable
      let compare a b = compare a.var_uid b.var_uid
    end)


let resolve_scope (b:block) : block =

  let gotos = ref []
  and labels = Hashtbl.create 16
  in

  (* update a scope updated, give source and destination scopes *)
  let update u src dst =
    u.scope_var_added   <- VarSet.elements (VarSet.diff dst src);
    u.scope_var_removed <- VarSet.elements (VarSet.diff src dst)
  in

  (* iterate on statements and expressions;
     fix break/continue/return scope;
     goto/switch scope are fixed after the iteration
   *)
  let rec stmt ((cur,brk,cnt,swt) as ctx) (s,r) =
    match s with
    | S_local_declaration _ -> ()
    | S_expression e -> expr ctx e
    | S_block b -> block ctx b
    | S_if (e,b1,b2) -> expr ctx e; block ctx b1; block ctx b2

    | S_while (e,b) | S_do_while (b,e) ->
       (* new scope for break and continue *)
       let ctx = cur, cur, cur, swt in
       expr ctx e; block ctx b

    | S_for (i,eo1,eo2,b) ->
       block ctx i;
       (* add the for init variable (if any) to the scope of the for *)
       let cur = VarSet.union cur (VarSet.of_list i.blk_local_vars) in
       (* new scope for break and continue *)
       let ctx = cur, cur, cur, swt in
       expr_opt ctx eo1;
       expr_opt ctx eo2;
       block ctx b

    | S_jump (S_goto (label, upd)) ->
       (* remember gotos to fix them later *)
       gotos := (label,r,upd,cur)::(!gotos)

    | S_jump (S_break upd) ->
       (* jump from current to break scope *)
       update upd cur brk

    | S_jump (S_continue upd) ->
       (* jump from current to continue scope *)
       update upd cur cnt

    | S_jump (S_return (_, upd)) ->
       (* jump from current to function return (empty scope) *)
       update upd cur VarSet.empty

    | S_jump (S_switch (e,b)) ->
       expr ctx e;
       (* new scope for break, remember the scope at switck for cases *)
       let ctx = cur, cur, cnt, cur in
       block ctx b

    | S_target (S_label label) ->
       (* remember label scopes to fix gotos later *)
       Hashtbl.add labels label cur

    | S_target (S_case (e,upd)) ->
       expr ctx e;
       (* jump from switch point to current scope *)
       update upd swt cur

    | S_target (S_default upd) ->
       (* jump from switch point to current scope *)
       update upd swt cur

  and block (cur,brk,cnt,swt) b =
    List.fold_left
      (fun cur sr ->
        stmt (cur,brk,cnt,swt) sr;
        (* add declated local variables to the scope along the way *)
        let cur = match fst sr with
          | S_local_declaration v -> VarSet.add v cur
          | _ -> cur
        in
        cur
      )
      cur b.blk_stmts
    |> ignore

  and expr ctx (e,_,_) =
    match e with
    | E_conditional (e1,e2,e3) -> List.iter (expr ctx) [e1;e2;e3]

    | E_binary_conditional (e1,e2)
    | E_array_subscript (e1,e2)
    | E_compound_assign (e1,_,_,e2,_)
    | E_binary (_,e1,e2)
    | E_assign (e1,e2)
    | E_comma (e1,e2)
    | E_atomic (_,e1,e2) -> List.iter (expr ctx) [e1;e2]

    | E_member_access (e1,_,_)
    | E_arrow_access (e1,_,_)
    | E_unary (_,e1)
    | E_increment (_,_,e1)
    | E_address_of e1
    | E_deref e1
    | E_cast (e1,_)
    | E_var_args e1 -> expr ctx e1

    | E_call (e,el) -> expr ctx e; Array.iter (expr ctx) el

    | E_character_literal _
    | E_integer_literal _
    | E_float_literal _
    | E_string_literal _
    | E_compound_literal _
    | E_variable _
    | E_function _
    | E_predefined _ -> ()

    | E_statement b -> block ctx b

  and expr_opt ctx eo = match eo with
    | None -> () | Some e -> expr ctx e

  in
  (* update block in-place *)
  let e = VarSet.empty in
  block (e,e,e,e) b;
  (* fix goto *)
  List.iter
    (fun (lbl,range,upd,src) ->
      try
        let dst = Hashtbl.find labels lbl in
        update upd src dst
      with Not_found ->
        failwith (Printf.sprintf "%s: unknown label '%s'"
                    (Clang_dump.string_of_range range) lbl)
    )
    !gotos;
  (* return the block *)
  b
(** Fill-in scope_update information in the AST.
    The block is modified in-place, and returned.
    Call after AST transformations that may change variable scopes.
 *)


(** {2 Errors} *)

let error range msg arg =
  failwith (Printf.sprintf "%s: %s: %s" (C.string_of_range range) msg arg)

let warning range msg arg =
  (* Printf.eprintf "WARNING %s: %s: %s\n" (C.string_of_range range) msg arg *)
  ()
