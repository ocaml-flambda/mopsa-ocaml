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
  Clang_dump - Simple (and ugly) printer for Clang_AST, used for debugging
 *)

open Clang_AST

   
(** {2 Names for constant variants} *)

val lang_name : lang -> string
val builtin_type_name : builtin_type -> string
val binary_operator_name : binary_operator -> string
val compound_assign_operator_name : compound_assign_operator -> string
val unary_operator_name : unary_operator -> string
val storage_class_name : storage_class -> string
val record_kind_name : record_kind -> string
val cast_kind_name : cast_kind -> string
val character_kind_name : character_kind -> string
val ident_type_name : ident_type -> string
val unary_expr_or_type_name : unary_expr_or_type -> string
val array_type_trait_name : array_type_trait -> string
val access_specifier_name : access_specifier -> string
val construction_kind_name : construction_kind -> string
val overloaded_operator_name : overloaded_operator -> string
val initialization_style_name : initialization_style -> string
val expression_trait_name : expression_trait -> string
val storage_duration_name : storage_duration -> string
val type_trait_name : type_trait -> string
val diag_level_name : diag_level -> string
val target_int_type_name : target_int_type -> string
val target_real_type_name : target_real_type -> string
val builtin_template_kind_name : builtin_template_kind -> string
val ref_qualifier_name : ref_qualifier -> string
val lambda_capture_default_name : lambda_capture_default -> string
val lambda_capture_kind_name : lambda_capture_kind -> string
val decl_kind_name : decl_kind -> string
val type_kind_name : typ -> string
val expr_kind_name : expr_kind -> string
val stmt_kind_name : stmt_kind -> string


(** {2 Basic AST and parsing structures} *)

val string_of_loc : loc -> string
val string_of_range : range -> string
val string_of_diagnostic : diagnostic -> string
val string_of_target_EABI : target_EABI -> string
val string_of_target_options : target_options -> string
val string_of_target_info : target_info -> string


(** {2 Names from declarations} *)

val name : name -> string
val enum_name : enum_decl -> string
val record_name : record_decl -> string


(** {2 High-level AST constructions} *)

val string_of_decl : decl -> string
val string_of_type : typ -> string
val string_of_type_qual : type_qual -> string
val string_of_expr : expr -> string
val string_of_stmt : stmt -> string
  
