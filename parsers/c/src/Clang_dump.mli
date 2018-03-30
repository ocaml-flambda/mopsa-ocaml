val sync : 'a -> unit
val bp_option :
  string -> (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a option -> unit
val bp_array :
  (Buffer.t -> 'a -> unit) -> string -> Buffer.t -> 'a array -> unit
val bp_list :
  (Buffer.t -> 'a -> unit) -> string -> Buffer.t -> 'a list -> unit
val lang_name : Clang_AST.lang -> string
val builtin_type_name : Clang_AST.builtin_type -> string
val binary_operator_name : Clang_AST.binary_operator -> string
val compound_assign_operator_name :
  Clang_AST.compound_assign_operator -> string
val unary_operator_name : Clang_AST.unary_operator -> string
val storage_class_name : Clang_AST.storage_class -> string
val record_kind_name : Clang_AST.record_kind -> string
val cast_kind_name : Clang_AST.cast_kind -> string
val character_kind_name : Clang_AST.character_kind -> string
val ident_type_name : Clang_AST.ident_type -> string
val unary_expr_or_type_name : Clang_AST.unary_expr_or_type -> string
val array_type_trait_name : Clang_AST.array_type_trait -> string
val access_specifier_name : Clang_AST.access_specifier -> string
val construction_kind_name : Clang_AST.construction_kind -> string
val overloaded_operator_name : Clang_AST.overloaded_operator -> string
val initialization_style_name : Clang_AST.initialization_style -> string
val expression_trait_name : Clang_AST.expression_trait -> string
val storage_duration_name : Clang_AST.storage_duration -> string
val type_trait_name : Clang_AST.type_trait -> string
val diag_level_name : Clang_AST.diag_level -> string
val target_int_type_name : Clang_AST.target_int_type -> string
val target_real_type_name : Clang_AST.target_real_type -> string
val builtin_template_kind_name : Clang_AST.builtin_template_kind -> string
val ref_qualifier_name : Clang_AST.ref_qualifier -> string
val lambda_capture_default_name : Clang_AST.lambda_capture_default -> string
val lambda_capture_kind_name : Clang_AST.lambda_capture_kind -> string
val decl_kind_name : Clang_AST.decl_kind -> string
val type_kind_name : Clang_AST.typ -> string
val expr_kind_name : Clang_AST.expr_kind -> string
val stmt_kind_name : Clang_AST.stmt_kind -> string
val string_of_loc : Clang_AST.loc -> string
val string_of_range : Clang_AST.range -> string
val string_of_diagnostic : Clang_AST.diagnostic -> string
val string_of_target_EABI : Clang_AST.target_EABI -> string
val string_of_target_options : Clang_AST.target_options -> string
val string_of_target_info : Clang_AST.target_info -> string
val name : Clang_AST.name -> string
val enum_name : Clang_AST.enum_decl -> string
val record_name : Clang_AST.record_decl -> string
module P :
  sig
    val p : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
    val name : Buffer.t -> Clang_AST.name -> unit
    val decl : string -> Buffer.t -> Clang_AST.decl -> unit
    val decl_name : Buffer.t -> Clang_AST.decl -> unit
    val namespace_decl : Buffer.t -> Clang_AST.namespace_decl -> unit
    val template_parameter_list :
      string -> Buffer.t -> Clang_AST.template_parameter_list -> unit
    val decl_list : string -> Buffer.t -> Clang_AST.decl list -> unit
    val function_decl : string -> Buffer.t -> Clang_AST.function_decl -> unit
    val cxx_method_decl : Buffer.t -> Clang_AST.cxx_method_decl -> unit
    val cxx_method_kind : Buffer.t -> Clang_AST.cxx_method_kind -> unit
    val cxx_constructor_initializer :
      Buffer.t -> Clang_AST.cxx_constructor_initializer -> unit
    val function_template_specialization :
      Buffer.t -> Clang_AST.function_template_specialization -> unit
    val var_decl : string -> Buffer.t -> Clang_AST.var_decl -> unit
    val var_decl_name : Buffer.t -> Clang_AST.var_decl -> unit
    val var_template_specialization :
      Buffer.t -> Clang_AST.var_template_specialization -> unit
    val enum_decl : Buffer.t -> Clang_AST.enum_decl -> unit
    val record_decl : string -> Buffer.t -> Clang_AST.record_decl -> unit
    val record_decl_name : Buffer.t -> Clang_AST.record_decl -> unit
    val field_decl : Buffer.t -> Clang_AST.field_decl -> unit
    val class_template_specialization :
      Buffer.t -> Clang_AST.class_template_specialization -> unit
    val cxx_base_specifier : Buffer.t -> Clang_AST.cxx_base_specifier -> unit
    val friend_decl : string -> Buffer.t -> Clang_AST.friend_decl -> unit
    val param_var_decl : Buffer.t -> Clang_AST.param_var_decl -> unit
    val function_decl_name : Buffer.t -> Clang_AST.function_decl -> unit
    val type_qual : Buffer.t -> Clang_AST.type_qual -> unit
    val qual : Buffer.t -> Clang_AST.qual -> unit
    val typ : Buffer.t -> Clang_AST.typ -> unit
    val template_type_param_type :
      Buffer.t -> Clang_AST.template_type_param_type -> unit
    val designator : Buffer.t -> Clang_AST.designator -> unit
    val offsetof : Buffer.t -> Clang_AST.offsetof_node -> unit
    val declaration_name : Buffer.t -> Clang_AST.declaration_name -> unit
    val name_specifier_loc :
      Buffer.t -> Clang_AST.name_specifier_loc list -> unit
    val name_specifier : Buffer.t -> Clang_AST.name_specifier list -> unit
    val name_specifier_kind : Buffer.t -> Clang_AST.name_specifier -> unit
    val template_argument_loc :
      Buffer.t -> Clang_AST.template_argument_loc -> unit
    val template_argument : Buffer.t -> Clang_AST.template_argument -> unit
    val template_name : Buffer.t -> Clang_AST.template_name -> unit
    val cxx_construct_expr : Buffer.t -> Clang_AST.cxx_construct_expr -> unit
    val capture : Buffer.t -> Clang_AST.capture -> unit
    val block_decl : string -> Buffer.t -> Clang_AST.block_decl -> unit
    val expr : Buffer.t -> Clang_AST.expr -> unit
    val lambda_capture : Buffer.t -> Clang_AST.lambda_capture -> unit
    val stmt : string -> Buffer.t -> Clang_AST.stmt -> unit
  end
val string_from_buffer : (Buffer.t -> 'a -> unit) -> 'a -> string
val string_of_decl : Clang_AST.decl -> string
val string_of_type : Clang_AST.typ -> string
val string_of_type_qual : Clang_AST.type_qual -> string
val string_of_expr : Clang_AST.expr -> string
val string_of_stmt : Clang_AST.stmt -> string
