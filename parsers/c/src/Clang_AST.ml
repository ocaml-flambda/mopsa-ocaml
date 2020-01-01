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
  Clang_AST - Raw Clang AST, in OCaml

  The definition of the AST types returned by Clang_parser.

  Tested with Clang 5.0 up to 7.0.1.
  As it was initially based on Clang 4, some features introduced in
  Clang 5 and later may be missing.
 *)


(**
   We support C and a subset of C++.

   Not supported:
   - Asm strings
   - attributes
   - C++ coroutines
   - C++ structured exception handling
   - Visual C++ specific (#pragma comment, etc.)
   - Objective-C
   - openMP
   - openCL
   - features added in Clang 5 and later
 *)

(**
  A function, enum, struct, etc. can be declared several times and defined
  at most once in a translation unit. Clang distinguishes every declaration
  and definition in the AST. We don't. Either the element is effectively
  defined, and we always expose the definition, or it is not, and we return
  the declarations.
  This may change in the future in case this turns out to be a bad idea.
  (we could do as Clang and always expose each and every declaration, and
  include an indirect pointer to the definition, if any).
 *)



(** {2 Locations} *)


type loc = {
    loc_line: int;
    loc_column: int;
    loc_file: string; (** empty for invalid locations *)
  }
(** Location in a source file *)

type range = {
    range_begin: loc;
    range_end: loc;
  }
(** Location range in a source file *)



(** {2 Comments} *)

type comment_kind =
  | RCK_Invalid (** Invalid comment. *)
  | RCK_OrdinaryBCPL (** Any normal BCPL comments. *)
  | RCK_OrdinaryC (** Any normal C comment. *)
  | RCK_BCPLSlash (** /// stuff  *)
  | RCK_BCPLExcl  (** //! stuff  *)
  | RCK_JavaDoc (** /** stuff */  *)
  | RCK_Qt (** /*! stuff */, also used by HeaderDoc *)
  | RCK_Merged 	(** Two or more documentation comments merged together. *)
           
type comment = {
    com_text: string; (** Raw comment text with comment markers. *)
    com_kind: comment_kind;
    com_range: range;
  }
(** Raw comment found in the source file. *)


(** {2 Macros} *)

type macro = {
    macro_name: string; (** Macro name *)
    macro_params: string list; (** List of parameter names *)
    macro_contents: string list; (** List of source tokens (as string) *)
    macro_loc: loc; (** Macro location *)
  }
             
             

(** {2 AST} *)

type uid = int
(**
 Nodes that can be referenced from several points of the AST (such as
 declaration) are tagged with an identifier which is unique across
 a translation unit.
 *)

type lang = Lang_C | Lang_CXX
(** Language in extern declaration. *)


type name = {
    name_print: string; (** human-readable name for the declaration, even if it is one of the special kinds of names (C++ constructor, etc. *)
    name_qualified: string; (** fully-qualified human-readable name (with namespace) *)
    name_declaration: declaration_name; (** actual (internal) name of the declaration, which may be a special name *)
  }

 and enum_decl = {
     enum_uid: uid; (** Unique declaration identifier *)
     enum_name: name;
     enum_num_positive_bits: int; (** Width in bits required to store all the non-negative enumerators of this enum. *)
     enum_num_negative_bits: int; (** Width in bits required to store all the negative enumerators of this enum. *)
     enum_is_complete: bool; (** true if this can be considered a complete type. *)
     enum_integer_type: type_qual option; (** Integer type this enum decl corresponds to. *)
     enum_promotion_type: type_qual option; (** Return the integer type that enumerators should promote to. *)
     enum_cst: enum_cst_decl list;
     enum_typedef: typedef_decl option; (** for anonymous enum, gets the typedef it is declared in, if any  *)
     enum_range: range;
     enum_com: comment list;
   }
 (** enum declaration. *)

 and enum_cst_decl = {
     enum_cst_uid: uid;
     enum_cst_name: name;
     enum_cst_val: Z.t;
     enum_cst_range: range;
     enum_cst_com: comment list;
   }
 (** enum constant declaration in an enum. *)

 and field_decl = {
     field_uid: uid;
     field_name: name;
     field_index: int;
     field_type: type_qual;
     field_bitwidth: int option; (** bit-field width, in bits, or none if not a bit-field *)
     field_is_unnamed_bitfield: bool; (** whether this is an unnamed bitfield. *)
     field_is_in_valid_record: bool; (** whether the field is in a record with no semantic error and thus has a valid layout information; otherwise, offset information are set to 0 *)
     field_offset: Int64.t; (** offset of the given field, in bits. *)
     field_variable_length_array: expr option; (** whether this field captures the variable length array type, and the corresponding size expression *)
     field_range: range;
     field_com: comment list;
   }
 (** field declaration in a struct or union. *)

 and record_decl = {
     record_uid: uid;
     record_name: name;
     record_kind: record_kind; (** distinguish between struct and union *)
     record_has_flexible_array_member: bool;
     record_has_volatile_member: bool;
     record_is_anonymous: bool; (** Whether this is an anonymous struct or union. To be an anonymous struct or union, it must have been declared without a name and there must be no objects of this type declared *)
     record_is_complete: bool; (** true if this can be considered a complete type. *)
     record_is_valid: bool; (** true if it has no semantic error and thus has a valid layout information; otherwise, size and offset information are set to 0 *)
     record_size: Int64.t; (** record size in characters, including padding  *)
     record_data_size: Int64.t; (** record size in characters, without padding *)
     record_alignment: Int64.t; (** record alignment in characters.  *)
     record_fields: field_decl list;
     record_typedef: typedef_decl option; (** for anonymous records, gets the typedef it is declared in, if any  *)
     record_range: range;
     record_com: comment list;

     (* C++ specific *)
     record_template: class_template_specialization option; (** (C++) template origin *)
     record_base_class: cxx_base_specifier list; (** (C++) direct (virtual and non virtual) base classes *)
     record_methods: function_decl list; (** (C++) all methods, including ctor, dtor, operators *)
     record_friends: friend_decl list; (** (C++) friends *)
   }
 (** struct or union declaration.  *)

 and cxx_base_specifier = {
     cxx_base_is_virtual: bool; (**  whether the base class is a virtual base class *)
     cxx_base_is_base_of_class: bool; (** whether this base class is a base of a class declared with the 'class' keyword (vs. one declared with the 'struct' keyword) *)
     cxx_base_is_pack_expansion: bool; (** whether this base specifier is a pack expansion *)
     cxx_base_get_inherit_constructors: bool; (** whether this base class's constructors get inherited *)
     cxx_base_access: access_specifier; (**  the access specifier for this base specifier; this is the actual base specifier as used for semantic analysis, so the result can never be AS_none *)
     cxx_base_type: type_qual; (**  the type of the base class; this type will always be an unqualified class type. *)
   }

 and record_kind = Record_Struct | Record_Union | Record_Class | Record_Interface
 (** struct or union kind
    (class and interface are not used now but here for future support of C++ / ObjC)
  *)

 and typedef_decl = {
     typedef_uid: uid;
     typedef_name: name;
     typedef_underlying_type: type_qual;
     typedef_range: range;
     typedef_com: comment list;
   }
 (** typedef declaration *)

 and function_decl = {
     function_uid: uid;
     function_name: name;
     function_body: stmt option; (** Retrieve the body (definition) of the function; None if declared but not defined in the translation unit *)
     function_is_variadic: bool; (** Whether this function is variadic. *)
     function_is_main: bool; (** Whether this function is "main", which is the entry point into an executable program. *)
     function_is_global: bool; (** Whether this is a global function. *)
     function_storage_class: storage_class; (** Storage class as written in the source. *)
     function_return_type: type_qual;
     function_params: param_var_decl array;
     function_range: range; (** Range of the declaration. *)
     function_name_range: range; (** Range of the name in the declaration. *)
     function_com: comment list;

     (* C++ *)
     function_template: function_template_specialization option; (** (C++) template origin *)
     function_overloaded_operator: overloaded_operator option; (** (C++) whether this is an overloaded operator *)
     function_method: cxx_method_decl option; (** (C++) additional information for methods; None if not a method *)
   }
 (** Represents a function declaration or definition. *)

 and cxx_method_decl = {
     method_parent_class: record_decl; (** parent class containing the method declaration *)
     method_is_instance: bool; (** instance method is true, static method if false *)
     method_is_virtual: bool; (** virtual method *)
     method_overridden: decl list; (** overridden declarations *)
     method_ref_qualifier: ref_qualifier; (** ref-qualifier associated with this method *)
     method_kind: cxx_method_kind
   }
 (** (C++) Additional function information for a method. *)

 and cxx_method_kind =
   | Method_Regular (** regular method *)
   | Method_Constructor of cxx_constructor_decl (** constructor *)
   | Method_Destructor (** destructor *)
   | Method_Conversion of bool (** conversion method; true if explicit *)

 and cxx_constructor_decl = {
     constructor_initializers: cxx_constructor_initializer list;
     constructor_explicit: bool; (** whether this function is explicit *)
     constructor_delegating: bool;(** whether the constructor creates a delegating construtor *)
     constructor_inheriting: bool; (** whether this is an implicit constructor synthesized to model a call to a constructor inherited from a base class *)
     constructor_target: function_decl option; (** when this constructor delegates to another, retrieve the target *)
     constructor_inherited: function_decl option; (** whether this base class's constructors get inherited. *)
   }
 (** (C++) Additional information for constructors. *)

 and cxx_constructor_initializer =
   | Constructor_init_Base of type_qual (** base class type *) * bool (** is virtual *)  * expr (** initializer *)
   | Constructor_init_Field of field_decl * expr
   | Constructor_init_Indirect_field of indirect_field_decl * expr
   | Constructor_init_Delegating of type_qual * expr
 (** (C++) Represents a C++ base or member initializer. This is part of a constructor initializer that initializes one non-static member variable or one base class. *)

 and var_decl = {
     var_uid: uid;
     var_name: name;
     var_type: type_qual;
     var_storage_class: storage_class; (** Storage class as written in the source. *)
     var_init: expr option; (** Optional initializer *)
     var_is_file_scoped: bool; (** File scoped variable declaration *)
     var_is_local: bool; (** Whether the variable is local (incl. parameter) *)
     var_range: range;
     var_com: comment list;

     (* C++ *)
     var_template: var_template_specialization option; (** (C++) template origin *)
   }
 (** Represents a variable declaration or definition. *)

 and param_var_decl = var_decl
 (** We identify parameter declarations with variable declarations. *)

 and storage_class =
   | SC_None
   | SC_Extern
   | SC_Static
   | SC_PrivateExtern
   | SC_Auto
   | SC_Register
 (** Storage classes. *)

 and qual = {
     qual_is_const: bool;
     qual_is_restrict: bool;
     qual_is_volatile: bool;
   }
 (** Type qualifiers. *)

 and typ =

   (* C TYPES *)

   | DecayedType of (** decayed type *) type_qual * (** pointee type *) type_qual
   (** A pointer type decayed from an array or function type. *)

   | ArrayType of array_type
   (** Array Declarators. *)

   | AtomicType of type_qual

   | AttributedType of type_qual
   (** An attributed type is a type to which a type attribute has been applied. TODO: attribute information. *)

   | BuiltinType of builtin_type

   | ComplexType of type_qual
   (** Complex values. *)

   | FunctionProtoType of fun_proto_type
   (** Represents a prototype with parameter type info, e.g. int foo(int)' or 'int foo(void)'. *)

   | FunctionNoProtoType of fun_no_proto_type
   (** Represents a K&R-style 'int foo()' function, which has no information available about its arguments. *)

   | ParenType of type_qual
   (** Sugar for parentheses used when specifying types. *)

   | PointerType of type_qual
   (** Pointer declarators. *)

   | EnumType of enum_decl
   (** Enumeration type. *)

   | RecordType of record_decl
   (** Struct or union type. *)

   | TypedefType of typedef_decl
   (** Type defined as a typedef *)

   | ElaboratedType of type_qual
   (** Represents a type that was referred to using an elaborated type keyword. *)

   | UnaryTransformType of unary_transform_type
   (** A unary type transform, which is a type constructed from another. *)

   | TypeOfExprType of expr
   (** Represents a typeof expression (a GCC extension). *)

   | TypeOfType of type_qual
   (** Represents a typeof(type), a gcc extension. *)


   (* C++ TYPES *)

   | DecltypeType of expr * type_qual option
   (** (C++) Represents the type decltype(expr) (C++11). *)

   | AutoType of bool (** true for decltype(auto), false for auto *)
   (** (C++) Represents a C++11 auto or C++14 decltype(auto) type. *)

   | DeducedTemplateSpecializationType of template_name
   (** (C++) Represents a C++17 deduced template specialization type. *)

   | DependentSizedExtVectorType of expr * type_qual
   (** (C++) Represents an extended vector type where either the type or size is dependent. *)

   | InjectedClassNameType of type_qual * template_name option * record_decl * template_argument array
   (** (C++) The injected class name of a C++ class template or class template partial specialization. *)

   | MemberPointerType of type_qual (** pointee type *) * typ (** class *)
   (** (C++) A pointer to member type per C++ 8.3.3 - Pointers to members.  *)

   | PackExpansionType of type_qual (** pattern *) * int option (** number of expansions, if known *)
   (** (C++) Represents a pack expansion of types. *)

   | LValueReferenceType of type_qual
   (** (C++) An lvalue reference type, per C++11 [dcl.ref]. *)

   | RValueReferenceType of type_qual
   (** (C++) An rvalue reference type, per C++11 [dcl.ref].  *)

   | SubstTemplateTypeParmPackType of string * template_type_param_type * template_argument
   (** (C++) Represents the result of substituting a set of types for a template type parameter pack. *)

   | SubstTemplateTypeParmType of template_type_param_type * type_qual
   (** (C++) Represents the result of substituting a type for a template type parameter. *)

   | TemplateSpecializationType of type_qual option (** aliased type *) * template_name * template_argument array
   (** (C++) Represents a type template specialization; the template must be a class template, a type alias template, or a template template parameter. *)

   | TemplateTypeParmType of template_type_param_type
   (** (C++) *)

   | DependentNameType of name_specifier list * string

   (** (C++) Represents a qualified type name for which the type name is dependent. *)

   | DependentTemplateSpecializationType of name_specifier list * string * template_argument array
   (** (C++) Represents a template specialization type whose template cannot be resolved, e.g. A<T>::template B<T> *)

   | UnresolvedUsingType of unresolved_using_typename_decl
   (** (C++) Represents the dependent type named by a dependently-scoped typename using declaration, e.g. using typename Base<T>::foo; Template instantiation turns these into the underlying type. *)


   (* Vectors *)

   | VectorType of type_qual * int (** number of elements *) * int (** kind *)
   (** Represents a GCC generic vector type. *)


   (* Unknown *)
   | UnknownType of (** type class *) int * (** type class name *) string
 (** Types *)

 and builtin_type =
   | Type_Void (** void *)
   | Type_Bool (** _Bool in C99 *)
   | Type_Char_U (** char for targets where it's unsigned *)
   | Type_UChar (** unsigned char, explicitly qualified  *)
   | Type_WChar_U (** wchar_t for targets where it's unsigned *)
   | Type_Char16 (** (C++) char16_t *)
   | Type_Char32 (** (C++) char32_t *)
   | Type_UShort (** unsigned short *)
   | Type_UInt (** unsigned int *)
   | Type_ULong (** unsigned long *)
   | Type_ULongLong (** unsigned long long *)
   | Type_UInt128 (** _uint128_t *)
   | Type_Char_S (** char for targets where it's signed *)
   | Type_SChar (** signed char, explicitly qualified *)
   | Type_WChar_S (** wchar_t for targets where it's signed *)
   | Type_Short (** short or signed short *)
   | Type_Int (** int or signed int *)
   | Type_Long (** long or signed long *)
   | Type_LongLong (** long long or signed long long  *)
   | Type_Int128 (** __int128_t *)
   | Type_Half (** __fp16 *)
   | Type_Float (** float *)
   | Type_Double (** double *)
   | Type_LongDouble (** long double *)
   | Type_Float128 (** __float128  *)
   | Type_NullPtr (** (C++0x) nullptr *)
   | Type_ObjCId (** primitive ObjC id *)
   | Type_ObjCClass (** pimitive ObjC Class *)
   | Type_ObjCSel (** primitive ObjC SEL *)
   | Type_OCLSampler (** sampler_t *)
   | Type_OCLEvent (** event_t *)
   | Type_OCLClkEvent (** clk_event_t *)
   | Type_OCLQueue (** queue_t *)
   | Type_OCLReserveID (** reserved_id_t *)
   | Type_Dependent (** expression whose type is totally unknown *)
   | Type_Overload (** unresolved overload set *)
   | Type_BoundMember (** (C++) bound non-static member function  *)
   | Type_PseudoObject (** pseudo-object such ObjC \@property *)
   | Type_UnknownAny (** __builtin_any_type *)
   | Type_BuiltinFn (** *)
   | Type_ARCUnbridgedCast (** case which in ARC would normally requier a __bridge *)
   | Type_OMPArraySection (** placeholder type for OpenMP array sections *)
 (** Builtin types *)

 and unary_transform_type = {
     unary_underlying_type: type_qual;
     unary_base_type: type_qual;
     unary_kind: unary_transform_kind;
   }
 (** A unary type transform, which is a type constructed from another. *)

 and unary_transform_kind = EnumUnderlyingType

 and array_type = {
     array_element_type: type_qual;
     array_size: array_size;
     array_size_modifier: array_size_modifier;
   }
 (** Array Declarators. *)

 and fun_proto_type = {
     proto_result_type: type_qual; (** type of an expression that calls a function of this type. *)
     proto_return_type: type_qual; (** return type *)
     proto_qual: qual; (** qualifiers *)
     proto_params: type_qual array; (** parameter types *)
     proto_variadic: bool; (** is the function variadic? *)
     proto_exception_spec: exception_specification_type; (** (C++) exception specification *)
     proto_noexcept_result: noexcept_result; (** (C++) interpretation of noexpect spec (Clang < 7) *)
     proto_exceptions: type_qual array; (** (C++) exception list in specification *)
     proto_has_trailing_return: bool; (** (C++) *)
     proto_ref_qualifier: ref_qualifier; (** (C++) ref-qualifier associated with this function type *)
   }
 (** Represents a prototype with parameter type info, e.g. 'int foo(int)' or 'int foo(void)'. 'void' is represented as having no parameters, not as having a single void parameter. *)

 and exception_specification_type =
   | EST_None (** no exception specification *)
   | EST_DynamicNone (** throw() *)
   | EST_Dynamic (** throw(T1, T2) *)
   | EST_MSAny (** Microsoft throw(...) extension *)
   | EST_BasicNoexcept (** noexcept *)
   | EST_ComputedNoexcept (** noexcept(expression) (Clang < 7) *)
   | EST_Unevaluated (** not evaluated yet, for special member function *)
   | EST_Uninstantiated (** not instantiated yet *)
   | EST_Unparsed (** not parsed yet *)
   | EST_DependentNoexcept (** noexcept(expression) (Clang >= 7) *)
   | EST_NoexceptFalse (** noexcept(false) (clang >= 7) *)
   | EST_NoexceptTrue (** noexcept(true) (clang >= 7) *)

and noexcept_result =
  | NR_NoNoexcept (** There is no noexcept specifier. *)
  | NR_BadNoexcept (** The noexcept specifier has a bad expression. *)
  | NR_Dependent (** The noexcept specifier is dependent. *)
  | NR_Throw (** The noexcept specifier evaluates to false. *)
  | NR_Nothrow (** The noexcept specifier evaluates to true. *)

 and fun_no_proto_type = {
     noproto_result_type: type_qual; (** type of an expression that calls a function of this type. *)
   }
 (** Represents a K&R-style 'int foo()' function, which has no information available about its arguments. *)

 and array_size =
   | Size_Constant of Z.t (** C arrays with a specified constant size. *)
   | Size_Variable of expr (** C array with a specified size that is not an integer-constant-expression *)
   | Size_Incomplete (** C array with an unspecified size *)
   | Size_Dependent (** (C++) array with dependent size *)
 (** Array size *)

 and array_size_modifier =
   | Size_Normal (** no modifier *)
   | Size_Static (** static keyword, as in int array[static 2] *)
   | Size_Star (** star modifier, as in int array[*] *)
(** Array size modifier *)

and ref_qualifier =
  | RQ_None (** No ref-qualifier was provided *)
  | RQ_LValue (** An lvalue ref-qualifier was provided (&) *)
  | RQ_RValue (** An rvalue ref-qualifier was provided (&&) *)
(** (C++) The kind of C++11 ref-qualifier associated with a function type. This determines whether a member function's "this" object can be an lvalue, rvalue, or neither. *)

and template_type_param_type = {
    template_type_param_depth: int;
    template_type_param_index: int;
    template_type_param_is_parameter_pack: bool;
    template_type_param_decl: template_type_param_decl option;
    template_type_param_identifier: string option;
  }

and type_qual = typ * qual
 (** Qualified type *)

 (** Possible kinds of declarations and definitions. *)
 and decl_kind =

   | TranslationUnitDecl of decl list
   (** The top declaration context. *)

   | EmptyDecl
   (** Represents an empty-declaration. *)

   | FieldDecl of field_decl
   (** Represents a field in a union or struct. *)

   | EnumConstantDecl of enum_cst_decl
   (** Represents an enumeration constant in an enum. *)

   | FileScopeAsmDecl of string

   | LinkageSpecDecl of lang * decl list
   (** This represents a linkage specification. For example: extern "C" void foo(); *)

   | LabelDecl of name
   (** Represents the declaration of a label. *)

   | EnumDecl of enum_decl
   (** Represents an enum. In C++11, enums can be forward-declared with a fixed underlying type, and in C we allow them to be forward-declared with no underlying type as an extension. *)

   | RecordDecl of record_decl
   (** Represents a struct, union, class.  *)

   | TypedefDecl of typedef_decl
   (** Represents the declaration of a typedef-name via the 'typedef' type specifier. *)

   | FunctionDecl of function_decl
   (** Represents a function declaration or definition. *)

   | VarDecl of var_decl
   (** Represents a variable declaration or definition. *)


   (* C++ DECLARATIONS *)

   | AccessSpecDecl
   (** (C++) Represents an access specifier followed by colon ':' *)

   | BlockDecl of block_decl
   (** (C++) This represents a block literal declaration, which is like an unnamed FunctionDecl. *)

   | FriendDecl of friend_decl
   (** (C++) Represents the declaration of a friend entity, which can be a function, a type, or a templated function or type. *)

   | StaticAssertDecl of static_assert
   (** (C++) Represents a C++11 static_assert declaration. *)

   | NamespaceAliasDecl of namespace_alias_decl
   (** (C++) Represents a C++namespace alias. *)

   | NamespaceDecl of namespace_decl
   (** (C++) Represent a C++ namespace. *)

   | BuiltinTemplateDecl of builtin_template_decl
   (** (C++) Represents the builtin template declaration which is used to implement __make_integer_seq and other builtin templates *)

   | ClassTemplateDecl of class_template_decl
   (** (C++) Declaration of a class template. *)

   | FunctionTemplateDecl of function_template_decl
   (** (C++) Declaration of a template function. *)

   | TypeAliasTemplateDecl of type_alias_template_decl
   (** (C++) Declaration of an alias template. *)

   | VarTemplateDecl of var_template_decl
   (** (C++) Declaration of a variable template. *)

   | TemplateTemplateParmDecl of template_template_param_decl
   (** (C++) Declares a template template parameter. *)

   | TemplateTypeParmDecl of template_type_param_decl
   (** (C++) Declaration of a template type parameter. *)

   | TypeAliasDecl of typedef_decl
   (** (C++) Represents the declaration of a typedef-name via a C++0x alias-declaration. *)

   | UnresolvedUsingTypenameDecl of unresolved_using_typename_decl
   (** (C++) Represents a dependent using declaration which was marked with typename. *)

   | UsingDecl of using_decl
   (** (C++) Represents a C++ using-declaration. *)

   | UsingDirectiveDecl of using_directive_decl
   (** (C++) Represents C++ using-directive. *)

   | UsingPackDecl of using_pack_decl
   (** (C++) Represents a pack of using declarations that a single using-declarator pack-expanded into. *)

   | UsingShadowDecl of using_shadow_decl
   (** (C++) Represents a shadow declaration introduced into a scope by a (resolved) using declaration. *)

   | BindingDecl of binding_decl
   (** (C++) A bining in a decomposition declaration. *)

   | IndirectFieldDecl of indirect_field_decl
   (** (C++) An instance of this class is created to represent a field injected from an anonymous union/struct into the parent scope. *)

   | UnresolvedUsingValueDecl of unresolved_using_value_decl
   (** (C++) Represents a dependent using declaration which was not marked with typename. *)

   | NonTypeTemplateParmDecl of non_type_template_param_decl
   (** (C++) Declares a non-type template parameter. *)


   (* UNKNOWN *)


   | UnknownDecl of (** declaration kind *) int * (** string representation of the offending code *) string
 (** Unhandled AST node *)


 and decl = {
     decl_kind: decl_kind;
     decl_access: access_specifier; (** (C++) access specifier *)
     decl_range: range;
     decl_comment: comment list; (** comment associated to the declaration *)
   }
 (** Represents one declaration (or definition), e.g. a variable, typedef, function, struct, etc. *)


and access_specifier =
  | AS_public
  | AS_protected
  | AS_private
  | AS_none  (**  special value "none" which means different things in different contexts *)
(** (C++) access specifier *)

 (** All the different kinds of expressions. *)
 and expr_kind =

   | ConditionalOperator of conditional_operator
   (** The ?: ternary operator *)

   | AddrLabelExpr of (** label *) name
   (** The GNU address of label extension, representing &&label *)

   | ArrayInitIndexExpr
   (** Represents the index of the current element of an array being initialized by an ArrayInitLoopExpr. *)

   | ArrayInitLoopExpr of array_init_loop_expr
   (** Represents a loop initializing the elements of an array *)

   | ArraySubscriptExpr of array_subscript_expr
   (** Array subscripting. *)

   | AtomicExpr of atomic_expr
   (** Variadic atomic builtins: __atomic_exchange, __atomic_fetch_*, __atomic_load, __atomic_store, and __atomic_compare_exchange_*, for the similarly-named C++11 instructions, and __c11 variants for <stdatomic.h>. *)

   | CompoundAssignOperator of compound_assign_expr
   (** For compound assignments (e.g. +=). *)

   | BinaryOperator of (** left argument *) expr * (** operator *) binary_operator * (** right argument *) expr
   (** A builtin binary operation expression such as "x + y" or "x <= y". The operands will already have been converted to appropriate types (e.g., by performing promotions or conversions). Note that assignment is a binary operator, not a coumpound assignment operator. *)

   | UnaryOperator of (** operator *) unary_operator * (** argument *) expr
   (**  This represents the unary-expression's (except sizeof and alignof) and the postinc/postdec operators from postfix-expression. *)

   | CallExpr of call_expr
   (** Represents a function call  *)

   | CastExpr of expr * cast_kind
   (** (Implicit or explicit) type conversion *)

   | CharacterLiteral of (** value *) Int32.t * (** kind *) character_kind

   | ChooseExpr of choose_expr
   (** GNU builtin-in function __builtin_choose_expr *)

   | CompoundLiteralExpr of (** initializer *) expr * (** is file scope? *) bool
   (** Compound literal *)

   | DeclRefExpr of decl
   (** A reference to a declared variable, function, enum, etc. *)

   | DesignatedInitExpr of (** sequence of designators *) designator array * (** designated initializer *) expr
   (** Represents a C99 designated initializer expression. A designated initializer expression (C99 6.7.8) contains one or more designators (which can be field designators, array designators, or GNU array-range designators) followed by an expression that initializes the field or element(s) that the designators refer to.

          NOTE: Designators may not be useful for us. If I understand correctly, Clang's semantic analysis removes them by generating explicit nested list of initializers and putting designated initializers at the correct place. Similarly, default value for missing initializers are put explicitly...
    *)

   | FloatingLiteral of (** C99 hexdecimal string representation *) string
   (** Floating point literal *)

   | GenericSelectionExpr of generic_selection_expr
   (** A generic selection (C11 6.5.1.1) contains an unevaluated controlling expression, followed by one or more generic associations. Each generic association specifies a type name and an expression, or "default" and an expression (in which case it is known as a default generic association). The type and value of the generic selection are identical to those of its result expression, which is defined as the expression in the generic association with a type name that is compatible with the type of the controlling expression, or the expression in the default generic association if no types are compatible. *)

   | GNUNullExpr
   (** Implements the GNU __null extension, which is a name for a null pointer constant that has integral type (e.g., int or long) and is the same size and alignment as a pointer. *)

   | ImaginaryLiteral of expr
   (** We support imaginary integer and floating point literals, like "1.0i". We represent these as a wrapper around FloatingLiteral and IntegerLiteral, and have a Complex type whose element type matches the subexpression. *)

   | ImplicitValueInitExpr
   (** Represents an implicitly-generated value initialization of an object of a given type. Implicit value initializations occur within semantic initializer list expressions (InitListExpr) as placeholders for subobject initializations not explicitly specified by the user. *)

   | InitListExpr of init_list_expr
   (** Describes an initializer list, which can be used to initialize objects of different types, including struct/class/union types, arrays, and vectors. *)

   | IntegerLiteral of Z.t

   | MemberExpr of member_expr
   (** Structure and union members. X->F and X.F. In C++, also method or overloaded operator. *)

   | NoInitExpr
   (** Represents a place-holder for an object not to be initialized by anything. This only makes sense when it appears as part of an updater of a DesignatedInitUpdateExpr. *)

   | OffsetOfExpr of (** offset of path *) offsetof_node array * (** evaluated offsetof *) Z.t option
   (** This represents an expression of the form offsetof(record-type, member-designator) *)

   | OpaqueValueExpr of opaque_expr
   (** An expression referring to an opaque object of a fixed type and value class *)

   | ParenExpr of expr
   (** This represents a parethesized expression *)

   | ParenListExpr of expr array
   (** This represents a parethesized expression *)

   | PredefinedExpr of (** ident type *) ident_type * (** function name *) string
   (** A predefined identifier such as func. *)

   | PseudoObjectExpr of pseudo_object_expr
   (** An expression which accesses a pseudo-object l-value. A pseudo-object is an abstract object, accesses to which are translated to calls. The pseudo-object expression has a syntactic form, which shows how the expression was actually written in the source code, and a semantic form, which is a series of expressions to be executed in order which detail how the operation is actually evaluated. Optionally, one of the semantic forms may also provide a result value for the expression. *)

   | StmtExpr of stmt list
   (** This is the GNU Statement Expression extension: ({int X=4; X;}). The StmtExpr contains a single CompoundStmt node, which it evaluates and takes the value of the last subexpression. A StmtExpr is always an r-value; values "returned" out of a StmtExpr will be copied. *)

   | StringLiteral of (** byte representation *) string * (** kind *) character_kind
   (** This represents a string literal expression, e.g. "foo" or L"bar" (wide strings) *)

   | UnaryExprOrTypeTraitExpr of (** operator type *) unary_expr_or_type * (** type specified, or type of the specified expression *) type_qual
   (** Expression with either a type or (unevaluated) expression operand. For expression operands, we only keep its type as it is all that matters for these operators. *)

   | VAArgExpr of expr
   (** Represents a call to the builtin function __builtin_va_arg. *)

   | FullExpr of expr
   (** Represents a "full-expression" node (clang >= 8) *)

   | ConstantExpr of expr
   (** An expression that occurs in a constant context. (clang >= 8) *)

   (* C++ EXPRESSIONS *)

   | ArrayTypeTraitExpr of array_type_trait_expr
   (** (C++) An embarcadero array type trait, as used in the implementation of __array_rank and __array_extent. *)

   | CXXBindTemporaryExpr of (** sub-expression *) expr (* TODO: temporary *)
   (** (C++) Represents binding an expression to a temporary. This ensures the destructor is called for the temporary. It should only be needed for non-POD, non-trivially destructable class types. *)

   | CXXBoolLiteralExpr of bool
   (** (C++) A boolean literal, per ([C++ lex.bool] Boolean literals).  *)

   | CXXConstructExpr of cxx_construct_expr
   (** (C++) Represents a call to a C++ constructor *)

   | CXXDefaultArgExpr of param_var_decl * expr
   (** (C++) A default argument (C++ [dcl.fct.default]). This wraps up a function call argument that was created from the corresponding parameter's default argument, when the call did not explicitly supply arguments for all of the parameters. *)

   | CXXDefaultInitExpr of (** the field whose initializer will be used *) field_decl * (** initialization expression *) expr
   (** (C++) A use of a default initializer in a constructor or in aggregate initialization. This wraps a use of a C++ default initializer (technically, a brace-or-equal-initializer for a non-static data member) when it is implicitly used in a mem-initializer-list in a constructor (C++11 [class.base.init]p8) or in aggregate initialization (C++1y [dcl.init.aggr]p7). *)

   | CXXDeleteExpr of cxx_delete_expr
   (** (C++) Represents a delete expression for memory deallocation and destructor calls, e.g. "delete[] pArray". *)

   | CXXDependentScopeMemberExpr of cxx_dependent_scope_member_expr
   (** (C++) Represents a C++ member access expression where the actual member referenced could not be resolved because the base expression or the member name was dependent. *)

   | CXXFoldExpr of cxx_fold_expr
   (** (C++) Represents a folding of a pack over an operator. This expression is always dependent and represents a pack expansion of the forms: ( expr op ... ) ( ... op expr ) ( expr op ... op expr ) *)

   | CXXInheritedCtorInitExpr of cxx_inherited_ctor
   (** (C++) Represents a call to an inherited base class constructor from an inheriting constructor. This call implicitly forwards the arguments from the enclosing context (an inheriting constructor) to the specified inherited base class constructor. *)

   | CXXNewExpr of cxx_new_expr
   (** (C++) Represents a new-expression for memory allocation and constructor calls, e.g: "new CXXNewExpr(foo)". *)

   | CXXNoexceptExpr of (** operand *) expr * (** value *) bool
   (** (C++) Represents a C++11 noexcept expression (C++ [expr.unary.noexcept]). The noexcept expression tests whether a given expression might throw. Its result is a boolean constant. *)

   | CXXNullPtrLiteralExpr
   (** (C++) The null pointer literal (C++11 [lex.nullptr]). Introduced in C++11, the only literal of type nullptr_t is nullptr. *)

   | CXXPseudoDestructorExpr of cxx_pseudo_destructor_expr
   (** (C++) Represents a C++ pseudo-destructor (C++ [expr.pseudo]). A pseudo-destructor is an expression that looks like a member access to a destructor of a scalar type, except that scalar types don't have destructors.*)

   | CXXScalarValueInitExpr
   (** (C++) An expression "T()" which creates a value-initialized rvalue of type T, which is a non-class type. See (C++98 [5.2.3p2]). *)

   | CXXStdInitializerListExpr of expr
   (** (C++) Implicit construction of a std::initializer_list<T> object from an array temporary within list-initialization (C++11 [dcl.init.list]p5). *)

   | CXXThisExpr of (** is implicit *) bool
   (** (C++) Represents the this expression in C++. *)

   | CXXThrowExpr of cxx_throw_expr
   (** (C++) A C++ throw-expression (C++ [except.throw]). *)

   | CXXTypeidExpr of cxx_typeid_expr
   (** (C++) A C++ typeid expression (C++ [expr.typeid]), which gets the type_info that corresponds to the supplied type, or the (possibly dynamic) type of the supplied expression. *)

   | CXXUnresolvedConstructExpr of cxx_unresolved_construct_expr
   (** (C++) Describes an explicit type conversion that uses functional notion but could not be resolved because one or more arguments are type-dependent. *)

   | DependentScopeDeclRefExpr of dependent_scope_decl_ref_expr
   (** (C++) A qualified reference to a name whose declaration cannot yet be resolved. *)

   | ExpressionTraitExpr of expression_trait_expr
   (** (C++) An expression trait intrinsic. *)

   | ExprWithCleanups of expr_with_cleanups
   (** (C++) Represents an expression – generally a full-expression – that introduces cleanups to be run at the end of the sub-expression's evaluation. *)

   | FunctionParmPackExpr of (** parameter pack which this expression refers to *) var_decl * (** expansions of the parameter pack *) var_decl array
   (** (C++) Represents a reference to a function parameter pack that has been substituted but not yet expanded. When a pack expansion contains multiple parameter packs at different levels, this node is used to represent a function parameter pack at an outer level which we have already substituted to refer to expanded parameters, but where the containing pack expansion cannot yet be expanded. *)

   | MaterializeTemporaryExpr of materialize_tmp_expr
   (** (C++) Represents a prvalue temporary that is written into memory so that a reference can bind to it. *)

   | PackExpansionExpr of pack_expansion_expr
   (** (C++) Represents a C++11 pack expansion that produces a sequence of expressions. *)

   | SizeOfPackExpr of size_of_pack_expr
   (** (C++) Represents an expression that computes the length of a parameter pack. *)

   | SubstNonTypeTemplateParmExpr of subst_non_type_template_param_expr
   (** (C++) Represents a reference to a non-type template parameter that has been substituted with a template argument. *)

   | SubstNonTypeTemplateParmPackExpr of subst_non_type_template_param_pack_expr
   (** (C++) Represents a reference to a non-type template parameter pack that has been substituted with a non-template argument pack. *)

   | TypeTraitExpr of type_trait_expr
   (** (C++) A type trait used in the implementation of various C++11 and Library TR1 trait templates. *)

   | UnresolvedLookupExpr of unresolved_lookup_expr
   (** (C++) A reference to a name which we were able to look up during parsing but could not resolve to a specific declaration. *)

   | UnresolvedMemberExpr of unresolved_member_expr
   (** (C++) Represents a C++ member access expression for which lookup produced a set of overloaded functions. *)

   | LambdaExpr of lambda_expr
   (** (C++) A C++ lambda expression, which produces a function object (of unspecified type) that can be invoked later. *)


   (*  Vectors *)

   | ConvertVectorExpr of expr
   (** Clang builtin function __builtin_convertvector This AST node provides support for converting a vector type to another vector type of the same arity *)

   | ExtVectorElementExpr of expr (** base *) * string (** accessor *)
   (** This represents access to specific elements of a vector, and may occur on the left hand side or right hand side. *)

   | ShuffleVectorExpr of expr array
   (** Clang-specific builtin-in function __builtin_shufflevector. *)


   (* UNKNOWN *)

   | UnknownExpr of (** statement (expression) class *) int *(** statement (expression) class name *) string
 (** Unhandled Expr node *)


 and conditional_operator = {
     cond_cond: expr; (** condition *)
     cond_true: expr; (** true expression *)
     cond_false: expr; (** false expression *)
   }
 (** The ?: ternary operator *)

 and array_init_loop_expr = {
     array_init_source: opaque_expr; (** source array *)
     array_init_init: expr; (** initializer *)
     array_init_size: Z.t; (** array size *)
   }
 (** Represents a loop initializing the elements of an array *)

and array_subscript_expr = {
    subscript_base: expr; (** base (array) *)
    subscript_index: expr; (** index expression *)
  }
(** Array subscripting (normalized: we always put the array expression first and the index expression last, although the source may state '4[A]' and not 'A[4]') *)

and atomic_expr = {
    atomic_op: int; (** Kind of atomic builtin operator. TODO: use a variant. *)
    atomic_ptr: expr; (** primary pointer *)
    atomic_order: expr; (** memory order *)
  }
(** Variadic atomic builtins. TODO: handle operators that have more that 2 arguments *)

 and binary_operator =
   | BO_Mul (** * *)
   | BO_Div (** / *)
   | BO_Rem (** % *)
   | BO_Add (** + *)
   | BO_Sub (** - *)
   | BO_Shl (** << *)
   | BO_Shr (** >> *)
   | BO_LT (** < *)
   | BO_GT (** > *)
   | BO_LE (** <= *)
   | BO_GE (** >= *)
   | BO_EQ (** == *)
   | BO_NE (** != *)
   | BO_And (** bitwise & *)
   | BO_Xor (** bitwise ^ *)
   | BO_Or (** bitwise | *)
   | BO_LAnd (** logical and *)
   | BO_LOr (** || logical or *)
   | BO_Comma (** , *)
   | BO_Assign (** = *)
   | BO_PtrMemD (** (C++) .* *)
   | BO_PtrMemI (** (C++) ->* *)
 (** Binary operators (including regular assignment) *)

 and compound_assign_operator =
   | BO_MulAssign (** *= *)
   | BO_DivAssign (** /= *)
   | BO_RemAssign (** %= *)
   | BO_AddAssign (** += *)
   | BO_SubAssign (** -= *)
   | BO_ShlAssign (** <<= *)
   | BO_ShrAssign (** >>= *)
   | BO_AndAssign (** &= *)
   | BO_XorAssign (** ^= *)
   | BO_OrAssign (** |= *)
 (** Compound assignment operators (excluding regular assignment) *)

 and compound_assign_expr = {
     compound_lval: expr; (** left argument *)
     compound_comp_lval_type: type_qual; (** the type the left argument is converted to before the operation *)
     compound_op: compound_assign_operator; (** operator *)
     compound_rval: expr; (** right argument *)
     compound_comp_result_type: type_qual; (** type of the computed result, before converted back to lvalue type *)
   }
   (** For compound assignments (e.g. +=), we keep track of the type the operation is performed in. Due to the semantics of these operators, the operands are promoted, the arithmetic performed, an implicit conversion back to the result type done, then the assignment takes place. This captures the intermediate type which the computation is done in. *)

 and call_expr = {
     call_callee: expr; (** callee expression *)
     call_call_decl: function_decl option; (** callee declaration, if found *)
     call_args: expr array; (** arguments *)
     call_operator: overloaded_operator option; (** (C++) if this is a call to an overloaded operator, give its name *)
   }
 (** Represents a function call. In C++, this also represents a method call or an overloaded operator call, in which case the callee is a MemberExpr contaning both the objet argument (possibly an implicit this made explicit) and the member function. We don't expose Clang's CXXMemberCallExpr and CXXOperatorCallExpr, which are redundant.
 *)

 and unary_operator =
   | UO_PostInc (** ++ *)
   | UO_PostDec (** -- *)
   | UO_PreInc (** ++ *)
   | UO_PreDec (** -- *)
   | UO_AddrOf (** & *)
   | UO_Deref (** * *)
   | UO_Plus (** + *)
   | UO_Minus (** - *)
   | UO_Not (** ~ *)
   | UO_LNot (** ! *)
   | UO_Real (** __real extension *)
   | UO_Imag (** __imag extension *)
   | UO_Extension (** __extension__ marker *)
   | UO_Coawait (** (C++) coroutines co_await operator *)

 and cast_kind =
   | CStyleCast (** An explicit cast in C (C99 6.5.4) or a C-style cast in C++ (C++ [expr.cast]), which uses the syntax (Type)expr *)
   | CXXFunctionalCast (** (C++) An explicit C++ type conversion that uses "functional" notation (C++ [expr.type.conv]). More *)
   | CXXConstCast (** A(C++)  C++ const_cast *)
   | CXXDynamicCast (** (C++) A C++ dynamic_cast *)
   | CXXReinterpretCast (** (C++) A C++ reinterpret_cast *)
   | CXXStaticCast (** (C++) A C++ static_cast *)
   | ImplicitCast (** Implicit type conversions, which have no direct representation in the original source code *)
 (** Different categories of type conversion *)

 and character_kind =
   | Char_Ascii
   | Char_Wide
   | Char_UTF8
   | Char_UTF16
   | Char_UTF32
 (** Kinds of character literals *)

 and choose_expr = {
     choose_cond: expr; (** condition *)
     choose_true: expr; (** left expression, for true *)
     choose_false: expr; (** right expression, for false *)
     choose_cond_true: bool; (** whether the condition is true *)
   }
 (** GNU builtin-in function __builtin_choose_expr *)

 and designator =
   | Designator_Field of field_decl
   | Designator_Array of expr
   | Designator_ArrayRange of expr * expr
 (** Represents a single C99 designator *)

 and generic_selection_expr = {
     select_controling: expr; (** unevaluated controlling expression *)
     select_assoc: expr array; (** generic associations *)
     select_result: int; (** index of the resulting expression *)
   }
 (** Generic selection  *)

 and init_list_expr = {
     init_list_init: expr array; (** initializers *)
     init_list_field_in_union: field_decl option; (** if this initializes an union field, specify which field *)
     init_list_filler: expr option; (** if this initializer list initializes an array with more elements than there are initializers in the list, specifies an expression to be used for value initialization of the rest of the elements *)
   }
 (** Describes an initializer list, which can be used to initialize objects of different types, including struct/class/union types, arrays, and vectors. *)

 and member_expr = {
     member_base: expr; (** base *)
     member_decl: decl; (** member  declaration to which this expession refers *)
     member_arrow: bool; (** true for arrow ->, false for dot . access *)
     member_qualifier: name_specifier_loc list option; (** (C++) if the member name was qualified, retrieves the nested-name-specifier that precedes the member name, with source-location information *)
     member_template_args: template_argument_loc array; (** (C++) template arguments provided as part of this template-id *)
     member_name: declaration_name; (** member declaration name *)
   }
 (** Structure and union members. X->F and X.F. In C++, also method or overloaded operator. *)

 and pseudo_object_expr = {
     pseudo_object_syntactic_form: expr; (** syntactic form *)
     pseudo_object_semantic: expr array; (** semantic forms *)
     pseudo_object_result: int option; (** index in the semantic form list of the expression that returns a value, if any *)
   }
 (** An expression which accesses a pseudo-object l-value. *)

 and offsetof_node =
   | Offsetof_Array of expr
   | Offsetof_Field of field_decl
   | Offsetof_Identifier of string
 (** A component in OffsetOfExpr *)

 and ident_type =
   | Ident_Func
   | Ident_Function
   | Ident_LFunction
   | Ident_FuncDName
   | Ident_FuncSig
   | Ident_PrettyFunction
   | Ident_PrettyFunctionNoVirtual
 (** Type of predefined expression *)

 and opaque_expr = {
     opaque_source: expr option;
     opaque_type: type_qual;
     opaque_range: range;
   }
 (** An opaque value, pointing to its source expression. *)

 and unary_expr_or_type =
   | UETT_SizeOf
   | UETT_AlignOf
   | UETT_PreferredAlignOf (* Clang >= 8 *)

 and array_type_trait =
   | ATT_ArrayRank
   | ATT_ArrayExtent
 (** (C++) Names for the array type traits. *)

 and array_type_trait_expr = {
     trait_trait: array_type_trait;
     trait_type: type_qual; (** queried type *)
     trait_value: Int64.t;
     trait_dimension: expr;
   }
 (** (C++) An embarcadero array type trait, as used in the implementation of __array_rank and __array_extent. *)

 and unresolved_lookup_expr = {
     unresolved_lookup_requires_ADL: bool; (** true if this declaration should be extended by argument-dependent lookup *)
     unresolved_lookup_is_implicit: bool; (** true if this lookup is overloaded *)
     unresolved_lookup_naming_class: record_decl option;  (** the naming class of this lookup. *)
     unresolved_lookup_name: declaration_name; (** name looked up *)
     unresolved_lookup_decls: decl list;
     unresolved_lookup_template: template_argument_loc array;
   }
   (** (C++) A reference to a name which we were able to look up during parsing but could not resolve to a specific declaration. This arises in several ways: we might be waiting for argument-dependent lookup; the name might resolve to an overloaded function; and eventually: the lookup might have included a function template. These never include UnresolvedUsingValueDecls, which are always class members and therefore appear only in UnresolvedMemberLookupExprs. *)

 and unresolved_member_expr = {
     unresolved_member_base: expr option; (** the base object of this member expressions, e.g., the x in x.m; None if implicit *)
     unresolved_member_base_type: type_qual;
     unresolved_member_is_implicit_access: bool; (** true if this is an implicit access, i.e., one in which the member being accessed was not written in the source *)
     unresolved_member_is_arrow: bool; (** true if this member expression used the '->' operator; otherwise, it used the '.' operator *)
     unresolved_member_naming_class: record_decl option; (** the naming class of this lookup. *)
     unresolved_member_member_name: declaration_name; (** the name of the member that this expression refers to *)
     unresolved_member_name: declaration_name; (** name looked up *)
     unresolved_member_decls: decl list;
     unresolved_member_template: template_argument_loc array;
   }
 (** (C++) Represents a C++ member access expression for which lookup produced a set of overloaded functions. The member access may be explicit or implicit. In the final AST, an explicit access always becomes a MemberExpr. An implicit access may become either a MemberExpr or a DeclRefExpr, depending on whether the member is static. *)

 and lambda_expr = {
     lambda_capture_default: lambda_capture_default; (** the default capture kind for this lambda. *)
     lambda_captures: lambda_capture list; (** this lambda's (implicit and explicit) captures *)
     lambda_capture_inits: expr list; (** the initialization expressions for this lambda's captures *)
     lambda_class: record_decl; (** the class that corresponds to the lambda *)
     lambda_call_operator: function_decl; (** the function call operator associated with this lambda expression *)
     lambda_template_parameter: template_parameter_list option; (** tf this is a generic lambda expression, retrieve the template parameter list associated with it; else None *)
     lambda_is_generic: bool; (** whether this is a generic lambda *)
     lambda_body: stmt; (** the body of the lambda *)
     lambda_is_mutable: bool; (** whether the lambda is mutable, meaning that any captures values can be modified *)
     lambda_has_explicit_parameters: bool; (** whether this lambda has an explicit parameter list vs. an implicit (empty) parameter list. *)
     lambda_has_explicit_result_type: bool; (** whether this lambda had its result type explicitly specified *)
   }
 (** (C++) A C++ lambda expression, which produces a function object (of unspecified type) that can be invoked later. C++11 lambda expressions can capture local variables, either by copying the values of those local variables at the time the function object is constructed (not when it is called!) or by holding a reference to the local variable. These captures can occur either implicitly or can be written explicitly between the square brackets ([...]) that start the lambda expression. C++1y introduces a new form of "capture" called an init-capture that includes an initializing expression (rather than capturing a variable), and which can never occur implicitly. *)

 and lambda_capture = {
     lambda_capture_kind: lambda_capture_kind;
     lambda_capture_this: bool; (** whether this capture handles the C++ this pointer *)
     lambda_capture_VLA_type: bool; (** whether this captures a variable length array bound expressio *)
     lambda_capture_captured_var: var_decl option; (** declaration of the local variable being captured, if any *)
     lambda_capture_is_implicit: bool; (** whether this was an implicit capture (not written between the square brackets introducing the lambda) *)
     lambda_capture_is_pack_expansion: bool; (** whether this capture is a pack expansion, which captures a function parameter pack *)
   }
 (** Describes the capture of a variable or of this, or of a C++1y init-capture. *)

 and lambda_capture_default =
   | LCD_None
   | LCD_ByCopy
   | LCD_ByRef
 (** (C++) The default, if any, capture method for a lambda expression. *)

 (*
 and lambda_capture_init_kind =
   | Lambda_NoInit (** [a] *)
   | Lambda_CopyInit (** [a = b], [a = {b}] *)
   | Lambda_DirectInit (** [a(b)] *)
   | Lambda_ListInit (** [a{b}] *)
 *)

 and lambda_capture_kind =
   | LCK_This (** Capturing the *this object by reference. *)
   | LCK_StarThis (** Capturing the *this object by copy *)
   | LCK_ByCopy (** Capturing by copy (a.k.a., by value) *)
   | LCK_ByRef 	(** Capturing by reference. *)
   | LCK_VLAType (** Capturing variable-length array type. *)
 (** The different capture forms in a lambda introducer. C++11 allows capture of this, or of local variables by copy or by reference. C++1y also allows "init-capture", where the initializer is an expression. *)


 and cxx_construct_expr = {
     construct_decl: function_decl;  (** constructor called *)
     construct_kind: construction_kind;
     construct_args: expr array; (** arguments *)
     construct_requires_zero_init: bool; (** whether the construction first requires zero-initialization before the initializer is called *)
     construct_temporary: bool; (** true epresents a C++ functional cast expression that builds a temporary object. *)
   }
 (** (C++) Represents a call to a C++ constructor *)

 and cxx_delete_expr = {
     delete_arg: expr; (** argument *)
     delete_op_delete: function_decl option; (** delete operator executed, if known *)
     delete_destroyed_type: type_qual option; (** type being destroyed; None for a dependent type *)
     delete_is_global: bool; (** global delete (::delete) used? *)
     delete_is_array: bool; (** is array form *)
     delete_array_want_size: bool; (**  whether the array deallocation function expects the size of the allocation as a parameter *)
   }
 (** (C++) Represents a delete expression for memory deallocation and destructor calls. *)

 and cxx_dependent_scope_member_expr = {
     dependent_base: expr option; (** base pointer expressionm or None for an implicit acccess *)
     dependent_base_type: type_qual; (** base type *)
     dependent_arrow: bool; (** true for arrow ->, false for dot . access *)
     dependent_qualifier: name_specifier_loc list; (** nested-name-specifier that qualifies the member name, with source location information. *)
     dependent_first_qualifier: decl option; (** first part of the nested-name-specifier that was found in the scope of the member access expression when the member access was initially parsed *)
     dependent_member: declaration_name; (** member that this expression refers to *)
     dependent_template_args: template_argument_loc array; (** template arguments provided as part of this template-id *)
   }
 (** (C++) Represents a C++ member access expression where the actual member referenced could not be resolved because the base expression or the member name was dependent. *)

 and cxx_fold_expr = {
     fold_pattern: expr; (** pattern (operand that contains an unexpanded pack *)
     fold_init: expr option; (** operand that doesn't contain a pack for a binary fold, for a binary fold; None otherwise *)
     fold_right_fold: bool; (** true for right-associated sequence of operators, false for left associated *)
     fold_operator: binary_operator; (** operator *)
   }

 and cxx_inherited_ctor = {
     inherited_ctor: function_decl; (** constructor called *)
     inherited_ctor_kind: construction_kind;
     inherited_ctor_constructs_vbase: bool; (** whether this constructor is actually constructing a base class (rather than a complete object) *)
     inherited_ctor_inherited_from_vbase: bool; (** whether the inherited constructor is inherited from a virtual base of the object we construct *)
   }
 (** (C++) Represents a call to an inherited base class constructor from an inheriting constructor. *)


 and cxx_new_expr = {
     new_alloctype: type_qual; (** allocated type *)
     new_op_new: function_decl option; (** new operator executed, if known *)
     new_op_delete: function_decl option; (** delete operator executed, if known *)
     new_array_size: expr option; (** array size expression, None if not array allocation *)
     new_is_global: bool; (** global new (::new) used? *)
     new_style: initialization_style;
     new_initialier: expr option; (** (optional) initialisation expression *)
     new_args: expr array; (** placement arguments *)
     new_construct: cxx_construct_expr option; (** construct expression, if any *)
     new_pass_alignment: bool; (** indicates whether the required alignment should be implicitly passed to the allocation function *)
     new_array_want_size: bool; (**  whether the array deallocation function expects the size of the allocation as a parameter *)
   }
 (** (C++) Represents a new-expression for memory allocation and constructor calls, e.g: "new CXXNewExpr(foo)". *)

 and cxx_pseudo_destructor_expr = {
     destructor_base: expr;
     destructor_qualifier: name_specifier_loc list option; (** if the member name was qualified, retrieves the nested-name-specifier that precedes the member name *)
     destructor_is_arrow: bool; (** whether this pseudo-destructor expression was written using an '->' (true) or '.' (false) *)
     destructor_destroyed_type: type_qual; (** retrieve the type being destroyed. *)
   }
 (** (C++) Represents a C++ pseudo-destructor (C++ [expr.pseudo]). *)

 and cxx_throw_expr = {
     throw_expr: expr option; (** thrown expression, or None for rethrow *)
     throw_is_thrown_variable_in_scope: bool; (**  whether the variable thrown by this expression (if any!) is within the innermost try block *)
   }
 (** (C++) A C++ throw-expression (C++ [except.throw]). This handles 'throw' (for re-throwing the current exception) and 'throw' assignment-expression *)

 and cxx_typeid_expr = {
     typeid_type_operand: type_qual option; (** typeid(type) form: get the type operandafter various required adjustments (removing reference types, cv-qualifiers); None for typeid(expr) *)
     typeid_expr_operand: expr option; (** typeid(expr) form: expression argument; None for typeid(type) *)
     typeid_is_potentially_evaluated: bool; (** determine whether this typeid has a type operand which is potentially evaluated, per C++11 [expr.typeid]p3 *)
   }
 (** (C++) A C++ typeid expression (C++ [expr.typeid]), which gets the type_info that corresponds to the supplied type, or the (possibly dynamic) type of the supplied expression. This represents code like typeid(int) or typeid( *objPtr ). *)

 and cxx_unresolved_construct_expr = {
     unresolved_construct_type_as_written: type_qual; (** the type that is being constructed, as specified in the source code *)
     unresolved_construct_args: expr array; (** arguments *)
   }
 (** (C++) Describes an explicit type conversion that uses functional notion but could not be resolved because one or more arguments are type-dependent. *)

 and construction_kind =
   | CK_Complete
   | CK_NonVirtualBase
   | CK_VirtualBase
   | CK_Delegating
 (** (C++) Kind of C++ constructor call. *)

 and dependent_scope_decl_ref_expr = {
     dependent_decl_name: declaration_name; (** the name that this expression refers to *)
     dependent_decl_qualifier_loc: name_specifier_loc list; (** nested-name-specifier that qualifies the name, with source location information *)
     dependent_decl_template_args: template_argument_loc array;
   }
 (** (C++) A qualified reference to a name whose declaration cannot yet be resolved. DependentScopeDeclRefExpr is similar to DeclRefExpr in that it expresses a reference to a declaration such as X<T>::value. The difference, however, is that an DependentScopeDeclRefExpr node is used only within C++ templates when the qualification (e.g., X<T>::) refers to a dependent type. In this case, X<T>::value cannot resolve to a declaration because the declaration will differ from one instantiation of X<T> to the next. Therefore, DependentScopeDeclRefExpr keeps track of the qualifier (X<T>::) and the name of the entity being referenced ("value"). Such expressions will instantiate to a DeclRefExpr once the declaration can be found. *)

 and expression_trait_expr = {
     expr_trait_trait: expression_trait;
     expr_trait_queried_expr: expr;
     expr_trait_value: bool;
   }
 (** (C++) An expression trait intrinsic. *)

 and expression_trait =
   | ET_IsLValueExpr
   | ET_IsRValueExpr

 and expr_with_cleanups = {
     cleanup_expr: expr;
     cleanup_cleanups: block_decl array;
   }
 (** (C++) Represents an expression – generally a full-expression – that introduces cleanups to be run at the end of the sub-expression's evaluation.The most common source of expression-introduced cleanups is temporary objects in C++, but several other kinds of expressions can create cleanups, including basically every call in ARC that returns an Objective-C pointer. This expression also tracks whether the sub-expression contains a potentially-evaluated block literal. The lifetime of a block literal is the extent of the enclosing scope. *)

 and block_decl = {
     block_body: stmt option; (** if this represents a declaration for a body of code, such as a function or method definition, this is the top-level stmt of the body; otherwise None *)
     block_params: param_var_decl array; (** arguments *)
     block_captures: capture array; (** captured variables *)
     block_captures_cxx_this: bool;
     block_is_variadic: bool;
     block_missing_return_type: bool;
     block_is_conversion_from_lambda: bool;
   }
 (** (C++) This represents a block literal declaration, which is like an unnamed FunctionDecl. For example: ^\{ statement-body \} or ^(int arg1, float arg2)\{ statement-body \} *)

 and capture = {
     capture_var: var_decl; (** the variable being captured *)
     capture_is_by_ref: bool; (** whether this is a "by ref" capture, i.e. a capture of a __block variable *)
     capture_is_nested: bool; (** whether this is a nested capture, i.e. the variable captured is not from outside the immediately enclosing function/block *)
     capture_copy_expr: expr option;
   }
 (** (C++) Contains all the information about a particular captured value. *)

 and friend_decl = {
     friend_type: type_qual option; (** if this friend declaration names an (untemplated but possibly dependent) type, return the type *)
     friend_decl: decl option; (** if this friend declaration doesn't name a type, return the inner declaration *)
     friend_is_unsupported: bool; (** wheter this friend kind is unsupported *)
     friend_template: template_parameter_list array;
     friend_range: range;
     friend_com: comment list;
   }
 (** (C++) Represents the declaration of a friend entity, which can be a function, a type, or a templated function or type. *)

 and template_parameter_list = {
     template_parameter_list_params: decl array;
     template_parameter_list_requires_clause: expr option;
   }
 (** (C++) Stores a list of template parameters for a TemplateDecl and its derived classes. *)

 and static_assert = {
     assert_expr: expr;
     assert_msg: string;
     assert_is_failed: bool;
   }
 (** (C++) Represents a C++11 static_assert declaration. *)

 and namespace_decl = {
     namespace_name: name;
     namespace_is_anonymous: bool; (** true if this is an anonymous namespace declaration *)
     namespace_is_inline: bool; (** true if this is an inline namespace declaration *)
   }
 (** (C++) Represent a C++ namespace. *)

 and namespace_alias_decl = {
     namespace_alias_name: name;
     namespace_alias_namespace: namespace_decl; (** the namespace declaration aliased by this directive *)
     namespace_alias_aliased_namespace: decl; (** namespace that this alias refers to, which may either be a NamespaceDecl or a NamespaceAliasDecl *)
     namespace_alias_qualifier: name_specifier_loc list; (** the nested-name-specifier that qualifies the name of the namespace, with source-location information *)
   }
 (** (C++) Represents a C++ namespace alias. *)

 and builtin_template_decl = {
     builtin_template_name: name;
     builtin_template_param: template_parameter_list; (** the list of template parameters *)
     builtin_template_decl: decl option; (**  underlying, templated declaration *)
     builtin_template_requires_clause: expr option; (** the constraint-expression from the associated requires-clause (if any) *)
     builtin_template_kind: builtin_template_kind;
   }
   (** (C++) Represents the builtin template declaration which is used to implement __make_integer_seq and other builtin templates *)

 (** Kinds of BuiltinTemplateDecl *)
 and builtin_template_kind =
   | BTK__make_integer_seq (** this names the __make_integer_seq BuiltinTemplateDecl *)
   | BTK__type_pack_element (** this names the __type_pack_element BuiltinTemplateDecl *)

 and class_template_decl  = {
     class_template_name: name;
     class_template_param: template_parameter_list; (** the list of template parameters *)
     class_template_decl: record_decl; (**  underlying, templated declaration *)
     class_template_requires_clause: expr option; (** the constraint-expression from the associated requires-clause (if any) *)
     class_template_specializations: record_decl list; (** specializations (NOTE: left empty for now) *)
     class_template_injected_type: type_qual; (** template specialization type of the injected-class-name for this class template *)
   }
 (** (C++) Declaration of a class template. *)

 and class_template_specialization = {
     class_template_specialization_decl: class_template_decl; (** original template *)
     class_template_specialization_args: template_argument array; (** template arguments *)
   }
 (** (C++) Additional record information when it comes from a template. *)

 and function_template_decl = {
     function_template_name: name;
     function_template_param: template_parameter_list; (** the list of template parameters *)
     function_template_decl: function_decl; (**  underlying function declaration of the template *)
     function_template_requires_clause: expr option; (** the constraint-expression from the associated requires-clause (if any) *)
     function_template_specializations: function_decl list; (** specializations (NOTE: left empty for now) *)
    }
 (** (C++) Declaration of a template function. *)

 and function_template_specialization = {
     function_template_specialization_decl: function_template_decl; (** original template *)
     function_template_specialization_args: template_argument array; (** template arguments *)
   }
 (** (C++) Additional function information when it comes from a template. *)

 and type_alias_template_decl = {
     alias_template_name: name;
     alias_template_param: template_parameter_list; (** the list of template parameters *)
     alias_template_decl: typedef_decl; (**  underlying, templated declaration *)
     alias_template_requires_clause: expr option; (** the constraint-expression from the associated requires-clause (if any) *)
   }
 (** (C++) Declaration of an alias template. *)

 and var_template_decl = {
     var_template_name: name;
     var_template_param: template_parameter_list; (** the list of template parameters *)
     var_template_decl: var_decl; (**  underlying, templated declaration *)
     var_template_requires_clause: expr option; (** the constraint-expression from the associated requires-clause (if any) *)
     var_template_specializations: var_decl list; (** specializations (NOTE: left empty for now) *)
  }
 (** (C++) Declaration of a variable template. *)

 and var_template_specialization = {
     var_template_specialization_decl: var_template_decl; (** original template *)
     var_template_specialization_args: template_argument array; (** template arguments *)
   }
 (** (C++) Additional variable information when it comes from a template. *)

 and template_template_param_decl = {
     template_param_name: name;
     template_param_param: template_parameter_list; (** the list of template parameters *)
     template_param_decl: decl option; (** underlying, templated declaration *)
     template_param_requires_clause: expr option; (** the constraint-expression from the associated requires-clause (if any) *)
     template_param_is_parameter_pack: bool; (** whether this template template parameter is a template parameter pack *)
     template_param_is_pack_expansion: bool; (** whether this parameter pack is a pack expansion *)
     template_param_expansion_template_param: template_parameter_list array; (** expansion template parameters in an expanded parameter pack *)
     template_param_default: template_argument_loc option; (** the default argument, if any *)
   }
 (** (C++) Declares a template template parameter, e.g., "T" in. template <template <typename> class T> class container \{ \}; *)

 and template_type_param_decl = {
     template_type_param_name: name;
     template_type_param_default: type_qual option; (** the default argument, if any *)
   }
 (** (C++) Declaration of a template type parameter. *)

 and unresolved_using_typename_decl = {
     unresolved_using_typename_name: name;
     unresolved_using_typename_qualifier: name_specifier_loc list;
     unresolved_using_typename_is_pack_expansion: bool;
   }
 (** (C++) Represents a dependent using declaration which was marked with typename. *)

 and using_decl = {
     using_name: name;
     using_qualifier: name_specifier_loc list;
     using_has_typename: bool;
   }
 (** (C++) Represents a C++ using-declaration. *)

 and using_directive_decl = {
     using_directive_name: name;
     using_directive_qualifier: name_specifier list;
     using_directive_namespace: namespace_decl; (** nominated namespace *)
   }
 (** (C++) Represents C++ using-directive. *)

 and using_pack_decl = {
     using_pack_name: name;
     using_pack_decl: decl; (** using declaration from which this was instantiated *)
     using_pack_expansion: decl array; (** set of using declarations that this pack expanded into; note that some of these may still be unresolved *)
   }
 (** (C++) Represents a pack of using declarations that a single using-declarator pack-expanded into. *)

 and using_shadow_decl = {
     using_shadow_name: name;
     using_shadow_target: decl; (** underlying declaration which has been brought into the local scope *)
     using_shwdow_using: using_decl; (** using declaration to which this declaration is tied *)
   }
 (** (C++) Represents a shadow declaration introduced into a scope by a (resolved) using declaration. *)

 and binding_decl = {
     binding_name: name;
     binding_type: type_qual;
     binding_binding: expr; (** the expression to which this declaration is bound. *)
     binding_holding_var: var_decl option; (** the variable (if any) that holds the value of evaluating the binding *)
   }
 (** (C++) A bining in a decomposition declaration. *)

 and indirect_field_decl = {
     indirect_field_name: name;
     indirect_field_type: type_qual;
     indirect_field_anon_field: field_decl;
     indirect_field_var: var_decl option;
     indirect_field_chain: decl array;
   }
   (** An instance of this class is created to represent a field injected from an anonymous union/struct into the parent scope. *)

 and unresolved_using_value_decl = {
     unresolved_using_value_name: name;
     unresolved_using_value_type: type_qual;
     unresovled_using_value_qualifier: name_specifier_loc list; (** nested-name-specifier that qualifies the name, with source-location information *)
     unresovled_using_value_is_access_declaration: bool; (** true if it is a C++03 access declaration (no 'using') *)
     unresovled_using_value_is_pack_expansion: bool; (** whether this is a pack expansion *)
   }
   (** Represents a dependent using declaration which was not marked with typename. *)

 and non_type_template_param_decl = {
     non_type_template_name: name;
     non_type_template_type: type_qual;
     non_type_template_default: expr option; (** the default argument, if any *)
     non_type_template_param_is_parameter_pack: bool; (** whether this template template parameter is a template parameter pack *)
     non_type_template_param_is_pack_expansion: bool; (** whether this parameter pack is a pack expansion *)
     non_type_template_param_expansion: type_qual array;
   }
 (** Declares a non-type template parameter. *)

 and materialize_tmp_expr = {
     materialize_tmp_expr: expr; (** temporary-generating subexpression whose value will be materialized into a glvalue *)
     materialize_storage: storage_duration; (**  storage duration for the materialized temporary *)
     materialize_extending_decl: decl option; (** declaration which triggered the lifetime-extension of this temporary, if any. *)
     materialize_is_bound_to_lvalue_reference: bool; (** whether this materialized temporary is bound to an lvalue reference; otherwise, it's bound to an rvalue reference *)
   }
 (** (C++) Represents a prvalue temporary that is written into memory so that a reference can bind to it. Prvalue expressions are materialized when they need to have an address in memory for a reference to bind to. This happens when binding a reference to the result of a conversion, e.g., const int &r = 1.0; Here, 1.0 is implicitly converted to an int. That resulting int is then materialized via a MaterializeTemporaryExpr, and the reference binds to the temporary. MaterializeTemporaryExprs are always glvalues (either an lvalue or an xvalue, depending on the kind of reference binding to it), maintaining the invariant that references always bind to glvalues. Reference binding and copy-elision can both extend the lifetime of a temporary. When either happens, the expression will also track the declaration which is responsible for the lifetime extension. *)

 and pack_expansion_expr = {
     pack_pattern: expr; (** pattern of the pack expansion *)
     pack_num_expansions: int option; (** umber of expansions that will be produced when this pack expansion is instantiated, if already known *)
   }
 (** (C++) Represents a C++11 pack expansion that produces a sequence of expressions. A pack expansion expression contains a pattern (which itself is an expression) followed by an ellipsis. *)

 and size_of_pack_expr = {
     pack_param: decl; (** corresponding parameter pack declaration *)
     pack_length: int option; (** length of parameter pack, or None if the pack is value-dependent *)
     pack_is_partially_substituted: bool; (** whether this represents a partially-substituted sizeof expression *)
     pack_partial_arguments: template_argument array;
   }
 (** (C++) Represents an expression that computes the length of a parameter pack: sizeof...(template-parameter) *)

 and subst_non_type_template_param_expr = {
     subst_replacement: expr;
     subst_parameter: non_type_template_param_decl;
   }
 (** (C++) Represents a reference to a non-type template parameter that has been substituted with a template argument. *)

 and subst_non_type_template_param_pack_expr = {
     subst_parameter_pack: non_type_template_param_decl;
     subst_argument_pack: template_argument;
   }
 (** (C++) Represents a reference to a non-type template parameter pack that has been substituted with a non-template argument pack. *)

 and type_trait_expr = {
     type_trait_trait: type_trait; (** which type trait this expression uses *)
     type_trait_value: bool option; (** None if value-dependent *)
     type_trait_args: type_qual array; (** argument types *)

   }
 (** (C++) A type trait used in the implementation of various C++11 and Library TR1 trait templates. *)

 and type_trait =
   | UTT_HasNothrowAssign
   | UTT_HasNothrowMoveAssign
   | UTT_HasNothrowCopy
   | UTT_HasNothrowConstructor
   | UTT_HasTrivialAssign
   | UTT_HasTrivialMoveAssign
   | UTT_HasTrivialCopy
   | UTT_HasTrivialDefaultConstructor
   | UTT_HasTrivialMoveConstructor
   | UTT_HasTrivialDestructor
   | UTT_HasVirtualDestructor
   | UTT_IsAbstract
   | UTT_IsArithmetic
   | UTT_IsArray
   | UTT_IsClass
   | UTT_IsCompleteType
   | UTT_IsCompound
   | UTT_IsConst
   | UTT_IsDestructible
   | UTT_IsEmpty
   | UTT_IsEnum
   | UTT_IsFinal
   | UTT_IsFloatingPoint
   | UTT_IsFunction
   | UTT_IsFundamental
   | UTT_IsIntegral
   | UTT_IsInterfaceClass
   | UTT_IsLiteral
   | UTT_IsLvalueReference
   | UTT_IsMemberFunctionPointer
   | UTT_IsMemberObjectPointer
   | UTT_IsMemberPointer
   | UTT_IsNothrowDestructible
   | UTT_IsObject
   | UTT_IsPOD
   | UTT_IsPointer
   | UTT_IsPolymorphic
   | UTT_IsReference
   | UTT_IsRvalueReference
   | UTT_IsScalar
   | UTT_IsSealed
   | UTT_IsSigned
   | UTT_IsStandardLayout
   | UTT_IsTrivial
   | UTT_IsTriviallyCopyable
   | UTT_IsUnion
   | UTT_IsUnsigned
   | UTT_IsVoid
   | UTT_IsVolatile
   | BTT_IsBaseOf
   | BTT_IsConvertible
   | BTT_IsConvertibleTo
   | BTT_IsSame
   | BTT_TypeCompatible
   | BTT_IsAssignable
   | BTT_IsNothrowAssignable
   | BTT_IsTriviallyAssignable
   | TT_IsConstructible
   | TT_IsNothrowConstructible
   | TT_IsTriviallyConstructible
 (** (C++) Names for traits that operate specifically on types. *)

 and name_specifier =
   | Name_specifier_Identifier of string (** an identifier *)
   | Name_specifier_Namespace of namespace_decl (** a namespace *)
   | Name_specifier_NamespaceAlias of namespace_alias_decl (** a namespace alias *)
   | Name_specifier_TypeSpec of typ (** a type *)
   | Name_specifier_TypeSpecWithTemplate of typ (** a type that was preceded by the 'template' keyword *)
   | Name_specifier_Global (** the global specifier '::' *)
 (** (C++) Nested name specifiers are the prefixes to qualified namespaces. For example, "foo::" in "foo::x" is a nested name specifier. Nested name specifiers are made up of a sequence of specifiers, each of which can be a namespace, type, identifier (for dependent names), decltype specifier, or the global specifier ('::'). The last two specifiers can only appear at the start of a nested-namespace-specifier.*)

 and name_specifier_loc = {
     name_specifier_kind: name_specifier;
     name_specifier_range: range;
   }

and storage_duration =
  | SD_FullExpression (** full-expression storage duration (for temporaries) *)
  | SD_Automatic (** automatic storage duration (most local variables) *)
  | SD_Thread (** thread storage duration *)
  | SD_Static  (** static storage duration *)
  | SD_Dynamic (** dynamic storage duration *)
(** (C++) Storage duration for an object (per C++ [basic.stc]) *)

and declaration_name =
  | Name_Identifier of string (** regular identifier *)
  | Name_CXXConstrucorName of type_qual (** C++ constructor *)
  | Name_CXXDestructorName of type_qual (** C++ destructor *)
  | Name_CXXConversionFunctionName of type_qual (** C++ conversion function *)
  | Name_CXXDeductionGuideName of decl (** C++ deduction guide, associated to a template *)
  | Name_CXXOperatorName of overloaded_operator
  | Name_CXXLiteralOperatorName of string (** Literal operator *)
  | Name_CXXUsingDirective of string (* TODO: check *)
(** (C++) The name of a declaration, inclusing special case for C++ names (constructors, overloaded operators, etc. *)

and overloaded_operator =
  | OO_New (** new *)
  | OO_Delete (** delete *)
  | OO_Array_New (** new[] *)
  | OO_Array_Delete (** delete[] *)
  | OO_Plus (** + *)
  | OO_Minus (** - *)
  | OO_Star (** * *)
  | OO_Slash (** / *)
  | OO_Percent (** % *)
  | OO_Caret (** ^ *)
  | OO_Amp (** & *)
  | OO_Pipe (** | *)
  | OO_Tilde (** ~ *)
  | OO_Exclaim (** ! *)
  | OO_Equal (** = *)
  | OO_Less (** < *)
  | OO_Greater (** > *)
  | OO_PlusEqual (** += *)
  | OO_MinusEqual (** -= *)
  | OO_StarEqual (** *= *)
  | OO_SlashEqual (** /= *)
  | OO_PercentEqual (** %= *)
  | OO_CaretEqual (** ^= *)
  | OO_AmpEqual (** &= *)
  | OO_PipeEqual (** |= *)
  | OO_LessLess (** << *)
  | OO_GreaterGreater (** >> *)
  | OO_LessLessEqual (** <<= *)
  | OO_GreaterGreaterEqual (** >>= *)
  | OO_EqualEqual (** == *)
  | OO_ExclaimEqual (** != *)
  | OO_LessEqual (** <= *)
  | OO_GreaterEqual (** >= *)
  | OO_AmpAmp (** && *)
  | OO_PipePipe (** || *)
  | OO_PlusPlus (** ++ *)
  | OO_MinusMinus (** -- *)
  | OO_Comma (** , *)
  | OO_ArrowStar (** ->* *)
  | OO_Arrow (** -> *)
  | OO_Call (** () *)
  | OO_Subscript (** [] *)
  | OO_Conditional (** ? (cannot be overloaded, but used in the overload resolution machinery) *)
  | OO_Coawait (** co_await *)
(** (C++) Overloadable C++ operators *)

and template_argument_loc =
  template_argument * loc
(** (C++) Location wrapper for a template_argument *)

and template_argument =
  | Template_argument_Null
  | Template_argument_Type of type_qual
  | Template_argument_Declaration of decl
  | Template_argument_NullPtr of type_qual
  | Template_argument_Integral of type_qual * Z.t
  | Template_argument_Template of template_name
  | Template_argument_Expression of expr
  | Template_argument_Pack of template_argument (** pattern *) * template_argument array (** argument pack *)
(** (C++) Represents a template argument. *)

and template_name =
  | Template_name_Template of decl (** a single template declaration *)
  | Template_name_OverloadedTemplate of decl list (** a set of overloaded template declarations *)
  | Template_name_QualifiedTemplate of name_specifier list * decl (** a qualified template, where the qualification is kept to describe the source code as wrtten *)
  | Template_name_DependentTemplate of name_specifier list * string option * overloaded_operator option (** a dependent template name that has not been resolved to a template (or set of templates) *)
  | Template_name_SubstTemplateTemplateParm of decl * template_name (** a template template parameter that has been substituted for some other template name *)
  | Template_name_SubstTemplateTemplateParmPack of decl * template_argument (** a template template parameter pack that has been substituted for a template template argument pack, but has not yet been expanded into individual arguments *)

and initialization_style =
  | New_NoInit (** New-expression has no initializer as written *)
  | New_CallInit (** New-expression has a C++98 paren-delimited initializer *)
  | New_ListInit (** New-expression has a C++11 list-initializer *)
(** (C++) Initialization style for new expression (C++). *)

and expr = {
     expr_kind: expr_kind;
     expr_type: type_qual option;
     expr_range: range;
   }
 (** This represents one expression.
    In C, expressions are a special kind of statements that can return a value,
    and so, have a type
  *)

 (** Kinds of statments. *)
 and stmt_kind =

   | AsmStmt
   (** Assembly statement TODO: contents *)

   | AttributedStmt of stmt
   (** Represents an attribute applied to a statement. TODO: attributes *)

   | BreakStmt of loc (** destination *)
   (** This represents a break. *)

   | CompoundStmt of stmt list
   (** This represents a group of statements like \{ stmt stmt \}. *)

   | ContinueStmt of (** destination *) loc
   (** This represents a continue. *)

   | DeclStmt of decl list
   (** Adaptor for mixing declarations with statements and expressions. CompoundStmt mixes statements, expressions and declarations (variables, types). Another example is ForStmt, where the first statement can be an expression or a declaration. *)

   | DoStmt of do_stmt
   (** This represents a 'do/while' stmt. *)

   | ExprStmt of expr
   (** An expression *)

   | ForStmt of for_stmt
   (** This represents a 'for (init;cond;inc)' stmt *)

   | GotoStmt of (** label *) name * (** label location *) loc
   (** This represents a direct goto. *)

   | IfStmt of if_stmt
   (** This represents an if/then/else. *)

   | IndirectGotoStmt of (** target *) expr * (** fixed target, if any *) name option
   (** This represents an indirect goto, with a computed label. *)

   | LabelStmt of name * stmt
   (** Represents a label, which has a substatement *)

   | NullStmt
   (** This is the null statement ";". *)

   | ReturnStmt of expr option
   (** This represents a return, optionally of an expression. *)

   | CaseStmt of case_stmt
   (** Case in a switch *)

   | DefaultStmt of (** substatement *) stmt
   (** Default statement in a switch *)

   | SwitchStmt of switch_stmt
   (** This represents a 'switch' stmt. *)

   | WhileStmt of while_stmt
   (** This represents a 'while' stmt. *)


   (* C++ STATEMEMTS *)

   | CXXForRangeStmt of cxx_for_range_stmt
   (** (C++) This represents C++0x [stmt.ranged]'s ranged for statement, represented as 'for (range-declarator : range-expression)' *)

   | CXXTryStmt of cxx_try_stmt
   (** (C++) Try block followed by catch blocks *)


   (* UNKNOWN *)

   | UnknownStmt of (** statement class *) int * (** statement class name *) string
   (** Unhandled Stmt node *)


 and do_stmt = {
     do_body: stmt; (** body *)
     do_cond: expr; (** condition *)
   }
 (** This represents a 'do/while' stmt. *)

 and for_stmt = {
     for_init: stmt option;
     for_cond: expr option; (** condition *)
     for_inc: expr option; (** increment *)
     for_body: stmt;
   }
 (** This represents a 'for (init;cond;inc)' stmt *)

 and if_stmt = {
     if_cond: expr option; (** condition *)
     if_then: stmt option; (** then *)
     if_else: stmt option; (** else *)
     if_init: stmt option; (** (C++) optional init statment in 'if constexpr' *)
   }
 (** This represents an if/then/else. *)

 and case_stmt = {
     case_value: expr;
     case_end: expr option; (** GNU extension: end of range in 'case 1..4' *)
     case_stmt: stmt; (** statement following case *)
   }
(** Case in a switch *)

 and switch_stmt = {
     switch_init: stmt option;
     switch_cond: expr; (** condition *)
     switch_body: stmt;
   }
(** This represents a 'switch' stmt. *)

 and while_stmt = {
     while_cond: expr; (** condition *)
     while_body: stmt;
   }
(** This represents a 'while' stmt. *)

 and cxx_for_range_stmt = {
     for_range_var: var_decl; (** loop variable *)
     for_range_init: expr; (** range init *)
     for_range_body: stmt;
   }
(**  (C++) This represents C++0x [stmt.ranged]'s ranged for statement, represented as 'for (range-declarator : range-expression)'. TODO: expose also the desugared form begin/end/cond/inc/loopvar? *)

 and cxx_try_stmt = {
     try_block: stmt; (** try block *)
     try_handlers: cxx_catch_stmt array (** catch blocks *)
   }
 (** (C++) Try block followed by catch blocks *)

 and cxx_catch_stmt = {
     catch_exception: var_decl option; (** identifier getting the exception, if any *)
     catch_type: type_qual option; (** caught type, or None for all types *)
     catch_handler: stmt; (** handler *)
   }
(** (C++) This represents a C++ catch block. *)

 and stmt = {
     stmt_kind: stmt_kind;
     stmt_range: range
   }
(** Represents a statement. *)



(** {2 Diagnostics} *)


type diag_level =
  | Level_Ignored
  | Level_Note
  | Level_Remark
  | Level_Warning
  | Level_Error
  | Level_Fatal
(** The level of a diagnostic. *)

type diagnostic = {
    diag_level: diag_level;
    diag_loc: loc;
    diag_message: string;
  }
(** Messages generated by Clang during parsing. *)



(** {2 Target information} *)

type target_EABI =
  | Target_EABI_Unknown
  | Target_EABI_Default
  | Target_EABI_EABI4
  | Target_EABI_EABI5
  | Target_EABI_GNU
(** EABI version *)

type target_options = {
    target_triple: string; (** if given, the name of the target triple to compile for. *)
    target_host_triple: string; (** when compiling for the device side, contains the triple used to compile for the host *)
    target_CPU: string; (** if given, the name of the target CPU to generate code for *)
    target_FP_math: string; (** if gven, the unit to use for floating point math *)
    target_ABI: string; (** if given, the name of the target ABI to use *)
    target_EABI_version: target_EABI; (** the EABI version to use *)
    target_linker_version: string; (** if given, the version string of the linker in use *)
    target_features_as_written: string list; (** the list of target specific features to enable or disable, as written on the command line *)
    target_features: string list; (** the list of target specific features to enable or disable; this should be a list of strings starting with by '+' or '-' *)
    (*target_reciprocals: string list; *) (* moved out in later versions of Clang *)
  }
(** Options for controlling the target. *)

type target_int_type =
  | Target_NoInt
  | Target_SignedChar
  | Target_UnsignedChar
  | Target_SignedShort
  | Target_UnsignedShort
  | Target_SignedInt
  | Target_UnsignedInt
  | Target_SignedLong
  | Target_UnsignedLong
  | Target_SignedLongLong
  | Target_UnsignedLongLong
(** Integer types used in the target definition.*)

type target_real_type =
  | Target_NoFloat
  | Target_Float
  | Target_Double
  | Target_LongDouble
  | Target_Float128
(** Real types used in the target definition *)

type target_info = {
    target_options: target_options;
    target_size_type: target_int_type; (** integer types *)
    target_intmax_type: target_int_type;
    target_ptrdiff_type: target_int_type;
    target_intptr_type: target_int_type;
    target_wchar_type: target_int_type;
    target_wint_type: target_int_type;
    target_char16_type: target_int_type;
    target_char32_type: target_int_type;
    target_int64_type: target_int_type;
    target_sigatomic_type: target_int_type;
    target_processid_type: target_int_type;
    target_pointer_width: int; (** scalar type width and alignment *)
    target_pointer_align: int;
    target_bool_width: int;
    target_bool_align: int;
    target_char_width: int;
    target_char_align: int;
    target_short_width: int;
    target_short_align: int;
    target_int_width: int;
    target_int_align: int;
    target_long_width: int;
    target_long_align: int;
    target_long_long_width: int;
    target_long_long_align: int;
    target_half_width: int;
    target_half_align: int;
    target_float_width: int;
    target_float_align: int;
    target_double_width: int;
    target_double_align: int;
    target_long_double_width: int;
    target_long_double_align: int;
    target_float128_width: int;
    target_float128_align: int;
    target_large_array_min_width: int; (** alignment of other objects *)
    target_large_array_align: int;
    target_suitable_align: int; (** alignment that is suitable for storing any object with a fundamental alignment requirement *)
    target_big_endian: bool;
    target_TLS_supported: bool;
    target_has_int128: bool;
    target_has_float128_type: bool;
    target_null_pointer_value: Int64.t; (** get integer value for null pointer *)
  }
(** Exposes information about the current target. *)
