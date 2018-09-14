(**
  Clang_dump - Simple (and ugly) printer for Clang_AST, used for debugging.

 
  Copyright (C) 2017 The MOPSA Project

  This program is free software: you can redistribute it and/or modify
  it under the terms of the CeCILL license V2.1.

  @author Antoine Miné
 *)


open Clang_AST

       

(* for debugging: the second sync allows dumping the buffer contents as soon as possible instead of storing it *)
let sync ch = ()
(*let sync ch = Printf.printf "%s%!" (Buffer.contents ch); Buffer.clear ch*)
                                                                      
let bp_option none f ch a =
  sync ch;
  match a with
  | None -> Printf.bprintf ch "%s" none
  | Some x -> f ch x
                             
let bp_array f sep ch ee =
  for i=0 to Array.length ee-1 do
    sync ch;
    Printf.bprintf ch "%s%a" (if i=0 then "" else sep) f ee.(i)
  done

let bp_list f sep ch ee =
  let first = ref true in
  List.iter
    (fun e ->
      sync ch;
      Printf.bprintf ch "%s%a" (if !first then "" else sep) f e;
      first := false
    ) ee
                                    
let lang_name = function
  | Lang_C -> "C"
  | Lang_CXX -> "C++"

let builtin_type_name = function
  | Type_Void -> "void"
  | Type_Bool -> "_Bool"
  | Type_Char_U -> "(unsigned) char"
  | Type_UChar -> "unsigned char"
  | Type_WChar_U -> "(unsigned) wchar_t"
  | Type_Char16 -> "char16_t"
  | Type_Char32 -> "char32_t"
  | Type_UShort -> "unsigned short"
  | Type_UInt -> "unsigned int"
  | Type_ULong -> "unsigned long"
  | Type_ULongLong -> "unsigned long long"
  | Type_UInt128 -> "_uint128_t"
  | Type_Char_S -> "(signed) char"
  | Type_SChar -> "signed char"
  | Type_WChar_S -> "(signed) wchar_t"
  | Type_Short -> "short"
  | Type_Int -> "int"
  | Type_Long -> "long"
  | Type_LongLong -> "long long"
  | Type_Int128 -> "__int128_t"
  | Type_Half -> "__fp16"
  | Type_Float -> "float"
  | Type_Double -> "double"
  | Type_LongDouble -> "long double"
  | Type_Float128 -> "__float128 "
  | Type_NullPtr -> "nullptr"
  | Type_ObjCId -> "id"
  | Type_ObjCClass -> "Class"
  | Type_ObjCSel -> "SEL"
  | Type_OCLSampler -> "sampler_t"
  | Type_OCLEvent -> "event_t"
  | Type_OCLClkEvent -> "clk_event_t"
  | Type_OCLQueue -> "queue_t"
  | Type_OCLReserveID -> "reserved_id_t"
  | Type_Dependent -> "totally_unknown"
  | Type_Overload -> "unresolved_overload"
  | Type_BoundMember -> "bound_member"
  | Type_PseudoObject -> "ObjC_property"
  | Type_UnknownAny -> "__builtin_any_type"
  | Type_BuiltinFn -> "builtin_fn"
  | Type_ARCUnbridgedCast -> "__bridge"
  | Type_OMPArraySection -> "array_sections"    

let binary_operator_name = function
  | BO_Mul -> "*"
  | BO_Div -> "/"
  | BO_Rem -> "%"
  | BO_Add -> "+"
  | BO_Sub -> "-"
  | BO_Shl -> "<<"
  | BO_Shr -> ">>"
  | BO_LT -> "<"
  | BO_GT -> ">"
  | BO_LE -> "<="
  | BO_GE -> ">="
  | BO_EQ -> "=="
  | BO_NE -> "!="
  | BO_And -> "&"
  | BO_Xor -> "^"
  | BO_Or -> "|"
  | BO_LAnd -> "&&"
  | BO_LOr -> "||"
  | BO_Comma -> ","
  | BO_Assign -> "="
  | BO_PtrMemD -> ".*"
  | BO_PtrMemI -> "->*"

let compound_assign_operator_name = function
  | BO_MulAssign -> "*="
  | BO_DivAssign -> "/="
  | BO_RemAssign -> "%="
  | BO_AddAssign -> "+="
  | BO_SubAssign -> "-="
  | BO_ShlAssign -> "<<="
  | BO_ShrAssign -> ">>="
  | BO_AndAssign -> "&="
  | BO_XorAssign -> "^="
  | BO_OrAssign -> "|="
                     
let unary_operator_name = function
  | UO_PostInc -> "post++"
  | UO_PostDec -> "post--"
  | UO_PreInc -> "pre++"
  | UO_PreDec -> "pre--"
  | UO_AddrOf -> "&"
  | UO_Deref -> "*"
  | UO_Plus -> "+"
  | UO_Minus -> "-"
  | UO_Not -> "~"
  | UO_LNot -> "!"
  | UO_Real -> "__real"
  | UO_Imag -> "__imag"
  | UO_Extension -> "__extension__"
  | UO_Coawait -> "co_await"

let storage_class_name = function
  | SC_None -> "none" 
  | SC_Extern -> "extern"
  | SC_Static -> "static"
  | SC_PrivateExtern -> "private_extern"
  | SC_Auto -> "auto"
  | SC_Register -> "register"

let record_kind_name = function
  | Record_Struct -> "struct"
  | Record_Union -> "union"
  | Record_Class -> "class"
  | Record_Interface -> "interface"

let cast_kind_name = function
  | CStyleCast -> "c_cast"
  | CXXFunctionalCast -> "functional_cast"
  | CXXConstCast -> "const_cast"
  | CXXDynamicCast -> "dynamic_cast"
  | CXXReinterpretCast -> "reinterpret_cast"
  | CXXStaticCast -> "static_cast"
  | ImplicitCast -> "implicit"

let character_kind_name = function
  | Char_Ascii -> "Ascii"
  | Char_Wide -> "Wide"
  | Char_UTF8 -> "UTF8"
  | Char_UTF16 -> "UTF16"
  | Char_UTF32 -> "UTF32"
                    
let ident_type_name = function
  | Ident_Func -> "func"
  | Ident_Function -> "function"
  | Ident_LFunction -> "lfunction"
  | Ident_FuncDName -> "funcdname"
  | Ident_FuncSig -> "funcsig"
  | Ident_PrettyFunction -> "prettyfunction"
  | Ident_PrettyFunctionNoVirtual  -> "prettyfunctionnvirtual"

let unary_expr_or_type_name = function
  | UETT_SizeOf -> "sizeof"
  | UETT_AlignOf -> "alignof"

let array_type_trait_name = function
   | ATT_ArrayRank -> "__array_rank"
   | ATT_ArrayExtent -> "__array_extent"
         
let access_specifier_name = function
  | AS_public -> "public"
  | AS_protected -> "protected"
  | AS_private -> "private"
  | AS_none -> ""

let construction_kind_name = function
  | CK_Complete -> "complete"
  | CK_NonVirtualBase -> "non-virtual base"
  | CK_VirtualBase -> "virtual base"
  | CK_Delegating -> "delegating"

let overloaded_operator_name = function
  | OO_New -> "new"
  | OO_Delete -> "delete"
  | OO_Array_New -> "new[]"
  | OO_Array_Delete -> "delete[]"
  | OO_Plus -> "+"
  | OO_Minus -> "-"
  | OO_Star -> "*"
  | OO_Slash -> "/"
  | OO_Percent -> "%"
  | OO_Caret -> "^"
  | OO_Amp -> "&"
  | OO_Pipe -> "|"
  | OO_Tilde -> "~"
  | OO_Exclaim -> "!"
  | OO_Equal -> "="
  | OO_Less -> "<"
  | OO_Greater -> ">"
  | OO_PlusEqual -> "+="
  | OO_MinusEqual -> "-="
  | OO_StarEqual -> "*="
  | OO_SlashEqual -> "/="
  | OO_PercentEqual -> "%="
  | OO_CaretEqual -> "^="
  | OO_AmpEqual -> "&="
  | OO_PipeEqual -> "|="
  | OO_LessLess -> "<<"
  | OO_GreaterGreater -> ">>"
  | OO_LessLessEqual -> "<<="
  | OO_GreaterGreaterEqual -> ">>="
  | OO_EqualEqual -> "=="
  | OO_ExclaimEqual -> "!="
  | OO_LessEqual -> "<="
  | OO_GreaterEqual -> ">="
  | OO_AmpAmp -> "&&"
  | OO_PipePipe -> "||"
  | OO_PlusPlus -> "++"
  | OO_MinusMinus -> "--"
  | OO_Comma -> ","
  | OO_ArrowStar -> "->*"
  | OO_Arrow -> "->"
  | OO_Call -> "()"
  | OO_Subscript -> "[]"
  | OO_Conditional -> "?"
  | OO_Coawait -> "co_await"
    
let initialization_style_name = function
  | New_NoInit -> "no init"
  | New_CallInit -> "call init"
  | New_ListInit -> "list init"

let expression_trait_name = function
  | ET_IsLValueExpr -> "lvalue expr"
  | ET_IsRValueExpr -> "rvalue expr"

let storage_duration_name = function
  | SD_FullExpression -> "full expr"
  | SD_Automatic -> "automatic"
  | SD_Thread -> "thread"
  | SD_Static -> "static"
  | SD_Dynamic -> "dynamic"

let type_trait_name = function
  | UTT_HasNothrowAssign -> "has_nothrow_assign"	
  | UTT_HasNothrowMoveAssign -> "has_nothrow_move_assign"    
  | UTT_HasNothrowCopy -> "has_nothrow_copy"
  | UTT_HasNothrowConstructor -> "has_nothrow_constructor"
  | UTT_HasTrivialAssign -> "has_trivial_assign"     
  | UTT_HasTrivialMoveAssign -> "has_trivial_move_assign"
  | UTT_HasTrivialCopy -> "has_trivial_copy"
  | UTT_HasTrivialDefaultConstructor -> "has_trivial_default_constructor"
  | UTT_HasTrivialMoveConstructor -> "has_trivial_move_constructor"
  | UTT_HasTrivialDestructor -> "has_trivial_destructor"
  | UTT_HasVirtualDestructor -> "has_virtual_destructor"
  | UTT_IsAbstract -> "is_abstract"     
  | UTT_IsArithmetic -> "is_arithmetic"
  | UTT_IsArray -> "is_array"
  | UTT_IsClass -> "is_class"
  | UTT_IsCompleteType -> "is_complete_type"	
  | UTT_IsCompound -> "is_compound"
  | UTT_IsConst -> "is_const"
  | UTT_IsDestructible -> "is_destructible"	
  | UTT_IsEmpty -> "is_empty"
  | UTT_IsEnum -> "is_enum"
  | UTT_IsFinal -> "is_final" 	
  | UTT_IsFloatingPoint -> "is_floating_point" 	
  | UTT_IsFunction -> "is_function"
  | UTT_IsFundamental -> "is_fundamental" 	
  | UTT_IsIntegral -> "is_integral" 	
  | UTT_IsInterfaceClass -> "is_interface_class" 	
  | UTT_IsLiteral -> "is_literal" 	
  | UTT_IsLvalueReference -> "is_lvalue_reference" 	
  | UTT_IsMemberFunctionPointer -> "is_member_function_pointer" 	
  | UTT_IsMemberObjectPointer -> "is_member_object_pointer" 	
  | UTT_IsMemberPointer -> "is_member_pointer" 	
  | UTT_IsNothrowDestructible -> "is_nothrow_destructible" 	
  | UTT_IsObject -> "is_object"     
  | UTT_IsPOD -> "is_pod" 	
  | UTT_IsPointer -> "is_pointer" 	
  | UTT_IsPolymorphic -> "is_polymrphic" 	
  | UTT_IsReference -> "is_reference" 	
  | UTT_IsRvalueReference -> "is_rvalue_reference" 	
  | UTT_IsScalar -> "is_scalar" 	
  | UTT_IsSealed -> "is_sealed" 	
  | UTT_IsSigned -> "is_signed" 	
  | UTT_IsStandardLayout -> "is_standard_layout" 	
  | UTT_IsTrivial -> "is_trivial" 	
  | UTT_IsTriviallyCopyable -> "is_trvially_copyable" 	
  | UTT_IsUnion -> "is_union" 	
  | UTT_IsUnsigned -> "is_unsigned" 	
  | UTT_IsVoid -> "is_void" 	
  | UTT_IsVolatile -> "is_volatile" 	
  | BTT_IsBaseOf -> "is_baseof" 	
  | BTT_IsConvertible -> "is_convertible" 	
  | BTT_IsConvertibleTo -> "is_convertibleTo" 	
  | BTT_IsSame -> "is_same" 	
  | BTT_TypeCompatible -> "type_compatible" 	
  | BTT_IsAssignable -> "is_assignable" 	
  | BTT_IsNothrowAssignable -> "is_nothrow_assignable" 	
  | BTT_IsTriviallyAssignable -> "is_trivially_assignable" 	
  | TT_IsConstructible -> "is_constructible" 	
  | TT_IsNothrowConstructible -> "is_nothrow_constructible" 	
  | TT_IsTriviallyConstructible -> "is_trivially_constructible"     
                                                                          
let diag_level_name = function
  | Level_Ignored -> "ignored"
  | Level_Note -> "note"
  | Level_Remark -> "remark"
  | Level_Warning -> "warning"
  | Level_Error -> "error"
  | Level_Fatal -> "fatal"

let target_int_type_name = function
  | Target_NoInt -> "no-int"
  | Target_SignedChar -> "signed char"
  | Target_UnsignedChar -> "unsigned char"
  | Target_SignedShort -> "short"
  | Target_UnsignedShort -> "unsigned short"
  | Target_SignedInt -> "int"
  | Target_UnsignedInt -> "unsigned"
  | Target_SignedLong -> "long"
  | Target_UnsignedLong -> "unsigned long"
  | Target_SignedLongLong -> "long long"
  | Target_UnsignedLongLong -> "unsigned long long"
                                   
let target_real_type_name = function
  | Target_NoFloat -> "no-float"
  | Target_Float -> "float"
  | Target_Double -> "double"
  | Target_LongDouble -> "long double"
  | Target_Float128 -> "float128"

let builtin_template_kind_name = function
  | BTK__make_integer_seq -> "make_integer_seq"
  | BTK__type_pack_element -> "type_pack_element"

let ref_qualifier_name = function
  | RQ_None -> "none"
  | RQ_LValue -> "lvalue"
  | RQ_RValue -> "rvalue"

let lambda_capture_default_name = function
  | LCD_None -> "None"
  | LCD_ByCopy -> "ByCopy"
  | LCD_ByRef -> "ByRef"

let lambda_capture_kind_name = function
  | LCK_This -> "this"
  | LCK_StarThis -> "*this"
  | LCK_ByCopy -> "ByCopy"
  | LCK_ByRef -> "ByRef"
  | LCK_VLAType -> "VLAType"


                     let decl_kind_name d =
  match d with
  | TranslationUnitDecl _ -> "TranslationUnitDecl"
  | EmptyDecl -> "EmptyDecl"
  | FileScopeAsmDecl _ -> "FileScopeAsmDecl"
  | LinkageSpecDecl _ -> "LinkageSpecDecl"
  | LabelDecl _ -> "LabelDecl"
  | EnumDecl _ -> "EnumDecl"
  | RecordDecl _ -> "RecordDecl"
  | TypedefDecl _ -> "TypedefDecl"
  | FunctionDecl _ -> "FunctionDecl"
  | VarDecl _ -> "VarDecl"
  | FieldDecl _ -> "FieldDecl"
  | EnumConstantDecl _ -> "EnumConstantDecl"
  | BlockDecl _ -> "BlockDecl"
  | UnknownDecl _ -> "UnknownDecl"
  | AccessSpecDecl -> "AccessSpecDecl"
  | FriendDecl _ -> "FriendDecl"
  | StaticAssertDecl _ -> "StaticAssertDecl"
  | NamespaceDecl _ -> "NamespaceDecl"
  | NamespaceAliasDecl _ -> "NamespaceAliasDecl"
  | BuiltinTemplateDecl _ -> "BuiltinTemplateDecl"
  | ClassTemplateDecl _ -> "ClassTemplateDecl"
  | FunctionTemplateDecl _ -> "FunctionTemplateDecl"
  | TypeAliasTemplateDecl _ -> "TypeAliasTemplateDecl"
  | VarTemplateDecl _ -> "VarTemplateDecl"
  | TemplateTemplateParmDecl _ -> "TemplateTemplateParmDecl"
  | TemplateTypeParmDecl _ -> "TemplateTypeParmDecl"
  | TypeAliasDecl _ -> "TypeAliasDecl"
  | UnresolvedUsingTypenameDecl _ -> "UnresolvedUsingTypenameDecl"
  | UsingDecl _ -> "UsingDecl"
  | UsingDirectiveDecl _ -> "UsingDirectiveDecl"
  | UsingPackDecl _ -> "UsingPackDecl"
  | UsingShadowDecl _ -> "UsingShadowDecl"
  | BindingDecl _ -> "BindingDecl"
  | IndirectFieldDecl _ -> "IndirectFieldDecl"
  | UnresolvedUsingValueDecl _ -> "UnresolvedUsingValueDecl"
  | NonTypeTemplateParmDecl _ -> "NonTypeTemplateParmDecl"

let type_kind_name t =
  match t with
  | DecayedType _ -> "DecayedType"
  | ArrayType _ -> "ArrayType" 
  | AtomicType _ -> "AtomicType"
  | AttributedType _ -> "AttributedType"
  | BuiltinType _ -> "BuiltinType"
  | ComplexType _ -> "ComplexType"
  | FunctionProtoType _ -> "FunctionProtoType"
  | FunctionNoProtoType _ -> "FunctionNoProtoType" 
  | ParenType _ -> "ParenType"
  | PointerType _ -> "PointerType"
  | EnumType _ -> "EnumType" 
  | RecordType _ -> "RecordType" 
  | TypedefType _ -> "TypedefType" 
  | ElaboratedType _ -> "ElaboratedType" 
  | UnaryTransformType _ -> "UnaryTransformType" 
  | TypeOfExprType _ -> "TypeOfExprType" 
  | DecltypeType _ -> "DecltypeType" 
  | AutoType _ -> "AutoType" 
  | DeducedTemplateSpecializationType _ -> "DeducedTemplateSpecializationType" 
  | DependentSizedExtVectorType  _ -> "DependentSizedExtVectorType" 
  | InjectedClassNameType _ -> "InjectedClassNameType"
  | MemberPointerType _ -> "MemberPointerType"
  | PackExpansionType _ -> "PackExpansionType"
  | LValueReferenceType _ -> "LValueReferenceType"
  | RValueReferenceType _ -> "RValueReferenceType"
  | SubstTemplateTypeParmPackType _ -> "SubstTemplateTypeParmPackType"
  | SubstTemplateTypeParmType _ -> "SubstTemplateTypeParmType"
  | TemplateSpecializationType _ -> "TemplateSpecializationType"
  | TemplateTypeParmType _ -> "TemplateTypeParmType" 
  | DependentNameType _ -> "DependentNameType" 
  | DependentTemplateSpecializationType _ -> "DependentTemplateSpecializationType"
  | UnresolvedUsingType _ -> "UnresolvedUsingType"
  | VectorType _ -> "VectorType"
  | UnknownType _ -> "UnknownType"
                       
let expr_kind_name e =
  match e with
  | ConditionalOperator _ -> "ConditionalOperator"
  | AddrLabelExpr _ -> "AddrLabelExpr"
  | ArrayInitIndexExpr -> "ArrayInitIndexExpr"
  | ArrayInitLoopExpr _ -> "ArrayInitLoopExpr"
  | ArraySubscriptExpr _ -> "ArraySubscriptExpr"
  | AtomicExpr _ -> "AtomicExpr"
  | CompoundAssignOperator _ -> "CompoundAssignOperator"
  | BinaryOperator _ -> "BinaryOperator"
  | UnaryOperator _ -> "UnaryOperator"
  | CallExpr _ -> "CallExpr"
  | CastExpr _ -> "CastExpr"
  | CharacterLiteral _ -> "CharacterLiteral"
  | ChooseExpr _ -> "ChooseExpr"
  | CompoundLiteralExpr _ -> "CompoundLiteralExpr"
  | DeclRefExpr _ -> "DeclRefExpr"
  | DesignatedInitExpr _ -> "DesignatedInitExpr"
  | FloatingLiteral _ -> "FloatingLiteral"
  | GenericSelectionExpr _ -> "GenericSelectionExpr"
  | GNUNullExpr -> "GNUNullExpr"
  | ImaginaryLiteral _ -> "ImaginaryLiteral"
  | ImplicitValueInitExpr -> "ImplicitValueInitExpr"
  | InitListExpr _ -> "InitListExpr"
  | IntegerLiteral _ -> "IntegerLiteral"
  | MemberExpr _ -> "MemberExpr"
  | NoInitExpr -> "NoInitExpr"
  | OffsetOfExpr _ -> "OffsetOfExpr"
  | OpaqueValueExpr _ -> "OpaqueValueExpr"
  | ParenExpr _ -> "ParenExpr"
  | ParenListExpr _ -> "ParenListExpr"
  | PredefinedExpr _ -> "PredefinedExpr"
  | PseudoObjectExpr _ -> "PseudoObjectExpr"
  | StmtExpr _ -> "StmtExpr"
  | StringLiteral _ -> "StringLiteral"
  | UnaryExprOrTypeTraitExpr _ -> "UnaryExprOrTypeTraitExpr"
  | VAArgExpr _ -> "VAArgExpr"
  | ArrayTypeTraitExpr _ -> "ArrayTypeTraitExpr"
  | CXXBindTemporaryExpr _ -> "CXXBindTemporaryExpr"
  | CXXBoolLiteralExpr _ -> "CXXBoolLiteralExpr"
  | CXXConstructExpr _ -> "CXXConstructExpr"
  | CXXDefaultArgExpr _ -> "CXXDefaultArgExpr"
  | CXXDefaultInitExpr _ -> "CXXDefaultInitExpr"
  | CXXDeleteExpr _ -> "CXXDeleteExpr"
  | CXXDependentScopeMemberExpr _ -> "CXXDependentScopeMemberExpr"
  | CXXFoldExpr _ -> "CXXFoldExpr"
  | CXXInheritedCtorInitExpr  _ -> "CXXInheritedCtorInitExpr"
  | CXXNewExpr _ -> "CXXNewExpr"
  | CXXNoexceptExpr _ -> "CXXNoexceptExpr"
  | CXXNullPtrLiteralExpr -> "CXXNullPtrLiteralExpr"
  | CXXPseudoDestructorExpr _ -> "CXXPseudoDestructorExpr"
  | CXXScalarValueInitExpr -> "CXXScalarValueInitExpr"
  | CXXStdInitializerListExpr _ -> "CXXStdInitializerListExpr"
  | CXXThisExpr _ -> "CXXThisExpr"
  | CXXThrowExpr _ -> "CXXThrowExpr"
  | CXXTypeidExpr _ -> "CXXTypeidExpr"
  | CXXUnresolvedConstructExpr _ -> "CXXUnresolvedConstructExpr"
  | DependentScopeDeclRefExpr _ -> "DependentScopeDeclRefExpr"
  | ExpressionTraitExpr _ -> "ExpressionTraitExpr"
  | ExprWithCleanups _ -> "ExprWithCleanups"
  | FunctionParmPackExpr _ -> "FunctionParmPackExpr"
  | MaterializeTemporaryExpr _ -> "MaterializeTemporaryExpr"
  | PackExpansionExpr _ -> "PackExpansionExpr"
  | SizeOfPackExpr _ -> "SizeOfPackExpr"
  | SubstNonTypeTemplateParmExpr _ -> "SubstNonTypeTemplateParmExpr"
  | SubstNonTypeTemplateParmPackExpr _ -> "SubstNonTypeTemplateParmPackExpr"
  | TypeTraitExpr _ -> "TypeTraitExpr"
  | UnresolvedLookupExpr _ -> "UnresolvedLookupExpr"
  | UnresolvedMemberExpr _ -> "UnresolvedMemberExpr"
  | LambdaExpr _ -> "LambdaExpr"
  | ConvertVectorExpr _ -> "ConvertVectorExpr"
  | ExtVectorElementExpr _ -> "ExtVectorElementExpr"
  | ShuffleVectorExpr _ -> "ShuffleVectorExpr"
  | UnknownExpr _ -> "UnknownExpr"

let stmt_kind_name s =
  match s with
  | AsmStmt -> "AsmStmt"
  | AttributedStmt _ -> "AttributedStmt"
  | BreakStmt _ -> "BreakStmt"
  | CompoundStmt _ -> "CompoundStmt"
  | ContinueStmt _ -> "ContinueStmt"
  | DeclStmt _ -> "DeclStmt"
  | DoStmt _ -> "DoStmt"
  | ExprStmt _ -> "ExprStmt"
  | ForStmt _ -> "ForStmt "
  | GotoStmt _ -> "GotoStmt"
  | IfStmt _ -> "IfStmt"
  | IndirectGotoStmt _ -> "IndirectGotoStmt"
  | LabelStmt _ -> "LabelStmt"
  | NullStmt -> "NullStmt"
  | ReturnStmt _ -> "ReturnStmt"
  | CaseStmt _ -> "CaseStmt"
  | DefaultStmt _ -> "DefaultStmt"
  | SwitchStmt _ -> "SwitchStmt"
  | WhileStmt _ -> "WhileStmt"
  | CXXForRangeStmt _ -> "CXXForRangeStmt"
  | CXXTryStmt _ -> "CXXTryStmt"
  | UnknownStmt _ -> "UnknownStmt"




let string_of_loc l =
  Printf.sprintf "%s:%i:%i" l.loc_file l.loc_line l.loc_column

let string_of_range r =
  if r.range_begin.loc_file = r.range_end.loc_file then
    if r.range_begin.loc_line = r.range_end.loc_line then
      Printf.sprintf
        "%s:%i:%i-%i"
        r.range_begin.loc_file r.range_begin.loc_line r.range_begin.loc_column
        r.range_end.loc_column
    else
      Printf.sprintf
        "%s:%i:%i-%i:%i"
        r.range_begin.loc_file r.range_begin.loc_line r.range_begin.loc_column
        r.range_end.loc_line r.range_end.loc_column
  else
    Printf.sprintf
      "%s:%i:%i-%s:%i:%i"
      r.range_begin.loc_file r.range_begin.loc_line r.range_begin.loc_column
      r.range_end.loc_file r.range_end.loc_line r.range_end.loc_column

        
let string_of_diagnostic d =
  Printf.sprintf
    "%s: %s: %s" (string_of_loc d.diag_loc) (diag_level_name d.diag_level) d.diag_message

let string_of_target_EABI e =
  match e with
  | Target_EABI_Unknown -> "Unknown"
  | Target_EABI_Default -> "Default"
  | Target_EABI_EABI4 -> "EABI4"
  | Target_EABI_EABI5 -> "EABI5"
  | Target_EABI_GNU -> "GNU"
                     
  
let string_of_target_options o =
  let list = bp_list (fun b -> Printf.bprintf b "%s") "," in
  let b = Buffer.create 10 in
  Printf.bprintf
    b "triple='%s' host_triple='%s' CPU='%s' FP_math='%s' ABI='%s' EABI='%s' linker='%s' features_as_written=%a features=%a"
    o.target_triple o.target_host_triple o.target_CPU
    o.target_FP_math o.target_ABI
    (string_of_target_EABI o.target_EABI_version)
    o.target_linker_version
    list o.target_features_as_written
    list o.target_features;
  Buffer.contents b
             

let string_of_target_info i =
  Printf.sprintf
    "%s size=%s intmax=%s ptrdiff=%s intptr=%s wcjar=%s wint=%s char16=%s char32=%s int64=%s sigatomic=%s processd=%s pointer=%i/%i bool=%i/%i char=%i/%i short=%i/%i int=%i/%i long=%i/%i long long==%i/%i half=%i/%i float=%i/%i double=%i/%i long double=%i/%i  float128=%i/%i large-array=%i/%i suitable-align=%i big-endian=%B TLS=%B has_int128=%B has_float128=%B null=%Li"
    (string_of_target_options i.target_options)
    (target_int_type_name i.target_size_type)
    (target_int_type_name i.target_intmax_type)
    (target_int_type_name i.target_ptrdiff_type)
    (target_int_type_name i.target_intptr_type)
    (target_int_type_name i.target_wchar_type)
    (target_int_type_name i.target_wint_type)
    (target_int_type_name i.target_char16_type)
    (target_int_type_name i.target_char32_type)
    (target_int_type_name i.target_int64_type)
    (target_int_type_name i.target_sigatomic_type)
    (target_int_type_name i.target_processid_type)
    i.target_pointer_width i.target_pointer_align
    i.target_bool_width i.target_bool_align
    i.target_char_width i.target_char_align
    i.target_short_width i.target_short_align
    i.target_int_width i.target_int_align
    i.target_long_width i.target_long_align
    i.target_long_long_width i.target_long_long_align
    i.target_half_width i.target_half_align
    i.target_float_width i.target_float_align
    i.target_double_width i.target_double_align
    i.target_long_double_width i.target_long_double_align
    i.target_float128_width i.target_float128_align
    i.target_large_array_min_width i.target_large_array_align    
    i.target_suitable_align
    i.target_big_endian i.target_TLS_supported
    i.target_has_int128 i.target_has_float128_type
    i.target_null_pointer_value


let name s = s.name_print

let enum_name e =
  if e.enum_name.name_print <> "" then name e.enum_name else
    match e.enum_typedef with
    | Some t -> "typedef:"^(name t.typedef_name)
    | None -> "<anon>"

let record_name e =
  if e.record_name.name_print <> "" then name e.record_name else
    match e.record_typedef with
    | Some t -> "typedef:"^(name t.typedef_name)
    | None -> "<anon>"

                
(** raw (and ugly) AST dump, for debugging purpose;
    we hide internal functions inside a module
 *)
module P = struct
  let p ch = Printf.bprintf ch
  let name ch s = p ch "%s" s.name_qualified (*name.print*)
  let rec decl indent ch d =
    sync ch;
    p ch "%s%s\n" indent (string_of_range d.decl_range);
    (match d.decl_comment with None -> () | Some c -> p ch "%s\n" c.com_text);
    let indent2 = indent^"  " in
    match d.decl_kind with
    | TranslationUnitDecl l -> p ch "%sTranslationUnitDecl\n%a" indent (decl_list indent2) l
    | EmptyDecl -> p ch "%sEmptyDecl\n" indent
    | FileScopeAsmDecl s -> p ch "%sFileScopeAsmDecl\n%s%s\n" indent indent2 s
    | LinkageSpecDecl (l,d) -> p ch "%sLinkageSpecDecl %s\n%a" indent (lang_name l) (decl_list indent2) d
    | LabelDecl d -> p ch "%sLabelDecl %a\n" indent name d
    | EnumDecl e -> p ch "%sEnumDecl %a\n" indent enum_decl e
    | RecordDecl e -> record_decl indent ch e
    | TypedefDecl t -> p ch "%sTypedefDecl %a %a\n" indent name t.typedef_name type_qual t.typedef_underlying_type
    | FunctionDecl f -> function_decl indent ch f
    | VarDecl v -> var_decl indent ch v
    | FieldDecl f -> p ch "%sFieldDecl %a^!" indent field_decl f
    | EnumConstantDecl e -> p ch "%sEnumConstantDecl %a = %s^!" indent name e.enum_cst_name (Z.to_string e.enum_cst_val)
    | BlockDecl b -> p ch "%sBlockDecl %a" indent (block_decl indent2) b
    | UnknownDecl (i,s) -> p ch "%sUnknownDecl kind=%i %s\n" indent i s
    | AccessSpecDecl -> p ch "%sAccessSpecDecl\n" indent
    | FriendDecl f -> friend_decl indent ch f
    | StaticAssertDecl s ->
       p ch "%sStaticAssertDecl isfailed=%B expr=%a msg=%s\n" indent
         s.assert_is_failed expr s.assert_expr s.assert_msg
    | NamespaceDecl n ->
       p ch "%sNamespaceDecl %a\n" indent namespace_decl n
    | NamespaceAliasDecl n ->
       p ch "%sNamespaceAliasDecl %a %a %a\n%a" indent
         name n.namespace_alias_name
         namespace_decl n.namespace_alias_namespace
         name_specifier_loc n.namespace_alias_qualifier
         (decl indent2) n.namespace_alias_aliased_namespace
     | BuiltinTemplateDecl b ->
       p ch "%sBuiltinTemplateDecl %a %s param=%a requires=%a\n%a" indent
         name b.builtin_template_name
         (builtin_template_kind_name b.builtin_template_kind)
         (template_parameter_list indent2) b.builtin_template_param
         (bp_option "<none>" expr) b.builtin_template_requires_clause
         (decl indent2) b.builtin_template_decl
    | ClassTemplateDecl b ->
       p ch "%sClassTemplateDecl %a param=%a requires=%a injected=%a nbspecs=%i\n%a" indent
         name b.class_template_name
         (template_parameter_list indent2) b.class_template_param
         (bp_option "<none>" expr) b.class_template_requires_clause
         type_qual b.class_template_injected_type
         (List.length b.class_template_specializations)
         (record_decl indent2) b.class_template_decl
    | FunctionTemplateDecl  b ->
      p ch "%sFunctionTemplateDecl %a param=%a requires=%a nbspecs=%i\n%a" indent
        name b.function_template_name
        (template_parameter_list indent2) b.function_template_param
        (bp_option "<none>" expr) b.function_template_requires_clause
        (List.length b.function_template_specializations)
        (function_decl indent2) b.function_template_decl
    | TypeAliasTemplateDecl b ->
      p ch "%sTypeAliasTemplateDecl %a param=%a requires=%a name=%a %a\n" indent
        name b.alias_template_name
        (template_parameter_list indent2) b.alias_template_param
        (bp_option "<none>" expr) b.alias_template_requires_clause
        name b.alias_template_decl.typedef_name
        type_qual b.alias_template_decl.typedef_underlying_type
   | VarTemplateDecl b ->
      p ch "%sVarTemplateDecl %a param=%a requires=%a  nbspecs=%i\b%a" indent
        name b.var_template_name
        (template_parameter_list indent2) b.var_template_param
        (bp_option "<none>" expr) b.var_template_requires_clause
        (List.length b.var_template_specializations)
        (var_decl indent2) b.var_template_decl          
   | TemplateTemplateParmDecl b->
        p ch "%sTemplateTemplateParmDecl %a param=%a requires=%a pack=%B expansion=%B [%a]\b%a" indent
        name b.template_param_name
        (template_parameter_list indent2) b.template_param_param
        (bp_option "<none>" expr) b.template_param_requires_clause
        b.template_param_is_parameter_pack
        b.template_param_is_pack_expansion
        (bp_array (template_parameter_list indent2) ";") b.template_param_expansion_template_param
        (decl indent2) b.template_param_decl
   | TemplateTypeParmDecl d ->
      p ch "%sTemplateTypeParmDecl %a def=%a\n" indent
        name d.template_type_param_name (bp_option "<none>" type_qual) d.template_type_param_default
   | TypeAliasDecl t ->
      p ch "%sTypeAliasDecl %a %a\n" indent
        name t.typedef_name type_qual t.typedef_underlying_type
   | UnresolvedUsingTypenameDecl t ->
      p ch "%sUnresolvedUsingTypenameDecl %a %a is_pack=%B\n" indent
        name t.unresolved_using_typename_name
        name_specifier_loc t.unresolved_using_typename_qualifier
        t. unresolved_using_typename_is_pack_expansion
  | UsingDecl t ->
     p ch "%sUsingDecl %a %a has_typename=%B\n" indent
       name t.using_name
       name_specifier_loc t.using_qualifier
       t.using_has_typename
  | UsingDirectiveDecl t ->
     p ch "%sUsingDirectiveDecl %a %a namespace=%a\n" indent
       name t.using_directive_name
       name_specifier t.using_directive_qualifier
       namespace_decl t.using_directive_namespace
  | UsingPackDecl t ->
      p ch "%sUsingPackDecl %a\n%a%a\n" indent
        name t.using_pack_name (decl indent2) t.using_pack_decl
        (bp_array (decl indent2) "") t.using_pack_expansion
  | UsingShadowDecl t ->
      p ch "%sUsingShadowDecl %a using: %a %a %B\n%a" indent
        name t.using_shadow_name
        name t.using_shwdow_using.using_name
        name_specifier_loc t.using_shwdow_using.using_qualifier
        t.using_shwdow_using.using_has_typename
        (decl indent2) t.using_shadow_target      
  | BindingDecl t ->
     p ch "%sBindingDecl %a %a binding=%a holding=%a" indent
       name t.binding_name
       type_qual t.binding_type
       expr t.binding_binding
       (bp_option "<none>\n" (var_decl indent2)) t.binding_holding_var
  | IndirectFieldDecl t ->
     p ch "%sIndirectFieldDecl %a %a field=%a\n%a%a" indent
       name t.indirect_field_name
       type_qual t.indirect_field_type
       field_decl t.indirect_field_anon_field
       (bp_option "<none>" (var_decl indent2)) t.indirect_field_var
       (bp_array (decl indent2) "") t.indirect_field_chain
  | UnresolvedUsingValueDecl t ->
     p ch "%sUnresolvedUsingValueDecl %a %a %a\n" indent
       name t.unresolved_using_value_name
       type_qual t.unresolved_using_value_type
       name_specifier_loc t.unresovled_using_value_qualifier
  | NonTypeTemplateParmDecl t ->
     p ch "%sNonTypeTemplateParmDecl %a %a def=%a %B %B expansion=[%a]\n" indent
       name t.non_type_template_name
       type_qual t.non_type_template_type
       (bp_option "<none>" expr) t.non_type_template_default
       t.non_type_template_param_is_parameter_pack
       t.non_type_template_param_is_pack_expansion
       (bp_array type_qual ";") t.non_type_template_param_expansion
  and decl_name ch d =
    sync ch;
    match d.decl_kind with
    | LabelDecl d -> name ch d
    | EnumDecl e -> name ch e.enum_name
    | RecordDecl e -> name ch e.record_name
    | TypedefDecl t -> name ch t.typedef_name
    | FunctionDecl f -> name ch f.function_name
    | VarDecl v -> name ch v.var_name
    | FieldDecl f -> name ch f.field_name
    | EnumConstantDecl e -> name ch e.enum_cst_name
    | NamespaceDecl n -> name ch n.namespace_name
    | NamespaceAliasDecl n -> name ch n.namespace_alias_name
    | BuiltinTemplateDecl b -> name ch b.builtin_template_name
    | ClassTemplateDecl b -> name ch b.class_template_name
    | FunctionTemplateDecl  b -> name ch b.function_template_name
    | TypeAliasTemplateDecl b -> name ch b.alias_template_name
    | VarTemplateDecl b -> name ch b.var_template_name
    | TemplateTemplateParmDecl b-> name ch b.template_param_name
    | TemplateTypeParmDecl d -> name ch d.template_type_param_name
    | TypeAliasDecl t -> name ch t.typedef_name
    | UnresolvedUsingTypenameDecl t -> name ch t.unresolved_using_typename_name
    | UsingDecl t -> name ch t.using_name
    | UsingPackDecl t -> name ch t.using_pack_name
    | UsingShadowDecl t -> name ch t.using_shadow_name
    | BindingDecl t -> name ch t.binding_name
    | IndirectFieldDecl t -> name ch t.indirect_field_name
    | UnresolvedUsingValueDecl t -> name ch t.unresolved_using_value_name
    | NonTypeTemplateParmDecl t -> name ch t.non_type_template_name
    | EmptyDecl | AccessSpecDecl | TranslationUnitDecl _ | FileScopeAsmDecl _
      | LinkageSpecDecl _ | BlockDecl _ | FriendDecl _ | StaticAssertDecl _
      | UsingDirectiveDecl _|UnknownDecl _ -> p ch "<none>"
  and namespace_decl ch n =
    sync ch;
    p ch "name=%a anon=%B inline=%B" name n.namespace_name n.namespace_is_anonymous n.namespace_is_inline
  and template_parameter_list indent ch t = 
    sync ch;
    p ch "[%a]:%a" (bp_array (decl indent) ";")
      t.template_parameter_list_params (bp_option "<none>" expr) t.template_parameter_list_requires_clause
  and decl_list indent ch = List.iter (fun a -> sync ch; decl indent ch a)
  and function_decl indent ch f =
    sync ch;
    let indent2 = indent^"  " in
    p ch "%sFunctionDecl %a%s var=%B main=%B global=%B storage=%s sig=(%a)->%a template=%a %a\n%a" indent
      name f.function_name
      (match f.function_overloaded_operator with None -> "" | Some x -> " "^(overloaded_operator_name x))
      f.function_is_variadic f.function_is_main f.function_is_global
      (storage_class_name f.function_storage_class)
      (bp_array param_var_decl ",") f.function_params
      type_qual f.function_return_type
      (bp_option "<none>" function_template_specialization) f.function_template
      (bp_option "" cxx_method_decl) f.function_method
      (bp_option "" (stmt indent2)) f.function_body
  and cxx_method_decl ch f =
    sync ch;
    p ch "method: class=%a instance=%B virtual=%B override=%i ref=%s %a"
      record_decl_name f.method_parent_class
      f.method_is_instance f.method_is_virtual
      (List.length f.method_overridden)
      (ref_qualifier_name f.method_ref_qualifier)
      cxx_method_kind f.method_kind
  and cxx_method_kind ch f =
    sync ch;
    match f with
    | Method_Regular -> ()
    | Method_Destructor -> p ch "destructor"
    | Method_Conversion b -> p ch "conversion explicit=%B" b
    | Method_Constructor b ->
       p ch "constructor explicit=%B delegate=%t init=[%a]"
         b.constructor_explicit
         (fun ch -> match b.constructor_delegating with None -> p ch "<none>" | Some x -> name ch x.function_name)
         (bp_list cxx_constructor_initializer ";") b.constructor_initializers
  and cxx_constructor_initializer ch f =
    sync ch;
    match f with
    | Constructor_init_Base (t,e,i) ->
       p ch "base(%a,%B,%a)" type_qual t e expr i
    | Constructor_init_Field (f,i) ->
       p ch "field(%a,%a)" name f.field_name expr i
    | Constructor_init_Indirect_field  (f,i) ->
       p ch "indirect_field(%a,%a)" name f.indirect_field_name expr i
    | Constructor_init_Delegating (t,i) ->
       p ch "delegate(%a,%a)" type_qual t expr i
  and function_template_specialization ch t =
    sync ch;
    p ch "%a(%a)"
      name t.function_template_specialization_decl.function_template_name
      (bp_array template_argument ",") t.function_template_specialization_args
  and var_decl indent ch v =
    sync ch;
    p ch "%sVarDecl %a %a file=%B local=%B storage=%s init=%a template=%a\n" indent
      name v.var_name type_qual v.var_type v.var_is_file_scoped
      v.var_is_local
      (storage_class_name v.var_storage_class)
      (bp_option "" expr) v.var_init
      (bp_option "<none>" var_template_specialization) v.var_template
  and var_decl_name ch v = sync ch; name ch v.var_name
  and var_template_specialization ch t =
    sync ch;
    p ch "%a(%a)"
      name t.var_template_specialization_decl.var_template_name
      (bp_array template_argument ",") t.var_template_specialization_args
  and enum_decl ch e =
    sync ch;
    p ch "%a pbit=%i:nbit=%i:complete=%B %a %a {%a}"
      name e.enum_name e.enum_num_positive_bits e.enum_num_negative_bits e.enum_is_complete
      (bp_option "<none>" type_qual) e.enum_integer_type (bp_option "<none>" type_qual) e.enum_promotion_type
      (bp_list (fun ch c -> p ch "%a=%s" name c.enum_cst_name (Z.to_string c.enum_cst_val)) ",") e.enum_cst
  and record_decl indent ch e =
    sync ch;
    let indent2 = indent^"  " in
    p ch "%sRecordDecl %s %s flexible=%B volatile=%B anon=%B complete=%B valid=%B size=%Li data-size=%Li align=%Li {%a} template=%a bases=[%a]\n%a%a" indent
      (record_kind_name e.record_kind) (record_name e)
      e.record_has_flexible_array_member e.record_has_volatile_member
      e.record_is_anonymous e.record_is_complete e.record_is_valid
      e.record_size e.record_data_size e.record_alignment
      (bp_list field_decl ",") e.record_fields
      (bp_option "<none>" class_template_specialization) e.record_template
      (bp_list cxx_base_specifier ";") e.record_base_class
      (bp_list (friend_decl indent2) "\n") e.record_friends
      (bp_list (function_decl indent2) "\n") e.record_methods
  and record_decl_name ch e = sync ch; name ch e.record_name
  and field_decl ch c =
    sync ch;
    p ch "%a:idx=%i:%a:bitwidth=%s:unnamed=%B:valid=%B:offset=%Li:vla=%a"
      name c.field_name c.field_index type_qual c.field_type
      (match c.field_bitwidth with None -> "-" | Some x -> string_of_int x)
      c.field_is_unnamed_bitfield c.field_is_in_valid_record c.field_offset
      (bp_option "<none>" expr) c.field_variable_length_array
  and class_template_specialization ch t =
    sync ch;
    p ch "%a(%a)"
      name t.class_template_specialization_decl.class_template_name
      (bp_array template_argument ",") t.class_template_specialization_args
  and cxx_base_specifier ch t =
    sync ch;
    p ch "%a:virt=%B:%s"
      type_qual t.cxx_base_type t.cxx_base_is_virtual (access_specifier_name t.cxx_base_access)
  and friend_decl indent ch f =
    sync ch;
    let indent2 = indent^"  " in
    p ch "%sFriendDecl unsupported=%B template=[%a]" indent
      f.friend_is_unsupported (bp_array (template_parameter_list indent2) " ; ") f.friend_template;
    (match f.friend_decl with Some d -> p ch " decl\n%a" (decl indent2) d | None -> ());
    (match f.friend_type with Some t -> p ch " type=%a\n" type_qual t | None -> ())     
  and param_var_decl ch a =
    sync ch;
    p ch "%a %a" type_qual a.var_type name a.var_name
  and function_decl_name ch d = sync ch; name ch d.function_name
  and type_qual ch (t,q) = sync ch;p ch "%a%a" typ t qual q
  and qual ch q =
    sync ch;
    p ch "%s%s%s"
      (if q.qual_is_const then "const " else "")
      (if q.qual_is_restrict then "restrict " else "")
      (if q.qual_is_volatile then "volatile " else "")
  and typ ch a =
    sync ch;
    match a with
    | DecayedType (t1,t2) -> p ch "decayed (%a) -> (%a)" type_qual t1 type_qual t2
    | ArrayType a ->
       p ch "%a[%s%a]"
         type_qual a.array_element_type
         (match a.array_size_modifier with
          | Size_Normal -> "" | Size_Static -> "static "| Size_Star -> "*")
         (fun ch -> function
           | Size_Constant c -> p ch "%s" (Z.to_string c)
           | Size_Variable e -> expr ch e
           | Size_Incomplete -> ()
           | Size_Dependent -> ()
         ) a.array_size 
    | AtomicType a -> p ch "atomic(%a)" type_qual a   
    | AttributedType a -> p ch "attributed(%a)" type_qual a 
    | BuiltinType b -> p ch "%s" (builtin_type_name b)
    | ComplexType a -> p ch "complex(%a)" type_qual a 
    | FunctionProtoType f ->
       p ch "function %a(%a)%s -> %a : %a"
         qual f.proto_qual
         (bp_array type_qual ",") f.proto_params
         (if f.proto_variadic then "..." else "")
         type_qual f.proto_return_type type_qual f.proto_result_type
    | FunctionNoProtoType f -> p ch "function(noproto : %a)" type_qual f.noproto_result_type
    | ParenType t -> p ch "(%a)" type_qual t
    | PointerType t -> p ch "%a*" type_qual t
    | EnumType e -> p ch "enum %s"(enum_name e)
    | RecordType e -> p ch "%s %s" (record_kind_name e.record_kind) (record_name e)
    | TypedefType t -> p ch "%a=%a" name t.typedef_name type_qual t.typedef_underlying_type
    | ElaboratedType t -> p ch "elaborated(%a)" type_qual t
    | UnaryTransformType t -> p ch "unary(%a) -> (%a)" type_qual t.unary_base_type type_qual t.unary_underlying_type
    | TypeOfExprType e -> p ch "typeof(%a)" expr e
    | DecltypeType (e,o) -> p ch "decltype(%a,%a)" expr e (bp_option "<none>" type_qual) o
    | AutoType b -> p ch "auto(%B)" b
    | DeducedTemplateSpecializationType t -> p ch "deduced(%a)" template_name t
    | DependentSizedExtVectorType (e,o) -> p ch "dependent_vector(%a,%a)" expr e type_qual o
    | InjectedClassNameType (a,b,c,d) ->
       p ch "injected(%a,%a,%a,{%a})"
         type_qual a (bp_option "<unknown>" template_name) b record_decl_name c (bp_array template_argument ",") d
    | MemberPointerType (t1,t2) -> p ch "member_ptr(%a,%a)" type_qual t1 typ t2
    | PackExpansionType (t,i) ->
       p ch "pack_expansion(%a,%s)"
         type_qual t (match i with None -> "<unknown>" | Some ii -> string_of_int ii)
    | LValueReferenceType t -> p ch "lvalue_ref(%a)" type_qual t
    | RValueReferenceType t -> p ch "rvalue_ref(%a)" type_qual t
    | SubstTemplateTypeParmPackType (s,t1,t2) ->
       p ch "subst_template_type_parm_pack(%s,%a,%a)"
         s template_type_param_type t1 template_argument t2
    | SubstTemplateTypeParmType (t1,t2) ->
       p ch "subst_template_type_parm(%a,%a)" template_type_param_type t1 type_qual t2
    | TemplateSpecializationType (t1,t2,t3) ->
       p ch "template_specialization(%a,%a,{%a})"
         (bp_option "<none>" type_qual) t1 template_name t2 (bp_array template_argument ",") t3
    | TemplateTypeParmType t -> p ch "template_type_parm%a" template_type_param_type t
    | DependentNameType (n,s) -> p ch "dependent_name(%a,%s)" name_specifier n s
    | DependentTemplateSpecializationType (n,s,a) ->
       p ch "dependent_template_specialization(%a,%s,{%a})"
         name_specifier n s (bp_array template_argument ",") a
    | UnresolvedUsingType t ->
       p ch "unresolved_using_value(%a,%a,%B)"
        name t.unresolved_using_typename_name
        name_specifier_loc t.unresolved_using_typename_qualifier
        t. unresolved_using_typename_is_pack_expansion
    | VectorType (t,n,m) -> p ch "vector(%a,%i,%i)" type_qual t n m
    | UnknownType (i,s) -> p ch "UnknownType:%s(%i)" s i
  and template_type_param_type ch t =
    sync ch;
    p ch "(%i,%i,%B,%t,%s)"
      t.template_type_param_depth t.template_type_param_index
      t.template_type_param_is_parameter_pack
      (fun ch -> match t.template_type_param_decl with Some x -> name ch x.template_type_param_name | None -> p ch "<none>")
      (match t.template_type_param_identifier with Some y -> y | None -> "<anon>")
  and designator ch a =
    sync ch;
    match a with
    | Designator_Field f -> p ch "field:%a" name f.field_name
    | Designator_Array e -> p ch "Array(%a)" expr e
    | Designator_ArrayRange (e1,e2) -> p ch "ArrayRange(%a,%a)" expr e1 expr e2
  and offsetof ch a =
    sync ch;
    match a with
    | Offsetof_Array e -> p ch "Array(%a)" expr e
    | Offsetof_Field f -> p ch "Field(%a)" name f.field_name
    | Offsetof_Identifier s -> p ch "Identifier(%s)" s
  and declaration_name ch a =
    sync ch;
    match a with
    | Name_Identifier s -> p ch "%s" s
    | Name_CXXConstrucorName t -> p ch "ctor<%a>" type_qual t
    | Name_CXXDestructorName t -> p ch "dtor<%a>" type_qual t
    | Name_CXXConversionFunctionName t -> p ch "cast<%a>" type_qual t
    | Name_CXXDeductionGuideName t -> p ch "deduc<%a>" decl_name t
    | Name_CXXOperatorName op -> p ch "operator<%s>" (overloaded_operator_name op)
    | Name_CXXLiteralOperatorName s -> p ch "literal<%s>" s
    | Name_CXXUsingDirective s -> p ch "using<%s>" s
  and name_specifier_loc ch x =
    sync ch;
    p ch "[%a]" (bp_list (fun ch y -> name_specifier_kind ch y.name_specifier_kind) "::") x
  and name_specifier ch x =
    sync ch;
    p ch "[%a]" (bp_list (fun ch y -> name_specifier_kind ch y) "::") x
  and name_specifier_kind ch x =
    sync ch;
    match x with
    | Name_specifier_Identifier s -> p ch "id(%s)" s
    | Name_specifier_Namespace s -> p ch "namespace(%a)" name s.namespace_name
    | Name_specifier_NamespaceAlias s -> p ch "namespace_alias(%a)" name s.namespace_alias_name
    | Name_specifier_TypeSpec s -> p ch "type(%a)" typ s
    | Name_specifier_TypeSpecWithTemplate s -> p ch "template_type(%a)" typ s
    | Name_specifier_Global -> p ch "global"
  and template_argument_loc ch (x,_) = sync ch; template_argument ch x
  and template_argument ch x =
    sync ch;
    match x with
    | Template_argument_Null -> p ch "null"
    | Template_argument_Type t -> p ch "type(%a)" type_qual t
    | Template_argument_Declaration d -> p ch "decl(%a)" decl_name d
    | Template_argument_NullPtr t -> p ch "NullPtr(%a)" type_qual t
    | Template_argument_Integral (_,z) -> p ch "int(%s)" (Z.to_string z)
    | Template_argument_Template x -> p ch "template(%a)" template_name x
    | Template_argument_Expression x -> p ch "expr(%a)" expr x
    | Template_argument_Pack (t,a) ->
       p ch "pack(%a,[%a])" template_argument t (bp_array template_argument ";") a
  and  template_name ch x =
    sync ch;
    match x with
    | Template_name_Template d -> p ch "template(%a)" decl_name d
    | Template_name_OverloadedTemplate d ->
       p ch "overloaded[%a]" (bp_list decl_name ";") d
    | Template_name_QualifiedTemplate (b,d) ->
       p ch "qual_template(%a,%a)" name_specifier b decl_name d
    | Template_name_DependentTemplate (n,so,oo) ->
       p ch "dependent(%a,%s)" name_specifier n
         (match so,oo with Some x,_ -> x | _,Some y -> overloaded_operator_name y | _ -> "?")
    | Template_name_SubstTemplateTemplateParm (d,t) ->
       p ch "subst(%a,%a)" decl_name d template_name t
    | Template_name_SubstTemplateTemplateParmPack (d,t) ->
        p ch "subst_pack(%a,%a)" decl_name d template_argument t
  and cxx_construct_expr ch a =
    sync ch;
    p ch "construct(%a,%s,{%a},%B,%B)"
      function_decl_name a.construct_decl
      (construction_kind_name a.construct_kind)
      (bp_array expr ",") a.construct_args
      a.construct_requires_zero_init a.construct_temporary
  and capture ch c =
    sync ch;
    p ch "[%a,%B,%B,%a]"
      name c.capture_var.var_name c.capture_is_by_ref c.capture_is_nested
      (bp_option "<none>" expr) c.capture_copy_expr
  and block_decl indent ch f =
    sync ch;
    p ch "Block[{%a},{%a},%B,%B,%B,%B,\n%a]"
      (bp_array param_var_decl ",") f.block_params
      (bp_array capture ",") f.block_captures
      f.block_captures_cxx_this f.block_is_variadic
      f.block_missing_return_type f.block_is_conversion_from_lambda
      (bp_option "<empty>" (stmt indent)) f.block_body
  and expr ch e =
    sync ch;
    (*dump_block true e;*)
    p ch "%a:" (bp_option "<notype>" type_qual) e.expr_type;
    match e.expr_kind with

    (* C *)
    | ConditionalOperator { cond_cond=e1; cond_true=e2; cond_false=e3; } ->
       p ch "ConditionalOperator(%a ? %a : %a)" expr e1 expr e1 expr e3
    | AddrLabelExpr l -> p ch "&&%a" name l
    | ArrayInitIndexExpr -> p ch "InitIndex"
    | ArrayInitLoopExpr { array_init_source=o; array_init_init=e; array_init_size=s; } ->
       p ch "InitLoop(%a,%a,%s)"
         (bp_option "<none>" expr) o.opaque_source expr e (Z.to_string s)
    | ArraySubscriptExpr e ->
       p ch "ArraySubscript(%a,%a)"
         expr e.subscript_base expr e.subscript_index
    | AtomicExpr a ->
       p ch "AtmoicExp(%i,%a,%a)"
         a.atomic_op expr a.atomic_ptr expr a.atomic_order
    | CompoundAssignOperator t ->
       p ch "CompoundAssign(%a,%a,%s,%a,%a)"
         expr t.compound_lval type_qual t.compound_comp_lval_type         
         (compound_assign_operator_name t.compound_op)
         expr t.compound_rval type_qual t.compound_comp_result_type
    | BinaryOperator (e1,op,e2) ->
       p ch "Binary(%a,%s,%a)"
         expr e1 (binary_operator_name op) expr e2
    | UnaryOperator (op,e) -> p ch "Unary(%s,%a)" (unary_operator_name op) expr e
    | CallExpr c ->
       p ch "Call(%a,%s,%t,{%a})"
         expr c.call_callee
         (match c.call_operator with None -> "" | Some d -> overloaded_operator_name d)
         (fun ch -> match c.call_call_decl with None -> p ch "<none>" | Some dd -> name ch dd.function_name)
         (bp_array expr ",") c.call_args
    | CastExpr (e,c) -> p ch "cast(%a,%s)" expr e (cast_kind_name c)
    | CharacterLiteral (i,k) -> p ch "Char(%s,%s)" (Int32.to_string i) (character_kind_name k)
    | ChooseExpr c ->
       p ch "ChooseExpr(%a=%B ? %a : %a)"
         expr c.choose_cond c.choose_cond_true
         expr c.choose_true expr c.choose_false
    | CompoundLiteralExpr (e,b) -> p ch "CompoundLiteral(%a,%B)" expr e b
    | DeclRefExpr d -> p ch "DeclRef(%a)" decl_name d
    | DesignatedInitExpr (d,e) -> p ch "Designated({%a},%a)" (bp_array designator ",") d expr e
    | FloatingLiteral s -> p ch "Float(%s)" s
    | GenericSelectionExpr { select_controling=c; select_assoc=l; select_result=i; } ->
       p ch "GenericSelectionExpr(%a,{%a},%i)" expr c (bp_array expr ",") l i
    | GNUNullExpr -> p ch "GNUNull"
    | ImaginaryLiteral e -> p ch "Imaginary(%a)"expr e
    | ImplicitValueInitExpr -> p ch "ImplicitValueInit"
    | InitListExpr i ->
       p ch "InitList({%a},%t,%a)"
         (bp_array expr ",") i.init_list_init
         (fun ch -> match i.init_list_field_in_union with None -> p ch "<none>" | Some d -> name ch d.field_name)
         (bp_option "<none>" expr) i.init_list_filler
    | IntegerLiteral z -> p ch "Integer(%s)" (Z.to_string z)
    | MemberExpr m ->
       p ch "Member(%a,%s,%t,%a,{%a},%a)"
         expr m.member_base (if m.member_arrow then "->" else ".")
         (fun ch -> match m.member_decl.decl_kind with FieldDecl f -> name ch f.field_name | _ -> p ch "<unknown>")
         (bp_option "" name_specifier_loc) m.member_qualifier
         (bp_array template_argument_loc ",") m.member_template_args
         declaration_name m.member_name
    | NoInitExpr -> p ch "NoInitExpr"
    | OffsetOfExpr (o,i) ->
       p ch "OffsetOf({%a},%s)"
         (bp_array offsetof ",") o (match i with None -> "<none>" | Some i -> Z.to_string i)
    | OpaqueValueExpr o -> p ch "OpaqueValue(%a)" (bp_option "<none>" expr) o.opaque_source
    | ParenExpr e -> p ch "Paren(%a)" expr e      
    | ParenListExpr e -> p ch "ParenList({%a})" (bp_array expr ",") e          
    | PredefinedExpr (i,s) -> p ch "Predefined(%s,%s)" (ident_type_name i) s
    | PseudoObjectExpr { pseudo_object_syntactic_form=e1; pseudo_object_semantic=e2; pseudo_object_result = i; } ->
       p ch "PseudoObject(%a,{%a},%s)"
         expr e1 (bp_array expr ",") e2
         (match i with None -> "<none>" | Some i -> string_of_int i)
    | StmtExpr s -> p ch "Stmt({\n%a})" (bp_list (stmt "--") "") s          
    | StringLiteral (s,k) -> p ch "String(%s,%s)" s (character_kind_name k)
    | UnaryExprOrTypeTraitExpr (op,t) -> p ch "UnaryType(%s,%a)" (unary_expr_or_type_name op) type_qual t
    | VAArgExpr e -> p ch "VAArgExpr(%a)" expr e

    (* C++*)
    | ArrayTypeTraitExpr a ->
       p ch "ArrayTypeTraitExpr(%s,%a,%Li,%a)"
         (array_type_trait_name a.trait_trait)
         type_qual a.trait_type a.trait_value expr a.trait_dimension
    | CXXBindTemporaryExpr e -> p ch "CXXBindTemporaryExpr(%a)" expr e
    | CXXBoolLiteralExpr b -> p ch "CXXBoolLiteralExpr(%B)" b
    | CXXConstructExpr c ->
       p ch "CXXConstructExpr(%a,%s,{%a},%B,%B)"
         function_decl_name c.construct_decl
         (construction_kind_name c.construct_kind)
         (bp_array expr ",") c.construct_args
         c.construct_requires_zero_init c.construct_temporary
    | CXXDefaultArgExpr (r,e) -> p ch "CXXDefaultArgExpr(%a,%a)"param_var_decl r expr e
    | CXXDefaultInitExpr (f,e) -> p ch "CXXDefaultInitExpr(%a,%a)" field_decl f expr e
    | CXXDeleteExpr d ->
       p ch "CXXDeleteExpr(%a,%a,%a,%B,%B,%B)"
         expr d.delete_arg
         (bp_option "<unknown>" function_decl_name) d.delete_op_delete
         (bp_option "<dependent>" type_qual) d.delete_destroyed_type
         d.delete_is_global d.delete_is_array d.delete_array_want_size
    | CXXDependentScopeMemberExpr d ->
       p ch "CXXDependentScopeMemberExpr(%a,%a,%s,%a,%a,{%a})"
         (bp_option "<implicit>" expr) d.dependent_base
         type_qual d.dependent_base_type
         (if d.dependent_arrow then "->" else ".")
         name_specifier_loc d.dependent_qualifier
         declaration_name d.dependent_member
         (bp_array template_argument_loc ",") d.dependent_template_args
    | CXXFoldExpr  c ->
       p ch "CXXFoldExpr(%a,%a,%B,%s)"
         expr c.fold_pattern
         (bp_option "<none>" expr) c.fold_init
         c.fold_right_fold
         (binary_operator_name c.fold_operator)
    | CXXInheritedCtorInitExpr  i ->
       p ch "CXXInheritedCtorInitExpr(%a,%s,%B,%B)"
         function_decl_name i.inherited_ctor
         (construction_kind_name i.inherited_ctor_kind)
         i.inherited_ctor_constructs_vbase
         i.inherited_ctor_inherited_from_vbase
    | CXXNewExpr n ->
       p ch "CXXNewExpr(%a,%a,%a,%a,%B,%s,%a,(%a),%a,%B,%B)"
         type_qual n.new_alloctype
         (bp_option "<unknown>" function_decl_name) n.new_op_new
         (bp_option "<unknown>" function_decl_name) n.new_op_delete
         (bp_option "noarray" expr) n.new_array_size
         n.new_is_global
         (initialization_style_name n.new_style)
         (bp_option "<noinit>" expr) n.new_initialier
         (bp_array expr ",") n.new_args
         (bp_option "<none>" cxx_construct_expr) n.new_construct
         n.new_pass_alignment
         n.new_array_want_size
    | CXXNoexceptExpr (e,b) -> p ch "CXXNoexceptExpr(%a,%B)" expr e b
    | CXXNullPtrLiteralExpr -> p ch "CXXNullPtrLiteralExpr"
    | CXXPseudoDestructorExpr d ->
       p ch "CXXPseudoDestructorExpr(%a,%a,%B,%a)"
         expr d.destructor_base
         (bp_option "<none>" name_specifier_loc) d.destructor_qualifier
         d.destructor_is_arrow
         type_qual d.destructor_destroyed_type
    | CXXScalarValueInitExpr -> p ch "CXXScalarValueInitExpr"
    | CXXStdInitializerListExpr e -> p ch "CXXStdInitializerListExpr(%a)" expr e
    | CXXThisExpr b -> p ch "CXXThisExpr(%B)" b
    | CXXThrowExpr t ->
       p ch "CXXThrowExpr(%a,%B)"
         (bp_option "<none>" expr) t.throw_expr t.throw_is_thrown_variable_in_scope
    | CXXTypeidExpr t ->
       p ch "CXXTypeidExpr(%a,%a,%B)"
         (bp_option "<none>" type_qual) t.typeid_type_operand
         (bp_option "<none>" expr) t.typeid_expr_operand
         t.typeid_is_potentially_evaluated
    | CXXUnresolvedConstructExpr u ->
       p ch "CXXUnresolvedConstructExpr(%a,%a)"
         type_qual u.unresolved_construct_type_as_written
         (bp_array expr ",") u.unresolved_construct_args
    | DependentScopeDeclRefExpr d ->
       p ch "DependentScopeDeclRefExpr(%a,%a,{%a})"
         declaration_name d.dependent_decl_name
         name_specifier_loc d.dependent_decl_qualifier_loc
         (bp_array template_argument_loc ",") d.dependent_decl_template_args
    | ExpressionTraitExpr d ->
       p ch "ExpressionTraitExpr(%s,%a,%B)"
         (expression_trait_name d.expr_trait_trait)
         expr d.expr_trait_queried_expr d.expr_trait_value
    | ExprWithCleanups c ->
       p ch "ExprWithCleanups(%a,{%a})"
         expr c.cleanup_expr (bp_array (block_decl "--") ";") c.cleanup_cleanups
    | FunctionParmPackExpr (a,b) ->
       p ch "FunctionParmPackExpr(%a,{%a})"
         param_var_decl a (bp_array param_var_decl ",") b
    | MaterializeTemporaryExpr a ->
       p ch "MaterializeTemporaryExpr(%a,%s,%a,%B)"
         expr a.materialize_tmp_expr
         (storage_duration_name a.materialize_storage)
         (bp_option "<none>" decl_name) a.materialize_extending_decl
         a.materialize_is_bound_to_lvalue_reference
    | PackExpansionExpr x ->
       p ch "PackExpansionExpr(%a,%s)"
         expr x.pack_pattern
         (match x.pack_num_expansions with Some i -> string_of_int i | None -> "<unknown>")
    | SizeOfPackExpr a ->
       p ch "SizeOfPackExpr(%a,%s,%B,{%a})"
         decl_name a.pack_param
         (match a.pack_length with None -> "?" | Some x -> string_of_int x)
         a.pack_is_partially_substituted
         (bp_array template_argument ",") a.pack_partial_arguments
    | SubstNonTypeTemplateParmExpr s ->
       p ch "SubstNonTypeTemplateParmExpr(%a,%a)"
         expr s.subst_replacement
         name s.subst_parameter.non_type_template_name
    | SubstNonTypeTemplateParmPackExpr s ->
       p ch "SubstNonTypeTemplateParmPackExpr(%a,%a)"
         name s.subst_parameter_pack.non_type_template_name
        template_argument s.subst_argument_pack
    | TypeTraitExpr t ->
       p ch "TypeTraitExpr(%s,%s,%a)"
         (type_trait_name t.type_trait_trait)
         (match t.type_trait_value with Some true -> "true" | Some false -> "false" | None -> "none")
         (bp_array type_qual ",") t.type_trait_args
    | UnresolvedLookupExpr e ->
       p ch "UnresolvedLookupExpr(%B,%B,%a,%a,{%a},{%a})"
         e.unresolved_lookup_requires_ADL e.unresolved_lookup_is_implicit
         (bp_option "<none>" record_decl_name) e.unresolved_lookup_naming_class
         declaration_name e.unresolved_lookup_name
         (bp_list decl_name ",") e.unresolved_lookup_decls
         (bp_array template_argument_loc ",") e.unresolved_lookup_template
    | UnresolvedMemberExpr e ->
      p ch "UnresolvedMemberExpr(%a,%a,%B,%B,%a,%a,%a,{%a},{%a})"
        (bp_option "<none>" expr) e.unresolved_member_base
        type_qual e.unresolved_member_base_type
        e.unresolved_member_is_implicit_access
        e.unresolved_member_is_arrow
        (bp_option "<none>" record_decl_name) e.unresolved_member_naming_class
        declaration_name e.unresolved_member_member_name
        declaration_name e.unresolved_member_name
        (bp_list decl_name ",") e.unresolved_member_decls
        (bp_array template_argument_loc ",") e.unresolved_member_template
    | LambdaExpr l ->
       p ch "LambdaExpr(%s,{%a},{%a},%a,%a,{%a},%B,{%a},%B,%B,%B)"
         (lambda_capture_default_name l.lambda_capture_default)
         (bp_list lambda_capture ",") l.lambda_captures
         (bp_list expr ",") l.lambda_capture_inits
         record_decl_name l.lambda_class
         function_decl_name l.lambda_call_operator
         (bp_option "<none>" (template_parameter_list "")) l.lambda_template_parameter
         l.lambda_is_generic
         (stmt "") l.lambda_body
         l.lambda_is_mutable
         l.lambda_has_explicit_parameters
         l.lambda_has_explicit_result_type
       
    (*  Vectors *)                      
    | ConvertVectorExpr e -> p ch "ConvertVectorExpr(%a)" expr e
    | ExtVectorElementExpr (e,s) -> p ch "ExtVectorElementExpr(%a,%s)" expr e s
    | ShuffleVectorExpr l -> p ch "ShuffleVectorExpr(%a)" (bp_array expr ",") l

    (* Unknown *)
    | UnknownExpr (i,s) ->
       p ch "UnknownExpr(%i,%s)"
         i s
  and lambda_capture ch l =
    p ch "capture(%s,%B,%B,%a,%B,%B)"
      (lambda_capture_kind_name l.lambda_capture_kind)
      l.lambda_capture_this l.lambda_capture_VLA_type
      (bp_option "<none>" var_decl_name) l.lambda_capture_captured_var
      l.lambda_capture_is_implicit l.lambda_capture_is_pack_expansion
  and stmt indent ch s =
    sync ch;
    let indent2 = indent^"  " in
    match s.stmt_kind with

    (* C *)
    | AsmStmt ->
       p ch "%sAsmStmt\n" indent
    | AttributedStmt s ->
       p ch "%sAttributedStmt %a" indent (stmt indent) s
    | BreakStmt l ->
       p ch "%sBreakStmt\n" indent
    | CompoundStmt s ->
       p ch "%sCompoundStmt\n" indent;
       List.iter (stmt indent2 ch) s
    | ContinueStmt l ->
       p ch "%sContinueStmt\n" indent
    | DeclStmt s ->
       p ch "%sDeclStmt\n" indent;
       List.iter (decl indent2 ch) s
    | DoStmt { do_body=s; do_cond=e; } ->
       p ch "%sDo\n%a%sWhile %a\n" indent (stmt indent2) s indent expr e;
    | ExprStmt e ->
       p ch "%sExprStmt %a\n" indent expr e
    | ForStmt { for_init=init; for_cond=c; for_inc=inc; for_body=b; } ->
       p ch "%sForStmt %a ; %a\n%sInit\n%a%sBody\n%a" indent
         (bp_option "<none>" expr) c (bp_option  "<none>"  expr) inc
         indent (bp_option "" (stmt indent2)) init
         indent (stmt indent2) b
    | GotoStmt (l,_) ->
       p ch "%sGotoStmt %a\n" indent name l
    | IfStmt { if_cond=c; if_then=t; if_else=e; if_init=i; } ->
       p ch "%sIfStmt %a\n%a%sThen\n%a%sElse\n%a" indent
         (bp_option  "<none>" expr) c
         (bp_option "" (stmt indent2)) i
         indent (bp_option "" (stmt indent2)) t
         indent (bp_option "" (stmt indent2)) e
    | IndirectGotoStmt (e,l) ->
       p ch "%sIndirectGotoStmt %a %t\n" indent
         expr e (fun ch -> match l with None -> () | Some l -> name ch l)
    | LabelStmt (l,s) ->
       p ch "%sLabelStmt %a\n%a" indent
         name l (stmt indent2) s
    | NullStmt ->
       p ch "%sNullStmt\n" indent
    | ReturnStmt e ->
       p ch "%sReturnStmt %a\n" indent (bp_option "" expr) e
    | CaseStmt { case_value=e1; case_end=e2; case_stmt=s; } ->
       p ch "%sCaseStmt %a %a\n%a" indent
         expr e1 (bp_option "" expr) e2 (stmt indent2) s
    | DefaultStmt s ->
       p ch "%sDefaultStmt\n%a" indent
         (stmt indent2) s
    | SwitchStmt { switch_init=i; switch_cond=c; switch_body=b; } ->
       p ch "%sSwitchStmt %a\n%sInit\n%a%sBody\n%a" indent
         expr c indent (bp_option "" (stmt indent2)) i indent (stmt indent2) b
    | WhileStmt { while_cond=e; while_body=s; } ->
       p ch "%sWhileStmt %a\n%a" indent
         expr e (stmt indent2) s

    (* C++ *)
    | CXXForRangeStmt r ->
       p ch "%sForRange %a : %a init=%a\n%a" indent
         name r.for_range_var.var_name type_qual r.for_range_var.var_type
          expr r.for_range_init (stmt indent2) r.for_range_body
    | CXXTryStmt t ->
       p ch "%sTry\n%a" indent
         (stmt indent2) t.try_block;
       Array.iter
         (fun h ->
           p ch "%sCatch %a : %a\n%a" indent
             (bp_option "<none>" (fun ch o -> name ch  o.var_name)) h.catch_exception
             (bp_option "<none>" type_qual) h.catch_type
             (stmt indent2) h.catch_handler             
         ) t.try_handlers

    (* Unknown *)
    | UnknownStmt (i,s) ->
       p ch "%sUnknownStmt %s(%i)\n" indent s i
end
             

             
let string_from_buffer f a =
  let buf = Buffer.create 128 in
  (try f buf a with Stack_overflow -> Buffer.add_string buf " *** STACK OVERFLOW ***");
  Buffer.contents buf
                  
let string_of_decl d = string_from_buffer (P.decl "") d
let string_of_type d = string_from_buffer P.typ d
let string_of_type_qual d = string_from_buffer P.type_qual d
let string_of_expr d = string_from_buffer P.expr d
let string_of_stmt d = string_from_buffer (P.stmt "") d
                                    
