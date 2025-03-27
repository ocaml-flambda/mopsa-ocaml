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
  Clang_to_C - Translates Clang AST to C AST and link C AST
 *)

open C_AST
open C_utils
open C_print
open C_simplify
open Mopsa_utils

module C =
  struct
    include Clang_AST
    include Clang_dump
    include Clang_utils
  end

(** {2 Debug} *)

let dump_decls = ref false
(* dump each C declarations found, for debugging *)

let log_rename = ref false
(* log when renaming (or assign a name to an anonymous) *)

let log_merge = ref false
(* log when merging declarations *)

let log_remove = ref false
(* log when removed (unused) declarations *)


(** {2 Config} *)

let dump_dir = ref "out"
(** Log destination directory. *)

let simplify = ref true
(* Whether to apply simplification *)

let fix_va_list = true
(* Clang 4 transforms va_list into struct __va_list_tag *; transform it back
   so that, after printing, Clang accepts the generated C code
   as it refuses to call __builtin_va_arg with anything other than va_list
 *)

let remove_unused_static = true
(* remove static functions that are not referenced in a translation unit
   to improve performance
 *)


(** {2 Conversion & linking} *)


type context = {
    ctx_name: string;
    mutable ctx_tu: translation_unit list;
    ctx_target: C.target_info;

    (* local to the current TU: *)
    (* maps from (TU-local) Clang uid to definition *)
    ctx_tu_enums: (C.uid,enum_type) Hashtbl.t;
    ctx_tu_records: (C.uid,record_type) Hashtbl.t;
    ctx_tu_typedefs: (C.uid,typedef) Hashtbl.t;
    ctx_tu_vars: (C.uid,variable) Hashtbl.t;
    ctx_tu_funcs: (C.uid,func) Hashtbl.t;
    ctx_tu_static_funcs: (string,func) Hashtbl.t;
    ctx_tu_static_vars: (string,variable) Hashtbl.t;

    (* shared among all TU *)
    mutable ctx_uid: uid; (* counter to generate new uid *)
    ctx_names: (string,string) Hashtbl.t; (* set to generate unique names *)

    (* maps for original (possibly empty) name to all definitions with that name *)
    ctx_enums: (string,enum_type) Hashtbl.t;
    ctx_enum_vals: (string,enum_value) Hashtbl.t;
    ctx_records: (string,record_type) Hashtbl.t;
    ctx_typedefs: (string,typedef) Hashtbl.t;
    ctx_vars: (string,variable) Hashtbl.t; (* globals (extern & static) *)
    ctx_funcs: (string,func) Hashtbl.t;
    mutable ctx_file_scope_asm: string RangeMap.t;

    ctx_simplify: C_simplify.context;

    mutable ctx_files : SetExt.StringSet.t; (** set of parsed files *)

    (* comments are stored in a map so that we can remove duplicates
       we attach the macros to each comment to apply preprocessing correctly later
     *)
    mutable ctx_comments: (comment * macro StringMap.t) list RangeMap.t;
    ctx_macros: (string,macro) Hashtbl.t;
  }
(** Structure used internally during project parsing & linking. *)



let create_context ?(min_uid=0) (project_name:string) (info:C.target_info) =
  { ctx_name = project_name;
    ctx_tu = [];
    ctx_target = info;
    ctx_tu_enums = Hashtbl.create 16;
    ctx_tu_records = Hashtbl.create 16;
    ctx_tu_typedefs = Hashtbl.create 16;
    ctx_tu_vars = Hashtbl.create 16;
    ctx_tu_funcs = Hashtbl.create 16;
    ctx_tu_static_vars = Hashtbl.create 16;
    ctx_tu_static_funcs = Hashtbl.create 16;
    ctx_uid = min_uid;
    ctx_enums = Hashtbl.create 16;
    ctx_enum_vals = Hashtbl.create 16;
    ctx_records = Hashtbl.create 16;
    ctx_typedefs = Hashtbl.create 16;
    ctx_vars = Hashtbl.create 16;
    ctx_funcs = Hashtbl.create 16;
    ctx_names = Hashtbl.create 16;
    ctx_simplify = C_simplify.create_context ~min_uid info;
    ctx_files = SetExt.StringSet.empty;
    ctx_comments = RangeMap.empty;
    ctx_macros = Hashtbl.create 16;
    ctx_file_scope_asm = RangeMap.empty;
  }

let new_uid ctx =
  let i = ctx.ctx_uid + 1 in
  ctx.ctx_uid <- i;
  i

let find_function name ctx =
  Hashtbl.find ctx.ctx_funcs name

let empty_scope () = {
    scope_var_added = [];
    scope_var_removed = [];
  }

let empty_block = {
    blk_stmts = [];
    blk_local_vars = [];
  }


let has_stub_comment l =
  List.exists
    (fun c ->
       let s = c.C.com_text in
       String.length s >= 3 && s.[2] = '$'
    ) l


let add_translation_unit (ctx:context) (tu_name:string) (decl:C.decl) (files: string list) (coms:comment list) (macros:C.macro list) (keep_static:bool) (forced_stub_list: string list) =
  (* utilities *)
  (* ********* *)

  let rec make_unique_name force hash org =
    (* choose a first name for anonymous *)
    let name = if org = "" then "anonymous" else org in
    (* loop to create a unique name *)
    let rec gen () =
      let n = Printf.sprintf "%s_%i" name (new_uid ctx) in
      if Hashtbl.mem ctx.ctx_names n then gen () else n
    in
    (* if the name exists in the hash, create a new unique one *)
    let unique = if org = "" || Hashtbl.mem hash org || force then gen () else name in
    Hashtbl.add ctx.ctx_names unique name;
    if !log_rename && unique <> org then Printf.printf "renamed '%s' into '%s'\n" org unique;
    unique
  in

  (* map from functions id (None denotes the toplevel)
     to static function referenced in the body
   *)
  let static_func_use = Hashtbl.create 16 in
  let get_func_ref id =
    try Hashtbl.find static_func_use id
    with Not_found -> UidSet.empty
  in
  let add_func_ref (caller:func option) (callee:func) =
    if callee.func_is_static then
      let id = match caller with Some f -> Some f.func_uid | None -> None in
      let old = get_func_ref id in
      Hashtbl.replace static_func_use id (UidSet.add callee.func_uid old)
  in


  (* init *)
  (* **** *)

  Hashtbl.clear ctx.ctx_tu_enums;
  Hashtbl.clear ctx.ctx_tu_records;
  Hashtbl.clear ctx.ctx_tu_typedefs;
  Hashtbl.clear ctx.ctx_tu_vars;
  Hashtbl.clear ctx.ctx_tu_funcs;
  Hashtbl.clear ctx.ctx_tu_static_vars;
  Hashtbl.clear ctx.ctx_tu_static_funcs;

  let tu = {
      tu_uid = new_uid ctx;
      tu_name;
      tu_range = decl.C.decl_range;
    }
  in

  (* converts macros to string map *)
  let macros_map =
    List.fold_left
      (fun acc m -> StringMap.add m.C.macro_name m acc)
      StringMap.empty
      macros
  in

  let out =
    if !dump_decls then
      Some (open_out (Filename.concat !dump_dir (Filename.basename tu_name)))
    else None
  in
  let debug f x =
    match out with
    | Some out -> output_string out (f x)
    | None -> ()
  in


  (* translate types *)
  (* *************** *)


  let fix_va_list (t,q) = match t with
    | T_pointer (T_record { record_org_name = "__va_list_tag" },_)
         when fix_va_list && Hashtbl.mem ctx.ctx_typedefs "va_list"
      ->
       T_typedef (Hashtbl.find ctx.ctx_typedefs "va_list"), q
    | _ -> t,q
  in

  let rec type_qual range ((t,q):C.type_qual) : type_qual  =
    let tt,qq = typ range t in
    tt, merge_qualifiers qq { qual_is_const = q.C.qual_is_const; }

  and typ range (t:C.typ) : type_qual =
    match t with

    | C.DecayedType (tq,_) -> type_qual range tq

    | C.ArrayType a ->
       (*
       if a.C.array_size_modifier <> C.Size_Normal
       then error None "unhandled array size modifer" (C.string_of_type t);
        *)
       let len = match a.C.array_size with
         | C.Size_Constant cst -> Length_cst cst
         | C.Size_Variable (Some e) -> Length_expr (expr None e)
         | C.Size_Variable None -> No_length
         | C.Size_Incomplete -> No_length
         | _ -> error range "unhandled array size" (C.string_of_type t)
       in
       T_array (type_qual range a.C.array_element_type, len), no_qual

    | C.AtomicType a -> type_qual range a

    | C.AttributedType a -> type_qual range a

    | C.BuiltinType b ->
       (match b with
        | C.Type_Void -> T_void, no_qual
        | C.Type_Bool -> T_bool, no_qual
        | C.Type_Char_U -> T_integer (Char UNSIGNED), no_qual
        | C.Type_UChar -> T_integer UNSIGNED_CHAR, no_qual
        | C.Type_UShort -> T_integer UNSIGNED_SHORT, no_qual
        | C.Type_UInt -> T_integer UNSIGNED_INT, no_qual
        | C.Type_ULong -> T_integer UNSIGNED_LONG, no_qual
        | C.Type_ULongLong -> T_integer UNSIGNED_LONG_LONG, no_qual
        | C.Type_UInt128 -> T_integer UNSIGNED_INT128, no_qual
        | C.Type_Char_S -> T_integer (Char SIGNED), no_qual
        | C.Type_SChar -> T_integer SIGNED_CHAR, no_qual
        | C.Type_Short -> T_integer SIGNED_SHORT, no_qual
        | C.Type_Int -> T_integer SIGNED_INT, no_qual
        | C.Type_Int128 -> T_integer SIGNED_INT128, no_qual
        | C.Type_Long -> T_integer SIGNED_LONG, no_qual
        | C.Type_LongLong -> T_integer SIGNED_LONG_LONG, no_qual
        | C.Type_Float -> T_float FLOAT, no_qual
        | C.Type_Double -> T_float DOUBLE, no_qual
        | C.Type_LongDouble -> T_float LONG_DOUBLE, no_qual
        | C.Type_Float128 -> T_float FLOAT128, no_qual
        | C.Type_BuiltinFn -> T_builtin_fn, no_qual
        | C.Type_unknown_builtin s -> T_unknown_builtin s, no_qual
        | _ -> error range "unhandled builtin type" (C.string_of_type t)
       )

    | C.ComplexType (tt,qq) ->
       (match type_qual range (tt,qq) with
        | T_float f, q -> T_complex f, q
        | _ -> error range "unhandled complex type" (C.string_of_type t)
       )

    | C.FunctionProtoType f ->
       let ftype_return = fix_va_list (type_qual range f.C.proto_return_type)
       and ftype_params = List.map (fun x -> fix_va_list (type_qual range x)) (Array.to_list f.C.proto_params)
       and ftype_variadic = f.C.proto_variadic in
       T_function (Some { ftype_return; ftype_params; ftype_variadic; }), no_qual

    | C.FunctionNoProtoType f -> T_function None, no_qual
    | C.ParenType tq -> type_qual range tq
    | C.PointerType tq -> T_pointer (type_qual range tq), no_qual
    | C.EnumType e -> T_enum (enum_decl e), no_qual
    | C.RecordType r -> T_record (record_decl r), no_qual
    | C.TypedefType t -> T_typedef (typedef_decl t), no_qual
    | C.ElaboratedType t -> type_qual range t
    | C.UnaryTransformType t -> type_qual range t.C.unary_underlying_type
    | C.TypeOfExprType { C.expr_type = Some t; } -> type_qual range t
    | C.TypeOfType t -> type_qual range t
    | C.VectorType (t,s,k) ->
       T_vector { vector_type = type_qual range t; vector_size = s; vector_kind = k; },
       no_qual
    | _ -> error range "unhandled type" (C.string_of_type t)


  (* translate type declarations *)
  (* *************************** *)

  and fmt_range fmt r = Format.fprintf fmt "%s--%s" (Clang_dump.string_of_loc r.Clang_AST.range_begin) (Clang_dump.string_of_loc r.Clang_AST.range_end)


  (* enums *)

  and enum_decl e =
    if Hashtbl.mem ctx.ctx_tu_enums e.C.enum_uid then
      (* already in translation unit *)
      Hashtbl.find ctx.ctx_tu_enums e.C.enum_uid
    else
      (* name *)
      let org_name = e.C.enum_name.C.name_print in
      let nice_name =
        if org_name <> "" then org_name else
          match e.C.enum_typedef with
          | Some t -> t.C.typedef_name.C.name_print
          | None -> ""
      in
      let range = e.C.enum_range in
      (* integer type *)
      let itype = match e.C.enum_integer_type with
      | None -> None
      | Some tq ->
        Some (
          match type_qual range tq with
          | T_integer i, _ -> i
          | _ -> SIGNED_INT
        )
      in
      (* create record *)
      let enum =
        { enum_uid = new_uid ctx;
          enum_org_name = org_name;
          enum_unique_name = make_unique_name false ctx.ctx_enums nice_name;
          enum_range = range;
          enum_values = [];
          enum_integer_type = itype;
          enum_defined = e.C.enum_cst <> [];
          enum_com = e.C.enum_com;
        }
      in
      (* fill in enumerator constants *)
      enum.enum_values <-
        List.map
          (fun e ->
            let org_name = e.C.enum_cst_name.C.name_print in
            let v =
              { enum_val_uid = new_uid ctx;
                enum_val_org_name = org_name;
                enum_val_unique_name = make_unique_name false ctx.ctx_enum_vals org_name;
                enum_val_value = e.C.enum_cst_val;
                enum_val_enum = enum;
                enum_val_range = e.C.enum_cst_range;
                enum_val_com = e.C.enum_cst_com;
              }
            in
            Hashtbl.add ctx.ctx_enum_vals org_name v;
            v
          ) e.C.enum_cst;
      (* sort them by value, and then by name if the values are equal *)
      enum.enum_values <-
        List.sort
          (fun v1 v2 ->
            let c = Z.compare v1.enum_val_value v2.enum_val_value in
            if c <> 0 then c
            else compare v1.enum_val_org_name v2.enum_val_org_name
          )
          enum.enum_values;
      (* attempt to merge with previous declaration *)
      let rec merge = function
        | [] ->
           (* new def *)
           Hashtbl.add ctx.ctx_enums org_name enum;
           if !log_merge then Printf.printf "no prior enum declaration found for '%s' ('%s')\n" org_name enum.enum_unique_name;
           enum
        | a::rest ->
           if type_unifiable ctx.ctx_target (T_enum enum) (T_enum a) then (
             (* found compatible type *)
             let () =
               if !log_merge then
                 Format.printf "found enum declaration for '%s' (%s@%a and %s@%a)@." org_name enum.enum_unique_name fmt_range enum.enum_range a.enum_unique_name fmt_range enum.enum_range
               else ()
             in
             enum_unify ctx.ctx_target a enum
           )
           else merge rest
      in
      let enum = merge (if org_name = "" then
                          (Hashtbl.fold (fun _ r acc ->
                               if Stdlib.compare r.enum_range enum.enum_range = 0 then r::acc else acc) ctx.ctx_enums [])
                        else Hashtbl.find_all ctx.ctx_enums org_name) in
      if nice_name <> "" then Hashtbl.add ctx.ctx_enums nice_name enum;
      Hashtbl.add ctx.ctx_tu_enums e.C.enum_uid enum;
      enum


  (* struct, unions *)

  and record_decl e =
    if Hashtbl.mem ctx.ctx_tu_records e.C.record_uid then
      (* already in translation unit *)
      Hashtbl.find ctx.ctx_tu_records e.C.record_uid
    else
      (* name *)
      let org_name = e.C.record_name.C.name_print in
      let nice_name =
        if org_name <> "" then org_name else
          match e.C.record_typedef with
          | Some t -> t.C.typedef_name.C.name_print
          | None -> ""
      in
      let range = e.C.record_range in
      (* create (empty) record *)
      let kind = match e.C.record_kind with
        | C.Record_Struct -> STRUCT
        | C.Record_Union -> UNION
        | t -> error range "unhandled record kind" (C.record_kind_name t)
      in
      let record =
        { record_uid = new_uid ctx;
          record_org_name = org_name;
          record_unique_name = make_unique_name false ctx.ctx_records nice_name;
          record_range = range;
          record_sizeof = Z.of_int64 e.C.record_size;
          record_alignof = Z.of_int64 e.C.record_alignment;
          record_fields = [||];
          record_kind = kind;
          record_defined = false;
          record_com = e.C.record_com;
        }
      in
      (* add empty record to context to handle recursive types *)
      Hashtbl.add ctx.ctx_tu_records e.C.record_uid record;
      (* fill in record fields *)
      record.record_fields <-
        Array.mapi
          (fun i f ->
            let org_name = f.C.field_name.C.name_print in
            let name =
              if org_name = "" then make_unique_name false ctx.ctx_names ""
              else org_name
            in
            let typ = type_qual f.C.field_range f.C.field_type in
            let typ = match f.C.field_bitwidth with
              | Some b -> T_bitfield (fst typ,b), snd typ
              | None -> typ
            in
            { field_uid = new_uid ctx;
              field_org_name = org_name;
              field_name = name;
              field_index = i;
              field_offset = Int64.to_int f.C.field_offset / 8;
              field_bit_offset = Int64.to_int f.C.field_offset mod 8;
              field_record = record;
              field_range = f.C.field_range;
              field_type = typ;
              field_com = f.C.field_com;
            }
          ) (Array.of_list e.C.record_fields);
      record.record_defined <- record.record_fields <> [||];
      (* attempt to merge with previous declaration *)
      let rec merge = function
        | [] ->
           (* new def *)
           if !log_merge then
             (Printf.printf "no prior record declaration found for '%s' ('%s')\n" org_name record.record_unique_name;
              Printf.printf "this decl: %s\n" (string_of_record_decl record);
              List.iter (fun r -> Printf.printf "candidate: %s\n" (string_of_record_decl r)) (Hashtbl.find_all ctx.ctx_records org_name)
             );
           Hashtbl.add ctx.ctx_records org_name record;
           record
        | a::rest ->
           if type_unifiable ctx.ctx_target (T_record record) (T_record a) then (
             (* found compatible type *)
             if !log_merge then Printf.printf "found prior record declaration for '%s' (%s and %s)\n" org_name record.record_unique_name a.record_unique_name;
             record_unify ctx.ctx_target a record
           )
           else merge rest
      in
      let record =
        if org_name = "" then (
          merge (Hashtbl.fold (fun _ r acc ->
              if Stdlib.compare r.record_range record.record_range = 0 then r::acc else acc) ctx.ctx_records [])
        )
        else merge (Hashtbl.find_all ctx.ctx_records org_name)
      in
      if nice_name <> "" then Hashtbl.add ctx.ctx_records nice_name record;
      Hashtbl.replace ctx.ctx_tu_records e.C.record_uid record;
      record


  (* typedef *)

  and typedef_decl t =
    if Hashtbl.mem ctx.ctx_tu_typedefs t.C.typedef_uid then
      (* already in translation unit *)
      Hashtbl.find ctx.ctx_tu_typedefs t.C.typedef_uid
    else
      let org_name = t.C.typedef_name.C.name_print in
      let range = t.C.typedef_range in
      let def = {
          typedef_uid = new_uid ctx;
          typedef_org_name = org_name;
          typedef_unique_name = make_unique_name false ctx.ctx_typedefs org_name;
          typedef_def = (T_void, no_qual);
          typedef_range = range;
          typedef_com = t.C.typedef_com;
        }
      in
      (* define *)
      Hashtbl.add ctx.ctx_tu_typedefs t.C.typedef_uid def;
      def.typedef_def <- type_qual range t.C.typedef_underlying_type;
      (* attempt to merge with previous declaration *)
      let rec merge = function
        | [] ->
           (* new def *)
           if !log_merge then Printf.printf "no prior typedef found for '%s' ('%s')\n" org_name def.typedef_unique_name;
           Hashtbl.add ctx.ctx_typedefs org_name def;
           def
        | a::rest ->
           if type_unifiable ctx.ctx_target (T_typedef def) (T_typedef a) then (
             (* found compatible type *)
             if !log_merge then Printf.printf "found prior typedef declaration for '%s' (%s and %s)\n" org_name def.typedef_unique_name a.typedef_unique_name;
             typedef_unify ctx.ctx_target a def
           )
           else merge rest
      in
      let def = merge (Hashtbl.find_all ctx.ctx_typedefs org_name) in
      Hashtbl.replace ctx.ctx_tu_typedefs t.C.typedef_uid def;
      def


  (* translate variable declarations *)
  (* ******************************* *)

  and var_decl (func:func option) v =
    if Hashtbl.mem ctx.ctx_tu_vars v.C.var_uid then
      (* already in translation unit *)
      Hashtbl.find ctx.ctx_tu_vars v.C.var_uid
    else
      let org_name = v.C.var_name.C.name_print in
      let range = v.C.var_range in
      let typ = type_qual range v.C.var_type in
      let kind = match v.C.var_storage_class, func with
        | C.SC_Extern, _ -> Variable_extern
        | C.SC_Static, Some f -> Variable_func_static f
        | C.SC_Static, None -> Variable_file_static tu
        | C.SC_PrivateExtern, _ -> Variable_file_static tu
        | _, Some f when v.C.var_is_local -> Variable_local f
        | _ -> Variable_global
      in
      (* get previous definition *)
      let rec find_extern = function
        | [] -> None
        | v::r ->
           if v.var_kind = Variable_global || v.var_kind = Variable_extern
           then Some v else find_extern r
      in
      let prev =
        if not (variable_is_global kind) then None
        else if variable_is_file_static kind then Hashtbl.find_opt ctx.ctx_tu_static_vars org_name
        else find_extern (Hashtbl.find_all ctx.ctx_vars org_name)
      in
      (* merge types *)
      let c = ref v.C.var_com in
      (match prev with
       | None ->
          if !log_merge then Printf.printf "no previous declaration found for variable %s\n" org_name
       | Some prev ->
          if !log_merge then Printf.printf "found previous declaration for variable %s (%s)\n" org_name prev.var_unique_name;
          try
            c := comment_unify !c prev.var_com;
            let t = type_unify_qual ctx.ctx_target prev.var_type typ in
            prev.var_type <- t;
            prev.var_com <- !c
          with Invalid_argument msg ->
            warning range "incompatible variable types" (Format.asprintf "variable %s (ranges %a %a) type1 %s, type2 %s, %s" org_name fmt_range range fmt_range prev.var_range (string_of_type_qual prev.var_type) (string_of_type_qual typ) msg)
      );
      (* make variable *)
      let var = match prev with
        | Some var -> var
        | None -> {
            var_uid = new_uid ctx;
            var_org_name = org_name;
            var_unique_name =
              if variable_is_global kind
              then make_unique_name (variable_is_static kind) ctx.ctx_vars org_name
              else org_name;
            var_type = typ;
            var_kind = kind;
            var_init = None;
            var_range = range;
            var_com = !c;
            var_before_stmts = [];
            var_after_stmts = [];
          }
      in
      if variable_is_global kind && prev = None then Hashtbl.add ctx.ctx_vars org_name var;
      Hashtbl.add ctx.ctx_tu_vars v.C.var_uid var;
      if variable_is_file_static kind then Hashtbl.add ctx.ctx_tu_static_vars org_name var;
      (* init *)
      if var.var_kind = Variable_extern && (kind = Variable_global || var.var_init <> None)
      then var.var_kind <- Variable_global;
      (match v.C.var_init with
       | None -> ()
       | Some i ->
          if var.var_init <> None
          then warning range "variable is defined twice with initializers" org_name;
          let init = init func i in
          if variable_is_global kind && !simplify then
            let before, init, after = simplify_global_init ctx.ctx_simplify init in
            var.var_init <- Some init;
            var.var_before_stmts <- before;
            var.var_after_stmts <- after
          else
            var.var_init <- Some init;
          var.var_range <- range
        );
      var


  (* translate function declarations *)
  (* ******************************* *)

  and func_decl f =
    if Hashtbl.mem ctx.ctx_tu_funcs f.C.function_uid then
      (* already in translation unit *)
      Hashtbl.find ctx.ctx_tu_funcs f.C.function_uid
    else
      let org_name = f.C.function_name.C.name_print in
      let static = match f.C.function_storage_class with
        | C.SC_Static | C.SC_PrivateExtern -> true
        | _ -> false
      in
      let range = f.C.function_range in
      let name_range = f.C.function_name_range in
      (* get previous definition *)
      let rec find_extern = function
        | [] -> None
        | f::r -> if f.func_is_static then find_extern r else Some f
      in
      let prev =
        if static then Hashtbl.find_opt ctx.ctx_tu_static_funcs org_name
        else find_extern (Hashtbl.find_all ctx.ctx_funcs org_name)
      in
      (* create structure *)
      let return = type_qual range f.C.function_return_type in
      let func =
        match prev with
        | Some func ->
           if !log_merge then Printf.printf "found previous declaration for function %s (%s)\n" org_name func.func_unique_name;
           func
        | None ->
           if !log_merge then Printf.printf "no previous declaration found for function %s\n" org_name;
           {
             func_uid = new_uid ctx;
             func_org_name = org_name;
             func_unique_name = make_unique_name static ctx.ctx_funcs org_name;
             func_is_static = static;
             func_return = return;
             func_parameters = [||];
             func_body = None;
             func_static_vars = [];
             func_local_vars = [];
             func_range = range;
             func_name_range = name_range;
             func_variadic = f.C.function_is_variadic;
             func_com = [];
           }
      in
      if static then Hashtbl.replace ctx.ctx_tu_static_funcs org_name func
      else if prev = None then Hashtbl.add ctx.ctx_funcs org_name func;
      Hashtbl.add ctx.ctx_tu_funcs f.C.function_uid func;
      (* fill in parameters & return *)
      let params =
        Array.map
          (fun p ->
            let v_org_name = p.C.var_name.C.name_print in
            let typ = fix_va_list (type_qual p.C.var_range p.C.var_type) in
            let var = {
                var_uid = new_uid ctx;
                var_org_name = v_org_name;
                var_unique_name = v_org_name;
                var_type = typ;
                var_kind = Variable_parameter func;
                var_init = None;
                var_range = p.C.var_range;
                var_com = p.C.var_com;
                var_before_stmts = [];
                var_after_stmts = [];
              }
            in
            Hashtbl.add ctx.ctx_tu_vars p.C.var_uid var;
            var
          ) f.C.function_params
      in
      (* check compatibility and merge declarations *)
      if params <> [||] && func.func_parameters <> [||] then (
        if Array.length params <> Array.length func.func_parameters
        then error range "multiple declaration of a function with different number of arguments" org_name;
        for i=0 to Array.length params-1 do
          try
            let t = type_unify_qual ctx.ctx_target params.(i).var_type func.func_parameters.(i).var_type in
            params.(i).var_type <- t;
            func.func_parameters.(i).var_type <- t;
            let c = comment_unify params.(i).var_com func.func_parameters.(i).var_com in
            params.(i).var_com <- c;
            func.func_parameters.(i).var_com <- c;
          with Invalid_argument msg ->
            warning range "multiple declaration of a function have incompatible argument type" msg
        done
      );
      (try
         let t = type_unify_qual ctx.ctx_target return func.func_return in
         func.func_return <- t
       with Invalid_argument msg ->
         warning range "multiple declaration of a function have incompatible return type" msg
      );
      (* favor argument names from functions with a body, a
         non-empty argument list, or from stubs
      *)
      if
        ((func.func_parameters = [||] &&  params <> [||]) ||
         has_stub_comment f.C.function_com)
        &&
        ((match func.func_body with
         | None -> true
         | Some b ->
           List.length b.blk_stmts = 0) || (List.mem func.func_org_name forced_stub_list) )
      then
      (
        func.func_parameters <- params;
        func.func_variadic <- f.C.function_is_variadic
      );

      let coms = List.map (fun m -> m, macros_map) f.C.function_com in
      func.func_com <- comment_macro_unify func.func_com coms;
      (* fill in body, override parameters in that case (to avoid inconsistencies) *)
      if func.func_body <> None && f.C.function_body <> None
      then warning range "function is defined twice with a body" org_name;
      (match f.C.function_body with
       | None -> ()
       | Some b ->
         func.func_parameters <- params;
         func.func_variadic <- f.C.function_is_variadic;
             func.func_body <-
            Some (stmt (Some func) b |> deblock |> resolve_scope);
          func.func_range <- range;
          func.func_name_range <- name_range
      );
      if !simplify then simplify_func ctx.ctx_simplify func;
      func


  (* translate statements *)
  (* ******************** *)

  and stmt (func:func option) (s:C.stmt) : statement list =
    let range = s.C.stmt_range in
    match s.C.stmt_kind with

    | C.AttributedStmt s -> stmt func s

    | C.CompoundStmt l -> [S_block (deblock (ListExt.map_merge (stmt func) l)), range]

    | C.NullStmt -> []

    | C.BreakStmt _ -> [S_jump (S_break (empty_scope())), range]

    | C.ContinueStmt _ -> [S_jump (S_continue (empty_scope())), range]

    | C.GotoStmt (lbl,_) -> [S_jump (S_goto (lbl.C.name_print, empty_scope())), range]

    | C.ReturnStmt (Some e) ->  [S_jump (S_return (Some (expr func e), empty_scope())), range]

    | C.ReturnStmt None -> [S_jump (S_return (None, empty_scope())), range]

    | C.SwitchStmt s ->
      if s.C.switch_init <> None
       then error range "unsupported init in switch statement" "";
       let c = expr func s.C.switch_cond
       and b = deblock (stmt func s.C.switch_body)
       in
       [S_jump (S_switch (c,b)), range]

    | C.CaseStmt s ->
      (* TODO: constant folding? *)
      begin match s.C.case_end with
        | Some case_end ->
          let values = match s.C.case_value.expr_kind, case_end.expr_kind with
            | ConstantExpr {expr_kind = IntegerLiteral b},
              ConstantExpr {expr_kind = IntegerLiteral e} ->
              let rec process i acc =
                if Z.(i <= e) then
                  process Z.(i + one) ({case_end with expr_kind = IntegerLiteral i}::acc)
                else
                  List.rev acc
              in
              process b []
            | _ ->
              error range "range case statement extension currently supports constant integers" (Format.asprintf "%s" (Clang_dump.string_of_expr (OptionExt.none_to_exn s.C.case_end)));
          in
          (S_target (S_case (List.map (expr func) values, empty_scope())), range)::
          (stmt func s.C.case_stmt)

        | None ->
       (* in case of nested case stmts, we extract all of them in other_values, and keep the leaf statements *)
       let process s =
         let rec aux acc s = match s.C.stmt_kind with
           | C.CaseStmt s -> aux (s.C.case_value :: acc) s.C.case_stmt
           | _ -> List.rev acc, s in
         aux [] s in
       let other_values, statements = process s.C.case_stmt in
       let values = s.C.case_value :: other_values in
       (S_target (S_case (List.map (expr func) values, empty_scope())), range)::
         (stmt func statements)
      end

    | C.DefaultStmt s ->
       (S_target (S_default (empty_scope())), range)::(stmt func s)

    | C.LabelStmt (lbl,s) ->
       (S_target (S_label lbl.C.name_print), range)::(stmt func s)

    | C.ExprStmt e -> [S_expression (expr func e), range]

    | C.DeclStmt decls ->
       ListExt.map_merge
         (fun d ->
           let range = d.C.decl_range in
           match d.C.decl_kind with
           | C.EmptyDecl -> []
           | C.EnumDecl e -> ignore (enum_decl e); []
           | C.RecordDecl r -> ignore (record_decl r); []
           | C.TypedefDecl d -> ignore (typedef_decl d); []
           | C.FunctionDecl f -> ignore (func_decl f); []
           | C.VarDecl v ->
              let var = var_decl func v in
              (match var.var_kind with
               | Variable_func_static f -> f.func_static_vars <- var::f.func_static_vars
               | Variable_local f -> f.func_local_vars <- var::f.func_local_vars
               | _ -> ()
              );
              if variable_is_global var.var_kind then []
              else [S_local_declaration var, range]
           | C.StaticAssertDecl a ->
              if a.C.assert_is_failed then
                warning decl.C.decl_range "static assertion failed: %s" a.C.assert_msg;
              []
           | _ -> error range "unhandled declaration in function" (C.decl_kind_name d.C.decl_kind)
         )
         decls

    | C.IfStmt s ->
       if s.C.if_init <> None
       then error range "unsupported init in if statement" "";
       let c = match s.C.if_cond with
         | None -> error range "if without a condition" ""
         | Some c -> expr func c
       and t = match s.C.if_then with
         | None -> empty_block
         | Some s -> deblock (stmt func s)
       and e = match s.C.if_else with
         | None -> empty_block
         | Some s -> deblock (stmt func s)
       in
       [S_if (c,t,e), range]

    | C.WhileStmt s ->
       let c = expr func s.C.while_cond
       and b = deblock (stmt func s.C.while_body) in
       [S_while (c,b), range]

    | C.DoStmt s ->
       let c = expr func s.C.do_cond
       and b = deblock (stmt func s.C.do_body) in
       [S_do_while (b,c), range]

    | C.ForStmt s ->
       let i = match s.C.for_init with
         | None -> empty_block
         | Some s -> deblock (stmt func s)
       and c = match s.C.for_cond with
         | None -> None
         | Some c -> Some (expr func c)
       and p = match s.C.for_inc with
         | None -> None
         | Some s -> Some (expr func s)
       and b = deblock (stmt func s.C.for_body)
       in
       [S_for (i,c,p,b), range]

    | C.AsmStmt s ->
       let a = {
           asm_style = s.C.asm_style;
           asm_is_simple = s.C.asm_is_simple;
           asm_is_volatile = s.C.asm_is_volatile;
           asm_body = s.C.asm_body;
           asm_outputs =
             Array.map
               (fun o -> {
                    asm_output_string = o.C.asm_output_string;
                    asm_output_expr = expr func o.C.asm_output_expr;
                    asm_output_constraint = o.C.asm_output_constraint;
                  }
               ) s.C.asm_outputs;
           asm_inputs =
             Array.map
               (fun o -> {
                    asm_input_string = o.C.asm_input_string;
                    asm_input_expr = expr func o.C.asm_input_expr;
                  }
               ) s.C.asm_inputs;
           asm_clobbers = s.C.asm_clobbers;
           asm_labels = s.C.asm_labels;
         }
       in
       [S_asm a, range]

    | s -> error range "unhandled statement" (C.stmt_kind_name s)

  (* remove useless levels of blocks *)
  and deblock (l:statement list) : block = match l with
    | [S_block b,_] -> deblock b.blk_stmts
    | _ -> make_block l


  (* translate expressions *)
  (* ********************* *)

  and check_type range t1 t2 =
    if not (type_qual_compatible ctx.ctx_target t1 t2) then
      error range "incompatible types" (Printf.sprintf "%s and %s" (string_of_type_qual t1) (string_of_type_qual t2))


  and atomic_op = function
   (* C11 _Atomic operations for <stdatomic.h>. *)
    | C.AO__c11_atomic_init -> AO__c11_atomic_init
    | C.AO__c11_atomic_load -> AO__c11_atomic_load
    | C.AO__c11_atomic_store -> AO__c11_atomic_store
    | C.AO__c11_atomic_exchange -> AO__c11_atomic_exchange
    | C.AO__c11_atomic_compare_exchange_strong -> AO__c11_atomic_compare_exchange_strong
    | C.AO__c11_atomic_compare_exchange_weak -> AO__c11_atomic_compare_exchange_weak
    | C.AO__c11_atomic_fetch_add -> AO__c11_atomic_fetch_add
    | C.AO__c11_atomic_fetch_sub -> AO__c11_atomic_fetch_sub
    | C.AO__c11_atomic_fetch_and -> AO__c11_atomic_fetch_and
    | C.AO__c11_atomic_fetch_or -> AO__c11_atomic_fetch_or
    | C.AO__c11_atomic_fetch_xor -> AO__c11_atomic_fetch_xor
    | C.AO__c11_atomic_fetch_nand -> AO__c11_atomic_fetch_nand
    | C.AO__c11_atomic_fetch_max -> AO__c11_atomic_fetch_max
    | C.AO__c11_atomic_fetch_min -> AO__c11_atomic_fetch_min

    (* GNU atomic builtins. *)
    | C.AO__atomic_load -> AO__atomic_load
    | C.AO__atomic_load_n -> AO__atomic_load_n
    | C.AO__atomic_store -> AO__atomic_store
    | C.AO__atomic_store_n -> AO__atomic_store_n
    | C.AO__atomic_exchange -> AO__atomic_exchange
    | C.AO__atomic_exchange_n -> AO__atomic_exchange_n
    | C.AO__atomic_compare_exchange -> AO__atomic_compare_exchange
    | C.AO__atomic_compare_exchange_n -> AO__atomic_compare_exchange_n
    | C.AO__atomic_fetch_add -> AO__atomic_fetch_add
    | C.AO__atomic_fetch_sub -> AO__atomic_fetch_sub
    | C.AO__atomic_fetch_and -> AO__atomic_fetch_and
    | C.AO__atomic_fetch_or -> AO__atomic_fetch_or
    | C.AO__atomic_fetch_xor -> AO__atomic_fetch_xor
    | C.AO__atomic_fetch_nand -> AO__atomic_fetch_nand
    | C.AO__atomic_add_fetch -> AO__atomic_add_fetch
    | C.AO__atomic_sub_fetch -> AO__atomic_sub_fetch
    | C.AO__atomic_and_fetch -> AO__atomic_and_fetch
    | C.AO__atomic_or_fetch -> AO__atomic_or_fetch
    | C.AO__atomic_xor_fetch -> AO__atomic_xor_fetch
    | C.AO__atomic_max_fetch -> AO__atomic_max_fetch
    | C.AO__atomic_min_fetch -> AO__atomic_min_fetch
    | C.AO__atomic_nand_fetch -> AO__atomic_nand_fetch
    | C.AO__atomic_test_and_set -> AO__atomic_test_and_set
    | C.AO__atomic_clear -> AO__atomic_clear

    (* GNU atomic builtins with atomic scopes. *)
    | C.AO__scoped_atomic_load -> AO__scoped_atomic_load
    | C.AO__scoped_atomic_load_n -> AO__scoped_atomic_load_n
    | C.AO__scoped_atomic_store -> AO__scoped_atomic_store
    | C.AO__scoped_atomic_store_n -> AO__scoped_atomic_store_n
    | C.AO__scoped_atomic_exchange -> AO__scoped_atomic_exchange
    | C.AO__scoped_atomic_exchange_n -> AO__scoped_atomic_exchange_n
    | C.AO__scoped_atomic_compare_exchange -> AO__scoped_atomic_compare_exchange
    | C.AO__scoped_atomic_compare_exchange_n -> AO__scoped_atomic_compare_exchange_n
    | C.AO__scoped_atomic_fetch_add -> AO__scoped_atomic_fetch_add
    | C.AO__scoped_atomic_fetch_sub -> AO__scoped_atomic_fetch_sub
    | C.AO__scoped_atomic_fetch_and -> AO__scoped_atomic_fetch_and
    | C.AO__scoped_atomic_fetch_or -> AO__scoped_atomic_fetch_or
    | C.AO__scoped_atomic_fetch_xor -> AO__scoped_atomic_fetch_xor
    | C.AO__scoped_atomic_fetch_nand -> AO__scoped_atomic_fetch_nand
    | C.AO__scoped_atomic_fetch_min -> AO__scoped_atomic_fetch_min
    | C.AO__scoped_atomic_fetch_max -> AO__scoped_atomic_fetch_max
    | C.AO__scoped_atomic_add_fetch -> AO__scoped_atomic_add_fetch
    | C.AO__scoped_atomic_sub_fetch -> AO__scoped_atomic_sub_fetch
    | C.AO__scoped_atomic_and_fetch -> AO__scoped_atomic_and_fetch
    | C.AO__scoped_atomic_or_fetch -> AO__scoped_atomic_or_fetch
    | C.AO__scoped_atomic_xor_fetch -> AO__scoped_atomic_xor_fetch
    | C.AO__scoped_atomic_nand_fetch -> AO__scoped_atomic_nand_fetch
    | C.AO__scoped_atomic_min_fetch -> AO__scoped_atomic_min_fetch
    | C.AO__scoped_atomic_max_fetch -> AO__scoped_atomic_max_fetch

    (* OpenCL 2.0 atomic builtins. *)
    | C.AO__opencl_atomic_init -> AO__opencl_atomic_init
    | C.AO__opencl_atomic_load -> AO__opencl_atomic_load
    | C.AO__opencl_atomic_store -> AO__opencl_atomic_store
    | C.AO__opencl_atomic_compare_exchange_weak -> AO__opencl_atomic_compare_exchange_weak
    | C.AO__opencl_atomic_compare_exchange_strong -> AO__opencl_atomic_compare_exchange_strong
    | C.AO__opencl_atomic_exchange -> AO__opencl_atomic_exchange
    | C.AO__opencl_atomic_fetch_add -> AO__opencl_atomic_fetch_add
    | C.AO__opencl_atomic_fetch_sub -> AO__opencl_atomic_fetch_sub
    | C.AO__opencl_atomic_fetch_and -> AO__opencl_atomic_fetch_and
    | C.AO__opencl_atomic_fetch_or -> AO__opencl_atomic_fetch_or
    | C.AO__opencl_atomic_fetch_xor -> AO__opencl_atomic_fetch_xor
    | C.AO__opencl_atomic_fetch_min -> AO__opencl_atomic_fetch_min
    | C.AO__opencl_atomic_fetch_max -> AO__opencl_atomic_fetch_max

    (* GCC does not support these they are a Clang extension. *)
    | C.AO__atomic_fetch_max -> AO__atomic_fetch_max
    | C.AO__atomic_fetch_min -> AO__atomic_fetch_min

    (* HIP atomic builtins. *)
    | C.AO__hip_atomic_load -> AO__hip_atomic_load
    | C.AO__hip_atomic_store -> AO__hip_atomic_store
    | C.AO__hip_atomic_compare_exchange_weak -> AO__hip_atomic_compare_exchange_weak
    | C.AO__hip_atomic_compare_exchange_strong -> AO__hip_atomic_compare_exchange_strong
    | C.AO__hip_atomic_exchange -> AO__hip_atomic_exchange
    | C.AO__hip_atomic_fetch_add -> AO__hip_atomic_fetch_add
    | C.AO__hip_atomic_fetch_sub -> AO__hip_atomic_fetch_sub
    | C.AO__hip_atomic_fetch_and -> AO__hip_atomic_fetch_and
    | C.AO__hip_atomic_fetch_or -> AO__hip_atomic_fetch_or
    | C.AO__hip_atomic_fetch_xor -> AO__hip_atomic_fetch_xor
    | C.AO__hip_atomic_fetch_min -> AO__hip_atomic_fetch_min
    | C.AO__hip_atomic_fetch_max -> AO__hip_atomic_fetch_max

  and expr (func:func option) e =
    let range = e.C.expr_range in
    let typ = match e.C.expr_type with
      | None -> error range "expression without type" (C.expr_kind_name e.C.expr_kind)
      | Some t -> type_qual range t
    in
    match e.C.expr_kind with

    | C.ConditionalOperator c ->
       E_conditional (expr func c.C.cond_cond, expr func c.C.cond_true, expr func c.C.cond_false),
       typ, range

    | C.BinaryConditionalOperator c ->
       E_binary_conditional (expr func c.C.bcond_cond, expr func c.C.bcond_false),
       typ, range

    | C.ArraySubscriptExpr e ->
       E_array_subscript (expr func e.C.subscript_base, expr func e.C.subscript_index),
       typ, range

    | C.CompoundAssignOperator c ->
       let op = match c.C.compound_op with
         | C.BO_MulAssign -> MUL
         | C.BO_DivAssign -> DIV
         | C.BO_RemAssign -> MOD
         | C.BO_AddAssign -> ADD
         | C.BO_SubAssign -> SUB
         | C.BO_ShlAssign -> LEFT_SHIFT
         | C.BO_ShrAssign -> RIGHT_SHIFT
         | C.BO_AndAssign -> BIT_AND
         | C.BO_XorAssign -> BIT_XOR
         | C.BO_OrAssign -> BIT_OR
       in
       E_compound_assign
         (expr func c.C.compound_lval, type_qual range c.C.compound_comp_lval_type,
          op, expr func c.C.compound_rval, type_qual range c.C.compound_comp_result_type),
       typ, range

   | C.BinaryOperator (l,op,r) ->
      let l,r = expr func l, expr func r in
      if op = C.BO_Assign then check_type range (expr_type l) (expr_type r);
      (match op with
       | C.BO_Mul -> E_binary (O_arithmetic MUL, l, r)
       | C.BO_Div -> E_binary (O_arithmetic DIV, l, r)
       | C.BO_Rem -> E_binary (O_arithmetic MOD, l, r)
       | C.BO_Add -> E_binary (O_arithmetic ADD, l, r)
       | C.BO_Sub -> E_binary (O_arithmetic SUB, l, r)
       | C.BO_Shl -> E_binary (O_arithmetic LEFT_SHIFT, l, r)
       | C.BO_Shr -> E_binary (O_arithmetic RIGHT_SHIFT, l, r)
       | C.BO_And -> E_binary (O_arithmetic BIT_AND, l, r)
       | C.BO_Xor -> E_binary (O_arithmetic BIT_XOR, l, r)
       | C.BO_Or -> E_binary (O_arithmetic BIT_OR, l, r)
       | C.BO_LT -> E_binary (O_logical LESS, l, r)
       | C.BO_GT -> E_binary (O_logical GREATER, l, r)
       | C.BO_LE -> E_binary (O_logical LESS_EQUAL, l, r)
       | C.BO_GE -> E_binary (O_logical GREATER_EQUAL, l, r)
       | C.BO_EQ -> E_binary (O_logical EQUAL, l, r)
       | C.BO_NE -> E_binary (O_logical NOT_EQUAL, l, r)
       | C.BO_LAnd -> E_binary (O_logical LOGICAL_AND, l, r)
       | C.BO_LOr -> E_binary (O_logical LOGICAL_OR, l, r)
       | C.BO_Comma -> E_comma (l, r)
       | C.BO_Assign -> E_assign (l, r)
       | _ -> error range "unhandled binary operator" (C.binary_operator_name op)
      ), typ, range

   | C.UnaryOperator (op,a) ->
      let (_,ta,_) as a = expr func a in
      (match op with
       | C.UO_PostInc -> E_increment (INC, POST, a)
       | C.UO_PostDec -> E_increment (DEC, POST, a)
       | C.UO_PreInc -> E_increment (INC, PRE, a)
       | C.UO_PreDec -> E_increment (DEC, PRE, a)
       | C.UO_AddrOf -> E_address_of a
       | C.UO_Deref -> E_deref a
       | C.UO_Plus -> let e,_,_ = a in e
       | C.UO_Minus -> E_unary (NEG, a)
       | C.UO_Not -> E_unary (BIT_NOT, a)
       | C.UO_LNot -> E_unary (LOGICAL_NOT, a)
       | C.UO_Extension -> let e,_,_ = a in e
       | _ -> error range "unhandled unary operator" (C.unary_operator_name op)
      ), typ, range

   | C.CallExpr e ->
      let c = expr func e.C.call_callee
      and a = Array.map (expr func) e.C.call_args
      in
      E_call (c,a), typ, range

   | C.CastExpr (e,k) ->
      let e = expr func e in
      let o = match k with
        | C.CStyleCast -> EXPLICIT
        | C.ImplicitCast -> IMPLICIT
        | _ -> error range "unhandled cast kind" (C.cast_kind_name k)
      in
      E_cast (e, o), typ, range

   | C.CharacterLiteral (z,k) ->
      E_character_literal (Z.of_int32 z, k), typ, range

   | C.ChooseExpr e ->
      expr func (if e.C.choose_cond_true then e.C.choose_true else e.C.choose_false)

   | C.CompoundLiteralExpr (i,scope) ->
      let func = if scope then None else func in
      E_compound_literal (init func i), typ, range

   | C.DeclRefExpr d ->
      (match d.C.decl_kind with
       | C.VarDecl v -> E_variable (var_decl func v)
       | C.FunctionDecl f ->
          let decl = func_decl f in
          add_func_ref func decl;
          E_function decl
       | C.EnumConstantDecl e -> E_integer_literal e.C.enum_cst_val
       | k -> error decl.C.decl_range "unhandled reference to a declaration in expression" (C.decl_kind_name k)
      ), typ, range

   | C.FloatingLiteral f ->
      E_float_literal f, typ, range

   | C.GenericSelectionExpr g ->
      expr func (g.C.select_assoc.(g.C.select_result))

   | C.IntegerLiteral v ->
      E_integer_literal v, typ, range

   | C.MemberExpr e ->
      let ee = expr func e.C.member_base in
      let ff = match e.C.member_decl.C.decl_kind with
        | C.FieldDecl f -> f
        | k -> error range "unhandled field declaration kind in member expression" (C.decl_kind_name k)
      in
      (if e.C.member_arrow then E_arrow_access (ee, ff.C.field_index, ff.C.field_name.C.name_print)
       else E_member_access (ee, ff.C.field_index, ff.C.field_name.C.name_print)
      ), typ, range

   | C.OffsetOfExpr (n,Some o) -> E_integer_literal o, typ, range
   | C.OffsetOfExpr (n,None) -> error range "offsetof incomplete type" ""

   | C.OpaqueValueExpr o ->
      (match o.C.opaque_source with
       | None ->
       (*error range "empty opaque expression" "" *)
       (* TODO: check the meanng of None in OpaqueValueExpr *)
          E_integer_literal Z.zero, typ, range
       | Some e -> expr func e
      )

   | C.ParenExpr e ->
      expr func e

   | C.PredefinedExpr (_,name) ->
      E_predefined name, typ, range

   | C.StmtExpr l ->
      E_statement (deblock (ListExt.map_merge (stmt func) l)), typ, range

   | C.StringLiteral (s,k) ->
      E_string_literal (s, k), typ, range

   | C.UnaryExprOrTypeTraitExpr (op,t) ->
      let tt,_ = type_qual range t in
      (try
         match op with
         | C.UETT_SizeOf -> sizeof_expr ctx.ctx_target range typ tt
         | C.UETT_AlignOf | C.UETT_PreferredAlignOf ->
            E_integer_literal (alignof_type ctx.ctx_target tt), typ, range
       with Invalid_argument msg ->
         warning range msg (C.string_of_type (fst t));
         E_integer_literal Z.zero, typ, range
      )
   | C.VAArgExpr e ->
      E_var_args (expr func e), typ, range

   | C.AtomicExpr e ->
      E_atomic (atomic_op e.C.atomic_op, Array.map (expr func) e.C.atomic_exprs),
      typ, range

   | C.FullExpr e | C.ConstantExpr e ->
      expr func e

   | C.ConvertVectorExpr e ->
      E_convert_vector (expr func e), typ, range

   | C.ExtVectorElementExpr (e,s) ->
      E_vector_element (expr func e, s), typ, range

   | ShuffleVectorExpr ea ->
      E_shuffle_vector (Array.map (expr func) ea), typ, range

   | e -> error range "unhandled expression" (C.expr_kind_name e)


  (* translate initializers *)
  (* ********************** *)

  and init (func:func option) e =
    let range = e.C.expr_range in
    let typ = match e.C.expr_type with
      | None -> error range "expression without type" (C.expr_kind_name e.C.expr_kind)
      | Some t -> type_qual range t
    in
    match e.C.expr_kind with

    | C.InitListExpr i ->
       let filler =
         match i.C.init_list_filler with
         | None -> None
         | Some i -> Some (init func i)
       in
       let list = Array.to_list i.C.init_list_init in
       I_init_list (List.map (init func) list, filler)

    | C.ImplicitValueInitExpr ->
       zero_init range (fst typ)
    (* I_init_implicit typ *) (* NOTE: we translate implicit-init into zero-init *)


    | _ -> I_init_expr (expr func e)



  (* translate toplevel declarations *)
  (* ******************************* *)

  and toplevel decl =
    match decl.C.decl_kind with

    (* types *)
    | C.EnumDecl e ->
       let d = enum_decl e in
       debug string_of_enum_decl d

    | C.RecordDecl r ->
       let d = record_decl r in
       debug string_of_record_decl d

    | C.TypedefDecl d ->
       let d = typedef_decl d in
       debug string_of_typedef d

    (* declarations *)
    | C.VarDecl v ->
       let d = var_decl None v in
       debug string_of_var_decl d

    | C.FunctionDecl f ->
       let d = func_decl f in
       debug string_of_func_decl d

    | C.EmptyDecl -> ()

    | C.StaticAssertDecl a ->
       if a.C.assert_is_failed then
         warning decl.C.decl_range "static assertion failed: %s" a.C.assert_msg

    | C.FileScopeAsmDecl s ->
       ctx.ctx_file_scope_asm <-
         RangeMap.add decl.C.decl_range s ctx.ctx_file_scope_asm;
       debug (fun x -> x) s

    | _ -> error decl.C.decl_range "unhandled toplevel declaration" (C.decl_kind_name decl.C.decl_kind)

  in

  (match decl.C.decl_kind with
  | C.TranslationUnitDecl decls -> List.iter toplevel decls
  | _ -> error decl.C.decl_range "expected TranslationUnitDecl" (C.decl_kind_name decl.C.decl_kind)
  );

  (* add parsed files *)
  ctx.ctx_files <- SetExt.StringSet.(union ctx.ctx_files (of_list files));

  (* add comments, attach macros, merging duplicates *)
  let c =
    List.fold_left
      (fun ctx c ->
        let r = c.Clang_AST.com_range in
        if RangeMap.mem r ctx then
          let old = RangeMap.find r ctx in
          if List.exists (fun (c',_) -> c = c') old then ctx
          else RangeMap.add r ((c,macros_map)::old) ctx
        else RangeMap.add r [(c,macros_map)] ctx
      )
      ctx.ctx_comments coms
  in
  ctx.ctx_comments <- c;

  (* add macros globally *)
  List.iter (fun macro ->
      if Hashtbl.mem ctx.ctx_macros macro.C.macro_name then
        begin
          let old_macro = Hashtbl.find ctx.ctx_macros macro.C.macro_name in
          if Compare.list Stdlib.compare
              macro.macro_contents old_macro.macro_contents
             != 0
          then
            let range = C.{ range_begin = macro.macro_loc; range_end = macro.macro_loc } in
            warning range "macro is defined twice" macro.macro_name
        end;
      Hashtbl.add ctx.ctx_macros macro.C.macro_name macro
    ) macros;


  (* shifts static funcs to funcs *)
  if remove_unused_static && not keep_static then (
    (* functions transitively referenced by id *)
    let rec close_func_ref id acc =
      if UidSet.mem id acc then acc else
        UidSet.fold close_func_ref
          (get_func_ref (Some id)) (UidSet.add id acc)
    in
    (* functions transitively referenced by toplevel or non-static *)
    let func_refs = ref  UidSet.empty in
    let toplevel = get_func_ref None in
    Hashtbl.iter
      (fun _ f ->
        if (not f.func_is_static) || UidSet.mem f.func_uid toplevel
        then func_refs := close_func_ref f.func_uid !func_refs
      )
      ctx.ctx_tu_funcs;
    (* only keep referenced static functions *)
    Hashtbl.iter
      (fun name f ->
        if UidSet.mem f.func_uid !func_refs then
          Hashtbl.add ctx.ctx_funcs f.func_org_name f
        else if !log_remove then
          Printf.printf "removing static function %s not used in translation unit\n" name;
      )
      ctx.ctx_tu_static_funcs
  )
  else
    (* keep all statics *)
    Hashtbl.iter
      (fun _ f -> Hashtbl.add ctx.ctx_funcs f.func_org_name f)
      ctx.ctx_tu_static_funcs;


  (match out with Some o -> close_out o | None -> ())


let link_project ctx =
  let cvt hash name =
    Hashtbl.fold
      (fun org_name def map -> StringMap.add (name def) def map) hash StringMap.empty
  in
  { proj_name = ctx.ctx_name;
    proj_tu = ctx.ctx_tu;
    proj_target = ctx.ctx_target;

    proj_typedefs = cvt ctx.ctx_typedefs (fun t -> t.typedef_unique_name);
    proj_enums = cvt ctx.ctx_enums (fun t -> t.enum_unique_name);
    proj_records = cvt ctx.ctx_records (fun t -> t.record_unique_name);
    proj_vars = cvt ctx.ctx_vars (fun t -> t.var_unique_name);
    proj_funcs = cvt ctx.ctx_funcs (fun t -> t.func_unique_name);
    proj_files = ctx.ctx_files |> SetExt.StringSet.elements;
    proj_file_scope_asm = ctx.ctx_file_scope_asm;
    proj_comments = ctx.ctx_comments;
    proj_macros = cvt ctx.ctx_macros (fun t -> t.macro_name);
  }


let get_parsed_files ctx = ctx.ctx_files |> SetExt.StringSet.elements
