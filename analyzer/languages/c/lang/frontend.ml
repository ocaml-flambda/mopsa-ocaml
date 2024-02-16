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

(** C front-end to translate parser AST into MOPSA AST *)


open Mopsa_c_parser
open C_AST
open Mopsa
open Universal.Ast
open Stubs.Ast
open Ast


let debug fmt = Debug.debug ~channel:"c.frontend" fmt


(** {2 Command-line options} *)
(** ======================== *)

let opt_clang = ref []
(** Extra options to pass to clang when parsing  *)

let opt_include_dirs = ref []
(** List of include directories *)

let opt_make_target = ref ""
(** Name of the target binary to analyze *)

let opt_without_libc = ref false
(** Disable stubs of the standard library *)

let opt_enable_cache = ref true
(** Enable the parser cache. *)

let opt_warn_all = ref false
(** Display all compiler warnings *)

let opt_use_stub = ref []
(** Lists of functions that the body will be replaced by a stub *)

let opt_library_only = ref false
(** Allow library-only targets in the .db files (used for multilanguage analysis) *)

let opt_target_triple = ref ""
(** Target architecture triple to analyze for (host if left empty) *)

let opt_stubs_files = ref []
(** Additional stub files to parse *)

let opt_ignored_translation_units = ref []
(** List of translation units to ignore during linking *)

let opt_save_preprocessed_file = ref ""
(** Where to save the preprocessed file *)

let () =
  register_language_option "c" {
    key = "-I";
    category = "C";
    doc = " add the directory to the search path for include files in C analysis";
    spec = ArgExt.Set_string_list opt_include_dirs;
    default = "";
  };
  register_language_option "c" {
    key = "-ccopt";
    category = "C";
    doc = " pass the option to the Clang frontend";
    spec = ArgExt.Set_string_list opt_clang;
    default = "";
  };
  register_language_option "c" {
    key = "-make-target";
    category = "C";
    doc = " binary target to analyze; used only when the Makefile builds multiple targets.";
    spec = ArgExt.Set_string opt_make_target;
    default = "";
  };
  register_language_option "c" {
    key = "-without-libc";
    category = "C";
    doc = " disable stubs of the standard C library.";
    spec = ArgExt.Set opt_without_libc;
    default = "false";
  };
  register_language_option "c" {
    key = "-disable-parser-cache";
    category = "C";
    doc = " disable the cache of the Clang parser.";
    spec = ArgExt.Clear opt_enable_cache;
    default = "unset";
  };
  register_language_option "c" {
    key = "-Wall";
    category = "C";
    doc = " display compiler warnings.";
    spec = ArgExt.Set opt_warn_all;
    default = "unset";
  };
  register_language_option "c" {
    key = "-use-stub";
    category = "C";
    doc = " list of functions for which the stub is used instead of the declaration.";
    spec = ArgExt.Set_string_list opt_use_stub;
    default = "";
  };
  register_language_option "c" {
    key = "-library-only";
    category = "C";
    doc = " allow library-only targets in the .db files (used for multilanguage analysis)";
    spec = ArgExt.Set opt_library_only;
    default = "false";
  };
  register_language_option "c" {
    key = "-additional-stubs";
    category = "C";
    doc = " additional stubs file";
    spec = ArgExt.Set_string_list opt_stubs_files;
    default = "";
  };
  register_language_option "c" {
    key = "-target-triple";
    category = "C";
    doc = " target architecture to analyze, as a triple (host if left empty).";
    spec = ArgExt.Set_string opt_target_triple;
    default = "";
  };
  register_language_option "c" {
    key = "-c-ignore-translation-units";
    category = "C";
    doc = " list of translation units ignored during linking.";
    spec = ArgExt.Set_string_list opt_ignored_translation_units;
    default = "";
  };
  register_language_option "c" {
    key = "-c-save-preprocessed-file";
    category = "C";
    doc = " save the whole analyzed project into a single preprocessed file passed as argument to this option";
    spec = ArgExt.Set_string opt_save_preprocessed_file;
    default = "";
  };
  ()




(** {2 Contexts} *)
(** ============ *)

type type_space = TS_TYPEDEF | TS_RECORD | TS_ENUM

type ctx = {
  ctx_prj : C_AST.project;
  (* project descriptor *)

  ctx_fun: Ast.c_fundec StringMap.t;
  (* cache of functions of the project *)

  ctx_type: (type_space*string,typ) Hashtbl.t;
  (* cache the translation of all named types;
     this is required for records defining recursive data-types
  *)

  ctx_vars: (int*string,var*C_AST.variable) Hashtbl.t;
  (* cache of variables of the project *)

  ctx_macros: C_AST.macro StringMap.t;
  (* cache of macros of the project *)

  ctx_predicates: Mopsa_c_stubs_parser.Passes.Preprocessor.predicate StringMap.t;
  (* cache of stub predicates *)

  ctx_stubs: (string,Mopsa_c_stubs_parser.Cst.stub) Hashtbl.t;
  (* cache of stubs CST, used for resolving aliases *)

  ctx_enums: Z.t StringMap.t;
  (* cache of enum values of the project *)
}

let input_files : string list ref = ref []
(** List of input files *)

let target_info = ref host_target_info
(** Target information used for parsing *)

let find_function_in_context ctx range (f: C_AST.func) =
  try StringMap.find f.func_unique_name ctx.ctx_fun
  with Not_found -> Exceptions.panic_at range "Could not find function %s in context" f.func_unique_name


(* Get the list of system headers encountered during parsing *)
let get_parsed_system_headers (ctx:Clang_to_C.context) : string list =
  let is_header f = Filename.extension f = ".h" in
  let is_system_header f = is_header f && not (List.mem f !input_files) in
  List.filter is_system_header (Clang_to_C.get_parsed_files ctx)


(* Get the list of all stub source files *)
let get_all_stubs () =
  let rec iter dir =
    if not (Sys.is_directory dir) then []
    else
      Sys.readdir dir |>
      Array.fold_left
        (fun acc f ->
           let ff = Filename.concat dir f in
           if Sys.is_directory ff then iter ff @ acc else ff :: acc
        ) []
  in
  iter (Filename.concat (Params.Paths.get_lang_stubs_dir "c" ()) "libc")


(* Find the stub of a given header file *)
let find_stubs_of_header header stubs =
  let stubs_dir = Filename.concat (Params.Paths.get_lang_stubs_dir "c" ()) "libc" in
  let stubd_dir_len = String.length stubs_dir in
  List.filter (fun stub ->
      let h = Filename.chop_extension stub ^ ".h" in
      let relative_h = String.sub h stubd_dir_len (String.length h - stubd_dir_len) in
      let regexp = Str.regexp (".*" ^ relative_h ^ "$") in
      Str.string_match regexp header 0
    ) stubs

(* Check if the source file is to be ignored *)
let is_ignored_translation_unit file =
  let n = String.length file in
  List.exists
    (fun file' ->
       let n' = String.length file' in
       n >= n' && (String.equal file' (String.sub file (n - n') n'))
    ) !opt_ignored_translation_units

(** {2 Entry point} *)
(** =============== *)

let rec parse_program (files: string list) =
  let open Clang_parser in
  let open Clang_to_C in

  if files = [] then panic "no input file";

  (* let's initialize target from the option *)
  if !opt_target_triple <> "" then 
    target_info :=
      get_target_info ({ Clang_AST.empty_target_options with target_triple = !opt_target_triple })
  ;
  Mopsa_c_stubs_parser.Cst.target_info := !target_info;
  let ctx = Clang_to_C.create_context "project" !target_info in
  let nb = List.length files in
  input_files := [];
  let () =
    try
      ListExt.iteri
        (fun i file ->
          match file, Filename.extension file with
          | _, (".c" | ".h" | ".i") -> parse_file "clang" ~nb:(i,nb) [] file false false ctx
          | _, (".cpp" | ".cc" | ".c++") -> parse_file "clang++" ~nb:(i,nb) [] file false true ctx
          | _, ".db" | ".db", _ -> parse_db file ctx
          | _, x -> Exceptions.panic "unknown C extension %s" x
        ) files;
    with Exceptions.SyntaxErrorList es ->
      panic "Parsing error raised:@.%a" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.") (fun fmt (range, msg) -> Format.fprintf fmt "%a: %s" pp_range range msg)) es in
  let () = parse_stubs ctx () in
  let prj = Clang_to_C.link_project ctx in
  let () =
    if !opt_save_preprocessed_file <> "" then
      let outch = open_out !opt_save_preprocessed_file in
      let () = C_print.print_project ~verbose:false outch prj in
      let () = warn "Preprocessed file generated. In order to keep libc stubs, we recommend running mopsa-c on:@.%s %a"
          !opt_save_preprocessed_file
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
             Format.pp_print_string)
          (List.filter
             (fun f ->
                Filename.check_suffix (Filename.dirname f) "share/mopsa/stubs/c/libc"
             ) prj.proj_files) in
      close_out outch;
  in
  {
    prog_kind = from_project prj;
    prog_range = mk_program_range files;
  }

and parse_db (dbfile: string) ctx : unit =
  if not (Sys.file_exists dbfile) then panic "file %s not found" dbfile;

  let open Clang_parser in
  let open Clang_to_C in
  let open Mopsa_build_db in

  let db = load_db dbfile in
  let srcs =
    if !opt_library_only then
      let libs = get_libraries db in
      let lib =
        if List.length libs = 1 then List.hd libs
        else if libs = [] then panic "no library in database"
        else if !opt_make_target = "" then
          panic "a target is required in a multi-library Makefile@.Possible targets:@\n@[%a@]"
            (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
               Format.pp_print_string)
            libs
        else
          try find_target !opt_make_target libs
          with Not_found ->
            panic "library target %s not found" !opt_make_target
      in
      get_library_sources db lib
    else
      let execs = get_executables db in
      let exec =
        (* No need for target selection if there is only one binary *)
        if List.length execs = 1
        then List.hd execs
        else if execs = []
        then panic "no binary in database"
        else if !opt_make_target = ""
        then panic "a target is required in a multi-binary database, use the -make-target option.@\nPossible targets:@\n @[%a@]"
               (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n") Format.pp_print_string)
               execs
        else
          try find_target !opt_make_target execs
          with Not_found ->
            panic "binary target %s not found" !opt_make_target
      in
      get_executable_sources db exec in
  let nb = List.length srcs in
  input_files := [];
  let cwd = Sys.getcwd() in
  ListExt.iteri
    (fun i src ->
       match src.source_kind with
       | SOURCE_C | SOURCE_CXX ->
         let cmd = if src.source_kind = SOURCE_C then "clang" else "clang++" in
         (* parse file in the original compilation directory *)
         Sys.chdir src.source_cwd;
         parse_file cmd ~nb:(i,nb) src.source_opts src.source_path !opt_enable_cache (src.source_kind=SOURCE_CXX) ctx;

       | _ -> if !opt_warn_all then warn "ignoring file %s" src.source_path
    ) srcs;
  (* make sure we get back to cwd in all cases *)
  Sys.chdir cwd

and parse_file (cmd: string) ?nb ?(stub=false) (opts: string list) (file: string) enable_cache ignore ctx =
  if not (Sys.file_exists file) then panic "file %s not found" file;
  debug "parsing file %s" file;
  (* clang does not like -MT and -MD options *)
  let opts = List.filter (fun x -> x != "-MT" && x != "-MD") opts in
  let opts' = ("-I" ^ (Paths.resolve_stub "c" "mopsa")) ::
              ("-include" ^ "mopsa.h") ::
              "-Wall" ::
              (* recent versions of Clang promote warnings as errors
                 -> we revert them to warnings
               *)
              "-Wno-error=incompatible-function-pointer-types" ::
              "-Wno-error=implicit-function-declaration" ::
              "-Qunused-arguments"::
              (List.map (fun dir -> "-I" ^ dir) !opt_include_dirs) @
              opts @
              !opt_clang
  in
  input_files := file :: !input_files;
  (* if adding a stub file, keep all static functions as they may be used
     by stub annotations
   *)
  C_parser.parse_file cmd file opts' ~target_options:!target_info.target_options !opt_warn_all enable_cache stub (ignore || is_ignored_translation_unit file) ctx !opt_use_stub


and parse_stubs ctx () =
  (** Add Mopsa stubs *)
  parse_file ~stub:true "clang" [] (Params.Paths.resolve_stub "c" "mopsa/mopsa.c") false false ctx;
  (** Add compiler builtins *)
  parse_file ~stub:true "clang" [] (Params.Paths.resolve_stub "c" "mopsa/compiler_builtins.c") false false ctx;
  List.iter (fun stub_file ->
      try
        parse_file ~stub:true "clang" [] (Filename.concat (Paths.get_stubs_dir ()) stub_file) false false ctx;
      with SyntaxErrorList es ->
        panic "Parsing error raised:@.%a" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.") (fun fmt (range, msg) -> Format.fprintf fmt "%a: %s" pp_range range msg)) es) !opt_stubs_files;
  if !opt_without_libc then ()
  else
    (** Add stubs of the included headers *)
    let headers = get_parsed_system_headers ctx in
    if headers = [] then ()
    else
      let module Set = SetExt.StringSet in
      let all_stubs = get_all_stubs () in
      let rec iter past_headers past_stubs wq =
        if Set.is_empty wq then ()
        else
          let h = Set.choose wq in
          let wq' = Set.remove h wq in
          if Set.mem h past_headers then
            iter past_headers past_stubs wq'
          else
            (* Get the stubs of the header *)
            let stubs = find_stubs_of_header h all_stubs in
            let new_headers = List.fold_left (fun acc stub ->
                if Set.mem stub past_stubs then acc
                else
                  (* Parse the stub and collect new parsed headers *)
                  let before = Clang_to_C.get_parsed_files ctx |> Set.of_list in
                  parse_file ~stub:true "clang" [] stub false false ctx;
                  let after = Clang_to_C.get_parsed_files ctx  |> Set.of_list in
                  Set.diff after before |>
                  Set.union acc
              ) Set.empty stubs
            in
            iter (Set.add h past_headers) (Set.union (Set.of_list stubs) past_stubs) (Set.union new_headers wq)
      in
      iter Set.empty Set.empty (Set.of_list headers)



and from_project prj =
  (* Preliminary parsing of functions *)
  let funcs_and_origins =
    StringMap.bindings prj.proj_funcs |>
    List.map snd |>
    List.map(fun f -> from_function f, f)
  in
  let funcs = List.fold_left (fun map (f, o) ->
      StringMap.add o.func_unique_name f map
    ) StringMap.empty funcs_and_origins
  in

  (* Prepare the parsing context *)
  let ctx = {
      ctx_fun = funcs;
      ctx_type = Hashtbl.create 16;
      ctx_prj = prj;
      ctx_vars = Hashtbl.create 16;
      ctx_macros = prj.proj_macros;
      ctx_predicates = from_stub_predicates prj.proj_comments;
      ctx_enums = StringMap.fold (fun _ enum acc ->
          enum.enum_values |> List.fold_left (fun acc v ->
              StringMap.add v.enum_val_org_name v.enum_val_value acc
            ) acc
        ) prj.proj_enums StringMap.empty;
      ctx_stubs = Hashtbl.create 16;
    }
  in

  (* Parse functions *)
  List.iter (fun (f, o) ->
      debug "parsing function %s" o.func_org_name;
      f.c_func_uid <- o.func_uid;
      f.c_func_org_name <- o.func_org_name;
      f.c_func_unique_name <- o.func_unique_name;
      f.c_func_return <- from_typ ctx o.func_return;
      f.c_func_parameters <- Array.to_list o.func_parameters |> List.map (from_var ctx);
      f.c_func_static_vars <- List.map (from_var ctx) o.func_static_vars;
      f.c_func_local_vars <- List.map (from_var ctx) o.func_local_vars;
      f.c_func_body <- from_body_option ctx (from_range o.func_range) o.func_body;
      (* Parse stub of functions that don't have a body or when they are
         selected by the option '-use-stub' *)
      if f.c_func_body = None || List.mem f.c_func_org_name !opt_use_stub then
        f.c_func_stub <- from_stub_comment ctx o
    ) funcs_and_origins;

  (* Parse stub directives *)
  let directives = from_stub_directives ctx prj.proj_comments in

  let globals = StringMap.bindings prj.proj_vars |>
                List.map snd |>
                List.map (fun v ->
                    from_var ctx v, from_var_init ctx v
                  )
  in


  Ast.C_program {
    c_globals = globals;
    c_functions = StringMap.bindings funcs |> List.split |> snd;
    c_stub_directives = directives;
  }


and find_target target targets =
  try
    List.find (fun t -> String.equal (Filename.basename t) target) targets
  with Not_found ->
    let re_permissive = Str.regexp ("^" ^ target ^ ".*") in
    let t = List.find (fun t -> Str.string_match re_permissive (Filename.basename t) 0) targets in
    warn "permissive search selected target %s (initial target: %s)" t target;
    t

(** {2 functions} *)
(** ============= *)

and from_function =
  fun func ->
    {
      c_func_org_name = func.func_org_name;
      c_func_unique_name = func.func_unique_name;
      c_func_uid = func.func_uid;
      c_func_is_static = func.func_is_static;
      c_func_return = T_any;
      c_func_parameters = [];
      c_func_body = None ;
      c_func_static_vars = [];
      c_func_local_vars = [];
      c_func_variadic = func.func_variadic;
      c_func_stub = None;
      c_func_range = from_range func.func_range;
      c_func_name_range = from_range func.func_name_range;
    }

(** {2 Scope update} *)
(** **************** *)

and from_scope_update ctx (upd:C_AST.scope_update) : Ast.c_scope_update =
  {
    c_scope_var_added = List.map (from_var ctx) upd.scope_var_added;
    c_scope_var_removed = List.map (from_var ctx) upd.scope_var_removed;
  }

(** {2 Statements} *)
(** ============== *)

and from_stmt ctx ((skind, range): C_AST.statement) : stmt =
  let srange = from_range range in
  let start_range = set_range_end srange (get_range_start srange) in
  let end_range = set_range_start srange (get_range_end srange) in
  let skind = match skind with
    | C_AST.S_local_declaration v ->
      let vv = from_var ctx v in
      let init = from_init_option ctx v.var_init in
      Ast.S_c_declaration (vv, init, from_var_scope ctx v.var_kind)
    | C_AST.S_expression e -> Universal.Ast.S_expression (from_expr ctx e)
    | C_AST.S_block block -> from_block ctx srange block |> Framework.Core.Ast.Stmt.skind
    | C_AST.S_if (cond, body, orelse) -> Universal.Ast.S_if (from_expr ctx cond, from_block ctx end_range body, from_block ctx end_range orelse)
    | C_AST.S_while (cond, body) -> Universal.Ast.S_while (from_expr ctx cond, from_block ctx end_range body)
    | C_AST.S_do_while (body, cond) -> S_c_do_while (from_block ctx end_range body, from_expr ctx cond)
    | C_AST.S_for (init, test, increm, body) -> S_c_for(from_block ctx start_range init, from_expr_option ctx test, from_expr_option ctx increm, from_block ctx end_range body)
    | C_AST.S_jump (C_AST.S_goto (label, upd)) -> S_c_goto (label,from_scope_update ctx upd)
    | C_AST.S_jump (C_AST.S_break upd) -> S_c_break (from_scope_update ctx upd)
    | C_AST.S_jump (C_AST.S_continue upd) -> S_c_continue (from_scope_update ctx upd)
    | C_AST.S_jump (C_AST.S_return (None, upd)) -> S_c_return (None,from_scope_update ctx upd)
    | C_AST.S_jump (C_AST.S_return (Some e, upd)) -> S_c_return (Some (from_expr ctx e), from_scope_update ctx upd)
    | C_AST.S_jump (C_AST.S_switch (cond, body)) -> Ast.S_c_switch (from_expr ctx cond, from_block ctx end_range body)
    | C_AST.S_target(C_AST.S_case(e,upd)) -> S_c_switch_case(from_expr ctx e, from_scope_update ctx upd)
    | C_AST.S_target(C_AST.S_default upd) -> S_c_switch_default (from_scope_update ctx upd)
    | C_AST.S_target(C_AST.S_label l) -> Ast.S_c_label l
  in
  {skind; srange}

and from_block ctx empty_range (block: C_AST.block) : stmt =
  let block_range =
    match block.blk_stmts with
    | [] -> empty_range
    | l ->
      let _,first = ListExt.hd l in
      let _,last = ListExt.last l in
      mk_orig_range (get_range_start (from_range first)) (get_range_end (from_range last))
  in
  mk_block
    (List.map (from_stmt ctx) block.blk_stmts)
    ~vars:(List.map (from_var ctx) block.blk_local_vars)
    block_range

and from_body_option (ctx) empty_range (block: C_AST.block option) : stmt option =
  match block with
  | None -> None
  | Some stmtl -> Some (from_block ctx empty_range stmtl)




(** {2 Expressions} *)
(** =============== *)

and from_expr ctx ((ekind, tc , range) : C_AST.expr) : expr =
  let erange = from_range range in
  let etyp = from_typ ctx tc in
  let ekind =
    match ekind with
    | C_AST.E_integer_literal n -> Universal.Ast.(E_constant (C_int n))
    | C_AST.E_float_literal f -> Universal.Ast.(E_constant (C_float (float_of_string f)))
    | C_AST.E_character_literal (c, k)  -> E_constant(Ast.C_c_character (c, from_character_kind k))
    | C_AST.E_string_literal (s, k) -> Universal.Ast.(E_constant (C_c_string (s, from_character_kind k)))
    | C_AST.E_variable v -> E_var (from_var ctx v, None)
    | C_AST.E_function f -> Ast.E_c_function (find_function_in_context ctx erange f)
    | C_AST.E_call (f, args) -> Universal.Ast.E_call(from_expr ctx f, Array.map (from_expr ctx) args |> Array.to_list)
    | C_AST.E_unary (op, e) -> E_unop (from_unary_operator op etyp, from_expr ctx e)
    | C_AST.E_binary (op, e1, e2) -> E_binop (from_binary_operator op etyp, from_expr ctx e1, from_expr ctx e2)
    | C_AST.E_cast (e,C_AST.EXPLICIT) -> Ast.E_c_cast(from_expr ctx e, true)
    | C_AST.E_cast (e,C_AST.IMPLICIT) -> Ast.E_c_cast(from_expr ctx e, false)
    | C_AST.E_assign (lval, rval) -> Ast.E_c_assign(from_expr ctx lval, from_expr ctx rval)
    | C_AST.E_address_of(e) -> Ast.E_c_address_of(from_expr ctx e)
    | C_AST.E_deref(p) -> Ast.E_c_deref(from_expr ctx p)
    | C_AST.E_array_subscript (a, i) -> Ast.E_c_array_subscript(from_expr ctx a, from_expr ctx i)
    | C_AST.E_member_access (r, i, f) -> Ast.E_c_member_access(from_expr ctx r, i, f)
    | C_AST.E_arrow_access (r, i, f) -> Ast.E_c_arrow_access(from_expr ctx r, i, f)
    | C_AST.E_statement s -> Ast.E_c_statement (from_block ctx erange s)
    | C_AST.E_predefined s -> Universal.Ast.(E_constant (C_c_string (s, Ast.C_char_ascii)))
    | C_AST.E_var_args e -> Ast.E_c_var_args (from_expr ctx e)
    | C_AST.E_conditional (cond,e1,e2) -> Ast.E_c_conditional(from_expr ctx cond, from_expr ctx e1, from_expr ctx e2)

    (* the following operations are removed from the AST by simplification
       in the parser, before calling the frontend
     *)
    | C_AST.E_binary_conditional (_,_) -> Exceptions.panic_at erange "E_binary_conditional not supported"
    | C_AST.E_compound_assign (_,_,_,_,_) -> Exceptions.panic_at erange "E_compound_assign not supported"
    | C_AST.E_comma (_,_) -> Exceptions.panic_at erange "E_comma not supported"
    | C_AST.E_increment (_,_,_) -> Exceptions.panic_at erange "E_increment not supported"
    | C_AST.E_compound_literal _ -> Exceptions.panic_at erange "E_compound_literal not supported"

    (* atomic builtins are stubbed in .h header and should not be encountered
       here
     *)
    | C_AST.E_atomic (op,e1,e2) ->
       Ast.E_c_atomic (op, from_expr ctx e1, from_expr ctx e2)

    (* vector builtins are not supported
       we display a warning but output an AST so that we can analyzer programs
       which include headers and libraries with vector builtins but do
       not actually use them
     *)
    | C_AST.E_convert_vector e ->
       if !opt_warn_all then warn_at erange "__builtin_convertvector not supported";
       (from_expr ctx e).ekind

    | C_AST.E_vector_element (e,s) ->
       if !opt_warn_all then warn_at erange "__builtin_vectorelement not supported";
       (from_expr ctx e).ekind

    | C_AST.E_shuffle_vector a ->
       if !opt_warn_all then warn_at erange "__builtin_shufflevector not supported";
       (from_expr ctx a.(0)).ekind

  in
  mk_expr ekind erange ~etyp

and from_expr_option ctx : C_AST.expr option -> expr option = function
  | None -> None
  | Some e -> Some (from_expr ctx e)

and from_unary_operator op t = match op with
  | C_AST.NEG -> O_minus
  | C_AST.BIT_NOT -> O_bit_invert
  | C_AST.LOGICAL_NOT -> O_log_not

and from_binary_operator op t = match op with
  | C_AST.O_arithmetic (C_AST.ADD) -> O_plus
  | C_AST.O_arithmetic (C_AST.SUB) -> O_minus
  | C_AST.O_arithmetic (C_AST.MUL) -> O_mult
  | C_AST.O_arithmetic (C_AST.DIV) -> O_div
  | C_AST.O_arithmetic (C_AST.MOD) -> O_mod
  | C_AST.O_arithmetic (C_AST.LEFT_SHIFT) -> O_bit_lshift
  | C_AST.O_arithmetic (C_AST.RIGHT_SHIFT) -> O_bit_rshift
  | C_AST.O_arithmetic (C_AST.BIT_AND) -> O_bit_and
  | C_AST.O_arithmetic (C_AST.BIT_OR) -> O_bit_or
  | C_AST.O_arithmetic (C_AST.BIT_XOR) -> O_bit_xor
  | C_AST.O_logical (C_AST.LESS) -> O_lt
  | C_AST.O_logical (C_AST.LESS_EQUAL) -> O_le
  | C_AST.O_logical (C_AST.GREATER) -> O_gt
  | C_AST.O_logical (C_AST.GREATER_EQUAL) -> O_ge
  | C_AST.O_logical (C_AST.EQUAL) -> O_eq
  | C_AST.O_logical (C_AST.NOT_EQUAL) -> O_ne
  | C_AST.O_logical (C_AST.LOGICAL_AND) -> Ast.O_c_and
  | C_AST.O_logical (C_AST.LOGICAL_OR) -> Ast.O_c_or

and from_character_kind : C_AST.character_kind -> Ast.c_character_kind = function
  | Clang_AST.Char_Ascii -> Ast.C_char_ascii
  | Clang_AST.Char_Wide -> Ast.C_char_wide
  | Clang_AST.Char_UTF8 -> Ast.C_char_utf8
  | Clang_AST.Char_UTF16 -> Ast.C_char_utf16
  | Clang_AST.Char_UTF32 -> Ast.C_char_utf8


(** {2 Variables} *)
(** ============= *)

and from_var ctx (v: C_AST.variable) : var =
  try Hashtbl.find ctx.ctx_vars (v.var_uid,v.var_unique_name) |> fst
  with Not_found ->
    let v' =
      mkv
        (v.var_org_name ^ ":" ^ (string_of_int v.var_uid))
        (V_cvar {
            cvar_orig_name = v.var_org_name;
            cvar_uniq_name = v.var_unique_name;
            cvar_scope = from_var_scope ctx v.var_kind;
            cvar_range = from_range v.var_range;
            cvar_uid = v.var_uid;
          })
        (from_typ ctx v.var_type)
    in
    let v'' = patch_array_parameters v' in
    Hashtbl.add ctx.ctx_vars (v.var_uid,v.var_unique_name) (v'', v);
    v''

(* Formal parameters of functions having array types should be
   considered as pointers *)
and patch_array_parameters v =
    if not (is_c_array_type v.vtyp) ||
       not (is_c_function_parameter v)
    then v
    else
      let t = under_array_type v.vtyp in
      { v with vtyp = T_c_pointer t }

and from_var_scope ctx = function
  | C_AST.Variable_global -> Ast.Variable_global
  | Variable_extern -> Variable_extern
  | Variable_local f -> Variable_local (from_function f)
  | C_AST.Variable_parameter f -> Variable_parameter (from_function f)
  | C_AST.Variable_file_static tu -> Variable_file_static tu.tu_name
  | C_AST.Variable_func_static f -> Variable_func_static (from_function f)

and from_var_init ctx v =
  match v.var_init with
  | Some i -> Some (from_init ctx i)
  | None -> None

and from_init_option ctx init =
  match init with
  | Some i -> Some (from_init ctx i)
  | None -> None

and from_init ctx init =
  match init with
  | I_init_expr e -> C_init_expr (from_expr ctx e)
  | I_init_list(il, i) -> C_init_list (List.map (from_init ctx) il, from_init_option ctx i)
  | I_init_implicit t -> C_init_implicit (from_typ ctx t)



(** {2 Types} *)
(** ========= *)

and from_typ ctx (tc: C_AST.type_qual) : typ =
  let typ, qual = tc in
  let typ' = from_unqual_typ ctx typ in
  if qual.C_AST.qual_is_const then
    T_c_qualified({c_qual_is_const = true; c_qual_is_restrict = false; c_qual_is_volatile = false}, typ')
  else
    typ'

and from_unqual_typ ctx (tc: C_AST.typ) : typ =
  match tc with
  | C_AST.T_void -> Ast.T_c_void
  | C_AST.T_bool -> Ast.T_c_bool
  | C_AST.T_integer t -> Ast.T_c_integer (from_integer_type t)
  | C_AST.T_float t -> Ast.T_c_float (from_float_type t)
  | C_AST.T_pointer t -> Ast.T_c_pointer (from_typ ctx t)
  | C_AST.T_array (t,l) -> Ast.T_c_array (from_typ ctx t, from_array_length ctx l)
  | C_AST.T_function None -> Ast.T_c_function None
  | C_AST.T_function (Some t) -> Ast.T_c_function (Some (from_function_type ctx t))
  | C_AST.T_builtin_fn -> Ast.T_c_builtin_fn
  | C_AST.T_typedef t ->
    if Hashtbl.mem ctx.ctx_type (TS_TYPEDEF,t.typedef_unique_name)
    then Hashtbl.find ctx.ctx_type (TS_TYPEDEF,t.typedef_unique_name)
    else
      let x = {
        c_typedef_org_name = t.typedef_org_name;
        c_typedef_unique_name = t.typedef_unique_name;
        c_typedef_def =  Ast.T_c_void;
        c_typedef_range = from_range t.typedef_range;
      }
      in
      let y = Ast.T_c_typedef x in
      Hashtbl.add ctx.ctx_type (TS_TYPEDEF,t.typedef_unique_name) y;
      x.c_typedef_def <-  from_typ ctx t.typedef_def;
      y
  | C_AST.T_record r ->
    if Hashtbl.mem ctx.ctx_type (TS_RECORD,r.record_unique_name)
    then Hashtbl.find ctx.ctx_type (TS_RECORD,r.record_unique_name)
    else
      let x = {
        c_record_kind =
          (match r.record_kind with C_AST.STRUCT -> C_struct | C_AST.UNION -> C_union);
        c_record_org_name = r.record_org_name;
        c_record_unique_name = r.record_unique_name;
        c_record_defined = r.record_defined;
        c_record_sizeof = r.record_sizeof;
        c_record_alignof = r.record_alignof;
        c_record_fields = [];
        c_record_range = from_range r.record_range;
      }
      in
      let y = Ast.T_c_record x in
      Hashtbl.add ctx.ctx_type (TS_RECORD,r.record_unique_name) y;
      x.c_record_fields <-
        List.map
          (fun f -> {
               c_field_org_name = f.field_org_name;
               c_field_name = f.field_name;
               c_field_offset = f.field_offset;
               c_field_bit_offset = f.field_bit_offset;
               c_field_type = from_typ ctx f.field_type;
               c_field_range = from_range f.field_range;
               c_field_index = f.field_index;
             })
          (Array.to_list r.record_fields);
      y
  | C_AST.T_enum e ->
    if Hashtbl.mem ctx.ctx_type (TS_ENUM,e.enum_unique_name)
    then Hashtbl.find ctx.ctx_type (TS_ENUM,e.enum_unique_name)
    else
      let x =
        Ast.T_c_enum {
          c_enum_org_name = e.enum_org_name;
          c_enum_unique_name = e.enum_unique_name;
          c_enum_defined = e.enum_defined;
          c_enum_values =
            List.map
              (fun v -> {
                   c_enum_val_org_name = v.enum_val_org_name;
                   c_enum_val_unique_name = v.enum_val_unique_name;
                   c_enum_val_value = v.enum_val_value;
                   c_enum_val_range = from_range v.enum_val_range;
                 }) e.enum_values;
          c_enum_integer_type = from_integer_type (OptionExt.none_to_exn e.enum_integer_type);
          c_enum_range = from_range e.enum_range;
        }
      in
      Hashtbl.add ctx.ctx_type (TS_ENUM,e.enum_unique_name) x;
      x
  | C_AST.T_bitfield (t,n) -> Ast.T_c_bitfield (from_unqual_typ ctx t, n)
  | C_AST.T_complex _ -> failwith "C_AST.T_complex not supported"
  | C_AST.T_vector v ->
     (* translate vector into array type *)
     let t = from_typ ctx v.vector_type in
     (* size is in bytes, length is in units of t *)
     let len = Z.div (Z.of_int v.vector_size) (sizeof_type_in_target t !target_info) in
     Ast.T_c_array (t, Ast.C_array_length_cst len)

and from_integer_type : C_AST.integer_type -> Ast.c_integer_type = function
  | C_AST.Char SIGNED -> Ast.C_signed_char
  | C_AST.Char UNSIGNED -> Ast.C_unsigned_char
  | C_AST.SIGNED_CHAR -> Ast.C_signed_char
  | C_AST.UNSIGNED_CHAR -> Ast.C_unsigned_char
  | C_AST.SIGNED_SHORT -> Ast.C_signed_short
  | C_AST.UNSIGNED_SHORT -> Ast.C_unsigned_short
  | C_AST.SIGNED_INT -> Ast.C_signed_int
  | C_AST.UNSIGNED_INT -> Ast.C_unsigned_int
  | C_AST.SIGNED_LONG -> Ast.C_signed_long
  | C_AST.UNSIGNED_LONG -> Ast.C_unsigned_long
  | C_AST.SIGNED_LONG_LONG -> Ast.C_signed_long_long
  | C_AST.UNSIGNED_LONG_LONG -> Ast.C_unsigned_long_long
  | C_AST.SIGNED_INT128 -> Ast.C_signed_int128
  | C_AST.UNSIGNED_INT128 -> Ast.C_unsigned_int128

and from_float_type : C_AST.float_type -> Ast.c_float_type = function
  | C_AST.FLOAT -> Ast.C_float
  | C_AST.DOUBLE -> Ast.C_double
  | C_AST.LONG_DOUBLE -> Ast.C_long_double
  | C_AST.FLOAT128 -> Ast.C_float128

and from_array_length ctx al = match al with
  | C_AST.No_length -> Ast.C_array_no_length
  | C_AST.Length_cst n -> Ast.C_array_length_cst n
  | C_AST.Length_expr e -> Ast.C_array_length_expr (from_expr ctx e)

and from_function_type ctx f =
  {
    c_ftype_return = from_typ ctx f.ftype_return;
    c_ftype_params = List.map (from_typ ctx) f.ftype_params;
    c_ftype_variadic = f.ftype_variadic;
  }

and find_field_index t f =
  try
    match fst t with
    | T_record {record_fields} ->
      let field = Array.to_list record_fields |>
                  List.find (fun field -> field.field_org_name = f)
      in
      field.field_index

    | T_typedef td -> find_field_index td.typedef_def f

    | _ -> Exceptions.panic "find_field_index: called on a non-record type %s"
             (C_print.string_of_type_qual t)
  with
    Not_found -> Exceptions.panic "find_field_index: field %s not found in type %s"
                   f (C_print.string_of_type_qual t)

and under_type t =
  match fst t with
  | T_pointer t' -> t'
  | T_array(t', _) -> t'
  | T_typedef td -> under_type td.typedef_def
  | _ -> Exceptions.panic "under_type: unsupported type %s"
           (C_print.string_of_type_qual t)

(** {2 Ranges and locations} *)
(** ======================== *)

and from_range (range:C_AST.range) =
  let open Clang_AST in
  let open Location in
  mk_orig_range
    {
      pos_file = range.range_begin.loc_file;
      pos_line = range.range_begin.loc_line;
      pos_column = range.range_begin.loc_column;
    }
    {
      pos_file = range.range_end.loc_file;
      pos_line = range.range_end.loc_line;
      pos_column = range.range_end.loc_column;
    }



(** {2 Stubs annotations} *)
(** ===================== *)

and from_stub_comment ctx f =
  try
    let stub = Mopsa_c_stubs_parser.Main.parse_function_comment f
          ctx.ctx_prj
          ctx.ctx_enums
          ctx.ctx_predicates
          ctx.ctx_stubs in
    Some (from_stub_func ctx f stub)
  with Mopsa_c_stubs_parser.Main.StubNotFound ->
    None

and from_stub_func ctx f stub =
  debug "parsing stub %s" f.func_org_name;
  {
    stub_func_name     = stub.stub_name;
    stub_func_params   = List.map (from_var ctx) (Array.to_list f.func_parameters);
    stub_func_body     = List.map (from_stub_section ctx) stub.stub_body;
    stub_func_range    = stub.stub_range;
    stub_func_locals   = List.map (from_stub_local ctx) stub.stub_locals;
    stub_func_assigns  = List.map (from_stub_assigns ctx) stub.stub_assigns;
    stub_func_return_type =
      match f.func_return with
      | (T_void, _) -> None
      | t -> Some (from_typ ctx t);
  }

and from_stub_section ctx section =
  match section with
  | S_leaf leaf -> S_leaf (from_stub_leaf ctx leaf)
  | S_case case -> S_case (from_stub_case ctx case)

and from_stub_leaf ctx leaf =
  match leaf with
  | S_local local       -> S_local (from_stub_local ctx local)
  | S_assumes assumes   -> S_assumes (from_stub_assumes ctx assumes)
  | S_requires requires -> S_requires (from_stub_requires ctx requires)
  | S_assigns assigns   -> S_assigns (from_stub_assigns ctx assigns)
  | S_ensures ensures   -> S_ensures (from_stub_ensures ctx ensures)
  | S_free free         -> S_free (from_stub_free ctx free)
  | S_message msg       -> S_message (from_stub_message ctx msg)

and from_stub_case ctx case =
  {
    case_label = case.Mopsa_c_stubs_parser.Ast.case_label;
    case_body  = List.map (from_stub_leaf ctx) case.case_body;
    case_locals = List.map (from_stub_local ctx) case.case_locals;
    case_assigns = List.map (from_stub_assigns ctx) case.case_assigns;
    case_range = case.case_range;
  }

and from_stub_requires ctx req =
  bind_range req @@ fun req ->
  from_stub_formula ctx req

and from_stub_free ctx free =
  bind_range free @@ fun free ->
  from_stub_expr ctx free

and from_stub_message ctx msg =
  bind_range msg @@ fun m ->
  { message_kind = from_stub_message_kind m.message_kind;
    message_body = m.message_body; }

and from_stub_message_kind = function
  | WARN    -> WARN
  | UNSOUND -> UNSOUND

and from_stub_assigns ctx assign =
  bind_range assign @@ fun assign ->
  {
    assign_target = from_stub_expr ctx assign.assign_target;
    assign_offset = List.map (from_stub_interval ctx) assign.assign_offset;
  }

and from_stub_local ctx loc =
  bind_range loc @@ fun loc ->
  { lvar = from_var ctx loc.lvar; lval = from_stub_local_value ctx loc.lval }

and from_stub_local_value ctx lval =
  match lval with
  | L_new res -> L_new res
  | L_call (f, args) ->
    let ff = find_function_in_context ctx f.range f.content in
    let t = T_c_function (Some {
          c_ftype_return = from_typ ctx f.content.func_return;
          c_ftype_params = Array.to_list f.content.func_parameters |>
                           List.map (fun p -> from_typ ctx p.var_type);
          c_ftype_variadic = f.content.func_variadic;
        })
    in
    let fff = mk_expr (Ast.E_c_function ff) f.range ~etyp:t in
    L_call (fff, List.map (from_stub_expr ctx) args)

and from_stub_ensures ctx ens =
  bind_range ens @@ fun ens ->
  from_stub_formula ctx ens

and from_stub_assumes ctx asm =
  bind_range asm @@ fun asm ->
  from_stub_formula ctx asm

and from_stub_formula ctx f =
  bind_range f @@ function
  | F_expr e -> F_expr (from_stub_expr ctx e)
  | F_bool b -> F_expr (mk_bool b f.range)
  | F_binop(op, f1, f2) -> F_binop(from_stub_log_binop op, from_stub_formula ctx f1, from_stub_formula ctx f2)
  | F_not f -> F_not (from_stub_formula ctx f)
  | F_forall (v, s, f) -> F_forall(from_var ctx v, from_stub_set ctx s, from_stub_formula ctx f)
  | F_exists (v, s, f) -> F_exists(from_var ctx v, from_stub_set ctx s, from_stub_formula ctx f)
  | F_in (v, s) -> F_in(from_stub_expr ctx v, from_stub_set ctx s)
  | F_otherwise (f, e) -> F_otherwise(from_stub_formula ctx f, from_stub_expr ctx e)
  | F_if (c, f1, f2) -> F_if(from_stub_formula ctx c, from_stub_formula ctx f1, from_stub_formula ctx f2)

and from_stub_set ctx s =
  match s with
  | S_interval i -> S_interval(from_stub_interval ctx i)
  | S_resource r -> S_resource r

and from_stub_interval ctx i =
  let lb = from_stub_expr ctx i.itv_lb in
  let ub = from_stub_expr ctx i.itv_ub in
  (* We can use operations on mathematical integers without worrying about overflows *)
  let lb = if i.itv_open_lb then (add lb one ~typ:T_int lb.erange) else lb in
  let ub = if i.itv_open_ub then (sub ub one ~typ:T_int ub.erange) else ub in
  (lb,ub)

and from_stub_expr ctx exp =
  let bind_range_expr (exp:Mopsa_c_stubs_parser.Ast.expr with_range) f =
    let ekind = f exp.content.kind
    in mk_expr ekind exp.range  ~etyp:(from_typ ctx exp.content.typ)
  in
  bind_range_expr exp @@ function
  | E_top t -> E_constant (C_top (from_typ ctx t))
  | E_int n -> E_constant (C_int n)
  | E_float f -> E_constant (C_float f)
  | E_string s -> E_constant (C_c_string (s, C_char_ascii)) (* FIXME: support other character kinds *)
  | E_char c -> E_constant (C_c_character (Z.of_int c, Ast.C_char_ascii)) (* FIXME: support other character kinds *)
  | E_invalid -> E_constant C_c_invalid
  | E_var v -> E_var (from_var ctx v, None)
  | E_unop (op, e) -> E_unop(from_stub_expr_unop op, from_stub_expr ctx e)
  | E_binop (op, e1, e2) -> E_binop(from_stub_expr_binop op, from_stub_expr ctx e1, from_stub_expr ctx e2)
  | E_addr_of e -> E_c_address_of(from_stub_expr ctx e)
  | E_deref p -> E_c_deref(from_stub_expr ctx p)
  | E_cast (t, explicit, e) -> E_c_cast(from_stub_expr ctx e, explicit)
  | E_subscript (a, i) -> E_c_array_subscript(from_stub_expr ctx a, from_stub_expr ctx i)
  | E_member (s, i, f) -> E_c_member_access(from_stub_expr ctx s, i, f)
  | E_arrow (p, i, f) -> E_c_arrow_access(from_stub_expr ctx p, i, f)
  | E_conditional(c, e1, e2) -> E_c_conditional(from_stub_expr ctx c, from_stub_expr ctx e1, from_stub_expr ctx e2)
  | E_builtin_call (PRIMED, [arg]) -> E_stub_primed(from_stub_expr ctx arg)
  | E_builtin_call (f, args) -> E_stub_builtin_call(f, List.map (from_stub_expr ctx) args)
  | E_return -> E_stub_return
  | E_raise s -> E_stub_raise s

and from_stub_log_binop = function
  | AND -> AND
  | OR -> OR
  | IMPLIES -> IMPLIES

and from_stub_expr_binop = function
  | Mopsa_c_stubs_parser.Cst.ADD -> O_plus
  | SUB -> O_minus
  | MUL -> O_mult
  | DIV -> O_div
  | MOD -> O_mod
  | RSHIFT -> O_bit_rshift
  | LSHIFT -> O_bit_lshift
  | LOR -> O_c_and
  | LAND -> O_c_or
  | LT -> O_lt
  | LE -> O_le
  | GT -> O_gt
  | GE -> O_ge
  | EQ -> O_eq
  | NEQ -> O_ne
  | BOR -> O_bit_or
  | BAND -> O_bit_and
  | BXOR -> O_bit_xor

and from_stub_expr_unop = function
  | Mopsa_c_stubs_parser.Cst.PLUS -> O_plus
  | MINUS -> O_minus
  | LNOT -> O_log_not
  | BNOT -> O_bit_invert


and from_stub_directives ctx com_map =
  C_AST.RangeMap.fold (fun range com acc ->
      try
        let stub = Mopsa_c_stubs_parser.Main.parse_directive_comment
            com
            range
            ctx.ctx_prj
            ctx.ctx_enums
            ctx.ctx_predicates
            ctx.ctx_stubs
        in
        from_stub_directive ctx stub :: acc
      with Mopsa_c_stubs_parser.Main.StubNotFound -> acc
    ) com_map []


and from_stub_directive ctx stub =
  {
    stub_directive_body    = List.map (from_stub_section ctx) stub.stub_body;
    stub_directive_range   = stub.stub_range;
    stub_directive_locals  = List.map (from_stub_local ctx) stub.stub_locals;
    stub_directive_assigns = List.map (from_stub_assigns ctx) stub.stub_assigns;
  }

and from_stub_predicates com_map =
  C_AST.RangeMap.fold (fun range com acc ->
      Mopsa_c_stubs_parser.Main.parse_predicates_comment com |>
      List.fold_left
        (fun acc pred ->
           let name = pred.Mopsa_c_stubs_parser.Passes.Preprocessor.pred_name in
           StringMap.add name pred acc
        ) acc
    ) com_map StringMap.empty

(* Front-end registration *)
let () =
  register_frontend {
    lang = "c";
    parse = parse_program;
  }
