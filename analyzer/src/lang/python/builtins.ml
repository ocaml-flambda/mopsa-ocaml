(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Language builtins. *)

open Framework.Ast
open Universal.Ast
open Ast
open Addr


(* let debug fmt = Format.kasprintf (fun str ->
 *     Format.printf "builtins: %s@." str
 *   ) fmt *)

let common_builtin_range = mk_fresh_range ()

let builtin_range name = tag_range common_builtin_range "%s" name

let mk_builtin_module_addr modl =
  let range = builtin_range modl in
  {
    addr_kind = A_py_module (M_builtin modl);
    addr_range = range;
    addr_uid = -1;
  }


let mk_builtin_class_addr base cls =
  let name = mk_dot_name base cls.py_cls_var.vname in
  let bases = List.map (fun base ->
      match ekind base with
      | E_var v -> find_builtin v.vname
      | _ -> assert false
    ) cls.py_cls_bases in
  let range = builtin_range name in
  {
    addr_kind = A_py_class (C_builtin name, bases);
    addr_range = range;
    addr_uid = -1;
  }

let mk_unsupported_class_addr base cls =
  let name = mk_dot_name base cls.py_cls_var.vname in
  let bases = List.map (fun base ->
      match ekind base with
      | E_var v -> find_builtin v.vname
      | _ -> assert false
    ) cls.py_cls_bases in
  let range = builtin_range name in
  {
    addr_kind = A_py_class (C_unsupported name, bases);
    addr_range = range;
    addr_uid = -1;
  }

let mk_builtin_function_addr base fundec =
  let name = mk_dot_name base fundec.py_func_var.vname in
  let range = builtin_range name in
  {
    addr_kind = A_py_function (F_builtin name);
    addr_range = range;
    addr_uid = -1;
  }

let mk_stub_function_addr base fundec =
  let name = mk_dot_name base fundec.py_func_var.vname in
  let fundec = {fundec with py_func_var = {fundec.py_func_var with vname = name}} in
  let range = builtin_range name in
  {
    addr_kind = A_py_function (F_user fundec);
    addr_range = range;
    addr_uid = -1;
  }

let mk_unsupported_function_addr base fundec =
  let name = mk_dot_name base fundec.py_func_var.vname in
  let range = builtin_range name in
  {
    addr_kind = A_py_function (F_unsupported name);
    addr_range = range;
    addr_uid = -1;
  }


let is_stub_decorator d =
  match ekind d with
  | E_py_attribute({ekind = E_var {vname = "mopsa"}}, "stub") -> true
  | _ -> false

let is_unsupported_decorator d =
  match ekind d with
  | E_py_attribute({ekind = E_var {vname = "mopsa"}}, "unsupported") -> true
  | _ -> false

let rec parse_functions base stmt =
  match skind stmt with
  | S_py_function({py_func_decors = []} as fundec) ->
    functions := (mk_builtin_function_addr base fundec) :: !functions

  | S_py_function({py_func_decors = [d]} as fundec) when is_stub_decorator d ->
    functions := (mk_stub_function_addr base fundec) :: !functions

  | S_py_function({py_func_decors = [d]} as fundec) when is_unsupported_decorator d ->
    functions := (mk_unsupported_function_addr base fundec) :: !functions

  | S_block(block) ->
    List.iter (parse_functions base) block

  | _ -> ()


let rec parse_classes modl stmt =
  match skind stmt with
  | S_py_class({py_cls_decors = [d]} as cls) when is_unsupported_decorator d ->
    let addr = mk_unsupported_class_addr modl cls in
    classes :=  addr :: !classes;

  | S_py_class(cls)->
    let addr = mk_builtin_class_addr modl cls in
    classes :=  addr :: !classes;
    let base = mk_dot_name modl cls.py_cls_var.vname in
    parse_functions (Some base) cls.py_cls_body

  | S_block(block) ->
    List.iter (parse_classes modl) block

  | _ -> ()

let rec parse_exceptions stmt =
  match skind stmt with
  | S_py_class(cls) ->
    let addr = mk_builtin_class_addr None cls in
    exceptions :=  addr :: !exceptions

  | S_block(block) ->
    List.iter parse_exceptions block

  | _ -> ()


let init_std_classes f =
  let body = Frontend.parse_file f in
  parse_classes None body

let init_std_functions f =
  let body = Frontend.parse_file f in
  parse_functions None body

let init_std_exceptions f =
  let body = Frontend.parse_file f in
  parse_exceptions body

let init_std dir =
  let fclass = dir ^ "/std_classes.py" in
  init_std_classes fclass;

  let ffun = dir ^ "/std_functions.py" in
  init_std_functions ffun;

  let fexn = dir ^ "/std_exceptions.py" in
  init_std_exceptions fexn;
  ()

let init_module filename =
  let body = Frontend.parse_file filename in
  let modl = Filename.basename filename |> Filename.remove_extension in
  modules := (mk_builtin_module_addr modl) :: !modules;
  parse_classes (Some modl) body;
  parse_functions (Some modl) body

let init_modules dir =
  Sys.readdir dir |>
  Array.iter (fun stub ->
      let filename = Filename.basename stub in
      if filename <> "std_classes.py" && filename <> "std_functions.py" && filename <> "std_exceptions.py" then
        init_module (dir ^ "/" ^ stub)
      else
        ()
    )

let setup () =
  Framework.Options.(common_options.stubs) |> List.iter
    (fun dir ->
       init_std dir;
       init_modules dir;
    );
  ()
