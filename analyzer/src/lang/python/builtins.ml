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
 *     Format.printf "%s@." str
 *   ) fmt *)

let common_builtin_range = mk_fresh_range ()

let builtin_range name = tag_range common_builtin_range "%s" name

let object_addr = {
  addr_kind = A_py_class (C_builtin "object", []);
  addr_range = builtin_range "object";
  addr_uid = -1;
}

let modules = ref []
let classes = ref [object_addr]
let functions = ref []
let exceptions = ref []

let all () = !modules @ !classes @ !functions @ !exceptions

let rec builtin_addr_to_name addr =
  match addr.addr_kind with
  | A_py_class(C_builtin(name), _) | A_py_function(F_builtin(name)) | A_py_module(M_builtin(name)) ->
    name

  | A_py_method(cls, func) -> (builtin_addr_to_name cls) ^ "." ^ (builtin_addr_to_name func)

  | _ -> assert false

let from_string name =
  List.find (fun addr -> name = builtin_addr_to_name addr) (all ())

let mk_unname base name =
  match base with
  | None -> name
  | Some base -> base ^ "." ^ name

let from_attribute obj attr =
  let base = from_string obj in
  match base.addr_kind with
  | A_py_module(M_builtin name)
  | A_py_class(C_builtin name, _) ->
    from_string (mk_unname (Some name) attr)

  | _ -> assert false

let is_builtin name = List.exists (fun addr -> name = builtin_addr_to_name addr) (all ())

let is_builtin_module name = List.exists (fun addr -> name = builtin_addr_to_name addr) !modules

let is_builtin_attribute name attr =
  let base = from_string name in
  match base.addr_kind with
  | A_py_module(M_builtin name)
  | A_py_class(C_builtin name, _) ->
    is_builtin (mk_unname (Some name) attr)

  | _ -> false

let mk_builtin_raise exn range = assert false

let mk_builtin_call f params range =
  mk_py_call (mk_addr (from_string f) range) params range


let find_type_class_name = function
  | T_int -> "int"
  | T_float -> "float"
  | T_bool -> "bool"
  | T_string -> "str"
  | _ -> assert false

let find_type_function t f =
  let cls = find_type_class_name t in
  from_attribute cls f

let mro name = assert false

let from_expr exp =
  match ekind exp with
  | E_var v -> from_string v.vname
  | _ -> assert false

let eval_attribute name attr range =
  let addr = from_attribute name attr in
  mk_addr addr range

let mk_builtin_module_addr modl =
  let range = builtin_range modl in
  {
    addr_kind = A_py_module (M_builtin modl);
    addr_range = range;
    addr_uid = -1;
  }


let mk_builtin_class_addr base cls =
  let name = mk_unname base cls.py_cls_var.vname in
  let bases = List.map from_expr cls.py_cls_bases in
  let range = builtin_range name in
  {
    addr_kind = A_py_class (C_builtin name, bases);
    addr_range = range;
    addr_uid = -1;
  }

let mk_builtin_function_addr base fundec =
  let name = mk_unname base fundec.py_func_var.vname in
  let range = builtin_range name in
  {
    addr_kind = A_py_function (F_builtin name);
    addr_range = range;
    addr_uid = -1;
  }

let rec parse_functions base stmt =
  match skind stmt with
  | S_py_function(fundec) ->
    functions := (mk_builtin_function_addr base fundec) :: !functions

  | S_block(block) ->
    List.iter (parse_functions base) block

  | _ -> ()


let rec parse_classes modl stmt =
  match skind stmt with
  | S_py_class(cls) ->
    let addr = mk_builtin_class_addr modl cls in
    classes :=  addr :: !classes;
    let base = mk_unname modl cls.py_cls_var.vname in
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
