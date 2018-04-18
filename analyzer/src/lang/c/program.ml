(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Main handler of standalone C programs. *)


open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Ast
open Framework.Eval
open Universal.Ast
open Ast

let name = "c.program"
let debug fmt = Debug.debug ~channel:name fmt

(** {2 String symbol table} *)

module StringTable = MapExt.Make(String)

type _ Framework.Context.key +=
  | KStringTable : var StringTable.t Framework.Context.key

let find_string_table ctx =
  Framework.Context.find KStringTable ctx

(** {2 Command line options} *)
let opt_function = ref "main"
(** Name of the function to be analyzed. *)

(** {2 Domain} *)

module Domain =
struct

  let create_string_table prog ctx =
    let range = mk_file_range prog.prog_file in
    let stmt = {skind = S_program prog; srange = range} in

    let counter = ref 0 in

    let table = Framework.Visitor.fold_stmt
        (fun table e ->
           match ekind e with
           | E_constant(C_c_string (s, _)) ->
             if StringTable.mem s table then table
             else
               let v = {
                 vname = "_string_" ^ (string_of_int !counter);
                 vuid = 0;
                 vtyp = type_of_string s;
                 vkind = V_orig;
               }
               in
               incr counter;
               StringTable.add s v table

           | _ -> table
        )
        (fun table stmt -> table)
        StringTable.empty stmt
    in
    table

  let init man ctx prog flow =
    match prog.prog_kind with
    | C_program(globals, funcs) ->
      let table = create_string_table prog ctx in
      let ctx = Framework.Context.add KStringTable table ctx in
      ctx, flow

    | _ -> ctx, flow

  let eval exp manager ctx flow = None

  let get_function_name fundec = fundec.c_func_var.vname

  let is_test fundec =
    let name = get_function_name fundec in
    if String.length name < 5 then false
    else String.sub name 0 4 = "test"

  let get_test_functions functions =
    List.filter is_test functions

  let mk_c_unit_tests file tests =
    let range = mk_file_range file in
    let tests =
      tests |> List.map (fun test ->
          let name = test.c_func_var.vname in
          let range = tag_range range "test %s" name in
          let cleaners = List.mapi (fun i (v, _) ->
              let range = tag_range range "cleaner %d" i in
              mk_remove_var v range
            ) test.c_func_local_vars
          in
          let body = mk_block (test.c_func_body :: cleaners) range in
          (name, body)
        )
    in
    mk_stmt (Universal.Ast.S_unit_tests (file, tests)) range


  let exec manager ctx stmt flow  =
    match skind stmt with
    | S_program({prog_kind = C_program(globals, functions); prog_file})
      when not Framework.Options.(common_options.unit_test_mode) ->
      begin
        try
          let main = List.find (function
                {c_func_var} -> c_func_var.vname = !opt_function
            ) functions
          in
          manager.exec ctx main.c_func_body flow |>
          return
        with Not_found ->
          Framework.Exceptions.panic "function %s not found" !opt_function
      end

    | S_program({prog_kind = C_program(globals, functions); prog_file})
      when Framework.Options.(common_options.unit_test_mode) ->
      let tests = get_test_functions functions in
      let stmt = mk_c_unit_tests prog_file tests in
      return (manager.exec ctx stmt flow)


    | _ -> None

  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain);
  Framework.Context.(register_key_equality {
      case = (let f : type a b. chain -> a key -> b key -> (a, b) eq option =
                fun chain k1 k2 ->
                  match k1, k2 with
                  | KStringTable, KStringTable -> Some Eq
                  | _ -> chain.check k1 k2
              in
              f);
    });
  Framework.Options.register (
    "-c-function", Arg.Set_string opt_function, " name of the function to be analyzed (default: main)"
  )
