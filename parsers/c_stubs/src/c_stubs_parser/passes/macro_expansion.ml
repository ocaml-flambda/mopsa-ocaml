(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(* Inline macro definitions *)

let debug fmt = Debug.debug ~channel:"c_stubs.passes.macro_expansion" fmt

let rec visit lexbuf out macros =
  let token = Lexer.read lexbuf in
  let lexeme = Lexing.lexeme lexbuf in
  match token with
  | Parser.EOF ->
    ()

  | Parser.IDENT(id)
    when MapExt.StringMap.mem id macros ->
    let content = MapExt.StringMap.find id macros in
    let lexbuf' = Lexing.from_string content in
    visit lexbuf' out macros;
    visit lexbuf out macros

  | _ ->
    Buffer.add_string out (lexeme ^ " ");
    visit lexbuf out macros


let doit stub prj =
  (* Create a macro table. For the moment, parameters are not supported *)
  let macros = C_AST.StringMap.fold (fun name macro acc ->
      if macro.Clang_AST.macro_params = [] then
        let content = String.concat " " macro.Clang_AST.macro_contents in
        MapExt.StringMap.add name content acc
      else
        acc
    ) prj.C_AST.proj_macros MapExt.StringMap.empty
  in
  let lexbuf = Lexing.from_string stub in
  let out = Buffer.create (String.length stub) in
  visit lexbuf out macros;
  Buffer.contents out
