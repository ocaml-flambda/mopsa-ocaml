(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(* Inline macro definitions *)

let debug fmt = Debug.debug ~channel:"c_stubs.passes.macro_expansion" fmt


let token_to_string = function
  | Parser.INT_CONST(n) -> Z.to_string n
  | FLOAT_CONST f -> string_of_float f
  | CHAR_CONST n ->
    let s =
      let ss = string_of_int n in
      if n < 10 then "00" ^ ss else
      if n < 100 then "0" ^ ss
      else ss
    in
    "'\\" ^ s ^ "'"
  | STRING_CONST s -> "\"" ^ s ^ "\""
  | INVALID -> "INVALID"
  | IDENT x -> x
  | AND -> "and"
  | OR -> "or"
  | IMPLIES -> "implies"
  | NOT -> "not"
  | ASSIGN -> "="
  | PLUS -> "+"
  | MINUS -> "-"
  | DIV -> "/"
  | MOD -> "%"
  | LAND -> "&&"
  | LOR -> "||"
  | BAND -> "&"
  | BOR -> "|"
  | LNOT -> "!"
  | BNOT -> "~"
  | ARROW -> "->"
  | STAR -> "*"
  | GT -> ">"
  | GE -> ">="
  | LT -> "<"
  | LE -> "<="
  | EQ -> "=="
  | NEQ -> "!="
  | RSHIFT -> ">>"
  | LSHIFT -> "<<"
  | BXOR -> "^"
  | LPAR -> "("
  | RPAR -> ")"
  | LBRACK -> "["
  | RBRACK -> "]"
  | COLON -> ":"
  | SEMICOL -> ";"
  | DOT -> "."
  | COMMA -> ","
  | PRIME -> "'"
  | BEGIN -> "/*$"
  | END -> "*/"
  | EOF -> ""
  | REQUIRES -> "requires"
  | LOCAL -> "local"
  | ASSIGNS -> "assigns"
  | CASE -> "case"
  | ASSUMES -> "assumes"
  | ENSURES -> "ensures"
  | PREDICATE -> "predicate"
  | TRUE -> "true"
  | FALSE -> "false"
  | FORALL -> "forall"
  | EXISTS -> "exists"
  | IN -> "in"
  | NEW -> "new"
  | FREE -> "free"
  | PRIMED -> "primed"
  | RETURN  -> "return"
  | SIZE  -> "size"
  | SIZEOF  -> "sizeof"
  | OFFSET  -> "offset"
  | BASE  -> "base"
  | PTR_VALID  -> "valid"
  | FLOAT_VALID  -> "float_valid"
  | FLOAT_INF -> "float_inf"
  | FLOAT_NAN -> "float_nan"
  | OLD -> "old"
  | VOID -> "void"
  | CHAR  -> "char"
  | INT  -> "int"
  | LONG  -> "long"
  | FLOAT  -> "float"
  | DOUBLE  -> "double"
  | SHORT  -> "short"
  | SIGNED -> "signed"
  | UNSIGNED  -> "unsigned"
  | CONST  -> "const"
  | STRUCT -> "struct"
  | UNION  -> "union"
  | ENUM  -> "enum"


let rec visit_macros lexbuf found out macros =
  let token = Lexer.read lexbuf in
  let lexeme = token_to_string token in
  match token with
  | Parser.EOF ->
    found

  | Parser.IDENT(id)
    when MapExt.StringMap.mem id macros ->
    let content = MapExt.StringMap.find id macros in
    let lexbuf' = Lexing.from_string content in
    let _ = visit_macros lexbuf' true out macros in
    let _ = visit_macros lexbuf true out macros in
    true

  | _ ->
    Buffer.add_string out (lexeme ^ " ");
    visit_macros lexbuf found out macros


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
  let macros_found = visit_macros lexbuf false out macros in
  let stub' = if macros_found then Buffer.contents out else stub in
  stub'
