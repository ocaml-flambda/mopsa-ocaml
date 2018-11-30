(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

{
open Lexing
open Parser

exception SyntaxError of string

let debug fmt = Debug.debug ~channel:"c_stubs.lexer" fmt

(* keyword table *)
let keywords = Hashtbl.create 10
let _ =
  List.iter (fun (a,b) -> Hashtbl.add keywords a b)
    [
     (* Constants *)
     "true",     TRUE;
     "false",    FALSE;

     (* Sections *)
     "requires", REQUIRES;
     "local", LOCAL;
     "assumes", ASSUMES;
     "assigns", ASSIGNS;
     "case", CASE;
     "ensures", ENSURES;
     "predicate", PREDICATE;

     (* Operators *)
     "and",  AND;
     "or",  OR;
     "implies", IMPLIES;
     "forall",  FORALL;
     "exists",  EXISTS;
     "in",    IN;

     (* Types *)
     "char", CHAR;
     "short", SHORT;
     "int", INT;
     "long", LONG;     
     "double", DOUBLE;
     "float", FLOAT;
     "signed", UNSIGNED;
     "unsigned", UNSIGNED;
     "const", CONST;
     "struct", STRUCT;
     "union", UNION;
     "enum", ENUM;


     (* Built-ins *)
     "old",     OLD;
     "size",   SIZE;
     "offset", OFFSET;
     "base", BASE;
     "new", NEW;
     "free", FREE;
     "return", RETURN;
   ]

}

let int = '-'? ['0'-'9'] ['0'-'9']*

let digit = ['0' - '9']
let digitpart = digit (['_'] | digit)*
let exponent = ('e' | 'E') ('+' | '-')? digitpart 
let fraction = '.' digitpart
let pointfloat = digitpart* fraction | digitpart '.'
let exponentfloat = (digitpart | pointfloat) exponent
let float = pointfloat | exponentfloat

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let newline_star = newline white? ('*' [^ '/'])?

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let begin_delimeter = "/*$"
let end_delimeter = "*/"

let line_comment = "//" [^ '\n' '\r']*

rule read =
  parse
  | white         { read lexbuf }
  | newline_star  { new_line lexbuf; read lexbuf }
  
  | int      { INT_CONST (Z.of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT_CONST (Format.printf "float %s@\n" (Lexing.lexeme lexbuf); float_of_string (Lexing.lexeme lexbuf)) }
  | '"'      { read_string (Buffer.create 17) lexbuf }

  | id as x  { try Hashtbl.find keywords x with Not_found -> IDENT x }

  | '['      { LBRACK }
  | ']'      { RBRACK }
  | "("      { LPAR }
  | ")"      { RPAR }
  | ':'      { COLON }
  | ';'      { SEMICOL }
  | ','      { COMMA }

  | "."      { DOT }
  | "->"     { ARROW }
  | "&"      { ADDROF }

  | "+"    { PLUS }
  | "-"    { MINUS }
  | "*"    { STAR }
  | "/"    { DIV }
  | "<"    { LT }
  | ">"    { GT }
  | "<="   { LE }
  | ">="   { GE }
  | "=="   { EQ }
  | "!="   { NEQ }
  | "&&"   { LAND }
  | "||"   { LOR }
  | "|"    { BOR }
  | "&"    { BAND }
  | "^"    { BXOR }
  | ">>"   { RSHIFT }
  | "<<"   { LSHIFT }
  | "!"    { LNOT }
  | "~"    { BNOT }

  | "="    { ASSIGN }

  | "{*"   { read_comment lexbuf; read lexbuf }

  | begin_delimeter  { BEGIN }
  | end_delimeter    { END }
  
  | line_comment  { read lexbuf }
  | "/*"          { ignore_block_comment lexbuf; read lexbuf } 

  | eof      { EOF }

  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_string buf =
  parse
  | '"'       { STRING_CONST (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

and read_comment = 
  parse
  | "*}"          { () }
  | [^ '\n' '\r'] { read_comment lexbuf }
  | newline       { new_line lexbuf; read_comment lexbuf }

and ignore_block_comment = 
  parse
  | "*/"          { () }
  | [^ '\n' '\r'] { ignore_block_comment lexbuf }
  | newline       { new_line lexbuf; ignore_block_comment lexbuf }
