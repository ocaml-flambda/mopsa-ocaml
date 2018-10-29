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

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white    { read lexbuf }
  | newline  { new_line lexbuf; read lexbuf }
  
  | int      { INT (Z.of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | '"'      { read_string (Buffer.create 17) lexbuf }

  | id as x  { try Hashtbl.find keywords x with Not_found -> IDENT x }

  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | '['      { LBRACK }
  | ']'      { RBRACK }
  | "("      { LPAR }
  | ")"      { RPAR }
  | ':'      { COLON }
  | ';'      { SEMICOL }

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

  | "(*" { read_comment lexbuf; read lexbuf }

  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
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

and read_comment = parse
| "*)" { () }
| [^ '\n' '\r'] { read_comment lexbuf }
| newline { new_line lexbuf; read_comment lexbuf }
