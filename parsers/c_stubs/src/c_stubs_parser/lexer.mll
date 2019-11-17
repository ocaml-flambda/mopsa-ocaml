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
     "INVALID",  INVALID;

     (* Sections *)
     "requires", REQUIRES;
     "local", LOCAL;
     "assumes", ASSUMES;
     "assigns", ASSIGNS;
     "case", CASE;
     "ensures", ENSURES;
     "predicate", PREDICATE;
     "warn", WARN;
     "alias", ALIAS;

     (* Operators *)
     "and",  AND;
     "or",  OR;
     "not", NOT;
     "implies", IMPLIES;
     "forall",  FORALL;
     "exists",  EXISTS;
     "in",    IN;

     (* Types *)
     "void", VOID;
     "char", CHAR;
     "short", SHORT;
     "int", INT;
     "long", LONG;     
     "double", DOUBLE;
     "float", FLOAT;
     "signed", SIGNED;
     "unsigned", UNSIGNED;
     "const", CONST;
     "struct", STRUCT;
     "union", UNION;
     "enum", ENUM;


     (* Built-ins *)
     "primed", PRIMED;  
     "size",   SIZE;
     "bytes",  BYTES;
     "sizeof_type",   SIZEOF_TYPE;
     "sizeof_expr",   SIZEOF_EXPR;
     "offset", OFFSET;
     "base", BASE;
     "new", NEW;
     "free", FREE;
     "return", RETURN;
     "valid_ptr", VALID_PTR;
     "valid", VALID_PTR; (* shortcut to valid_ptr *)
     "valid_float", VALID_FLOAT;
     "float_inf", FLOAT_INF;
     "float_nan", FLOAT_NAN;
   ]

   let char_for_backslash = function
       | 'n' -> '\010'
       | 'r' -> '\013'
       | 'b' -> '\008'
       | 't' -> '\009'
       | c   -> c

   let decimal_code c d u =
       100 * (Char.code c - 48) + 10 * (Char.code d - 48) + (Char.code u - 48)

   (* Return the integer constant in a C string literal *)
   let z_of_int_literal n =
       if Str.string_match (Str.regexp "\\(-?[0-9]+\\).*") n 0 then
              Z.of_string (Str.matched_group 1 n)
       else
	      assert false
}

let int = '-'? ['0'-'9'] ['0'-'9']*

let long_suffix = 'l' | 'L'
let unsigned_long_suffix = "ul" | "UL"
let long_long_suffix = "ll" | "LL"
let unsigned_long_long_suffix = "ull" | "ULL"

let digit = ['0' - '9']
let digitpart = digit (['_'] | digit)*
let exponent = ('e' | 'E') ('+' | '-')? digitpart 
let fraction = '.' digitpart
let pointfloat = digitpart* fraction | digitpart '.'
let exponentfloat = (digitpart | pointfloat) exponent
let float = '-'? (pointfloat | exponentfloat)

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
                 
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let begin_delimeter = "/*$" | "/*$$" | "/*$$$"
let end_delimeter = "*/"

let line_comment = "//" [^ '\n' '\r']*

rule read =
  parse
  | begin_delimeter  { BEGIN }

  | white            { read lexbuf }
  | newline          { new_line lexbuf; skip_line_header lexbuf }

  | int unsigned_long_suffix       { INT_CONST (z_of_int_literal (Lexing.lexeme lexbuf), UNSIGNED_LONG) }
  | int unsigned_long_long_suffix  { INT_CONST (z_of_int_literal (Lexing.lexeme lexbuf), UNSIGNED_LONG_LONG) }
  | int long_long_suffix           { INT_CONST (z_of_int_literal (Lexing.lexeme lexbuf), LONG_LONG) }
  | int long_suffix                { INT_CONST (z_of_int_literal (Lexing.lexeme lexbuf), LONG) }
  | int                            { INT_CONST (z_of_int_literal (Lexing.lexeme lexbuf), NO_SUFFIX) }

  | float    { FLOAT_CONST (float_of_string (Lexing.lexeme lexbuf)) }

  | '"'      { read_string (Buffer.create 17) lexbuf }

  (* Char lexer inspired from https://github.com/let-def/ocamllex/blob/master/lexer.mll *)
  | "'" [^ '\\'] "'"
    { CHAR_CONST (Char.code(Lexing.lexeme_char lexbuf 1)) }
  
  | "'" '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
    { CHAR_CONST (Char.code(char_for_backslash (Lexing.lexeme_char lexbuf 2))) }

  | "'" '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9'] as u) "'"
    { let v = decimal_code c d u in
      if v > 255 then
         raise (SyntaxError ("Illegal escape sequence " ^ Lexing.lexeme lexbuf))
      else
        CHAR_CONST v
    }

  | "'"      { PRIME }

  | id as x  { try Hashtbl.find keywords x with Not_found -> IDENT x }

  | '['      { LBRACK }
  | ']'      { RBRACK }
  | "("      { LPAR }
  | ")"      { RPAR }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | ':'      { COLON }
  | ';'      { SEMICOL }
  | ','      { COMMA }

  | "."      { DOT }
  | "->"     { ARROW }

  | "+"    { PLUS }
  | "-"    { MINUS }
  | "*"    { STAR }
  | "/"    { DIV }
  | "%"    { MOD }
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

  | "//"   { read_comment lexbuf; read lexbuf }
  
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
  | _ { raise (SyntaxError ("Illegal string character #1: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

and read_comment = 
  parse
  | [^ '\n' '\r'] { read_comment lexbuf }
  | newline       { new_line lexbuf; () }
  | _ { raise (SyntaxError ("Illegal string character #2: " ^ Lexing.lexeme lexbuf)) }

and ignore_block_comment = 
  parse
  | "*/"          { () }
  | [^ '\n' '\r'] { ignore_block_comment lexbuf }
  | newline       { new_line lexbuf; ignore_block_comment lexbuf }
  | _ { raise (SyntaxError ("Illegal string character #3: " ^ Lexing.lexeme lexbuf)) }

and skip_line_header =
  parse
  | newline       { new_line lexbuf; skip_line_header lexbuf }
  | white         { skip_line_header lexbuf }
  | "*"           { read lexbuf }
  | end_delimeter { END }
  | _             { read lexbuf }
