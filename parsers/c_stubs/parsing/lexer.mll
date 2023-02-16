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
open Mopsa_utils
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
     "top",      TOP;
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
     "warn", WARN;
     "unsound", UNSOUND;
     "alias", ALIAS;
     "predicate", PREDICATE;

     (* Operators *)
     "and",  AND;
     "or",  OR;
     "not", NOT;
     "implies", IMPLIES;
     "forall",  FORALL;
     "exists",  EXISTS;
     "in",    IN;
     "otherwise", OTHERWISE;
     (* Types *)
     "void", VOID;
     "char", CHAR;
     "short", SHORT;
     "int", INT;
     "long", LONG;     
     "double", DOUBLE;
     "float", FLOAT;
     "__float128", FLOAT128;
     "signed", SIGNED;
     "unsigned", UNSIGNED;
     "const", CONST;
     "volatile", VOLATILE;
     "restrict", RESTRICT;
     "struct", STRUCT;
     "union", UNION;
     "enum", ENUM;


     (* Built-ins *)
     "primed", PRIMED;  
     "size",   LENGTH;
     "length",   LENGTH;
     "bytes",  BYTES; (* bytes should be renamed as size *)
     "index", INDEX;
     "sizeof_type",   SIZEOF_TYPE;
     "sizeof_expr",   SIZEOF_EXPR;
     "offset", OFFSET;
     "base", BASE;
     "new", NEW;
     "free", FREE;
     "return", RETURN;
     "raise", RAISE;
     "valid_float", VALID_FLOAT;
     "float_inf", FLOAT_INF;
     "float_nan", FLOAT_NAN;
     "cast", CAST;
     "alive", ALIVE;
     "resource", RESOURCE;
     "if", IF;
     "then", THEN;
     "else", ELSE;
     "end", END;
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

   exception EndDelimiterFound

   (* Update lexbuf position after a new line. [s] represents a string places just
      after the new line that was read in the same regexp. *)
   let new_line lexbuf s =
       let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { pos with
            pos_lnum = pos.pos_lnum + 1;
            pos_bol = pos.pos_cnum - (String.length s)
        }

    let token_to_string = function
        | ASSUMES -> "assumes"
        | ASSIGNS -> "assigns"
        | ASSIGN -> "="
        | ARROW -> "->"
        | AND -> "and"
        | ALIVE -> "alive"
        | ALIAS -> "alias"
        | EOF   -> ""
        | COMMA -> ","
        | WARN -> "warn"
        | VOLATILE -> "volatile"
        | VOID -> "void"
        | VALID_FLOAT -> "valid_float"
        | UNSOUND -> "unsound"
        | UNSIGNED -> "unsigned"
        | UNION -> "union"
        | TRUE -> "true"
        | TOP -> "top"
        | STRUCT -> "struct"
        | STRING_CONST s -> "\"" ^ s ^ "\""
        | STAR -> "*"
        | SIZEOF_TYPE -> "sizeof_type"
        | SIZEOF_EXPR -> "sizeof_expr"
        | SIGNED -> "signed"
        | SHORT -> "short"
        | SEMICOL -> ";"
        | SHARP -> "#"
        | RSHIFT -> ">>"
        | RPAR -> ")"
        | RETURN -> "return"
        | RESTRICT -> "restrict"
        | RESOURCE -> "resource"
        | REQUIRES -> "requires"
        | RBRACK -> "]"
        | RBRACE -> "}"
        | RAISE -> "raise"
        | OTHERWISE -> "otherwise"
        | QUESTION -> "?"
        | PRIMED -> "primed"
        | PRIME -> "'"
        | PLUS -> "+"
        | OR -> "or"
        | OFFSET -> "offset"
        | NOT -> "not"
        | NEW -> "new"
        | NEQ -> "!="
        | MOD -> "%"
        | MINUS -> "-"
        | LT -> "<"
        | LSHIFT -> "<<"
        | LPAR -> "("
        | LOR -> "||"
        | LONG -> "long"
        | LOCAL -> "local"
        | LNOT -> "!"
        | LENGTH -> "length"
        | LE -> "<="
        | LBRACK -> "["
        | LBRACE -> "{"
        | LAND -> "&&"
        | INVALID -> "INVALID"
        | INT_CONST (i,_) -> Z.to_string i
        | INT -> "int"
        | INDEX -> "index"
        | IN -> "in"
        | IMPLIES -> "implies"
        | IDENT id -> id
        | GT -> ">"
        | GE -> ">="
        | FREE -> "free"
        | FORALL -> "forall"
        | FLOAT_NAN -> "float_nan"
        | FLOAT_INF -> "float_inf"
        | FLOAT_CONST f -> string_of_float f
        | FLOAT -> "float"
        | FALSE -> "false"
        | EXISTS -> "exists"
        | EQ -> "=="
        | ENUM -> "enum"
        | ENSURES -> "ensures"
        | END_DELIM -> "*/"
        | DOUBLE -> "double"
        | FLOAT128 -> "__float128"
        | DOT -> "."
        | DIV -> "/"
        | CONST -> "const"
        | COLON -> ":"
        | CHAR_CONST c -> Format.asprintf "'\\x%x'" c
        | CHAR -> "char"
        | CAST -> "cast"
        | CASE -> "case"
        | BYTES -> "bytes"
        | BXOR -> "^"
        | BOR -> "|"
        | BNOT -> "~"
        | BEGIN_DELIM -> "/*$"
        | BASE -> "base"
        | BAND -> "&"
        | IF -> "if"
        | THEN -> "then"
        | ELSE -> "else"
        | END -> "end"
        | PREDICATE -> "predicate"
}

let digit = ['0' - '9']
let bindigit = ['0' '1']
let octdigit = ['0' - '7']
let hexdigit = digit | (['a' - 'f' 'A' - 'F'])
let nonzerodigit = ['1' - '9']
let decinteger = nonzerodigit (['_'] | digit)* | '0' (['_'] '0')*
let bininteger = '0' ('b' | 'B') (['_'] | bindigit)+
let octinteger = '0' ('o' | 'O') (['_'] | octdigit)+
let hexinteger = '0' ('x' | 'X') (['_'] | hexdigit)+
let integer = decinteger | bininteger | octinteger | hexinteger

let long_suffix = 'l' | 'L'
let unsigned_long_suffix = "ul" | "UL"
let long_long_suffix = "ll" | "LL"
let unsigned_long_long_suffix = "ull" | "ULL"

let digitpart = digit (['_'] | digit)*
let exponent = ('e' | 'E') ('+' | '-')? digitpart 
let fraction = '.' digitpart
let pointfloat = digitpart* fraction | digitpart '.'
let exponentfloat = (digitpart | pointfloat) exponent
let float = (pointfloat | exponentfloat)

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
                 
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let begin_delimiter = "/*$" | "/*$=" | "/*$!"
let end_delimiter = "*/"

let line_comment = "//" [^ '\n' '\r']*

rule read =
  parse
  | begin_delimiter  { BEGIN_DELIM }
  | end_delimiter    { END_DELIM }


  | white              { read lexbuf }
  | newline (white* end_delimiter as s)    { new_line lexbuf s; END_DELIM }
  | newline (white* '*'? as s) { new_line lexbuf s; read lexbuf }

  | integer unsigned_long_suffix       { INT_CONST (z_of_int_literal (Lexing.lexeme lexbuf), UNSIGNED_LONG) }
  | integer unsigned_long_long_suffix  { INT_CONST (z_of_int_literal (Lexing.lexeme lexbuf), UNSIGNED_LONG_LONG) }
  | integer long_long_suffix           { INT_CONST (z_of_int_literal (Lexing.lexeme lexbuf), LONG_LONG) }
  | integer long_suffix                { INT_CONST (z_of_int_literal (Lexing.lexeme lexbuf), LONG) }
  | integer                            { INT_CONST (z_of_int_literal (Lexing.lexeme lexbuf), NO_SUFFIX) }

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
  | "?"    { QUESTION }
  | "#"    { SHARP }

  | "="    { ASSIGN }

  | "//"   { try read_comment lexbuf; read lexbuf with EndDelimiterFound -> END_DELIM }

  | eof      { EOF }

  | id as x  { try Hashtbl.find keywords x with Not_found -> IDENT x }

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
  | [^ '\n' '\r']      { read_comment lexbuf }
  | newline (white* end_delimiter as s) { new_line lexbuf s; raise EndDelimiterFound }
  | newline (white* '*'? as s) { new_line lexbuf s; () }
  | _ { raise (SyntaxError ("Illegal string character #2: " ^ Lexing.lexeme lexbuf)) }
