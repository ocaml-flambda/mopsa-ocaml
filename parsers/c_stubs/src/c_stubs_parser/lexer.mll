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
     "INVALID",  INVALID;

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
     "signed", UNSIGNED;
     "unsigned", UNSIGNED;
     "const", CONST;
     "struct", STRUCT;
     "union", UNION;
     "enum", ENUM;


     (* Built-ins *)
     "primed", PRIMED;  
     "size",   SIZE;
     "sizeof",   SIZEOF;
     "offset", OFFSET;
     "base", BASE;
     "new", NEW;
     "free", FREE;
     "return", RETURN;
     "ptr_valid", PTR_VALID;
     "valid", PTR_VALID; (* shortcut to ptr_valid *)
     "float_valid", FLOAT_VALID;
     "float_inf", FLOAT_INF;
     "float_nan", FLOAT_NAN;

     (* Deprecated *)
     "old", OLD;
   ]

   let char_for_backslash = function
       | 'n' -> '\010'
       | 'r' -> '\013'
       | 'b' -> '\008'
       | 't' -> '\009'
       | c   -> c

   let decimal_code  c d u =
       100 * (Char.code c - 48) + 10 * (Char.code d - 48) + (Char.code u - 48)

}

let int = '-'? ['0'-'9'] ['0'-'9']*

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

let begin_delimeter = "/*$"
let end_delimeter = "*/"

let line_comment = "//" [^ '\n' '\r']*

rule read =
  parse
  | white    { read lexbuf }
  | newline (white* "*")? { new_line lexbuf; read lexbuf }  
  | int      { INT_CONST (Z.of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT_CONST ((*Format.printf "float %s@\n" (Lexing.lexeme lexbuf);*) float_of_string (Lexing.lexeme lexbuf)) }
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

  | begin_delimeter  { BEGIN }
  | end_delimeter    { END }
  | newline white* end_delimeter    { new_line lexbuf; END }
  
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