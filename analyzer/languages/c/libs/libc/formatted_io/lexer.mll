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
  open Parser

let format = ref false

let buffer_from s =
  let b = Buffer.create 12 in
  Buffer.add_string b s;
  b


}

let num = ['0'-'9'] ['0'-'9']*

(* TODO:
   - map z, j, t to size_t, intmax_t, ptrdiff_t instead of long long
   - detect unsupported n, $, *
*)

rule read =
  parse
  | "%%"        { format := false; stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "%"         { format := true; read lexbuf } (*format := true; stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }*)
  | "+"         { if !format then PLUS else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "-"         { if !format then MINUS else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "0"         { if !format then ZERO else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | " "         { if !format then SPACE else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "#"         { if !format then SHARP else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | num         { if !format then NUM (int_of_string (Lexing.lexeme lexbuf)) else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "*"         { if !format then STAR else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "."         { if !format then DOT else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "hh"      	{ if !format then HH else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "h"         { if !format then H else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "ll"        { if !format then LL else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "l"         { if !format then L else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "L"         { if !format then CAP_L else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "z"         { if !format then LL else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "t"         { if !format then LL else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "j"         { if !format then LL else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "d"         { if !format then (format := false; D) else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "i"         { if !format then (format := false; I) else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "u"         { if !format then (format := false; U) else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "f" | "F"   { if !format then (format := false; F) else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "g" | "G"   { if !format then (format := false; G) else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "a" | "A"   { if !format then (format := false; A) else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "p"         { if !format then (format := false; P) else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "s"		      { if !format then (format := false; S) else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "ls"	      { if !format then (format := false; WS) else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "x" | "X"	  { if !format then (format := false; X) else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "o"	  	    { if !format then (format := false; O) else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | "c" 	      { if !format then (format := false; C) else stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }
  | eof         { EOF }
  | _           { format := false; stringer (buffer_from (Lexing.lexeme lexbuf)) lexbuf }

and stringer buf = parse
  | "%%" { Buffer.add_string buf (Lexing.lexeme lexbuf); stringer buf lexbuf }
  | "%" { format := true; TOK_string (Buffer.contents buf) }
  | eof { TOK_string (Buffer.contents buf) }
  | [^ '%']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); stringer buf lexbuf }
