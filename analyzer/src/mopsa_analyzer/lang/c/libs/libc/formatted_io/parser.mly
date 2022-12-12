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

(** Parser of C stubs *)

%{
  open Ast
  open Placeholder


  let mk_output_placeholder width precision typ =
    let mk w p t = {
	op_width = w;
	op_precision = p;
	op_typ = t;
      }
    in
    let width_arg_placeholder = match snd width with
      | true -> [mk None None (Int C_unsigned_int)]
      | _ -> []
    in
    let precision_arg_placeholder = match snd precision with
      | true -> [mk None None (Int C_unsigned_int)]
      | _ -> []
    in
    width_arg_placeholder @ precision_arg_placeholder @ [mk (fst width) (fst precision) typ]


  let mk_input_placeholder width typ =
    {
      ip_width = width;
      ip_typ = typ;
    }

%}

%token PLUS MINUS ZERO SHARP SPACE
%token <int> NUM
%token STAR DOT
%token H HH L LL CAP_L
%token D I U F G E A P S WS X O C
%token EOF

%start parse_output_format
%start parse_input_format

%type <Placeholder.output_placeholder list> parse_output_format
%type <Placeholder.input_placeholder list> parse_input_format

%type<input_placeholder> input_placeholder
%type<input_placeholder list> input_placeholder_list
%type<placeholder_type> input_typ
%type<int option> input_width
%type<unit> output_flag output_flag_list
%type<output_placeholder list> output_placeholder output_placeholder_list
%type<int option * bool> output_precision
%type<placeholder_type> output_typ
%type<int option * bool> output_width

%%


(** Parser of output formats *)

parse_output_format:
  |  l=output_placeholder_list EOF { l }

output_placeholder_list:
  | { [] }
  | hd=output_placeholder tl=output_placeholder_list { hd @ tl }

output_placeholder:
  | output_flag_list w=output_width p=output_precision t=output_typ { mk_output_placeholder w p t }


output_flag_list:
  | { }
  | output_flag output_flag_list { }

output_flag:
  | PLUS  {  }
  | MINUS {  }
  | ZERO  {  }
  | SHARP {  }
  | SPACE {  }

output_width:
  | n=NUM { Some n, false }
  | STAR  { None, true }
  |       { None, false }

output_precision:
  | DOT n=NUM { Some n, false }
  | DOT       { Some 0, false }
  | DOT STAR  { None, true }
  |           { None, false }

output_typ:
  (* Char *)
  | C                      { Int C_unsigned_char }

  (* Signed integers *)
  | HH D | HH I            { Int C_signed_char }
  | H D  | H I             { Int C_signed_short }
  | D    | I               { Int C_signed_int }
  | L D  | L I             { Int C_signed_long }
  | LL D | LL I            { Int C_signed_long_long }

  (* Unsigned integers *)
  | HH U | HH X | HH O     { Int C_unsigned_char }
  | H U  | H X  | H O      { Int C_unsigned_short }
  | U    | X    | O        { Int C_unsigned_int }
  | L U  | L X  | L O      { Int C_unsigned_long }
  | LL U | LL X | LL O     { Int C_unsigned_long_long }

  (* Floats *)
  | E | F | G | A          { Float C_double }
  | CAP_L E | CAP_L F | CAP_L G | CAP_L A  { Float C_long_double }

  (* String and pointers *)
  | S                      { String }
  | WS                     { WideString }
  | P                      { Pointer }




(** Parser of input format *)

parse_input_format:
  | l=input_placeholder_list EOF { l }

input_placeholder_list:
  | { [] }
  | STAR input_placeholder tl=input_placeholder_list { tl }
  | hd=input_placeholder tl=input_placeholder_list   { hd :: tl }

input_placeholder:
  | w=input_width t=input_typ { mk_input_placeholder w t }

input_width:
  | n=NUM { Some n }
  |       { None }

input_typ:
  (* Char *)
  | C                      { Int C_unsigned_char }

  (* Signed integers *)
  | HH D | HH I            { Int C_signed_char }
  | H D  | H I             { Int C_signed_short }
  | D    | I               { Int C_signed_int }
  | L D  | L I             { Int C_signed_long }
  | CAP_L D | CAP_L I      { Int C_signed_long_long }

  (* Unsigned integers *)
  | HH U | HH X | HH O           { Int C_unsigned_char }
  | H U  | H X  | H O            { Int C_unsigned_short }
  | U    | X    | O              { Int C_unsigned_int }
  | L U  | L X  | L O            { Int C_unsigned_long }
  | CAP_L U | CAP_L X | CAP_L O  { Int C_unsigned_long_long }

  (* Floats *)
  | E | F | G | A                          { Float C_float }
  | L E | L F | L G | L A                  { Float C_double }
  | CAP_L E | CAP_L F | CAP_L G | CAP_L A  { Float C_long_double }

  (* String and pointers *)
  | S                      { String }
  | P                      { Pointer }
