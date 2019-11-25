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

%token PLUS MINUS ZERO SHARP
%token <int> NUM
%token STAR DOT
%token H HH L LL
%token D I U F G E A P S X O C
%token EOF

%start parse_output_format
%start parse_input_format

%type <Placeholder.output_placeholder list> parse_output_format
%type <Placeholder.intput_placeholder list> parse_input_format

%%


(** Parser of output formats *)

parse_output_format:
  |  l=output_placeholder_list EOF { l }

output_placeholder_list:
  | { [] }
  | hd=output_placeholder tl=output_placeholder_list { hd @ tl }

output_placeholder:
  | output_flag_list w=output_width p=output_precision t=typ { mk_output_placeholder w p t }


output_flag_list:
  | { }
  | output_flag output_flag_list { }

output_flag:
  | PLUS  {  }
  | MINUS {  }
  | ZERO  {  }
  | SHARP {  }

output_width:
  | n=NUM { Some n, false }
  | STAR  { None, true }
  |       { None, false }

output_precision:
  | DOT n=NUM { Some n, false }
  | DOT STAR  { None, true }
  |           { None, false }

typ:
  (* Char *)
  | C                      { Int C_signed_char }

  (* Signed integers *)
  | D | H D | HH D         { Int C_signed_int }
  | I | H I | HH I         { Int C_signed_int }
  | L D | L I              { Int C_signed_long }
  | LL D | LL I            { Int C_signed_long_long }

  (* Unsigned integers *)
  | U | H U | HH U | X | O { Int C_unsigned_int }
  | L U                    { Int C_unsigned_long }
  | LL U                   { Int C_unsigned_long_long }

  (* Floats *)
  | F | L F | E | L E | G | L G | A | L A { Float C_double }

  (* String and pointers *)
  | S                      { String }
  | P                      { Pointer }




(** Parser of input format *)

parse_input_format:
  | l=input_placeholder_list EOF { l }

input_placeholder_list:
  | { [] }
  | STAR input_placeholder tl=input_placeholder_list { tl }
  | hd=input_placeholder tl=input_placeholder_list   { hd :: tl }

input_placeholder:
  | w=input_width t=typ { mk_input_placeholder w t }

input_width:
  | n=NUM { Some n }
  |       { None }
