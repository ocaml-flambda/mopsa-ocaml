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

%}

%token PERCENT
%token PLUS MINUS ZERO SHARP
%token NUM STAR DOT
%token H HH L LL
%token D I U F G E A P S X O C
%token EOF

%start parse_fprintf
%start parse_fscanf

%type <Placeholder.placeholder list> parse_fprintf
%type <Placeholder.placeholder list> parse_fscanf

%%

(** Parser of fprintf format *)

parse_fprintf:
  |  l=fprintf_place_holder_list EOF { l }

fprintf_place_holder_list:
  | { [] }
  | hd=fprintf_place_holder tl=fprintf_place_holder_list { hd @ tl }

fprintf_place_holder:
  | PERCENT fprintf_flag_list w=fprintf_width p=fprintf_precision t=typ { w @ p @ [t] }


fprintf_flag_list:
  | { }
  | fprintf_flag fprintf_flag_list { }

fprintf_flag:
  | PLUS  {  }
  | MINUS {  }
  | ZERO  {  }
  | SHARP {  }

fprintf_width:
  | NUM   { [] }
  | STAR  { [Int C_unsigned_int] }
  |       { [] }

fprintf_precision:
  | DOT NUM  { [] }
  | DOT STAR { [ Int C_unsigned_int ] }
  |          { [] }

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




(** Parser of fscanf format *)

parse_fscanf:
  | l=fscanf_place_holder_list EOF { l }

fscanf_place_holder_list:
  | { [] }
  | PERCENT STAR fscanf_place_holder tl=fscanf_place_holder_list { tl }
  | PERCENT hd=fscanf_place_holder tl=fscanf_place_holder_list   { hd :: tl }

fscanf_place_holder:
  | fscanf_width t=typ { t }

fscanf_width:
  | NUM {  }
  |     {  }
