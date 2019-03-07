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

(** Extensible type of programs *)

type prog_kind = ..

type program = {
  prog_kind : prog_kind;
  prog_range : Location.range;
}

let program_pp_chain = TypeExt.mk_print_chain (fun fmt prg ->
    Exceptions.panic "program_pp_chain: unknown program"
  )

let program_compare_chain = TypeExt.mk_compare_chain (fun p1 p2 ->
    compare p1 p2
  )

let register_program (info:program TypeExt.info) =
  TypeExt.register info program_compare_chain program_pp_chain

let compare_program p1 p2 = TypeExt.compare program_compare_chain p1 p2

let pp_program fmt program = TypeExt.print program_pp_chain fmt program
