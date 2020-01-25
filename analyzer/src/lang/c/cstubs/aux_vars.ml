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


(** Common auxiliary variables used by stubs transfer functions *)

open Mopsa
open Universal.Ast
open Stubs.Ast
open Ast



(** {2 Bytes attribute} *)
(** =================== *)

(** The size of stub resource `@` are kept in the C variable `bytes(@)`  *)
type var_kind +=
  | V_c_bytes of addr

let pp_bytes fmt addr =
  Format.fprintf fmt "bytes(%a)" pp_addr addr

let () =
  register_var {
    print = (fun next fmt v ->
        match v.vkind with
        | V_c_bytes addr -> pp_bytes fmt addr
        | _ -> next fmt v
      );

    compare = (fun next v1 v2 ->
        match v1.vkind, v2.vkind with
        | V_c_bytes a1, V_c_bytes a2 -> compare_addr a1 a2
        | _ -> next v1 v2
      );
  }


let mk_bytes_var addr =
  let name =
    let () = pp_bytes Format.str_formatter addr in
    Format.flush_str_formatter ()
  in
  mkv name (V_c_bytes addr) (T_c_integer C_unsigned_long)

let mk_bytes addr range =
  let v = mk_bytes_var addr in
  mk_var v ~mode:addr.addr_mode range
