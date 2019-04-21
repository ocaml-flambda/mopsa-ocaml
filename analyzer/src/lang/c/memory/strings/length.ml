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

(** String length abstraction.

    This abstract domain implements the technique presented in [1]. It
    abstracts C strings by the position of the first `\0` character in
    the base memory block. The length is kept in an underlying numeric
    domain. Therefore, the domain is implemented as a stack domain, to
    allow sharing the underlying domain with others.

    The domain is stateless. It abstraction is performed by rewriting
    statements/expressions in C into equivalent ones in Universal using
    the length variable.

    [1] M. Journault, A. Miné, A. Ouadjaout. Modular static analysis
    of string manipulations in C programs. SAS 2018. LNCS, vol. 11002.
*)

open Mopsa
open Universal.Ast
open Ast
open Universal.Zone
open Zone
open Common.Base
open Common.Points_to

module Domain =
struct

  (** {2 Domain header} *)
  (** ***************** *)

  let name = "c.memory.strings.length"

  let interface = {
    iexec = {
      provides = [Z_c_low_level];
      uses = [Z_u_num];
    };
    ieval = {
      provides = [Z_c_low_level, Z_u_num];
      uses = [Z_c, Z_c_points_to];
    }
  }

  (** {2 Initialization procedure} *)
  (** **************************** *)
  
  let init prog man flow = flow

  (** {2 Abstract transformers} *)
  (** ************************* *)

  (** Create a length variable *)
  let mk_length_var base range =
    let org_vname =
      let () = Format.fprintf Format.str_formatter "%a_length" pp_base base in
      Format.flush_str_formatter ()
    in
    let vuid = base_uid base in
    let uniq_vname = org_vname ^ ":" ^ (string_of_int vuid) in
    let v = mkv org_vname uniq_vname vuid T_int in
    mk_var v range

  type zero_pos =
    | NotFound of Z.t (** number of consumed bytes *)
    | Found of Z.t
  
  let init_variable length init size range man sman flow =
    (* Find the position of the first zero, or reach the end of the
       allocation space *)
    let rec find_zero i init =
      match init with
      | C_init_expr {ekind = E_constant(C_c_character (c, _))} ->
        if Z.equal c Z.zero then Found i else NotFound Z.one

      | C_init_expr {ekind = E_constant(C_c_string (s, _))} ->
        let rec aux j =
          if Z.add i j |> Z.equal size
          then NotFound j
          else if Z.lt j (Z.of_int @@ String.length s)
          then
            if int_of_char (String.get s (Z.to_int j)) = 0
            then Found Z.(i + j)
            else aux Z.(j + one)
          else NotFound j
        in
        aux Z.zero

      | _ -> assert false
    in
    let byte =
      match find_zero Z.zero init with
      | Found n -> n
      | NotFound _ -> size
    in
    sman.post (mk_assign length (mk_z byte range) range) flow




  (** Declaration of a C variable *)
  let declare_variable v man sman flow =
    let scope, init, range =
      match v.vkind with
      | V_c { var_scope; var_init; var_range } -> var_scope, var_init, var_range
      | _ -> assert false
    in

    let length = mk_length_var (V v) range in

    match scope, init with
    | Variable_global, None
    | Variable_file_static _, None ->
      sman.post (mk_assign length (mk_zero range) range) flow

    | Variable_local _, None
    | Variable_func_static _, None ->
      let size = sizeof_type v.vtyp in
      sman.post (mk_assign length (mk_z_interval Z.zero size range) range) flow

    | _, Some init ->
      let size = sizeof_type v.vtyp in
      init_variable length init size range man sman flow

    | _ -> assert false

  (** Transformers entry point *)
  let exec zone stmt man sman flow =
    match skind stmt with
    | S_c_declaration v -> declare_variable v man sman flow |>
                           Option.return
    | _ -> None
                             
  (** {2 Abstract evaluations} *)
  (** ************************ *)

  (** Evaluations entry point *)
  let eval zone exp man flow =
    panic ~loc:__LOC__ "eval not implemented"

  let ask query man flow = None


end

let () =
  Core.Sig.Stacked.Stateless.register_stack (module Domain)
