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
    abstracts C strings by the position of the first `\0` character within the
    base memory block.

    The length is kept in an underlying numeric domain. Therefore, the domain
    is implemented as a stack domain, to allow sharing the underlying domain
    with others.

    The domain is stateless, because abstraction is performed by rewriting
    statements/expressions in C into equivalent ones in Universal over
    the length variable. Not internal state is required.

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

  (** Domain header *)
  (** ************* *)

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

  let init prog man flow = flow


  (** {2 Length auxiliary variable} *)
  (** ***************************** *)

  let mk_var_length (v:var) : var =
    let org_vname = v.org_vname ^ "_length" in
    let uniq_vname = v.uniq_vname ^ "_length" in
    let vuid = v.vuid in (** FIXME: this is not safe. We should use
                             flow-insensitive contexts to keep bindings of
                             C vars <-> length vars, so we generate new
                             vuid if a binding does not already exists. *)
    mkv org_vname uniq_vname vuid T_int


  (** {2 Abstract transformers} *)
  (** ************************* *)

  (** Initialize the length of a global variable *)
  let init_global_var_length v man flow =
    let init, range = match v.vkind with
      | V_c { var_init; var_range } -> var_init, var_range
      | _ -> assert false
    in

    let l = mk_var (mk_var_length v) ~mode:STRONG range in

    (** Add the variable in the underlying numeric domain *)
    let flow = man.exec ~zone:Z_u_num (mk_add l range) flow in

    match init with
    | None ->
      (** Uninitialized global variables are filled with 0s. See C99 6.7.8.10 *)
      man.exec ~zone:Z_u_num (mk_assign l (mk_zero range) range) flow

    | Some init ->
      assert false


  (** Abstract transformers entry point *)
  let exec zone stmt man flow =
    panic ~loc:__LOC__ "exec not implemented"


  (** {2 Abstract evaluations} *)
  (** ************************ *)

  let eval zone exp man flow =
    panic ~loc:__LOC__ "eval not implemented"


  (** {2 Query handler} *)
  (** ***************** *)

  let ask query man flow = None


end

let () =
  Core.Sig.Stacked.Stateless.register_stack (module Domain)
