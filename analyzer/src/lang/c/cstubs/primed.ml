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

(** Management of primed variables. *)


open Mopsa
open Framework.Core.Sig.Domain.Intermediate
open Universal.Ast
open Stubs.Ast
open Common.Points_to
open Ast
open Zone
open Universal.Zone
open Common.Base



module Domain =
struct

  (** Abstract element *)
  (** ================ *)

  (* We keep the set of primed bases in order to create them once when
     encountering a sequence of assigns clauses. For example, the sequence:

        assigns: a[0];
        assigns: a[1];

     should create a single primed base a'.
  *)

  module BaseSet = Framework.Lattices.Powerset.Make(struct
      type t = base
      let compare = compare_base
      let print = pp_base
    end)

  type t = BaseSet.t


  (** Domain identification *)
  (** ===================== *)

  include GenDomainId(struct
      type nonrec t = t
      let name = "c.cstubs.primed"
    end)

  let interface= {
    iexec = {
      provides = [Z_c];
      uses     = [Z_c];
    };

    ieval = {
      provides = [];
      uses     = [];
    }
  }

  let alarms = []




  (** Initialization of environments *)
  (** ============================== *)

  let init _ _ flow =  flow


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec_stub_assigns target ofssets range man flow =
    assert false


  let exec_stub_rename_primed target offsets range man flow =
    assert false


  let exec zone stmt man flow  =
    match skind stmt with
    | S_stub_assigns(target,offsets) ->
      exec_stub_assigns target offsets stmt.srange man flow |>
      OptionExt.return

    | S_stub_rename_primed(target,offsets) ->
      exec_stub_rename_primed target offsets stmt.srange man flow |>
      OptionExt.return

    | _ -> None



  (** Evaluation of expressions *)
  (** ========================= *)

  let eval_stub_primed e range man flow =
    assert false

  let eval zone exp man flow =
    match ekind exp with
    | E_stub_primed e ->
      eval_stub_primed e exp.erange man flow |>
      OptionExt.return


    | _ -> None

  let ask _ _ _ = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
