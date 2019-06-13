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

(** Evaluation of printf functions *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Universal.Ast
open Ast
open Zone


module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.libs.clib.print"
    end)


  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {
      provides = [];
      uses = []
    };
    ieval = {
      provides = [
        Z_c, Z_c_low_level
      ];
      uses = []
    }
  }


  (** {2 Transfer functions} *)
  (** ====================== *)

  let init _ _ flow =  flow

  let exec zone stmt man flow = None


  (** {2 Evaluation entry point} *)
  (** ========================== *)

  let eval zone exp man flow =
    match ekind exp with

    (* 𝔼⟦ printf(...) ⟧ *)
    | E_c_builtin_call("printf", args)
    | E_c_builtin_call("__printf_chk", args) ->
      warn_at exp.erange "printf: unsound implementation";
      Eval.singleton (mk_top s32 exp.erange) flow |>
      Option.return

    (* 𝔼⟦ fprintf(...) ⟧ *)
    | E_c_builtin_call("fprintf", args)
    | E_c_builtin_call("__fprintf_chk", args) ->
      warn_at exp.erange "fprintf: unsound implementation";
      Eval.singleton (mk_top s32 exp.erange) flow |>
      Option.return

    (* 𝔼⟦ vprintf(...) ⟧ *)
    | E_c_builtin_call("vprintf", args)
    | E_c_builtin_call("__vprintf_chk", args) ->
      warn_at exp.erange "vprintf: unsound implementation";
      Eval.singleton (mk_top s32 exp.erange) flow |>
      Option.return

    (* 𝔼⟦ vfprintf(...) ⟧ *)
    | E_c_builtin_call("vfprintf", args)
    | E_c_builtin_call("__vfprintf_chk", args) ->
      warn_at exp.erange "vfprintf: unsound implementation";
      Eval.singleton (mk_top s32 exp.erange) flow |>
      Option.return

      (* 𝔼⟦ sprintf(...) ⟧ *)
    | E_c_builtin_call("sprintf", args)
    | E_c_builtin_call("__sprintf_chk", args)
    | E_c_builtin_call("__builtin___sprintf_chk", args) ->
      warn_at exp.erange "sprintf: unsound implementation";
      Eval.singleton (mk_top s32 exp.erange) flow |>
      Option.return


    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
