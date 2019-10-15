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

(** Evaluation of scanf functions *)

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
      let name = "c.libs.clib.scanf"
    end)


  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {
      provides = [];
      uses = [Z_c]
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

  exception UnsupportedFormat

  let rec parse format i args range man flow =
    if String.length format = i
    then Post.return flow
    else
      match format.[i] with
      | ' '
      | '\\' -> parse format (i + 2) args range man flow
      | '%' -> parse_type format (i + 1) args range man flow
      | _ -> raise UnsupportedFormat

  and parse_type format i args range man flow =
    if format.[i] = '%'
    then parse format (i + 1) args range man flow
    else
      let t, i' = match format.[i] with
        | 'c' -> s8, i+1
        | 'd' -> s32, i+1
        | 'u' -> u32, i+1
        | 'l' when format.[i+1] = 'd' -> sl, i+2
        | 'l' when format.[i+1] = 'u' -> ul, i+2
        | 'l' when format.[i+1] = 'l' && format.[i+2] = 'd' -> sll, i+3
        | 'l' when format.[i+1] = 'l' && format.[i+2] = 'u' -> ull, i+3
        | 'h' when format.[i+1] = 'd' -> s16, i+2
        | 'h' when format.[i+1] = 'u' -> u16, i+2
        | 'f' -> T_c_float C_float, i+1
        | 'l' when format.[i+1] = 'f' -> T_c_float C_double, i+2
        | 'L' when format.[i+1] = 'f' -> T_c_float C_long_double, i+2
        | _ -> raise UnsupportedFormat
      in
      let hd = List.hd args in
      let ptr = T_c_pointer t in
      man.post (mk_assign (mk_c_deref (mk_c_cast hd ptr range) range) (mk_top t range) range) flow >>= fun _ flow ->
      parse format i' (List.tl args) range man flow


  let assign_args_from_string_format (format:string) args range man flow =
    try parse format 0 args range man flow
    with UnsupportedFormat ->
      Soundness.warn_at range "ignoring side-effect of scanf format %s" format;
      Post.return flow



  (** Assign arbitrary values to arguments *)
  let assign_args format args range man flow =
    match ekind (remove_casts format) with
    | E_constant(C_c_string (str,_)) -> assign_args_from_string_format str args range man flow
    | _ ->
      Soundness.warn_at range "ignoring side-effect of scanf format %a" pp_expr format;
      Post.return flow


  (** Evaluation entry point *)
  let eval zone exp man flow =
    match ekind exp with

    (* 𝔼⟦ scanf ⟧ *)
    | E_c_builtin_call("scanf", format :: args) ->
      assign_args format args exp.erange man flow >>$? fun _ flow ->
      Eval.singleton (mk_top s32 exp.erange) flow |>
      Option.return

    (* 𝔼⟦ fscanf ⟧ *)
    | E_c_builtin_call("fscanf", stream :: format :: args) ->
      assign_args format args exp.erange man flow >>$? fun _ flow ->
      Eval.singleton (mk_top s32 exp.erange) flow |>
      Option.return

      (* 𝔼⟦ sscanf ⟧ *)
    | E_c_builtin_call("sscanf", src :: format :: args) ->
      assign_args format args exp.erange man flow >>$? fun _ flow ->
      Eval.singleton (mk_top s32 exp.erange) flow |>
      Option.return


    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
