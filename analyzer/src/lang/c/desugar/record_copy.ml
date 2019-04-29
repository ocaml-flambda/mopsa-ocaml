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

(** Removal of syntaxic sugar in record assignment *)

open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Ast
open Zone

(** {2 Domain definition} *)
(** ===================== *)

module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  let name = "c.desugar.record_copy"
  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {
      provides = [Z_c];
      uses = []
    };

    ieval = {
      provides = [];
      uses = []
    }
  }

  (** Initialization *)
  (** ============== *)

  let init _ _ flow = flow

  (** Evaluations *)
  (** *********** *)

  let eval _ _ _ _ = None

  (** Post-conditions *)
  (** *************** *)

  let rec remove_casts e =
    match ekind e with
    | E_c_cast(e', _) -> remove_casts e'
    | _ -> e

  let rec exec zone stmt man flow =
    match skind stmt with
    | S_c_declaration ({ vkind = V_c { var_init = Some (C_init_expr rval) } } as v)
      when v.vtyp |> remove_typedef_qual |> is_c_record_type  ->
      exec zone (mk_assign (mk_var v stmt.srange) rval stmt.srange) man flow

    | S_assign(lval, rval)
      when lval |> etyp |> remove_typedef_qual |> is_c_record_type &&
           rval |> etyp |> remove_typedef_qual |> is_c_record_type ->
      begin
        debug "assign record";
        let range = srange stmt in
        let rval = remove_casts rval in

        let t1 = lval |> etyp |> remove_typedef_qual
        and t2 = rval |> etyp |> remove_typedef_qual in

        if compare_typ t1 t2 <> 0 then
          Exceptions.panic "[%s] assignment of records with uncompatible \
                            types: %a %a" name pp_typ t1 pp_typ t2
        else
          begin
            let fields, record_kind = match t1 with
              | T_c_record{c_record_fields; c_record_kind} -> c_record_fields, c_record_kind
              | _ -> assert false
            in
            match record_kind with
            | C_struct ->
              fields |> List.fold_left (fun flow field ->
                  let lval = mk_c_member_access lval field range in
                  let rval = mk_c_member_access rval field range in
                  let stmt = {stmt with skind = S_assign(lval, rval)} in
                  man.exec stmt flow
                ) flow
              |> Post.return
              |> Option.return
            | C_union ->
              begin
                let fieldopt, _ = List.fold_left (fun (accfield, accsize) field ->
                    let size = field.c_field_type |> sizeof_type in
                    if Z.geq size accsize then
                      (Some field, size)
                    else (accfield, accsize)
                  ) (None, Z.zero) fields
                in
                match fieldopt with
                | Some field ->
                  let lval = mk_c_member_access lval field range in
                  let rval = mk_c_member_access rval field range in
                  let stmt = {stmt with skind = S_assign(lval, rval)} in
                  man.exec stmt flow
                  |> Post.return
                  |> Option.return
                | None -> Exceptions.panic "[%s] all fields have size 0" name
              end
          end
      end
    | _ -> None

  (** Queries *)
  (** ******* *)

  let ask _ _ _  = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
