(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Removal of syntaxic sugar in record assignment *)

open Framework.Essentials
open Ast
open Zone

(** {2 Domain definition} *)
(** ===================== *)

module Domain : Framework.Domains.Stateless.S =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += Dc_desugar_record_copy : unit domain
  let id = Dc_desugar_record_copy
  let name = "c.desugar.record_copy"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | Dc_desugar_record_copy -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {
    export = [Z_c];
    import = [Z_c]
  }

  let eval_interface = {
    export = [];
    import = []
  }

  (** Initialization *)
  (** ============== *)

  let init _ _ _ =
    None

  (** Evaluations *)
  (** *********** *)

  let eval _ _ _ _ = None

  (** Post-conditions *)
  (** *************** *)

  let exec zone stmt man flow =
    match skind stmt with
    | S_assign(lval, rval)
      when lval |> etyp |> is_c_record_type &&
           rval |> etyp |> is_c_record_type ->
      begin
        let range = srange stmt in
        let t1 = lval |> etyp |> remove_typedef |> remove_qual
        and t2 = lval |> etyp |> remove_typedef |> remove_qual in
        if compare t1 t2 != 0 then
          Debug.fail "[%s] assignment of records with uncompatible \
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
                  man.exec ~zone:Z_c stmt flow
                ) flow
              |> Post.of_flow
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
                  man.exec ~zone:Z_c stmt flow
                  |> Post.of_flow
                  |> Option.return
                | None -> Debug.fail "[%s] all fields have size 0" name
              end
          end
      end
    | _ -> None

  (** Queries *)
  (** ******* *)

  let ask _ _ _  = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
