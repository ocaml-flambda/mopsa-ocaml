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

(** {2 Domain definition} *)
(** ===================== *)

module Domain : Framework.Domains.Stateless.S =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += Dc_desugar_records : unit domain
  let id = Dc_desugar_records
  let name = "c.desugar.records"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | Dc_desugar_records -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {
    export = [Framework.Zone.Z_top];
    import = [Framework.Zone.Z_top]
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
      let range = srange stmt in
      let t1 = lval |> etyp |> remove_typedef |> remove_qual
      and t2 = lval |> etyp |> remove_typedef |> remove_qual in
      if compare t1 t2 != 0 then
        Debug.fail "[%s] assignment of records with uncompatible \
                     types: %a %a" name pp_typ t1 pp_typ t2
      else
        begin
          let fields = match t1 with
            | T_c_record{c_record_fields} -> c_record_fields
            | _ -> assert false
          in
          fields |> List.fold_left (fun flow field ->
              let lval = mk_c_member_access lval field range in
              let rval = mk_c_member_access rval field range in
              let stmt = {stmt with skind = S_assign(lval, rval)} in
              man.exec ~zone:(Framework.Zone.Z_top) stmt flow
            ) flow
          |> Post.of_flow
          |> Option.return
        end
    | _ -> None

  (** Queries *)
  (** ******* *)

  let ask _ _ _  = None

end
