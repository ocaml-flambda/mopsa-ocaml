(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** array access and structure member access transformation to pointer
   arithmetic*)

open Framework.Essentials
open Ast

(** {2 Domain definition} *)
(** ===================== *)

module Domain : Framework.Domains.Stateless.S =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_desugar_aggregate : unit domain
  let id = D_c_desugar_aggregate
  let name = "c.desugar.aggregate"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_desugar_aggregate -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {
    export = [];
    import = []
  }

  let eval_interface = {
    export = [Zone.Z_c, Zone.Z_c_scalar];
    import = []
  }

  (** Initialization *)
  (** ============== *)

  let init _ _ _ =
    None

  let exec _ _ _ _ = None

  let eval zone exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_c_array_subscript(e1, e2) ->
      let t = exp |> etyp |> Ast.pointer_type in
      Eval.singleton (
        {exp with
         ekind = E_c_deref (
             mk_expr
               ~etyp:t
               (E_binop(Universal.Ast.O_plus , e1, e2))
               (tag_range range "pointer_arithmetic")
           )
        }) flow
      |> Option.return
    | E_c_member_access (e, i, f) ->
      let t = etyp e in
      if is_c_record_type t then
        let x = align_byte t i in
        let exp =
          {exp with ekind =
                      E_c_deref (
                        mk_expr
                          ~etyp:(t |> Ast.pointer_type)
                          (E_binop(
                              Universal.Ast.O_plus,
                              (
                                (mk_c_cast
                                   (mk_c_address_of e (tag_range range "pointer_arithmetic &"))
                                   (T_c_integer C_signed_char |> Ast.pointer_type)
                                   (tag_range range "pointer_arithmetic cast")
                                )
                              ),
                              (Universal.Ast.mk_int x (tag_range range "pointer_arithmetic int"))
                            )
                          )
                          (tag_range range "pointer_arithmetic+")
                      )
          }
        in
        Eval.singleton exp flow
        |> Option.return
      else
        Debug.fail "[c.desugar.aggregate] member_access on non record type"
    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
