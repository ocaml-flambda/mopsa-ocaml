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
open Universal.Ast
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

  let rec eval zone exp man flow =
    let range = erange exp in
    match ekind exp with
    (* 𝔼⟦ a[i] ⟧ = *(a + i) *)
    | E_c_array_subscript(a, i) ->
      let t = exp |> etyp |> Ast.pointer_type in
      let exp' = mk_c_deref (mk_binop a O_plus i ~etyp:t range) range in
      Eval.singleton exp' flow |>
      OptionExt.return

    (* 𝔼⟦ s.f ⟧ = *(( typeof(s.f)* )(( char* )(&s) + alignof(s.f))) *)
    | E_c_member_access (s, i, f) ->
      let st = etyp s in
      let t = etyp exp in
      let align = mk_int (align_byte st i) range in

      let exp' =
        mk_c_deref
          (mk_c_cast
             (mk_binop
                (mk_c_cast (mk_c_address_of s range) (pointer_type s8) range)
                O_plus
                align
                range
             )
             (pointer_type t)
             range
          )
          range
      in
      Eval.singleton exp' flow |>
      OptionExt.return

    (* 𝔼⟦ p->f ⟧ = *(( typeof(p->f)* )(( char* )p + alignof(p->f))) *)
    | E_c_arrow_access(p, i, f) ->
      let st = under_pointer_type p.etyp in
      let t = etyp exp in
      let align = mk_int (align_byte st i) range in

      let exp' =
        mk_c_deref
          (mk_c_cast
             (mk_binop
                (mk_c_cast p (pointer_type s8) range)
                O_plus
                align
                range
             )
             (pointer_type t)
             range
          )
          range
      in
      Eval.singleton exp' flow |>
      OptionExt.return


    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
