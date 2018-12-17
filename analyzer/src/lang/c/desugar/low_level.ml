(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)


open Mopsa
open Universal.Ast
open Ast
open Zone

(** {2 Domain definition} *)
(** ===================== *)

module Domain : Framework.Domains.Stateless.S =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_desugar_low_level : unit domain
  let id = D_c_desugar_low_level
  let name = "c.desugar.low_level"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_desugar_low_level -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {
    export = [];
    import = [Z_c]
  }

  let eval_interface = {
    export = [Z_c, Z_c_low_level];
    import = [Z_c, Z_c_low_level]
  }

  (** Initialization *)
  (** ============== *)

  let init _ _ _ =
    None

  let exec _ _ _ _ = None

  let rec eval zone exp man flow =
    match ekind exp with
    (* 𝔼⟦ a[i] ⟧ = *(a + i) *)
    | E_c_array_subscript(a, i) ->
      man.eval ~zone:(Z_c, Z_c_low_level) a flow |> Eval.bind_return @@ fun a flow ->
      man.eval ~zone:(Z_c, Z_c_low_level) i flow |> Eval.bind @@ fun i flow ->

      let t = exp |> etyp |> Ast.pointer_type in
      let exp' = mk_c_deref (mk_binop a O_plus i ~etyp:t exp.erange) exp.erange in

      Eval.singleton exp' flow

    (* 𝔼⟦ s.f ⟧ = *(( typeof(s.f)* )(( char* )(&s) + alignof(s.f))) *)
    | E_c_member_access (s, i, f) ->
      let ss = mk_c_address_of s exp.erange in
      man.eval ~zone:(Z_c, Z_c_low_level) ss flow |> Eval.bind_return @@ fun ss flow ->

      let st = etyp s in
      let t = etyp exp in
      let align = mk_int (align_byte st i) exp.erange in

      let exp' =
        mk_c_deref
          (mk_c_cast
             (mk_binop
                (mk_c_cast ss (pointer_type s8) exp.erange)
                O_plus
                align
                exp.erange
             )
             (pointer_type t)
             exp.erange
          )
          exp.erange
      in
      Eval.singleton exp' flow

    (* 𝔼⟦ p->f ⟧ = *(( typeof(p->f)* )(( char* )p + alignof(p->f))) *)
    | E_c_arrow_access(p, i, f) ->
      man.eval ~zone:(Z_c, Z_c_low_level) p flow |> Eval.bind_return @@ fun p flow ->

      let st = under_pointer_type p.etyp in
      let t = etyp exp in
      let align = mk_int (align_byte st i) exp.erange in

      let exp' =
        mk_c_deref
          (mk_c_cast
             (mk_binop
                (mk_c_cast p (pointer_type s8) exp.erange)
                O_plus
                align
                exp.erange
             )
             (pointer_type t)
             exp.erange
          )
          exp.erange
      in
      Eval.singleton exp' flow

    (* 𝔼⟦ &(a[i]) ⟧ = a + i *)
    | E_c_address_of { ekind = E_c_array_subscript(a,i) } ->
      man.eval ~zone:(Z_c, Z_c_low_level) a flow |>
      Eval.bind_return @@ fun a flow ->

      man.eval ~zone:(Z_c, Z_c_low_level) i flow |>
      Eval.bind @@ fun i flow ->

      let exp' = { exp with ekind = E_binop(O_plus, a, i) } in
      Eval.singleton exp' flow

    (* 𝔼⟦ &(p->f) ⟧ = ( typeof(p->f)* )(( char* )p + alignof(p->f)) *)
    | E_c_address_of { ekind = E_c_arrow_access(p, i, f) } ->
      man.eval ~zone:(Z_c, Z_c_low_level) p flow |> Eval.bind_return @@ fun p flow ->

      let st = under_pointer_type p.etyp in
      let t = etyp exp in
      let align = mk_int (align_byte st i) exp.erange in

      let exp' =
        mk_c_cast
          (mk_binop
             (mk_c_cast p (pointer_type s8) exp.erange)
             O_plus
             align
             exp.erange
          )
          (pointer_type t)
          exp.erange
      in
      Eval.singleton exp' flow

    (* 𝔼⟦ &*x ⟧ = x *)
    | E_c_address_of { ekind = E_c_deref x } ->
      man.eval ~zone:(Z_c, Z_c_low_level) x flow |>
      Eval.return

    (* 𝔼⟦ *&x ⟧ = x *)
    | E_c_deref { ekind = E_c_address_of x } ->
      man.eval ~zone:(Z_c, Z_c_low_level) x flow |>
      Eval.return

    | E_c_assign(lval, rval) ->
      debug "E_c_assign %a" pp_zone2 zone;
      man.eval rval ~zone:(Z_c, Z_c_low_level) flow |>
      Eval.bind_return @@ fun rval flow ->
      let flow = man.exec ~zone:Z_c (mk_assign lval rval exp.erange) flow in
      Eval.singleton rval flow

    | E_c_statement {skind = S_block l} ->
      begin
        match List.rev l with
        | {skind = S_expression e}::q ->
          let q' = List.rev q in
          let stmt' = mk_block q' (tag_range (erange exp) "block'") in
          let flow' = man.exec ~zone:Z_c stmt' flow in
          man.eval ~zone:(Z_c, Z_c_low_level) e flow' |>
          Eval.return

        | _ ->
          panic "E_c_statement %a" pp_expr exp
      end

    | E_c_statement {skind = S_expression e} ->
      man.eval ~zone:(Z_c, Z_c_low_level) e flow |>
      Eval.return

    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
