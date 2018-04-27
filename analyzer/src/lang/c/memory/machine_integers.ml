(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Machine representation of C integers. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Lattice
open Framework.Ast
open Universal.Ast
open Universal.Utils
open Ast

let name = "c.memory.machine_integers"
let debug fmt = Debug.debug ~channel:name fmt

let range_leq (a,b) (c,d) =
  Z.leq c a && Z.leq b d

let wrap_z (z : Z.t) ((l,h) : Z.t * Z.t) : Z.t =
  Z.( l + ((z - l) mod (h-l+one)) )
(** Abstract domain. *)
module Domain =
struct

  (*==========================================================================*)
  (**                        {2 Transfer functions}                           *)
  (*==========================================================================*)

  let init man ctx prog flow = ctx, flow

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    | E_binop(O_div, e, e') when exp |> etyp |> is_c_int_type ->
      man.eval ctx e flow |>
      eval_compose (fun e flow ->
          man.eval ctx e' flow |>
          eval_compose (fun e' flow ->
              let emint' = {e' with etyp = T_int} in
              let cond = {ekind = E_binop(O_eq, emint', mk_z Z.zero (tag_range range "div0"));
                          etyp  = T_bool;
                          erange = tag_range range "div0cond"
                         }
              in
              assume_to_eval cond
                (fun tflow ->
                   let cur = man.flow.get TCur tflow in
                   let tflow2 = man.flow.add (Alarms.TDivideByZero range) cur tflow
                                |> man.flow.set TCur man.env.bottom
                   in
                   oeval_singleton (None, tflow2, []))
                (fun fflow -> re_eval_singleton (man.eval ctx) (Some {exp with etyp = T_int}, flow, []))
                man ctx flow ()
            )
        )
    | E_constant(C_c_character (c, _)) ->
      re_eval_singleton (man.eval ctx) (Some (mk_z c exp.erange), flow, [])

    | E_c_cast({ekind = E_constant (C_int z)}, _) when exp |> etyp |> is_c_int_type ->
      let r = exp |> etyp |> rangeof in
      if range_leq (z,z) r then
        oeval_singleton (Some (mk_z z range), flow, [])
      else
        let cur = man.flow.get TCur flow in
        let flow2 = man.flow.add (Alarms.TIntegerOverflow range) cur flow in
        oeval_singleton (Some (mk_z (wrap_z z r) (tag_range range "wrapped")), flow2, [])

    | E_c_cast(e, _) when exp |> etyp |> is_c_int_type && e |> etyp |> is_c_int_type ->
      man.eval ctx e flow |>
      eval_compose (fun e flow ->
          let t  = etyp exp in
          let t' = etyp e in
          let r = rangeof t in
          let r' = rangeof t' in
          if range_leq r' r then
            re_eval_singleton (man.eval ctx) (Some e, flow, [])
          else
            begin
              let rmin, rmax = r in
              let e_mint = {e with etyp = T_int} in
              let condle = {ekind = E_binop(O_le, e_mint, mk_z rmax (tag_range range "wrap_le_z"));
                            etyp  = T_bool;
                            erange = tag_range range "wrap_le"
                           } in
              let condge = {ekind = E_binop(O_ge, e_mint, mk_z rmin (tag_range range "wrap_ge_z"));
                            etyp  = T_bool;
                            erange = tag_range range "wrap_ge"
                           } in
              let cond =
                {ekind = E_binop(O_log_and, condle, condge);
                 etyp = T_bool;
                 erange = tag_range range "wrap_full"
                }
              in
              assume_to_eval cond
                (fun tflow -> oeval_singleton (Some {e with etyp = T_int}, tflow, []))
                (fun fflow ->
                   let cur = man.flow.get TCur fflow in
                   let fflow2 = man.flow.add (Alarms.TIntegerOverflow range) cur fflow in
                   re_eval_singleton (man.eval ctx)
                     (Some({ekind  = E_unop(O_wrap(rmin, rmax), e);
                            etyp   = T_int;
                            erange = tag_range range "wrap"
                           }), fflow2, []
                     )
                ) man ctx flow ()
            end
        )

    | E_c_cast(e, _) ->
      re_eval_singleton (man.eval ctx) (Some e, flow, [])

    (* | E_binop(O_plus, e, e') when exp |> etyp |> is_c_int_type ->
     *   let () = debug "etyp %a : %a" Framework.Pp.pp_expr exp Framework.Pp.pp_typ (exp |> etyp) in
     *   let rmin, rmax = exp |> etyp |> rangeof in
     *   let exp_mint = {exp with etyp = T_int} in
     *   let condle = {ekind = E_binop(O_le, exp_mint, mk_z rmax (tag_range range "wrap_le_z"));
     *                 etyp  = T_bool;
     *                 erange = tag_range range "wrap_le"
     *                } in
     *   let condge = {ekind = E_binop(O_ge, exp_mint, mk_z rmin (tag_range range "wrap_ge_z"));
     *                 etyp  = T_bool;
     *                 erange = tag_range range "wrap_ge"
     *                } in
     *   let cond =
     *     {ekind = E_binop(O_log_and, condle, condge);
     *      etyp = T_bool;
     *      erange = tag_range range "wrap_full"
     *     }
     *   in
     *   assume_to_eval cond
     *     (fun tflow -> oeval_singleton (Some {exp with etyp = T_int}, tflow, []))
     *     (fun fflow ->
     *        let cur = man.flow.get TCur fflow in
     *        let fflow2 = man.flow.add (Alarms.TIntegerOverflow range) cur fflow in
     *        re_eval_singleton (man.eval ctx)
     *          (Some({ekind  = E_unop(O_wrap(rmin, rmax), exp);
     *                 etyp   = T_int;
     *                 erange = tag_range range "wrap"
     *                }), fflow2, []
     *          )
     *     ) man ctx flow () *)
    | _ -> None

  let exec man ctx stmt flow =
    match skind stmt with
    | S_c_local_declaration(v, init) when is_c_int_type v.vtyp ->
      let flow =
        match init with
        | None -> flow
        | Some (C_init_expr e) -> man.exec ctx (mk_assign (mk_var v stmt.srange) e stmt.srange) flow
        | Some (Ast.C_init_list (_,_)) -> assert false
        | Some (Ast.C_init_implicit _) -> assert false
      in
      return flow

    | _ -> None

  let ask _ _ _ _ = None

end


(*==========================================================================*)
(**                            {2 Setup}                                    *)
(*==========================================================================*)

let setup () =
  register_domain name (module Domain)
