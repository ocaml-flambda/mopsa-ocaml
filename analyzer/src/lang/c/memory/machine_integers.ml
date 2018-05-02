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

let range_cond e_mint rmin rmax range =
  let condle = {ekind = E_binop(O_le, e_mint, mk_z rmax (tag_range range "wrap_le_z"));
                etyp  = T_bool;
                erange = tag_range range "wrap_le"
               } in
  let condge = {ekind = E_binop(O_ge, e_mint, mk_z rmin (tag_range range "wrap_ge_z"));
                etyp  = T_bool;
                erange = tag_range range "wrap_ge"
               } in
  {ekind = E_binop(O_log_and, condle, condge);
   etyp = T_bool;
   erange = tag_range range "wrap_full"
  }



let is_c_int_op = function
  | O_div t | O_mod t | O_mult t | O_plus t | O_minus t when t |> is_c_int_type -> true
  | _ -> false

let is_c_div = function
  | O_div t | O_mod t when t |> is_c_type -> true
  | _ -> false

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
    | E_binop(op, e, e')
      when op |> is_c_div ->
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
            (fun fflow -> re_eval_singleton (man.eval ctx)
                (Some { exp with ekind = E_binop(to_math_op op, e, e')}, flow, [])
            )
            man ctx flow ()
        )

    | E_binop(op, e, e') when is_c_int_op op ->
      let typ = etyp exp in
      let nop = to_math_op op in
      man.eval ctx {exp with ekind = E_binop(nop, e, e') } flow |>
      eval_compose (fun e flow ->
          let rmin, rmax = typ |> rangeof in
          let cond = range_cond e rmin rmax (erange e) in
          assume_to_eval cond
            (fun tflow ->
               oeval_singleton (Some e, tflow, []))
            (fun fflow ->
               let cur = man.flow.get TCur fflow in
               let fflow2 = man.flow.add (Alarms.TIntegerOverflow range) cur fflow in
               re_eval_singleton (man.eval ctx)
                 (Some({ekind  = E_unop(O_wrap(rmin, rmax), e);
                        etyp   = typ;
                        erange = tag_range range "wrap"
                       }), fflow2, []
                 )
            ) man ctx flow ()
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
              let cond = range_cond e_mint rmin rmax range in
              let () = debug "condition : %a" Framework.Pp.pp_expr cond in
              assume_to_eval cond
                (fun tflow -> debug "true flow : %a" man.flow.print tflow ; oeval_singleton (Some {e with etyp = t}, tflow, []))
                (fun fflow ->
                   debug "false flow : %a" man.flow.print fflow ;
                   let cur = man.flow.get TCur fflow in
                   let fflow2 = man.flow.add (Alarms.TIntegerOverflow range) cur fflow
                              |> man.flow.remove TCur
                   in
                   re_eval_singleton (man.eval ctx)
                     (Some({ekind  = E_unop(O_wrap(rmin, rmax), e);
                            etyp   = t;
                            erange = tag_range range "wrap"
                           }), fflow2, []
                     )
                ) man ctx flow ()
            end
        )

    | _ -> None

  let exec man ctx stmt flow = None


  let ask _ _ _ _ = None

end


(*==========================================================================*)
(**                            {2 Setup}                                    *)
(*==========================================================================*)

let setup () =
  register_domain name (module Domain)
