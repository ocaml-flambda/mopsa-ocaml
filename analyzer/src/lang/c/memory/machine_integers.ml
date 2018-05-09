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

let is_c_int_op = function
  | O_div t | O_mod t | O_mult t | O_plus t | O_minus t when t |> is_c_int_type -> true
  | _ -> false

let is_c_div = function
  | O_div t | O_mod t when t |> is_c_type -> true
  | _ -> false

let cast_alarm = ref true

let check_overflow typ man ctx range f1 f2 exp =
  let rmin, rmax = rangeof typ in

  let rec fast_check e flow =
    let v = man.ask ctx (Universal.Numeric.Query.QIntInterval e) flow in
    match v with
    | None -> assert false
    | Some itv ->
      debug "overflow interval = %a" Universal.Numeric.Values.Int.print itv;
      if Universal.Numeric.Values.Int.is_bottom itv then oeval_singleton (None, flow, [])
      else
        try
          let l, u = Universal.Numeric.Values.Int.get_bounds itv in
          if Z.geq l rmin && Z.leq u rmax then f1 e flow
          else if Z.lt u rmin || Z.gt l rmax then f2 e flow
          else full_check e flow
        with Universal.Numeric.Values.Int.Unbounded ->
          full_check e flow

  and full_check e flow =
    let cond = range_cond e rmin rmax (erange e) in
    assume_to_eval cond
      (fun tflow -> f1 e flow)
      (fun fflow -> f2 e flow)
      man ctx flow ()
  in
  eval_compose (fun e flow ->
      (* Start with a fast interval check *)
      fast_check e flow
    ) exp

let check_division man ctx range f1 f2 exp =
  let rec fast_check e flow =
    let v = man.ask ctx (Universal.Numeric.Query.QIntInterval e) flow in
    match v with
    | None -> assert false
    | Some itv ->
      debug "div interval = %a" Universal.Numeric.Values.Int.print itv;
      if Universal.Numeric.Values.Int.is_bottom itv then oeval_singleton (None, flow, [])
      else
        try
          let l, u = Universal.Numeric.Values.Int.get_bounds itv in
          if Z.gt l Z.zero || Z.lt u Z.zero then f1 e flow
          else if Z.equal u Z.zero && Z.equal l Z.zero then f2 e flow
          else full_check e flow
        with Universal.Numeric.Values.Int.Unbounded ->
          full_check e flow

  and full_check e' flow =
    let emint' = {e' with etyp = T_int} in
    let cond = {ekind = E_binop(O_eq, emint', mk_z Z.zero (tag_range range "div0"));
                etyp  = T_bool;
                erange = tag_range range "div0cond"
               }
    in
    assume_to_eval cond
      (fun tflow -> f2 e' tflow)
      (fun fflow -> f1 e' fflow)
      man ctx flow ()
  in
  eval_compose (fun e' flow ->
      fast_check e' flow
    ) exp


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
      check_division man ctx range
        (fun e' tflow -> re_eval_singleton (man.eval ctx)
            (Some { exp with ekind = E_binop(to_math_op op, e, e')}, tflow, [])
        )
        (fun e' fflow ->
           let cur = man.flow.get TCur fflow in
           let tflow2 = man.flow.add (Alarms.TDivideByZero range) cur fflow
                        |> man.flow.set TCur man.env.bottom
           in
           oeval_singleton (None, tflow2, [])
        )

    | E_unop(op, e) when is_c_int_op op ->
      let typ = etyp exp in
      let rmin, rmax = rangeof typ in
      let nop = to_math_op op in
      man.eval ctx {exp with ekind = E_unop(nop, e) } flow  |>
      check_overflow typ man ctx range
        (fun e tflow -> oeval_singleton (Some e, tflow, []))
        (fun e fflow ->
           let cur = man.flow.get TCur fflow in
           let () = debug "Adding alarm flow : %a" man.env.print cur in
           let fflow2 = man.flow.add (Alarms.TIntegerOverflow range) cur fflow
           in
           re_eval_singleton (man.eval ctx)
             (Some({ekind  = E_unop(O_wrap(rmin, rmax), e);
                    etyp   = typ;
                    erange = tag_range range "wrap"
                   }), fflow2, []
             )
        )

    | E_binop(op, e, e') when is_c_int_op op ->
      let typ = etyp exp in
      let rmin, rmax = rangeof typ in
      let nop = to_math_op op in
      man.eval ctx {exp with ekind = E_binop(nop, e, e') } flow |>
      check_overflow typ man ctx range
        (fun e tflow -> oeval_singleton (Some e, tflow, []))
        (fun e fflow ->
           let cur = man.flow.get TCur fflow in
           let () = debug "Adding alarm flow : %a" man.env.print cur in
           let fflow2 = man.flow.add (Alarms.TIntegerOverflow range) cur fflow
           in
           re_eval_singleton (man.eval ctx)
             (Some({ekind  = E_unop(O_wrap(rmin, rmax), e);
                    etyp   = typ;
                    erange = tag_range range "wrap"
                   }), fflow2, []
             )
        )


    | E_c_cast(e, b) when exp |> etyp |> is_c_int_type && e |> etyp |> is_c_int_type ->
      let t  = etyp exp in
      let t' = etyp e in
      let r = rangeof t in
      let r' = rangeof t' in
      if range_leq r' r then
        re_eval_singleton (man.eval ctx) (Some e, flow, [])
      else
        let rmin, rmax = rangeof t in
        man.eval ctx e flow |>
        check_overflow t man ctx range
          (fun e tflow -> oeval_singleton (Some {e with etyp = t}, tflow, []))
          (fun e fflow ->
             if b && not (!cast_alarm) then
               begin
                 debug "false flow : %a" man.flow.print fflow ;
                 re_eval_singleton (man.eval ctx)
                   (Some({ekind  = E_unop(O_wrap(rmin, rmax), e);
                          etyp   = t;
                          erange = tag_range range "wrap"
                         }), fflow, []
                   )
               end
             else
               begin
                 debug "false flow : %a" man.flow.print fflow ;
                 let cur = man.flow.get TCur fflow in
                 let fflow2 = man.flow.add (Alarms.TIntegerOverflow range) cur fflow
                 in
                 re_eval_singleton (man.eval ctx)
                   (Some({ekind  = E_unop(O_wrap(rmin, rmax), e);
                          etyp   = t;
                          erange = tag_range range "wrap"
                         }), fflow2, []
                   )
               end
          )

    | E_constant(C_c_character (c, _)) ->
      re_eval_singleton (man.eval ctx) (Some ({exp with ekind = E_constant (C_int c)}), flow, [])

    | E_c_cast({ekind = E_constant (C_int z)}, _) when exp |> etyp |> is_c_int_type ->
      let r = exp |> etyp |> rangeof in
      if range_leq (z,z) r then
        oeval_singleton (Some (mk_z z range), flow, [])
      else
        let cur = man.flow.get TCur flow in
        let flow2 = man.flow.add (Alarms.TIntegerOverflow range) cur flow in
        oeval_singleton (Some (mk_z (wrap_z z r) (tag_range range "wrapped")), flow2, [])

    | _ -> None

  let exec man ctx stmt flow = None


  let ask _ _ _ _ = None

end


(*==========================================================================*)
(**                            {2 Setup}                                    *)
(*==========================================================================*)

let setup () =
  register_domain name (module Domain);
  Framework.Options.register (
    "-cast-alarm",
    Arg.Bool(fun b -> cast_alarm := b),
    "cast overflow alarms (default: true)"
  );
