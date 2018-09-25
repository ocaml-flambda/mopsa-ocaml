(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Machine representation of C integers *)

open Framework.Essentials
open Universal.Ast
open Ast
open Zone
open Universal.Zone

module Itv_Value = Universal.Numeric.Values.Intervals.Value

let name = "c.machine_integers"
let debug fmt = Debug.debug ~channel:name fmt

let range_leq (a,b) (c,d) =
  Z.leq c a && Z.leq b d

let wrap_z (z : Z.t) ((l,h) : Z.t * Z.t) : Z.t =
  Z.( l + ((z - l) mod (h-l+one)) )

let is_c_int_op = function
  | O_div | O_mod | O_mult | O_plus | O_minus -> true
  | _ -> false

let is_c_div = function
  | O_div | O_mod -> true
  | _ -> false

let cast_alarm = ref true

let check_overflow typ man range f1 f2 exp flow =
  let rmin, rmax = rangeof typ in
  let rec fast_check e flow =
    let itv = man.ask (Itv_Value.Q_interval e) flow in
    debug "overflow interval = %a" Itv_Value.print itv;
    if Itv_Value.is_bottom itv then Eval.empty_singleton flow
    else
    if Itv_Value.is_bounded itv then
      let l, u = Itv_Value.bounds itv in
      if Z.geq l rmin && Z.leq u rmax then f1 e flow
      else if Z.lt u rmin || Z.gt l rmax then f2 e flow
      else full_check e flow
    else
      full_check e flow

  and full_check e flow =
    let cond = range_cond e rmin rmax (erange e) in
    Eval.assume
      ~zone:Z_u_num
      cond
      ~fthen:(fun tflow -> f1 e flow)
      ~felse:(fun fflow -> f2 e flow)
      man flow
  in
  fast_check exp flow

let check_division man range f1 f2 exp flow =
  let rec fast_check e flow =
    let itv = man.ask (Itv_Value.Q_interval e) flow in
    debug "div interval = %a" Itv_Value.print itv;
    if Itv_Value.is_bottom itv then Eval.empty_singleton flow
    else
    if Itv_Value.is_bounded itv then
      let l, u = Itv_Value.bounds itv in
      if Z.gt l Z.zero || Z.lt u Z.zero then f1 e flow
      else if Z.equal u Z.zero && Z.equal l Z.zero then f2 e flow
      else full_check e flow
    else
      full_check e flow

  and full_check e' flow =
    let emint' = {e' with etyp = T_int} in
    let cond = {ekind = E_binop(O_eq, emint', mk_z Z.zero (tag_range range "div0"));
                etyp  = T_bool;
                erange = tag_range range "div0cond"
               }
    in
    Eval.assume
      ~zone:Z_u_num
      cond
      ~fthen:(fun tflow -> f2 e' flow)
      ~felse:(fun fflow -> f1 e' flow)
      man flow
  in
  fast_check exp flow

let is_math_type = function
  | T_int | T_float -> true
  | _ -> false

let to_universal_type t =
  if is_c_scalar_type t then
    if is_c_int_type t then
      T_int
    else
      T_float
  else
    match t with
    | T_int | T_float | T_bool | T_any | T_c_void -> t
    | _ -> Debug.fail "[to_universal_type] in machine_integers called \
                       on non scalar type ; %a" pp_typ t

let var_machine_integers v =
  {v with vtyp = to_universal_type v.vtyp}


(** {2 Domain definition} *)
(** ===================== *)

module Domain : Framework.Domains.Stateless.S =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_machine_integers : unit domain
  let id = D_c_machine_integers
  let name = name
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_machine_integers -> Some Eq
    | _ -> None

  let debug fmt = debug fmt

  (** Zoning definition *)
  (** ================= *)

  let eval_interface =
    {
      export = [Z_c_scalar_num, Z_u_num];
      import = [Z_c_scalar_num, Z_u_num];
    }
  let exec_interface =
    {
      import = [Z_u_num];
      export = [Z_c_scalar_num]
    }

  let rec eval zone exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_binop(op, e, e') when op |> is_c_div && exp |> etyp |> is_c_type ->
      eval_binop op e e' exp man flow |>
      Option.lift @@
      Eval.bind @@
      check_division man range
        (fun e' tflow -> Eval.singleton e' tflow)
        (fun e' fflow ->
           let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack fflow in
           let alarm = mk_alarm Alarms.ADivideByZero exp.erange ~cs in
           let flow1 = Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man fflow |>
                       Flow.set T_cur man.bottom man
           in
           Eval.empty_singleton flow1
        )

    | E_unop(op, e) when is_c_int_op op && exp |> etyp |> is_c_type ->
      let () = debug "case 1" in
      let typ = etyp exp in
      let rmin, rmax = rangeof typ in
      eval_unop op e exp man flow  |>
      Option.lift @@
      Eval.bind @@
      check_overflow typ man range
        (fun e tflow -> Eval.singleton e tflow)
        (fun e fflow ->
           let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack fflow in
           let alarm = mk_alarm Alarms.AIntegerOverflow exp.erange ~cs in
           let flow1 = Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man fflow |>
                       Flow.set T_cur man.bottom man
           in
           Eval.singleton
             {ekind  = E_unop(O_wrap(rmin, rmax), e);
              etyp   = typ;
              erange = tag_range range "wrap"} flow1
        )

    | E_binop(op, e, e') when is_c_int_op op && exp |> etyp |> is_c_type ->
        let () = debug "case 2" in
        let typ = etyp exp in
        let rmin, rmax = rangeof typ in
        eval_binop op e e' exp man flow |>
        Option.lift @@
        Eval.bind @@
        check_overflow typ man range
          (fun e tflow -> Eval.singleton e tflow)
          (fun e fflow ->
             let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack fflow in
             let alarm = mk_alarm Alarms.AIntegerOverflow exp.erange ~cs in
             let flow1 = Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man fflow |>
                         Flow.set T_cur man.bottom man
             in
             Eval.singleton
               {ekind  = E_unop(O_wrap(rmin, rmax), e);
                etyp   = typ;
                erange = tag_range range "wrap"} flow1
          )

    | E_c_cast({ekind = E_constant (C_int z)}, _) when exp |> etyp |> is_c_int_type ->
      let () = debug "case 9" in
      let r = exp |> etyp |> rangeof in
      if range_leq (z,z) r then
        Eval.singleton (mk_z z range) flow
        |> Option.return
      else
        let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack flow in
        let alarm = mk_alarm Alarms.AIntegerOverflow exp.erange ~cs in
        let flow1 = Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man flow |>
                    Flow.set T_cur man.bottom man
        in
        Eval.singleton (mk_z (wrap_z z r) (tag_range range "wrapped")) flow1
        |> Option.return

    | E_c_cast(e, b) when exp |> etyp |> is_c_int_type && e |> etyp |> is_c_int_type ->
      let () = debug "case 5" in
      eval (Z_c_scalar_num, Z_u_num) e man flow |>
      Option.lift @@ Eval.bind @@ fun e' flow ->
      let t  = etyp exp in
      let t' = etyp e in
      let r = rangeof t in
      let r' = rangeof t' in
      if range_leq r' r then
        Eval.singleton e' flow
      else
        let rmin, rmax = rangeof t in
        check_overflow t man range
          (fun e tflow -> Eval.singleton {e with etyp = to_universal_type t} tflow)
          (fun e fflow ->
             if b && not (!cast_alarm) then
               begin
                 debug "false flow : %a" (Flow.print man) fflow ;
                 Eval.singleton
                   ({ekind  = E_unop(O_wrap(rmin, rmax), e);
                     etyp   = to_universal_type t;
                     erange = tag_range range "wrap"
                    }) fflow

               end
             else
               begin
                 let cs = Flow.get_annot Universal.Iterators.Interproc.Callstack.A_call_stack fflow in
                 let alarm = mk_alarm Alarms.AIntegerOverflow exp.erange ~cs in
                 let flow1 = Flow.add (alarm_token alarm) (Flow.get T_cur man flow) man fflow |>
                             Flow.set T_cur man.bottom man
                 in
                 Eval.singleton
                   {ekind  = E_unop(O_wrap(rmin, rmax), e);
                    etyp   = to_universal_type t;
                    erange = tag_range range "wrap"
                   }
                   flow1
               end
          ) e' flow

    | E_constant(C_c_character (c, _)) ->
      let () = debug "case 6" in
      Eval.singleton {exp with ekind = E_constant (C_int c)} flow
      |> Option.return

    | E_var(v, mode) ->
      let () = debug "case 8" in
      Eval.singleton
        {exp with
         ekind = E_var({v with vtyp = to_universal_type v.vtyp}, mode);
         etyp = to_universal_type (etyp exp)}
        flow
      |> Option.return

    | _ ->
      None

  and eval_binop op e e' exp man flow =
    eval (Z_c_scalar_num, Z_u_num) e man flow |>
    Option.bind @@
    Eval.bind_opt @@ fun e flow ->

    eval (Z_c_scalar_num, Z_u_num) e' man flow |>
    Option.lift @@
    Eval.bind @@ fun e' flow ->

    let exp' = {exp with
                ekind = E_binop(op, e, e');
                etyp = to_universal_type exp.etyp
               }
    in
    Eval.singleton exp' flow


  and eval_unop op e exp man flow =
    eval (Z_c_scalar_num, Z_u_num) e man flow |>
    Option.lift @@
    Eval.bind @@ fun e flow ->

    let exp' = {exp with
                ekind = E_unop(op, e);
                etyp = to_universal_type exp.etyp
               }
    in
    Eval.singleton exp' flow


  let exec zone stmt man flow =
    match skind stmt with
    | S_assign(lval, rval) when etyp lval |> is_c_int_type ->
      man.eval ~zone:(Z_c_scalar_num, Z_u_num) lval flow |>
      Post.bind_opt man @@ fun lval' flow ->

      man.eval ~zone:(Z_c_scalar_num, Z_u_num) rval flow |>
      Post.bind_opt man @@ fun rval' flow ->

      man.exec ~zone:Z_u_num (mk_assign lval' rval' stmt.srange) flow |>
      Post.of_flow |>
      Option.return

    | S_remove_var v when is_c_int_type v.vtyp ->
      let v' = {v with vtyp = to_universal_type v.vtyp} in
      man.exec ~zone:Z_u_num (mk_remove_var v' stmt.srange) flow |>
      Post.of_flow |>
      Option.return

    | S_assume(e) ->
      man.eval ~zone:(Z_c_scalar_num, Z_u_num) e flow |>
      Post.bind_opt man @@ fun e' flow ->

      man.exec ~zone:Z_u_num (mk_assume e' stmt.srange) flow |>
      Post.of_flow |>
      Option.return

    | _ -> None

  let ask _ _ _ =
    None

  let init _ _ _ =
    None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
