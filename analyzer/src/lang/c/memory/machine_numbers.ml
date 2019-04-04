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

(** Machine representation of C integers and floats *)

open Mopsa
open Universal.Ast
open Ast
open Zone
open Universal.Zone

module Itv = Universal.Numeric.Values.Intervals.Integer.Value

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
    let itv = man.ask (Itv.EvalQuery.query e) flow in
    if Itv.is_bottom itv then Eval.empty_singleton flow
    else
    if Itv.is_bounded itv then
      let l, u = Itv.bounds itv in
      if Z.geq l rmin && Z.leq u rmax then f1 e flow
      else if Z.lt u rmin || Z.gt l rmax then f2 e flow
      else full_check e flow
    else
      full_check e flow

  and full_check e flow =
    let cond = range_cond e rmin rmax (erange e) in
    assume_eval
      ~zone:Z_u_num
      cond
      ~fthen:(fun tflow -> f1 e flow)
      ~felse:(fun fflow -> f2 e flow)
      man flow
  in
  fast_check exp flow

let check_division man range f1 f2 e e' flow =
  let rec fast_check () =
    let itv = man.ask (Itv.EvalQuery.query e') flow in
    if Itv.is_bottom itv then Eval.empty_singleton flow
    else
    if Itv.is_bounded itv then
      let l, u = Itv.bounds itv in
      if Z.gt l Z.zero || Z.lt u Z.zero then f1 flow
      else if Z.equal u Z.zero && Z.equal l Z.zero then f2 flow
      else full_check ()
    else
      full_check ()

  and full_check () =
    let cond = {ekind = E_binop(O_eq, e', mk_z Z.zero (tag_range range "div0"));
                etyp  = T_bool;
                erange = tag_range range "div0cond"
               }
    in
    assume_eval
      ~zone:Z_u_num
      cond
      ~fthen:(fun tflow -> f2 tflow)
      ~felse:(fun fflow -> f1 fflow)
      man flow
  in
  fast_check ()

let to_universal_type t =
  match t with
  | T_bool | T_int | T_float _ | T_any -> t
  | _ ->
    match remove_typedef_qual t with
    | T_c_bool -> T_bool
    | T_c_integer _ -> T_int
    | T_c_enum _ -> T_int
    | T_c_float C_float -> T_float F_SINGLE
    | T_c_float C_double -> T_float F_DOUBLE
    | T_c_float C_long_double -> T_float F_LONG_DOUBLE
    | _ -> panic ~loc:__LOC__ "non integer type %a" pp_typ t

let to_universal_var v =
  {v with vtyp = to_universal_type v.vtyp}

let to_universal_expr e =
  match ekind e with
  | E_var (v, mode) ->
    {
      e with
      ekind = E_var (to_universal_var v, mode);
      etyp = to_universal_type v.vtyp
    }

  | _ -> assert false

(** {2 Domain definition} *)
(** ===================== *)

module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  let name = "c.memory.machine_numbers"
  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {
      provides = [Z_c_scalar];
      uses = [Z_u_num];
    };
    ieval = {
      provides = [Z_c_scalar, Z_u_num];
      uses = [Z_c_scalar, Z_u_num];
    }
  }

  let rec eval zone exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_binop(op, e, e') when op |> is_c_div &&
                              e  |> etyp |> is_c_int_type &&
                              e' |> etyp |> is_c_int_type ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow |>
      Eval.bind_return @@ fun e flow ->

      man.eval ~zone:(Z_c_scalar, Z_u_num) e' flow |>
      Eval.bind @@ fun e' flow ->

      check_division man range
        (fun tflow ->
           let exp' = mk_binop e op e' ~etyp:(to_universal_type exp.etyp) range in
           Eval.singleton exp' tflow
        )
        (fun fflow ->
           let flow' = raise_alarm Alarms.ADivideByZero exp.erange ~bottom:true man fflow in
           Eval.empty_singleton flow'
        ) e e' flow

    | E_unop(op, e) when is_c_int_op op &&
                         e |> etyp |> is_c_int_type ->
      let typ = etyp exp in
      let rmin, rmax = rangeof typ in
      eval_unop op e exp man flow  |>
      Option.lift @@
      Eval.bind @@
      check_overflow typ man range
        (fun e tflow -> Eval.singleton e tflow)
        (fun e fflow ->
           let flow1 = raise_alarm Alarms.AIntegerOverflow exp.erange ~bottom:false man fflow in
           Eval.singleton
             {ekind  = E_unop(O_wrap(rmin, rmax), e);
              etyp   = to_universal_type typ;
              erange = tag_range range "wrap"} flow1
        )

    | E_binop(op, e, e') when is_c_int_op op &&
                              e  |> etyp |> is_c_int_type &&
                              e' |> etyp |> is_c_int_type ->
        let typ = etyp exp in
        let rmin, rmax = rangeof typ in
        eval_binop op e e' exp man flow |>
        Option.lift @@
        Eval.bind @@
        check_overflow typ man range
          (fun e tflow -> Eval.singleton e tflow)
          (fun e fflow ->
             let flow1 = raise_alarm Alarms.AIntegerOverflow exp.erange ~bottom:false man fflow in
             Eval.singleton
               {ekind  = E_unop(O_wrap(rmin, rmax), e);
                etyp   = to_universal_type typ;
                erange = tag_range range "wrap"} flow1
          )

    | E_c_cast(e, b) when exp |> etyp |> is_c_int_type &&
                          e   |> etyp |> is_c_num_type
      ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow |>
      Option.return |> Option.lift @@ Eval.bind @@ fun e' flow ->
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
                 Eval.singleton
                   ({ekind  = E_unop(O_wrap(rmin, rmax), e);
                     etyp   = to_universal_type t;
                     erange = tag_range range "wrap"
                    }) fflow

               end
             else
               begin
                 let flow1 = raise_alarm Alarms.AIntegerOverflow exp.erange ~bottom:false man fflow in
                 Eval.singleton
                   {ekind  = E_unop(O_wrap(rmin, rmax), e);
                    etyp   = to_universal_type t;
                    erange = tag_range range "wrap"
                   }
                   flow1
               end
          ) e' flow

    | E_binop(O_c_and, e1, e2) ->
        assume_eval
          e1 ~zone:(Z_c_scalar)
          ~fthen:(fun flow ->
              assume_eval
                e2 ~zone:(Z_c_scalar)
                ~fthen:(fun flow ->
                    Eval.singleton (mk_one exp.erange) flow
                  )
                ~felse:(fun flow ->
                    Eval.singleton (mk_zero exp.erange) flow
                  )
                man flow
            )
          ~felse:(fun flow ->
              Eval.singleton (mk_zero exp.erange) flow
            )
          man flow |>
        Option.return

    | E_binop(O_c_or, e1, e2) ->
      assume_eval
        e1 ~zone:(Z_c_scalar)
        ~fthen:(fun flow ->
            Eval.singleton (mk_one exp.erange) flow
          )
        ~felse:(fun flow ->
            assume_eval
              e2 ~zone:(Z_c_scalar)
              ~fthen:(fun flow ->
                  Eval.singleton (mk_one exp.erange) flow
                )
              ~felse:(fun flow ->
                  Eval.singleton (mk_zero exp.erange) flow
                )
              man flow
          )
        man flow |>
      Option.return

    | E_c_cast(e, b) when exp |> etyp |> is_c_float_type &&
                          e   |> etyp |> is_c_int_type->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow |>
      Eval.bind_return @@ fun e flow ->
      let exp' = {
        ekind = E_unop (O_cast, e);
        etyp = to_universal_type exp.etyp;
        erange = exp.erange
      }
      in
      Eval.singleton exp' flow

    | E_binop(op, e, e') when exp |> etyp |> is_c_num_type ->
      eval_binop op e e' exp man flow

    | E_unop(op, e) when exp |> etyp |> is_c_num_type ->
      eval_unop op e exp man flow

    | E_c_cast(e, b) when e |> etyp |> is_c_num_type->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow |>
      Option.return

    | E_constant(C_c_character (c, _)) ->
      Eval.singleton {exp with ekind = E_constant (C_int c); etyp = to_universal_type exp.etyp} flow
      |> Option.return

    | E_constant(C_int _ | C_int_interval _ | C_float _ | C_float_interval _) ->
      Eval.singleton {exp with etyp = to_universal_type exp.etyp} flow
      |> Option.return

    | E_constant(C_top t) when is_c_int_type t ->
      let l, u = rangeof t in
      let exp' = mk_z_interval l u ~typ:(to_universal_type t) exp.erange in
      Eval.singleton exp' flow |>
      Option.return

    | E_constant(C_top t) when is_c_float_type t ->
      let exp' = mk_top (to_universal_type t) exp.erange in
      Eval.singleton exp' flow |>
      Option.return

    | E_var _ ->
      Eval.singleton (to_universal_expr exp) flow |>
      Option.return

    | _ ->
      None

  and eval_binop op e e' exp man flow =
    man.eval ~zone:(Z_c_scalar, Z_u_num) e flow |>
    Eval.bind_return @@ fun e flow ->

    man.eval ~zone:(Z_c_scalar, Z_u_num) e' flow |>
    Eval.bind @@ fun e' flow ->

    let exp' = {exp with
                ekind = E_binop(op, e, e');
                etyp = to_universal_type exp.etyp
               }
    in
    Eval.singleton exp' flow


  and eval_unop op e exp man flow =
    man.eval ~zone:(Z_c_scalar, Z_u_num) e flow |>
    Eval.bind_return @@ fun e flow ->

    let exp' = {exp with
                ekind = E_unop(op, e);
                etyp = to_universal_type exp.etyp
               }
    in
    Eval.singleton exp' flow


  let exec zone stmt man stman flow =
    match skind stmt with
    | S_assign(lval, rval) when etyp lval |> is_c_num_type ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) lval flow |>
      Option.return |> Option.lift @@ Post.bind_eval man.lattice @@
      fun lval' flow ->

      man.eval ~zone:(Z_c_scalar, Z_u_num) rval flow |>
      Post.bind_eval man.lattice @@
      fun rval' flow ->

      man.exec ~zone:Z_u_num (mk_assign lval' rval' stmt.srange) flow |>
      Post.return

    | S_add v when is_c_num_type v.etyp ->
      let vv = to_universal_expr v in
      man.exec ~zone:Z_u_num (mk_add vv stmt.srange) flow |>
      Post.return |>
      Option.return

    | S_expand(v, vl) when is_c_num_type v.etyp ->
      let vv = to_universal_expr v in
      let vvl = List.map to_universal_expr vl in
      man.exec ~zone:Z_u_num (mk_expand vv vvl stmt.srange) flow |>
      Post.return |>
      Option.return

    | S_remove v when is_c_num_type v.etyp ->
      let vv = to_universal_expr v in
      man.exec ~zone:Z_u_num (mk_remove vv stmt.srange) flow |>
      Post.return |>
      Option.return

    | S_rename(v1, v2) when is_c_num_type v1.etyp &&
                            is_c_num_type v2.etyp
      ->
      let vv1 = to_universal_expr v1 in
      let vv2 = to_universal_expr v2 in
      man.exec ~zone:Z_u_num (mk_rename vv1 vv2 stmt.srange) flow |>
      Post.return |>
      Option.return


    | S_assume(e) ->
      man.eval ~zone:(Z_c_scalar, Z_u_num) e flow |>
      Option.return |> Option.lift @@ Post.bind_eval man.lattice @@
      fun e' flow ->

      man.exec ~zone:Z_u_num (mk_assume e' stmt.srange) flow |>
      Post.return

    | _ -> None


  let ask _ _ _ =
    None

  let init _ _ _ =
    None

end

let () =
  Framework.Core.Sig.Stateless.Stacked.register_stack (module Domain)
