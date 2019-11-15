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


(** Support for variadic functions *)


open Mopsa
open Framework.Core.Sig.Domain.Stateless
open Universal.Ast
open Ast
open Zone
module Itv = Universal.Numeric.Values.Intervals.Integer.Value


module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.libs.variadic"
    end)


  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {
      provides = [];
      uses = [Z_c_low_level; Universal.Zone.Z_u_num]
    };
    ieval = {
      provides = [
        Z_c, Z_c_low_level
      ];
      uses = [
        Z_c, Common.Points_to.Z_c_points_to;
        Z_c_scalar, Universal.Zone.Z_u_num
      ]
    }
  }

  let alarms = []

  (** Flow-insensitive annotations *)
  (** ============================ *)

  let unnamed_args_ctx =
    let module C = Context.GenUnitKey(
      struct
        type v = var (** last named parameter *) *
                 var list (** unnamed parameters *)

        type t = v list

        let print fmt stack =
          Format.fprintf fmt "@[<v 2>unnamed args:@,%a@]"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "@,")
               (fun fmt (named,unnamed) ->
                  Format.pp_print_list
                    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
                    pp_var
                    fmt unnamed
               )
            ) stack
      end
      )
    in
    C.key

  let get_unnamed_args flow =
    Flow.get_ctx flow |> Context.find_unit unnamed_args_ctx |> List.hd

  let push_unnamed_args (last, unnamed) flow =
    Flow.map_ctx (fun ctx ->
        let stack =
          try Context.find_unit unnamed_args_ctx ctx
          with Not_found -> []
        in
      Context.add_unit unnamed_args_ctx ((last, unnamed) :: stack) ctx
    ) flow

  let pop_unnamed_args flow =
    Flow.map_ctx (fun ctx ->
        let stack =
          try
            let stack = Context.find_unit unnamed_args_ctx ctx in
            List.tl stack
          with Not_found ->
            []
        in
        Context.add_unit unnamed_args_ctx stack ctx
    ) flow


  (** {2 Transfer functions} *)
  (** ====================== *)

  let init _ _ flow =  flow

  let exec zone stmt man flow = None



  (** {2 Variadic functions} *)
  (** ====================== *)

  (** Evaluate a call to a variadic function *)
  let call_variadic_function fundec args range man flow =
    (* Partition args into named and unnamed arguments *)
    let named, last, unnamed =
      let rec doit params args =
        match params, args with
        | [last], arg :: args -> [arg], last, args
        | _ :: params, arg :: args ->
          let named, last, unnamed = doit params args in
          arg :: named, last, unnamed
        | _ -> assert false
      in
      doit fundec.c_func_parameters args
    in

    (* Assign each unnamed argument to a temporary variable *)
    let vars, flow =
      unnamed |>
      List.fold_left (fun (vars, flow) unnamed ->
          let tmp = mktmp ~typ:unnamed.etyp () in
          let flow' = man.exec (mk_assign (mk_var tmp range) unnamed range) flow in
          tmp :: vars, flow'
        ) ([], flow)
    in

    (* Put vars in the annotation *)
    let flow = push_unnamed_args (last,vars) flow in

    (* Call the function with only named arguments *)
    let fundec' = {fundec with c_func_variadic = false} in
    man.eval (mk_c_call fundec' named range) flow >>= fun ret flow ->
    let flow =
      List.fold_left (fun flow unnamed ->
          man.exec ~zone:Z_c_low_level (mk_remove_var unnamed range) flow
        ) flow vars |>
      pop_unnamed_args
    in
    Eval.return ret flow


  (* Create a counter variable for a va_list *)
  let mk_valc_var va_list range =
    let v = mk_attr_var va_list "counter" T_int in
    mk_var v range


  (* Initialize a counter *)
  let init_valc_var valc range man flow =
    man.exec (mk_add valc range) ~zone:Universal.Zone.Z_u_num flow |>
    man.exec (mk_assign valc (mk_zero range) range) ~zone:Universal.Zone.Z_u_num


  (* Resolve a pointer to a va_list *)
  let resolve_va_list ap range man flow =
    let open Common.Points_to in
    man.eval ap ~zone:(Z_c, Z_c_points_to) flow >>$ fun pt flow ->

    match ekind pt with
    | E_c_points_to (P_block (V ap, offset)) ->
      let base_size = sizeof_type ap.vtyp in
      let elem_size = sizeof_type (under_type ap.vtyp) in

      (* We do not consider the case of arrays of va_list *)
      if not (Z.equal base_size elem_size)
      then panic_at range "arrays of va_list not supported";

      (* In this case, only offset 0 is OK *)
      assume
        (mk_binop offset O_eq (mk_zero range) ~etyp:u8 range)
        ~fthen:(fun flow ->
            Result.singleton ap flow
          )
        ~felse:(fun flow ->
            man.eval offset ~zone:(Z_c_scalar,Universal.Zone.Z_u_num) flow >>$ fun offset flow ->
            Common.Alarms.(raise_c_out_bound_alarm ~base:(V ap) ~offset ~size:(mk_z base_size range) range (Core.Sig.Stacked.Manager.of_domain_man man) flow) |>
            Result.empty_singleton
          )
        ~zone:Z_c
        man flow

    | _ -> panic_at range "resolve_va_list: pointed object %a not supported" pp_expr pt


  (** Evaluate calls to va_start *)
  let va_start ap param range man flow =
    let last, _ = get_unnamed_args flow in

    (* Ensure that param is the last named parameter *)
    if last.vname <> param.vname
    then panic_at range "va_start: %a is not the last named parameter"
        pp_var param
    ;

    resolve_va_list ap range man flow >>$ fun ap flow ->

    (* Initialize the counter *)
    let valc = mk_valc_var ap range in
    let flow = init_valc_var valc range man flow in

    Eval.empty_singleton flow


  (** Evaluate calls to va_arg *)
  let va_arg ap typ range man flow =
    let _, unnamed = get_unnamed_args flow in

    resolve_va_list ap range man flow >>$ fun ap flow ->

    let valc = mk_valc_var ap range in

    (* Check that value of the counter does not exceed the number of
       unnamed arguments *)
    assume
      (mk_binop valc O_lt (mk_int (List.length unnamed) range) range)
      ~fthen:(fun flow ->
          (* Compute the interval of the counter *)
          let itv = man.ask (Universal.Numeric.Common.mk_int_interval_query valc) flow |>
                    Itv.meet (Itv.of_int 0 (List.length unnamed - 1))
          in

          (* Iterate over possible values of the counter *)
          let evl = Itv.map (fun n ->
              let arg = List.nth unnamed (Z.to_int n) in
              (* Increment the counter *)
              let flow = man.exec
                  (mk_assign valc (mk_z (Z.succ n) range) range)
                  ~zone:Universal.Zone.Z_u_num
                  flow
              in
              Eval.singleton (mk_var arg range) flow
            ) itv
          in

          Eval.join_list evl ~empty:(Eval.empty_singleton flow)
        )
      ~felse:(fun flow ->
          (* Raise an alarm since no next argument can be fetched by va_arg *)
          let flow' = Common.Alarms.(raise_c_no_next_va_arg ~va_list:ap ~counter:valc ~args:unnamed range (Sig.Stacked.Manager.of_domain_man man) flow) in
          Eval.empty_singleton flow'
        )
      ~zone:Universal.Zone.Z_u_num
      man flow


  (** Evaluate calls to va_end *)
  let va_end ap range man flow =
    resolve_va_list ap range man flow >>$ fun ap flow ->

    let valc = mk_valc_var ap range in

    (* Remove the counter *)
    let flow' = man.exec (mk_remove valc range) ~zone:Universal.Zone.Z_u_num flow in
    Eval.empty_singleton flow'



  (** {2 Evaluation entry point} *)
  (** ========================== *)

  let eval zone exp man flow =
    match ekind exp with

    (* 𝔼⟦ variadic f(...) ⟧ *)
    | E_call ({ ekind = E_c_function ({c_func_variadic = true} as fundec)}, args) ->
      call_variadic_function fundec args exp.erange man flow |>
      Option.return

    (* 𝔼⟦ va_start(ap, param) ⟧ *)
    | E_c_builtin_call("__builtin_va_start", [ap; { ekind = E_var (param, _) }]) ->
      va_start ap param exp.erange man flow |>
      Option.return

    (* 𝔼⟦ va_arg(ap) ⟧ *)
    | E_c_var_args(ap) ->
      va_arg ap exp.etyp exp.erange man flow |>
      Option.return

    (* 𝔼⟦ va_end(ap) ⟧ *)
    | E_c_builtin_call("__builtin_va_end", [ap]) ->
      va_end ap exp.erange man flow |>
      Option.return

    (* 𝔼⟦ va_copy(src, dst) ⟧ *)
    | E_c_builtin_call("__builtin_va_copy", [src; dst]) ->
      panic_at exp.erange "__builtin_va_copy not supported"

    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Core.Sig.Domain.Stateless.register_domain (module Domain)
