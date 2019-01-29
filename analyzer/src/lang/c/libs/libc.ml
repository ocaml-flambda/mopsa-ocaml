(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Evaluation of built-in Libc functions *)

open Mopsa
open Universal.Ast
open Ast
open Zone
module Itv = Universal.Numeric.Values.Intervals.Value

let is_builtin_function = function
  | "__builtin_constant_p"
  | "__builtin_va_start"
  | "__builtin_va_end"
  | "__builtin_va_copy"
    -> true

  | _ -> false


module Domain =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_libs_libc : unit domain
  let id = D_c_libs_libc
  let name = "c.libs.libc"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_c_libs_libc -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {export = []; import = [Universal.Zone.Z_u_num]}
  let eval_interface = {
    export = [
      Z_c, Z_c_low_level
    ];
    import = [
      Z_c, Memory.Common.Points_to.Z_c_points_to
    ]
  }

  (** Flow-insensitive annotations *)
  (** ============================ *)

  type ('a, _) Annotation.key +=
    (** List of unnamed arguments in a variadic function *)
    | A_c_unnamed_args: ('a,
                         var (** last named parameter *) *
                         var list (** unnamed parameters *)
                        ) Annotation.key

  let () =
    let open Annotation in
    register_stateless_annot {
      eq = (
        let f: type a b. (a, b) key -> (var * var list, b) eq option =
          function
          | A_c_unnamed_args -> Some Eq
          | _ -> None
        in
        f
      );
      print = (fun fmt (last, unnamed) ->
          Format.fprintf fmt "unnamed args: @[%a@]"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
               pp_var
            ) unnamed
        );
    } ()

  let get_unnamed_args flow =
    Flow.get_annot A_c_unnamed_args flow

  let set_unnamed_args (last, unnamed) flow =
    Flow.set_annot A_c_unnamed_args (last, unnamed) flow

  (** {2 Transfer functions} *)
  (** ====================== *)

  let init prog man flow = None

  let exec zone stmt man flow = None


  (** {2 Variadic functions} *)
  (** ====================== *)

  (** Evaluate a call to a variadic function *)
  let call_variadic_function fundec args range man flow =
    (* FIXME: for the moment, the domain does not supporting cascading
       calls to variadic functions *)
    if Flow.mem_annot A_c_unnamed_args flow
    then panic_at range "cascading calls to variadic functions not supported";

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
    let flow = set_unnamed_args (last,vars) flow in

    (* Call the function with only named arguments *)
    let fundec' = {fundec with c_func_variadic = false} in
    man.eval (mk_c_call fundec' named range) flow |>

    (* Remove unnamed arguments and the annotation *)
    Eval.map_flow (fun flow ->
        let flow =
          List.fold_left (fun flow unnamed ->
              man.exec ~zone:Z_c (mk_remove_var unnamed range) flow
            ) flow vars
        in
        Flow.rm_annot A_c_unnamed_args flow
      )

  (* Create a counter variable for a va_list *)
  let mk_vlc_var va_list range =
    let name = "$" ^ va_list.org_vname ^ "_counter" in
    let v = mkv name (name ^ (string_of_int va_list.vuid)) va_list.vuid T_int in
    mk_var v range

  (* Initialize a counter *)
  let init_vlc_var vlc range man flow =
    man.exec (mk_add vlc range) ~zone:Universal.Zone.Z_u_num flow |>
    man.exec (mk_assign vlc (mk_zero range) range) ~zone:Universal.Zone.Z_u_num

  (* Resolve a pointer to a va_list *)
  let resolve_va_list ap range man flow =
    let open Memory.Common.Points_to in
    man.eval ap ~zone:(Z_c, Z_c_points_to) flow |>
    Eval.bind @@ fun pt flow ->

    match ekind pt with
    | E_c_points_to (P_block (V ap, { ekind = E_constant (C_int o)})) when Z.equal o Z.zero ->
      Eval.singleton ap flow

    | _ -> panic_at range "resolve_va_list: pointed object %a not supported" pp_expr pt

  (** Evaluate calls to va_start *)
  let va_start ap param range man flow =
    let last, _ = get_unnamed_args flow in

    (* Ensure that param is the last named parameter *)
    if last.org_vname != param.org_vname
    then panic_at range "va_start: %a is not the last named parameter"
        pp_var param
    ;

    resolve_va_list ap range man flow |>
    Eval.bind @@ fun ap flow ->

    (* Initialize the counter *)
    let vlc = mk_vlc_var ap range in
    let flow = init_vlc_var vlc range man flow in

    Eval.empty_singleton flow

  (** Evaluate calls to va_arg *)
  let va_arg ap typ range man flow =
    let _, unnamed = get_unnamed_args flow in

    resolve_va_list ap range man flow |>
    Eval.bind @@ fun ap flow ->

    let vlc = mk_vlc_var ap range in

    (* Check that value of the counter does not exceed the number of
       unnamed arguments *)
    Eval.assume
      (mk_binop vlc O_lt (mk_int (List.length unnamed) range) range)
      ~fthen:(fun flow ->
          (* Compute the interval of the counter *)
          let itv = man.ask (Itv.Q_interval vlc) flow |>
                    Itv.meet () (Itv.of_int 0 (List.length unnamed - 1))
          in

          (* Iterate over possible values of the counter *)
          let evl = Itv.map (fun n ->
              let arg = List.nth unnamed (Z.to_int n) in
              (* Increment the counter *)
              let flow = man.exec
                  (mk_assign vlc (mk_z (Z.succ n) range) range)
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
          let flow' = raise_alarm Alarms.AVaArgNoNext range ~bottom:true man flow in
          Eval.empty_singleton flow'
        )
      ~zone:Universal.Zone.Z_u_num
      man flow

  (** Evaluate calls to va_end *)
  let va_end ap range man flow =
    resolve_va_list ap range man flow |>
    Eval.bind @@ fun ap flow ->

    let vlc = mk_vlc_var ap range in

    (* Remove the counter *)
    let flow' = man.exec (mk_remove vlc range) ~zone:Universal.Zone.Z_u_num flow in
    Eval.empty_singleton flow'


  (** {2 Evaluation entry point} *)
  (** ========================== *)

  let eval zone exp man flow =
    match ekind exp with
    (* 𝔼⟦ __builtin_constant_p(e) ⟧ *)
    | E_c_builtin_call("__builtin_constant_p", [e]) ->
      (* __builtin_constant_ determines if [e] is known to be constant
         at compile time *)
      let ret =
        match ekind e with
        | E_constant _ -> mk_one ~typ:s32 exp.erange
        | _ -> mk_z_interval Z.zero Z.one ~typ:s32 exp.erange
      in
      Eval.singleton ret flow |>
      Eval.return

    (* 𝔼⟦ f(...) ⟧ *)
    | E_call ({ ekind = E_c_function ({c_func_variadic = true} as fundec)}, args) ->
      call_variadic_function fundec args exp.erange man flow |>
      Eval.return

    (* 𝔼⟦ va_start(ap, param) ⟧ *)
    | E_c_builtin_call("__builtin_va_start", [ap; { ekind = E_var (param, _) }]) ->
      va_start ap param exp.erange man flow |>
      Eval.return

    (* 𝔼⟦ va_arg(ap) ⟧ *)
    | E_c_var_args(ap) ->
      va_arg ap exp.etyp exp.erange man flow |>
      Eval.return

    (* 𝔼⟦ va_end(ap) ⟧ *)
    | E_c_builtin_call("__builtin_va_end", [ap]) ->
      va_end ap exp.erange man flow |>
      Eval.return

    (* 𝔼⟦ va_copy(src, dst) ⟧ *)
    | E_c_builtin_call("__builtin_va_copy", [src; dst]) ->
      panic_at exp.erange "__builtin_va_copy not supported"

    | _ -> None

  let ask _ _ _  = None

end

let () =
  Framework.Domains.Stateless.register_domain (module Domain)
