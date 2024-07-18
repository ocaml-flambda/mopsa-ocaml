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
open Sig.Abstraction.Stateless
open Universal.Ast
open Ast
module Itv = Universal.Numeric.Values.Intervals.Integer.Value
open Common.Alarms
open Common.Base



module Domain =
struct


  (** Domain identification *)
  (** ===================== *)

  include GenStatelessDomainId(struct
      let name = "c.libs.variadic"
    end)

  let universal = Semantic "Universal"

  let checks = [ CHK_C_INVALID_MEMORY_ACCESS;
                 CHK_C_INSUFFICIENT_VARIADIC_ARGS ]

  (** Flow-insensitive annotations *)
  (** ============================ *)

  module C1 = GenContextKey(
    struct

      type 'a t = (callstack * typ list) VarMap.t

      let print pp fmt va_list_map =
        Format.fprintf fmt "@[<v 2>va_list:@,%a@]"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "@,")
             (fun fmt (va_list, (cs, typs)) ->
                Format.fprintf fmt "%a: [%a] @@ %a"
                  pp_var va_list
                  (Format.pp_print_list
                     ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
                     pp_typ
                  ) typs
                  pp_callstack_short cs
             )
          ) (VarMap.bindings va_list_map)
    end
    )

  let va_list_map_ctx = C1.key

  let get_va_list_map flow =
    try Flow.get_ctx flow |> find_ctx va_list_map_ctx
    with Not_found -> VarMap.empty

  let set_va_list_map ua flow =
    let ctx = Flow.get_ctx flow in
    Flow.set_ctx (add_ctx va_list_map_ctx ua ctx) flow

  module C2 = GenContextKey(
    struct

      type 'a t = typ list

      let print pp fmt typs =
        Format.fprintf fmt "types of unnamed parameters: %a"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
             pp_typ
          ) typs
    end
    )

  let unnamed_params_typs_ctx = C2.key

  let get_unnamed_params_typs flow =
    try Flow.get_ctx flow |> find_ctx unnamed_params_typs_ctx
    with Not_found -> []

  let set_unnamed_params_typs t flow =
    let ctx = Flow.get_ctx flow in
    Flow.set_ctx (add_ctx unnamed_params_typs_ctx t ctx) flow


  (** {2 Transfer functions} *)
  (** ====================== *)

  let init _ _ flow =  flow

  let exec stmt man flow = None


  (************************)
  (** Auxiliary variables *)
  (************************)

  type var_kind +=
    (* Unnamed parameter of a variadic function *)
    | V_c_variadic_param of callstack * int

    (* Counter of retrieved unnamed parameters *)
    | V_c_variadic_counter of var

  let () = register_var {
      print = (fun next fmt v ->
          match vkind v with
          | V_c_variadic_param(f::_, i) ->
            Format.fprintf fmt "variadic-param(%s, %d)"
              f.call_fun_orig_name
              i
          | V_c_variadic_counter(va_list) ->
            Format.fprintf fmt "variadic-counter(%a)"
              pp_var va_list
          | _ ->
            next fmt v
        );
      compare = (fun next v1 v2 ->
         match vkind v1, vkind v2 with
         | V_c_variadic_param(cs1, i1), V_c_variadic_param(cs2, i2) ->
           Compare.pair compare_callstack Int.compare
             (cs1, i1) (cs2, i2)
         | V_c_variadic_counter(v1), V_c_variadic_counter(v2) ->
           compare_var v1 v2
         | _ -> next v1 v2
      );
    }

  let mk_param cs i t =
    let uniq = Format.asprintf "variadic-param(%a, %d)" pp_callstack_short cs i in
    mkv uniq (V_c_variadic_param(cs, i)) t

  let mk_counter va_list =
    let uniq = Format.asprintf "variadic-counter(%s)" va_list.vname in
    mkv uniq (V_c_variadic_counter(va_list)) T_int


  (** {2 Variadic functions} *)
  (** ====================== *)

  (** Evaluate a call to a variadic function *)
  let call_variadic_function fundec args range man flow =
    (* Get the previous map of va_list variabls *)
    let old_va_list_map = get_va_list_map flow in
    let old_unnamed_typs = get_unnamed_params_typs flow in

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

    (* Add unnamed parameters and initialize them *)
    let cs = Flow.get_callstack flow in
    let cs' = push_callstack fundec.c_func_org_name ~uniq:fundec.c_func_unique_name range cs in
    let params, typs, _, post =
      unnamed |>
      List.fold_left (fun (params, typs, i, acc) arg ->
          let param = mk_param cs' i arg.etyp in
          let acc' =
            acc >>% fun flow ->
            man.exec (mk_add_var param range) flow >>% fun flow ->
            man.exec (mk_assign (mk_var param range) arg range) flow
          in
          param :: params, arg.etyp :: typs, i + 1, acc'
        ) ([], [], 0, Post.return flow)
    in
    post >>% fun flow ->

    let flow = set_unnamed_params_typs (List.rev typs) flow in

    (* Call the function with only named arguments *)
    let fundec' = {fundec with c_func_variadic = false} in
    man.eval (mk_c_call fundec' named range) flow >>$ fun ret flow ->

    (* Clean the state *)
    set_va_list_map old_va_list_map flow |>
    set_unnamed_params_typs old_unnamed_typs |>
    Eval.singleton ret
      ~cleaners:(
        List.map (fun p -> mk_remove_var p range) params
      )

  (* Resolve a pointer to a va_list *)
  let resolve_va_list ap range man flow =
    let open Common.Points_to in
    resolve_pointer ap man flow >>$ fun pt flow ->
    match pt with
    | P_block ({ base_kind = Var ap; base_valid = true }, offset, mode) ->
      let base_size = sizeof_type ap.vtyp flow in
      let elem_size = sizeof_type (under_type ap.vtyp) flow in

      (* We do not consider the case of arrays of va_list *)
      if not (Z.equal base_size elem_size)
      then panic_at range "arrays of va_list not supported";

      (* In this case, only offset 0 is OK *)
      assume
        (mk_binop offset O_eq (mk_zero range) range)
        ~fthen:(fun flow ->
            safe_c_memory_access_check range man flow |>
            Cases.singleton ap
          )
        ~felse:(fun eflow ->
            raise_c_out_bound_alarm (mk_var_base ap) (mk_z base_size range) offset (under_type ap.vtyp) range man flow eflow |>
            Cases.empty
          )
        ~route:universal
        man flow

    | _ -> panic_at range "resolve_va_list: pointed object %a not supported" pp_points_to pt


  (** Evaluate calls to va_start *)
  let va_start ap param range man flow =
    let va_list_map = get_va_list_map flow in
    let typs = get_unnamed_params_typs flow in
    let cs = Flow.get_callstack flow in

    (* Ensure that param is the last named parameter *)
    let f = List.hd cs in
    let f = find_c_fundec_by_name f.call_fun_orig_name flow in
    let last_param = List.rev f.c_func_parameters |> List.hd in
    if Format.asprintf "%a" pp_var last_param <> Format.asprintf "%a" pp_var param
    then panic_at range "va_start: %a is not the last named parameter"
        pp_var param
    ;

    (* Resolve the pointer to va_list variable *)
    resolve_va_list ap range man flow >>$ fun va_list flow ->
    let va_list_map = VarMap.add va_list (cs, typs) va_list_map in
    let flow = set_va_list_map va_list_map flow in

    (* Initialize the counter *)
    let counter = mk_var (mk_counter va_list) range in
    man.exec (mk_add counter range) flow >>% fun flow ->
    man.exec (mk_assign counter zero range) flow >>% fun flow ->
    Eval.singleton (mk_unit range) flow


  (** Evaluate calls to va_arg *)
  let va_arg ap typ range man flow =
    let va_list_map = get_va_list_map flow in

    (* Resolve the pointer to va_list variable *)
    resolve_va_list ap range man flow >>$ fun va_list flow ->
    let cs, typs = VarMap.find va_list va_list_map in

    (* Check that value of the counter does not exceed the number of
       unnamed arguments *)
    let counter = mk_var (mk_counter va_list) range in
    let total = List.length typs in
    assume
      (mk_binop counter O_lt (mk_int total range) range)
      ~fthen:(fun flow ->
          let flow = safe_variadic_args_number range man flow in
          (* Compute the interval of the counter *)
          let itv = ask_and_reduce man.ask (Universal.Numeric.Common.mk_int_interval_query counter) flow |>
                    Itv.meet (Itv.of_int 0 (total - 1))
          in

          (* Iterate over possible values of the counter *)
          let evl = Itv.map (fun n ->
              let n = Z.to_int n in
              let t = List.nth typs n in
              let param = mk_var (mk_param cs n t) range in
              let param =
                let tt = remove_typedef_qual t in
                let ttyp = remove_typedef_qual typ in
                let xor a b = (a || b) && not (a && b) in
                if compare_typ tt ttyp = 0 then
                  param
                else if xor (is_c_pointer_type tt) (is_c_pointer_type typ) then
                  mk_top typ range
                else 
                  mk_c_cast param typ range
              in
              (* Increment the counter *)
              man.exec (mk_assign counter (mk_int (n + 1) range) range) flow >>%
              man.eval param
            ) itv
          in

          Eval.join_list evl ~empty:(fun () -> Eval.empty flow)
        )
      ~felse:(fun eflow ->
          (* Raise an alarm since no next argument can be fetched by va_arg *)
          let flow' = raise_c_insufficient_variadic_args va_list counter typs range man flow eflow in
          Eval.empty flow'
        )
      ~route:universal
      man flow


  (** Evaluate calls to va_end *)
  let va_end ap range man flow =
    (* Resolve the pointer to va_list variable *)
    resolve_va_list ap range man flow >>$ fun va_list flow ->

    (* Remove the counter *)
    let counter = mk_var (mk_counter va_list) range in
    man.exec (mk_remove counter range) flow >>%
    Eval.singleton (mk_unit range)


  (** Evaluate calls to va_copy *)
  let va_copy dst src range man flow =
    let va_list_map = get_va_list_map flow in
    resolve_va_list dst range man flow >>$ fun dst_va_list flow ->
    resolve_va_list src range man flow >>$ fun src_va_list flow ->
    let info = VarMap.find src_va_list va_list_map in
    let va_list_map = VarMap.add dst_va_list info va_list_map in
    let flow = set_va_list_map va_list_map flow in
    let dst_counter = mk_var (mk_counter dst_va_list) range in
    let src_counter = mk_var (mk_counter src_va_list) range in
    man.exec (mk_add dst_counter range) flow >>%
    man.exec (mk_assign dst_counter src_counter range) >>%
    Eval.singleton (mk_unit range)


  (** {2 Evaluation entry point} *)
  (** ========================== *)

  let eval exp man flow =
    match ekind exp with

    (* 𝔼⟦ variadic f(...) ⟧ *)
    | E_call ({ ekind = E_c_function ({c_func_variadic = true} as fundec)}, args) ->
      call_variadic_function fundec args exp.erange man flow |>
      OptionExt.return

    (* 𝔼⟦ va_start(ap, param) ⟧ *)
    | E_c_builtin_call("__builtin_va_start", [ap; { ekind = E_var (param, _) }]) ->
      va_start ap param exp.erange man flow |>
      OptionExt.return

    (* 𝔼⟦ va_arg(ap) ⟧ *)
    | E_c_var_args(ap) ->
      va_arg ap exp.etyp exp.erange man flow |>
      OptionExt.return

    (* 𝔼⟦ va_end(ap) ⟧ *)
    | E_c_builtin_call("__builtin_va_end", [ap]) ->
      va_end ap exp.erange man flow |>
      OptionExt.return

    (* 𝔼⟦ va_copy(src, dst) ⟧ *)
    | E_c_builtin_call("__builtin_va_copy", [dst; src]) ->
      va_copy dst src exp.erange man flow |>
      OptionExt.return

    | _ -> None

  let ask _ _ _  = None

  let print_expr man flow printer exp =
    match remove_typedef_qual @@ etyp exp with
    | T_c_array (T_c_record({c_record_kind = C_struct} as record), _)
      when record.c_record_org_name = "__va_list_tag" ->
      let va_list_map = get_va_list_map flow in
      let range = exp.erange in 
      resolve_va_list exp range man flow |>
      Cases.iter_result
        (fun va_list flow ->
           let cs, typs = VarMap.find va_list va_list_map in
           let counter = mk_var (mk_counter va_list) range in
           let _, args =
             List.fold_left (fun (pos, acc) t ->
                 (pos+1, mk_var (mk_param cs pos t) range :: acc)
               ) (0, []) typs in
           List.iter (fun e -> man.print_expr flow printer e) (counter::args)
        )

    | _ -> ()

end

let () =
  register_stateless_domain (module Domain)
