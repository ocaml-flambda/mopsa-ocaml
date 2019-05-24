open Mopsa
open Core.Sig.Domain.Lowlevel
open Addr
open Typing
open Addr_env
open Ast
open Universal.Ast
open MapExt

let name = "python.types.typechecking"

let opt_python_modular_interproc : bool ref = ref true
let opt_python_modular_interproc_cache_size : int ref = ref 10

let () =
  register_domain_option name {
    key = "-py-no-modular-interproc";
    category = "Python";
    doc = " do not use the modular interprocedural analysis";
    spec = ArgExt.Clear opt_python_modular_interproc;
    default = "use it";
  };
  register_domain_option name {
    key = "-py-mod-interproc-size";
    category = "Python";
    doc = " size of the cache in the modular interprocedural analysis";
    spec = ArgExt.Set_int opt_python_modular_interproc_cache_size;
    default = string_of_int !opt_python_modular_interproc_cache_size;
  };



module Domain =
struct
  (* TODO: lists and polymorphism? *)

  module AD = Addr_env.Domain
  module TD = Typing.Domain

  module Iter = Framework.Transformers.Domain.Lowlevel.Sequence.Make
      (Framework.Core.Sig.Domain.Intermediate.MakeLowlevelDomain(AD))
      (Framework.Core.Sig.Domain.Intermediate.MakeLowlevelDomain(TD))

  include Iter

  let name = "python.types.typechecking"
  let debug fmt = Debug.debug ~channel:name fmt

  module Fctx = Context.GenPolyKey(
    struct
      type 'a t = ('a flow * expr option * 'a flow) list StringMap.t
      let print p fmt ctx = Format.fprintf fmt "Function cache context (py): %a@\n"
          (StringMap.fprint
             MapExt.printer_default
             (fun fmt s -> Format.fprintf fmt "%s" s)
             (Format.pp_print_list
                (fun fmt (flow_in, oexpr, flow_out) -> ()
                   (* Format.fprintf fmt "flow_in =@\n%a@\nflow_out =@\n%a@\noexpr = %a@\n"
                    *   (Flow.print_w_lprint p) flow_in
                    *   (Flow.print_w_lprint p) flow_out
                    *   (Option.print pp_expr) oexpr *)
                )
             )
          )
          ctx

    end
    )

  let find_signature man funname in_flow =
    if !opt_python_modular_interproc then
      try
        let cache = Context.find_poly Fctx.key (Flow.get_ctx in_flow) in
        let flows = StringMap.find funname cache in
        Some (List.find (fun (flow_in, _, _) ->
            Flow.subset man.lattice in_flow flow_in
          ) flows)
      with Not_found -> None
    else
      None

  let store_signature funname in_flow eval_res out_flow =
    let old_ctx = try Context.find_poly Fctx.key (Flow.get_ctx out_flow) with Not_found -> StringMap.empty in
    let old_sig = try StringMap.find funname old_ctx with Not_found -> [] in
    let new_sig =
      if List.length old_sig < !opt_python_modular_interproc_cache_size then (in_flow, eval_res, out_flow)::old_sig
      else (in_flow, eval_res, out_flow) :: (List.rev @@ List.tl @@ List.rev old_sig) in
    let new_ctx = StringMap.add funname new_sig old_ctx in
    Flow.set_ctx (Context.add_poly Fctx.key new_ctx (Flow.get_ctx out_flow)) out_flow

  let init prog man flow =
    Iter.init prog man flow |>
    Flow.map_ctx (Context.init_poly Fctx.init)

  let eval zs exp man flow =
    let range = exp.erange in
    match ekind exp with
    | E_py_sum_call (f, args) ->
      let func = match ekind f with
        | E_function (User_defined func) -> func
        | _ -> assert false in
      Eval.eval_list man.eval args flow |>
      Eval.bind (fun args in_flow ->
          let in_flow_cur = Flow.set T_cur (Flow.get T_cur man.lattice in_flow) man.lattice (Flow.bottom (Flow.get_ctx in_flow))
          and in_flow_other = Flow.remove T_cur in_flow in
          match find_signature man func.fun_name in_flow_cur with
          | None ->
            man.eval ~zone:(Universal.Zone.Z_u, Z_any) (mk_call func args range) in_flow_cur |>
            Eval.bind_lowlevel (fun oeval_res out_flow cleaners ->
                match oeval_res with
                | None ->
                  let out_flow = exec_block_on_all_flows cleaners man out_flow in
                  let flow = store_signature func.fun_name in_flow_cur oeval_res out_flow in
                  Eval.case oeval_res (Flow.join man.lattice in_flow_other flow)

                | Some eval_res ->
                  man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) eval_res out_flow |>
                  Eval.bind (fun eval_res out_flow ->
                      debug "eval_res = %a@\ncleaners = %a@\n" pp_expr eval_res pp_stmt (mk_block cleaners range);
                      let out_flow = exec_block_on_all_flows cleaners man out_flow in
                      let flow = store_signature func.fun_name in_flow_cur (Some eval_res) out_flow in
                      Eval.singleton eval_res (Flow.join man.lattice in_flow_other flow)
                    )
              )
          | Some (_, oout_expr, out_flow) ->
            Debug.debug ~channel:"profiling" "reusing %s at range %a" func.fun_name pp_range func.fun_range;
            debug "reusing something in function %s@\nchanging in_flow=%a@\ninto out_flow=%a@\n" func.fun_name (Flow.print man.lattice) in_flow (Flow.print man.lattice) out_flow;
            Eval.case oout_expr (Flow.join man.lattice in_flow_other out_flow)
        )
      |> Option.return

    | _ -> Iter.eval zs exp man flow

  (* let opt_polymorphism = ref false
   *
   * let () =
   *   register_domain_option name {
   *     key = "-pyty-polymorphism";
   *     category = "Python";
   *     doc = " enable type polymorphism";
   *     spec = ArgExt.Set opt_polymorphism;
   *     default = "false"
   *   } *)

  let extract_types_aset (t: TD.t) (aset: AD.ASet.t) : TD.Polytypeset.t =
    AD.ASet.fold (fun addr acc ->
        match addr with
        | Def addr ->
          let to_join = match addr.addr_kind with
            (* | A_py_var _ (\* TODO: est-ce que c'est ok? *\) *)
            | A_py_instance _ -> (TD.TMap.find addr t)
            | A_py_class (c, b) ->
              let ty = Class (c, b) in
              TD.Polytypeset.singleton ty
            | A_py_module m ->
              TD.Polytypeset.singleton (Module m)
            | A_py_function f ->
              TD.Polytypeset.singleton (Function f)
            | _ -> Debug.warn "%a@\n" pp_addr addr;
              TD.Polytypeset.empty
          in
          TD.Polytypeset.union acc to_join
        | _ -> acc
      ) aset TD.Polytypeset.empty

  let extract_types (a: AD.t) (t: TD.t) (v: var) : TD.Polytypeset.t =
    extract_types_aset t (AD.AMap.find v a)

  module VarSet = SetExt.Make
      (struct
        type t = var
        let compare = compare_var
        let print = pp_var
      end
      )

  type partition = (VarSet.t * TD.Polytypeset.t) list

  type intersectedpartition = (VarSet.t * TD.Polytypeset.t * TD.Polytypeset.t) list

  let pp_i fmt (vs, tys1, tys2) =
    Format.fprintf fmt "[%a -> %a | %a]" (VarSet.fprint SetExt.printer_default pp_var) vs TD.Polytypeset.print tys1 TD.Polytypeset.print tys2

  let pp_ip fmt ip =
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n") pp_i fmt ip

  let pp_e fmt (vs, tys1) =
    Format.fprintf fmt "[%a -> %a]" (VarSet.fprint SetExt.printer_default pp_var) vs TD.Polytypeset.print tys1

  let pp_p fmt p =
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n") pp_e fmt p

  let create_partition (a: AD.t) (t: TD.t) : partition =
    AD.AMap.fold (fun var aset acc ->
        let tys = match aset with
          | TOP -> TD.Polytypeset.top
          | _ ->   extract_types_aset t aset in
          let sameclass, others = ListExt.partition (fun (_, tys') -> TD.Polytypeset.equal tys tys') acc in
          match sameclass with
          | [] -> (VarSet.singleton var, tys):: others
          | [(vars, _)] -> (VarSet.add var vars, tys):: others
          | _ -> assert false
      ) a []

  let intersect_partitions (p: partition) (p': partition) : intersectedpartition =
    let lift_left (p: partition) : intersectedpartition = List.map (fun (vars, tys) -> (vars, tys, TD.Polytypeset.empty)) p in
    let intersect_one (part: intersectedpartition) ((vars, tys): VarSet.t * TD.Polytypeset.t) : intersectedpartition =
      List.fold_left (fun acc (vars_part, tys_part, tys_other) ->
          let vars_int, vars_diff = VarSet.inter vars_part vars, VarSet.diff vars_part vars in
          let res_int  = vars_int, tys_part, TD.Polytypeset.union tys_other tys
          and res_diff = vars_diff, tys_part, tys_other in
          match VarSet.is_empty vars_int, VarSet.is_empty vars_diff with
          | true, true   -> acc
          | false, false -> res_int  :: res_diff :: acc
          | true, false  -> res_diff :: acc
          | false, true  -> res_int  :: acc
        ) [] part in
    List.fold_left (fun acc part' -> intersect_one acc part') (lift_left p) p'



  (* let join man a a' =
   *   Framework.Core.Sig.Domain.Intermediate.lift_binop
   *     (fun (hd, tl) (hd', tl') ->
   *        (\* if !opt_polymorphism then *\)
   *        match hd, hd' with
   *        | AD.AMap.Map.Top, _ | _, AD.AMap.Map.Top -> Iter.top
   *        | _ ->
   *          match tl, tl' with
   *          | TD.TMap.Map.Top, _ | _, TD.TMap.Map.Top -> Iter.top
   *          | _ ->
   *            debug "hd, tl = %a, %a@\n@\nhd', tl' = %a, %a@\n" AD.print hd TD.print tl AD.print hd' TD.print tl';
   *            let p = create_partition hd tl
   *            and p' = create_partition hd' tl' in
   *            let ip = intersect_partitions p p' in
   *            let ip = List.filter (fun (vars, ty1, ty2) -> not (TD.Polytypeset.is_top ty1) && not (TD.Polytypeset.is_top ty2) && VarSet.cardinal vars > 1 && TD.Polytypeset.cardinal ty1 > 0 && TD.Polytypeset.cardinal ty2 > 0 && not (TD.Polytypeset.cardinal ty1 = 1 && TD.Polytypeset.equal ty1 ty2))
   *                ip in
   *            debug "interesting partitions:@[@\n%a@]@\n" pp_ip ip;
   *            (\* FIXME: is that necessary? *\)
   *            let jhd = AD.join hd hd' and jtl = TD.join tl tl' in
   *            let rhd, rabsheap = List.fold_left (fun (rhd, rabsheap) (vars, ty1, ty2) ->
   *                let alpha = get_fresh_a_py_var () in
   *                let addr = {addr_uid = alpha; addr_kind = A_py_var alpha; addr_mode = WEAK} in
   *                let types = TD.Polytypeset.join ty1 ty2 in
   *                let rhd = VarSet.fold (fun var rhd -> AD.AMap.add var (AD.ASet.singleton (Def addr)) rhd) vars rhd in
   *                let rabsheap = TD.TMap.add addr types rabsheap in
   *                (rhd, rabsheap)
   *              ) (jhd, jtl) ip in
   *            debug "result is %a@\n%a@\n" AD.print rhd TD.print rabsheap;
   *            rhd, rabsheap
   *            (\* else
   *             *   Iter.join annot (hd, tl) (hd', tl') *\)
   *     ) man a a' *)


  let ask : type r. r query -> ('a, t) man -> 'a flow -> r option = fun query man flow ->
    match query with
    | Framework.Engines.Interactive.Q_print_var ->
      Some (
        fun fmt v ->
          let amap, tmap = get_domain_env T_cur man flow in
          let ret = ref [] in
          AD.AMap.iter (fun var addrs  ->
              if var.org_vname = v then
                AD.ASet.iter (fun addr ->
                    match addr with
                    | Def addr ->
                      if TD.TMap.mem addr tmap then
                        let ty = TD.TMap.find addr tmap in
                        ret := (fun fmt -> Format.fprintf fmt "%s ⇝ %a ⇝ %a" var.uniq_vname pp_addr addr TD.Polytypeset.print ty) :: !ret
                      else
                        ret := (fun fmt -> Format.fprintf fmt "%s ⇝ %a ⇝ notinmap" var.uniq_vname pp_addr addr) :: !ret
                    | Undef_local ->
                      ret := (fun fmt -> Format.fprintf fmt "%s ⇝ undef_local" var.uniq_vname) :: !ret
                    | Undef_global ->
                      ret := (fun fmt -> Format.fprintf fmt "%s ⇝ undef_global" var.uniq_vname) :: !ret
                  ) addrs
            ) amap;
          Format.fprintf fmt "@[<v>%a@]"
            (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
               (fun fmt pp -> pp fmt)
            ) !ret
      )
    | _ -> Iter.ask query man flow


end

let () = Framework.Core.Sig.Domain.Lowlevel.register_domain (module Domain)
