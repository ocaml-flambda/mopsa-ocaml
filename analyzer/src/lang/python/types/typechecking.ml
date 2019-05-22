open Mopsa
open Core.Sig.Domain.Lowlevel
open Addr
open Typing
open Addr_env
open Ast
open Universal.Ast
open MapExt


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

  type signature =
    {in_args : (addr * TD.Polytypeset.t) list; in_args_out : (addr * TD.Polytypeset.t) list; out : addr * TD.Polytypeset.t }

  let function_cache : signature list StringMap.t ref = ref StringMap.empty

  let pp_signature fmt (a, p) = Format.fprintf fmt "(%a, %a)" pp_addr a TD.Polytypeset.print p

  let pp_slist fmt l = Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_signature fmt l

  let extract_addrty cur eobj = match ekind eobj with
    | E_py_object (addr, _) -> (addr, TD.find addr (snd cur))
    | _ -> Exceptions.panic "%a@\n" pp_expr eobj

  let find_signature funname inargs =
    try
      let sigs = StringMap.find funname !function_cache in
      Some (
        List.find (fun {in_args} -> Compare.list
                      (fun (a1, p1) (a2, p2) ->
                         Compare.compose [(fun () -> compare_addr a1 a2);
                                          (fun () -> if TD.Polytypeset.equal p1 p2 then 0 else -1)])
                      in_args inargs = 0) sigs)
    with Not_found -> None

  let store_signature funname sign =
    let old_sigs = try StringMap.find funname !function_cache with Not_found -> [] in
    function_cache := StringMap.add funname (sign::old_sigs) !function_cache

  let eval zs exp man flow =
    let range = exp.erange in
    match ekind exp with
    | E_py_sum_call (f, args) ->
      (* les adresses ne changent pas in/out, on pourrait peut être bouger dans type? mais pour le polymorphisme?
         si l'env change ?
         **exceptions ?**

         faire un truc plus simple où l'on garde l'environnement courant ? ça permettrait d'activer le polymorphisme?
      *)
      let func = match ekind f with
        | E_function (User_defined func) -> func
        | _ -> assert false in
      Eval.eval_list man.eval args flow |>
      Eval.bind (fun args flow ->
          let cur = get_domain_env T_cur man flow in
          let in_args = List.map (extract_addrty cur) args in

          match find_signature func.fun_name in_args with
          | None ->
            man.eval ~zone:(Universal.Zone.Z_u, Z_any) (mk_call func args range) flow |>
            Eval.bind (fun eval_res flow ->
                man.eval ~zone:(Zone.Z_py, Zone.Z_py_obj) eval_res flow |>
                Eval.bind (fun eval_res flow ->
                    (* vérifier qu'on a les m^ token de flows qu'avant ? (modulo return) *)

                    (* c'est plus cur *)
                    let cur = man.get (Flow.get T_cur man.lattice flow) in
                    let in_args_out = List.map (extract_addrty cur) args in
                    let out = extract_addrty cur eval_res in
                    store_signature func.fun_name {in_args; in_args_out; out};

                    debug "in_args = %a@\nout_args = %a@\nout = %a@\n" pp_slist in_args pp_slist in_args_out pp_signature out;
                    Eval.singleton eval_res flow
                  )
              )
          | Some {in_args; in_args_out; out} ->
            let tmap = List.fold_left (fun tmap (a, pty) ->
                TD.add a pty tmap) (snd cur) in_args_out in
            let flow = set_domain_env T_cur (fst cur, tmap) man flow in
            Eval.singleton (mk_py_object (fst out, None) range) flow
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
