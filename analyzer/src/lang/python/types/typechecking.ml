open Mopsa
open Core.Sig.Domain.Lowlevel
open Addr
open Typing
open Addr_env
open Ast
open Universal.Ast
open MapExt

let name = "python.types.typechecking"

let opt_py_type_polymorphism = ref false

let () =
  register_domain_option name {
    key = "-py-type-polymorphism";
    category = "Python";
    doc = " enable type polymorphism";
    spec = ArgExt.Set opt_py_type_polymorphism;
    default = "false"
  }



module Domain =
struct
  (* TODO: lists and polymorphism? *)

  module AD = Addr_env.Domain
  module TD = Typing.Domain

  module Iter = Framework.Transformers.Domain.Intermediate.Sequence.Make(AD)(TD)

  type t = Iter.t
  let id = Iter.id

  let name = "python.types.typechecking"
  let debug fmt = Debug.debug ~channel:name fmt

  let interface = Iter.interface
  let bottom = Iter.bottom
  let top = Iter.top
  let is_bottom = Iter.is_bottom
  let print = Iter.print

  let simplified_man (bigman:('a, t, 's) Framework.Core.Sig.Stacked.Lowlevel.man) : ('a, t) man =
    { lattice = bigman.lattice;
      get = bigman.get;
      set = bigman.set;

      post = bigman.post;
      exec = bigman.exec;
      eval = bigman.eval;
      ask = bigman.ask;

      get_log = bigman.get_log;
      set_log = bigman.set_log
    }

  let subset (bigman:('a, t, 's) Framework.Core.Sig.Stacked.Lowlevel.man) uctx a a' =
    Iter.subset (bigman.get a) (bigman.get a'), a, a'

  let join (bigman:('a, t, 's) Framework.Core.Sig.Stacked.Lowlevel.man) uctx a a' =
    Iter.join (bigman.get a) (bigman.get a'), a, a'

  let meet (bigman:('a, t, 's) Framework.Core.Sig.Stacked.Lowlevel.man) uctx a a' =
    Iter.meet (bigman.get a) (bigman.get a'), a, a'

  let widen (bigman:('a, t, 's) Framework.Core.Sig.Stacked.Lowlevel.man) uctx a a' =
    Iter.widen uctx (bigman.get a) (bigman.get a'), a, a', true

  let merge _ _ _ _ = failwith "todo"

  let init prog bigman flow = Iter.init prog (simplified_man bigman) flow
  let exec zone stmt bigman flow = Iter.exec zone stmt (simplified_man bigman) flow
  let eval zs expr bigman flow = Iter.eval zs expr (simplified_man bigman) flow

  let ask : type r. r query -> ('a, t, 's ) Framework.Core.Sig.Stacked.Lowlevel.man -> 'a flow -> r option = fun query bigman flow ->
    let man = simplified_man bigman in
    match query with
    | Framework.Engines.Interactive.Q_print_var ->
      Some (
        fun fmt v ->
          let amap, tmap = get_domain_env T_cur man flow in
          let ret = ref [] in
          AD.AMap.iter (fun var addrs  ->
              if get_orig_vname var = v then
                AD.ASet.iter (fun addr ->
                    match addr with
                    | Def addr ->
                      if TD.TMap.mem addr tmap then
                        let ty = TD.TMap.find addr tmap in
                        ret := (fun fmt -> Format.fprintf fmt "%s ⇝ %a ⇝ %a" var.vname pp_addr addr TD.Polytypeset.print ty) :: !ret
                      else
                        ret := (fun fmt -> Format.fprintf fmt "%s ⇝ %a ⇝ notinmap" var.vname pp_addr addr) :: !ret
                    | Undef_local ->
                      ret := (fun fmt -> Format.fprintf fmt "%s ⇝ undef_local" var.vname) :: !ret
                    | Undef_global ->
                      ret := (fun fmt -> Format.fprintf fmt "%s ⇝ undef_global" var.vname) :: !ret
                  ) addrs
            ) amap;
          Format.fprintf fmt "@[<v>%a@]"
            (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
               (fun fmt pp -> pp fmt)
            ) !ret
      )
    | _ -> Iter.ask query man flow

  let refine _ _ _ = failwith "todo"



  (* let extract_types_aset (t: TD.t) (aset: AD.ASet.t) : TD.Polytypeset.t =
   *   AD.ASet.fold (fun addr acc ->
   *       match addr with
   *       | Def addr ->
   *         let to_join = match addr.addr_kind with
   *           (\* | A_py_var _ (\\* TODO: est-ce que c'est ok? *\\) *\)
   *           | A_py_instance _ -> (TD.TMap.find addr t)
   *           | A_py_class (c, b) ->
   *             let ty = Class (c, b) in
   *             TD.Polytypeset.singleton ty
   *           | A_py_module m ->
   *             TD.Polytypeset.singleton (Module m)
   *           | A_py_function f ->
   *             TD.Polytypeset.singleton (Function f)
   *           | _ -> Debug.warn "%a@\n" pp_addr addr;
   *             TD.Polytypeset.empty
   *         in
   *         TD.Polytypeset.union acc to_join
   *       | _ -> acc
   *     ) aset TD.Polytypeset.empty
   *
   * let extract_types (a: AD.t) (t: TD.t) (v: var) : TD.Polytypeset.t =
   *   extract_types_aset t (AD.AMap.find v a)
   *
   * module VarSet = SetExt.Make
   *     (struct
   *       type t = var
   *       let compare = compare_var
   *       let print = pp_var
   *     end
   *     )
   *
   * type partition = (VarSet.t * TD.Polytypeset.t) list
   *
   * type intersectedpartition = (VarSet.t * TD.Polytypeset.t * TD.Polytypeset.t) list
   *
   * let pp_i fmt (vs, tys1, tys2) =
   *   Format.fprintf fmt "[%a -> %a | %a]" (VarSet.fprint SetExt.printer_default pp_var) vs TD.Polytypeset.print tys1 TD.Polytypeset.print tys2
   *
   * let pp_ip fmt ip =
   *   Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n") pp_i fmt ip
   *
   * let pp_e fmt (vs, tys1) =
   *   Format.fprintf fmt "[%a -> %a]" (VarSet.fprint SetExt.printer_default pp_var) vs TD.Polytypeset.print tys1
   *
   * let pp_p fmt p =
   *   Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n") pp_e fmt p
   *
   * let create_partition (a: AD.t) (t: TD.t) : partition =
   *   AD.AMap.fold (fun var aset acc ->
   *       let tys = match aset with
   *         | TOP -> TD.Polytypeset.top
   *         | _ ->   extract_types_aset t aset in
   *         let sameclass, others = ListExt.partition (fun (_, tys') -> TD.Polytypeset.equal tys tys') acc in
   *         match sameclass with
   *         | [] -> (VarSet.singleton var, tys):: others
   *         | [(vars, _)] -> (VarSet.add var vars, tys):: others
   *         | _ -> assert false
   *     ) a []
   *
   * let intersect_partitions (p: partition) (p': partition) : intersectedpartition =
   *   let lift_left (p: partition) : intersectedpartition = List.map (fun (vars, tys) -> (vars, tys, TD.Polytypeset.empty)) p in
   *   let intersect_one (part: intersectedpartition) ((vars, tys): VarSet.t * TD.Polytypeset.t) : intersectedpartition =
   *     List.fold_left (fun acc (vars_part, tys_part, tys_other) ->
   *         let vars_int, vars_diff = VarSet.inter vars_part vars, VarSet.diff vars_part vars in
   *         let res_int  = vars_int, tys_part, TD.Polytypeset.union tys_other tys
   *         and res_diff = vars_diff, tys_part, tys_other in
   *         match VarSet.is_empty vars_int, VarSet.is_empty vars_diff with
   *         | true, true   -> acc
   *         | false, false -> res_int  :: res_diff :: acc
   *         | true, false  -> res_diff :: acc
   *         | false, true  -> res_int  :: acc
   *       ) [] part in
   *   List.fold_left (fun acc part' -> intersect_one acc part') (lift_left p) p'
   *
   * (\* join_agroup, join_akind should be defined as compare_addr, pp_addr *\)
   * let join_agroup ag1 ag2 =
   *   match ag1, ag2 with
   *   | G_all, G_all -> G_all
   *   | _ -> assert false
   *
   * let join_akind ak1 ak2 =
   *   match ak1, ak2 with
   *   | _ -> assert false
   *
   * let join_amode am1 am2 =
   *   match am1, am2 with
   *   | WEAK, _ | _, WEAK -> WEAK
   *   | STRONG, STRONG -> STRONG
   *
   * let join man a a' =
   *   if !opt_py_type_polymorphism then
   *     Framework.Core.Sig.Domain.Intermediate.lift_binop
   *       (fun (hd, tl) (hd', tl') ->
   *          match hd, hd' with
   *          | AD.AMap.Map.Top, _ | _, AD.AMap.Map.Top -> Iter.top
   *          | _ ->
   *            match tl, tl' with
   *            | TD.TMap.Map.Top, _ | _, TD.TMap.Map.Top -> Iter.top
   *            | _ ->
   *              let hd, hd' = AD.AMap.fold2o
   *                  (fun k a acc -> acc)
   *                  (fun k a acc -> acc)
   *                  (fun k as1 as2 (hd, hd') ->
   *                     if AD.ASet.cardinal as1 <= 1 && AD.ASet.cardinal as2 <= 1 then
   *                       let a1, a2 = match AD.ASet.choose as1, AD.ASet.choose as2 with
   *                         | Def a1, Def a2 -> a1, a2
   *                         | _ -> failwith "otod" in
   *                       let addr_12 = {addr_group = join_agroup a1.addr_group a2.addr_group;
   *                                      addr_kind = join_akind a1.addr_kind a2.addr_kind;
   *                                      addr_mode = join_amode a1.addr_mode a2.addr_mode} in
   *                       (hd, hd')
   *                     else
   *                       failwith "todo"
   *                  ) hd hd' (hd, hd') in
   *
   *              debug "hd, tl = %a, %a@\n@\nhd', tl' = %a, %a@\n" AD.print hd TD.print tl AD.print hd' TD.print tl';
   *              let p = create_partition hd tl
   *              and p' = create_partition hd' tl' in
   *              let ip = intersect_partitions p p' in
   *              let ip = List.filter (fun (vars, ty1, ty2) -> not (TD.Polytypeset.is_top ty1) && not (TD.Polytypeset.is_top ty2) && VarSet.cardinal vars > 1 && TD.Polytypeset.cardinal ty1 > 0 && TD.Polytypeset.cardinal ty2 > 0 && not (TD.Polytypeset.cardinal ty1 = 1 && TD.Polytypeset.equal ty1 ty2))
   *                  ip in
   *              debug "interesting partitions:@[@\n%a@]@\n" pp_ip ip;
   *              (\* FIXME: is that necessary? *\)
   *              let jhd = AD.join hd hd' and jtl = TD.join tl tl' in
   *              let rhd, rabsheap = List.fold_left (fun (rhd, rabsheap) (vars, ty1, ty2) ->
   *                  let alpha = get_fresh_a_py_var () in
   *                  let addr = {addr_group = G_all; addr_kind = A_py_var alpha; addr_mode = WEAK} in
   *                  let types = TD.Polytypeset.join ty1 ty2 in
   *                  let rhd = VarSet.fold (fun var rhd -> AD.AMap.add var (AD.ASet.singleton (Def addr)) rhd) vars rhd in
   *                  let rabsheap = TD.TMap.add addr types rabsheap in
   *                  (rhd, rabsheap)
   *                ) (jhd, jtl) ip in
   *              debug "result is %a@\n%a@\n" AD.print rhd TD.print rabsheap;
   *              rhd, rabsheap
   *       ) man a a'
   *   else Iter.join man a a'
  *)



end

let () = Framework.Core.Sig.Stacked.Lowlevel.register_stack (module Domain)
