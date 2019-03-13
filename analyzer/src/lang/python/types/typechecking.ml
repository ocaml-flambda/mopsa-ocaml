(* TODO: using union-find is probably useless, maybe arrays instead? *)
open Mopsa
open Addr
open Typing
open Addr_env
open Ast
open Universal.Ast
open Unionfind

type addr_kind +=
    A_py_var of int

let pyvarcounter = ref (-1)

let get_fresh_a_py_var () =
  incr pyvarcounter;
  !pyvarcounter

let greek_of_int =
  let list = ["α"; "β"; "γ"; "δ"; "ε"; "ζ"; "η"; "θ"; "ι"; "κ"; "λ"; "μ"; "ν"; "ξ"; "ο"; "π"; "ρ"; "σ"; "τ"; "υ"; "φ"; "χ"; "ψ"; "ω"] in
  let listn = List.length list in
  fun n ->
    let letter = List.nth list (n mod listn) in
    if n < listn then letter
    else Format.sprintf "%s(%d)" letter (n / listn)

let () =
  Format.(
    register_addr {
      print =
        (fun default fmt a -> match a with
           | A_py_var a -> Format.fprintf fmt "%s" (greek_of_int a)
           | _ -> default fmt a
        );
      compare =
        (fun default a1 a2 ->
           match a1, a2 with
           | A_py_var v1, A_py_var v2 -> Pervasives.compare v1 v2
           | _ -> default a1 a2);
    }
  )



module Domain =
struct
  (* TODO: lists? *)

  module AD = Addr_env.Domain
  module TD = Typing.Domain

  module Iter = Framework.Domains.Iter.Make(AD)(TD)
  include Iter

  let name = "python.types.typechecking"
  let debug fmt = Debug.debug ~channel:name fmt

  module VarEls =
  struct
    type t = var
    let compare = compare_var
    let print = pp_var
  end

  module Uf = Unionfind(VarEls)

  let extract_type_aset (a: AD.t) (t: TD.t) (aset: AD.ASet.t) : Typing.polytype option =
    if AD.ASet.cardinal aset = 1 then
      let addr = AD.ASet.choose aset in
      match addr with
      | Def addr ->
        begin match addr.addr_kind with
        | A_py_instance _ ->
          let tys = TD.TMap.find addr t.abs_heap in
          if TD.Polytypeset.cardinal tys = 1 then
            Some (TD.Polytypeset.choose tys)
          else
            None
        | A_py_class (c, b) ->
          let ty = Class (c, b) in Some ty
        | A_py_module m ->
          Some (Module m)
        | A_py_function f ->
          Some (Function f)
        | _ -> Debug.warn "%a@\n" pp_addr addr; None
        end
      | _ -> None
    else
      None

  let extract_type (a: AD.t) (t: TD.t) (v: var) : Typing.polytype option =
    (* debug "extracting type of var %a in %a %a@\n" pp_var v AD.print a TD.print t; *)
    extract_type_aset a t (AD.AMap.find v a)

  let extract_types_aset (t: TD.t) (aset: AD.ASet.t) : TD.Polytypeset.t =
    let annot = Annotation.empty in
    AD.ASet.fold (fun addr acc ->
        match addr with
        | Def addr ->
          let to_join = match addr.addr_kind with
            | A_py_instance _ ->
  (TD.TMap.find addr t.abs_heap)
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
          TD.Polytypeset.union annot acc to_join
        | _ -> acc
      ) aset TD.Polytypeset.empty

  let extract_types (a: AD.t) (t: TD.t) (v: var) : TD.Polytypeset.t =
    extract_types_aset t (AD.AMap.find v a)


  let sametype_vars (a: AD.t) (t: TD.t) : Uf.t =
    let module TypeMap = MapExt.Make(struct
        type t = Typing.polytype
        let compare = Typing.compare_polytype
        let print = Typing.pp_polytype
      end) in
    let map = TypeMap.empty in
    let map = AD.AMap.fold (fun var aset acc ->
        let ty = extract_type_aset a t aset in
        match ty with
        | None -> acc
        | Some ty ->
          let old = if TypeMap.mem ty acc then TypeMap.find ty acc else [] in
          TypeMap.add ty (var::old) acc) a map in
    let domain = AD.AMap.fold (fun var _ acc -> var::acc) a [] in
    let equivs = TypeMap.fold (fun ty vars acc -> vars :: acc) map [] in
    debug "uf = %a@\n" Uf.print_llist equivs;
    Uf.from_llist domain equivs


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
        let tys = extract_types_aset t aset in
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
          let res_int  = vars_int, tys_part, TD.Polytypeset.union Annotation.empty tys_other tys
          and res_diff = vars_diff, tys_part, tys_other in
          match VarSet.is_empty vars_int, VarSet.is_empty vars_diff with
          | true, true   -> acc
          | false, false -> res_int  :: res_diff :: acc
          | true, false  -> res_diff :: acc
          | false, true  -> res_int  :: acc
        ) [] part in
    List.fold_left (fun acc part' -> intersect_one acc part') (lift_left p) p'

  let join annot (hd, tl) (hd', tl') =
    let p = create_partition hd tl
    and p' = create_partition hd' tl' in
    let ip = intersect_partitions p p' in
    let ip = List.filter (fun (vars, ty1, ty2) -> VarSet.cardinal vars > 1 && TD.Polytypeset.cardinal ty1 > 0 && TD.Polytypeset.cardinal ty2 > 0) (* && not (TD.Polytypeset.equal ty1 ty2)) *)
        ip in
    debug "interesting partitions:@[@\n%a@]@\n" pp_ip ip;
    (* FIXME: is that necessary? *)
    let jhd, jtl = Iter.join annot (hd, tl) (hd', tl') in
    let rhd, rabsheap = List.fold_left (fun (rhd, rabsheap) (vars, ty1, ty2) ->
        let alpha = get_fresh_a_py_var () in
        let addr = {addr_uid = alpha; addr_kind = A_py_var alpha; addr_mode = WEAK} in
        let types = TD.Polytypeset.join annot ty1 ty2 in
        let rhd = VarSet.fold (fun var rhd -> AD.AMap.add var (AD.ASet.singleton (Def addr)) rhd) vars rhd in
        let rabsheap = TD.TMap.add addr types rabsheap in
        (rhd, rabsheap)
      ) (jhd, jtl.abs_heap) ip in
    let rtl = {jtl with abs_heap = rabsheap} in
    debug "result is %a@\n%a@\n" AD.print rhd TD.print rtl;
    rhd, {jtl with abs_heap = rabsheap}

end

let () =
  register_domain (module Domain)
